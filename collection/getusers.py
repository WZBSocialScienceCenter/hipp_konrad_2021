"""
Fetch user profile data from GitHub that were created at the sampled dates.

Call script as:

    python getusers.py <inputfile|date> [countryqueriesfile]

where

- <inputfile|date> is a required argument with either the CSV file of sampled dates (data/sampled_dates*.csv) or
  a single date in Y-m-d format (used for debugging)
- [countryqueriesfile] is an optional argument pointing to a CSV file of search queries (data/city_searches.csv) to
  search for specific locations

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

import sys
import signal
from itertools import product
from time import sleep
from datetime import datetime

import requests
import requests.exceptions
import pandas as pd
from github import Github
from github.GithubException import GithubException, RateLimitExceededException

from accountdata import GITHUB_ACCESS_TOKEN
from utils import store_pickle, load_data_from_pickle, sleep_on_exc

#%%

OUTPUT_PICKLE = 'data/collected_users_2020-06-17.pickle'
MAX_REQUESTS_TIMEOUT_SEC = 10
MAX_FATAL_ERRORS = 10
DEFAULT_WAIT_TIME_CONN_ERROR_MINUTES = 1
STORE_AFTER_EACH_PAGE = False
FETCH_CONTRIBS = False    # set to False because we use scrape_contribs.py to fetch older contributions, too

QUERY_USER_CONTRIBS = """query {
    user(login: "%s") {
        contributionsCollection {
            contributionCalendar {
                totalContributions weeks {
                    contributionDays {
                        contributionCount
                        date
                    }
                }
            }
        }
    }
}"""

#%%

g = Github(GITHUB_ACCESS_TOKEN, timeout=MAX_REQUESTS_TIMEOUT_SEC)
abort_script = False

#%%


def fetch_contributions(username):
    resp = requests.post(
        'https://api.github.com/graphql',
        json={"query": QUERY_USER_CONTRIBS % username},
        headers={'Authorization': 'bearer ' + GITHUB_ACCESS_TOKEN},
        timeout=MAX_REQUESTS_TIMEOUT_SEC
    )

    return resp.status_code, resp.json()


def user_data(user):
    userdata = user.raw_data
    keys = [k for k in userdata.keys() if k == 'avatar_url' or not k.endswith('_url')]
    return {k: userdata[k] for k in keys}


def store_output(data):
    user_stats = {(u['login'], len(u.get('contributions', [])))
                  for dailydata in data.values()
                  for u in dailydata['data']}
    print('got %d user profiles so far / %d with contributions data' %
          (len(user_stats), sum(map(lambda x: x[1], user_stats))))

    store_pickle(data, OUTPUT_PICKLE, message_label='user')


def handle_abort(signum, frame):
    global abort_script
    print('received signal %d – aborting script...' % signum)
    abort_script = True


for signame in ('SIGINT', 'SIGHUP', 'SIGTERM'):
    sig = getattr(signal, signame, None)
    if sig is not None:
        signal.signal(sig, handle_abort)

#%%

# https://help.github.com/en/github/searching-for-information-on-github/searching-users
# https://pygithub.readthedocs.io/en/latest/github.html?highlight=search_users#github.MainClass.Github.search_users

#%%

if len(sys.argv) < 2:
    print('call script as: %s <CSV file of sampled dates / string for single sample date> [CSV file of country search '
          'queries]',
          file=sys.stderr)
    exit(1)

scriptarg = sys.argv[1]
country_searches = [None]
country_search_is_city = [None]

if scriptarg.endswith('.csv'):
    print('loading sample dates from', scriptarg)
    sampledates = pd.read_csv(scriptarg)['date'].to_list()
    print('loaded %d sample dates' % (len(sampledates)))
else:
    print('using single sample date', scriptarg)
    sampledates = [scriptarg]

if len(sys.argv) >= 3:
    print('loading country search queries from', sys.argv[2])
    country_searches_df = pd.read_csv(sys.argv[2])
    country_searches = country_searches_df['country'].to_list()
    if len(country_searches_df.columns) > 1:
        country_search_is_city = country_searches_df['is_city'].to_list()
    else:
        country_search_is_city = None
    print('loaded %d country search queries' % (len(country_searches)))

collected_user_data = load_data_from_pickle(OUTPUT_PICKLE, {})

#%%

known_users = {u['login'] for dailydata in collected_user_data.values() for u in dailydata['data']}
input_changed = False

#%%

for i_country_query, country_query in enumerate(country_searches):
    country_query_is_city = None
    if country_query:
        country_query_is_city = country_search_is_city is not None and country_search_is_city[i_country_query]
        print('country query %d/%d: %s (is_city: %d)' %
              (i_country_query + 1, len(country_searches), country_query, country_query_is_city))

    if country_query_is_city is True:
        country_sampledates = [None]
    else:
        country_sampledates = sampledates

    for i_sample_day, sample_day in enumerate(country_sampledates):
        users_search_query = 'repos:>0'

        if sample_day:
            print('sample day %d/%d: %s' % (i_sample_day+1, len(country_sampledates), sample_day))
            users_search_query += ' created:%s' % sample_day
            assert datetime.strptime(sample_day, '%Y-%m-%d').strftime('%Y-%m-%d') == sample_day,\
                'check correct date format'
        else:
            print('will not query account creation date')

        if country_query:
            users_search_query += ' location:%s' % country_query

        for sort_field, sort_dir in product(('joined', 'followers'), ('asc', 'desc')):
            print('searching users with query "%s" / sort: %s %s' % (users_search_query, sort_field, sort_dir))

            if country_query:
                collected_data_key = country_query
                if sample_day:
                    collected_data_key += ' ' + sample_day
            else:
                if not sample_day:
                    raise RuntimeError('there must be a sample_day given, when there is no country_query')

                collected_data_key = sample_day

            if collected_data_key not in collected_user_data:
                collected_user_data[collected_data_key] = {
                    'completed_searches': [],
                    'data': []
                }
                input_changed = True

            search_type = sort_field + '_' + sort_dir
            if search_type in collected_user_data[collected_data_key]['completed_searches'] \
                    or 'ALL' in collected_user_data[collected_data_key]['completed_searches']:
                print('> skip search (already completed)')
                continue

            users = g.search_users(users_search_query, sort=sort_field, order=sort_dir)

            i_page = 0
            i_user = 0
            limits = None
            n_fatal_errors = 0
            n_users = 1    # will be set to proper value on first iteration

            while not abort_script and i_user < n_users:  # iterate through pages
                try:
                    print('> page %d' % (i_page+1))
                    limits = g.get_rate_limit()
                    print('>> limits: %d search, %d core, %d graphql' %
                          (limits.search.remaining, limits.core.remaining, limits.graphql.remaining))
                    page = users.get_page(i_page)
                    n_users = users.totalCount
                    i_page_user = 0

                    # iterate through user profiles in pages
                    while not abort_script and i_page_user < len(page) and i_user < n_users:
                        user = page[i_page_user]
                        print('>> user %d/%d' % (i_user+1, n_users))
                        if user.login in known_users:
                            print('>>> already fetched data for user', user.login)
                        else:
                            profile = user_data(user)
                            profile['contributions'] = {}

                            if profile['name'] and ' ' in profile['name']:
                                if FETCH_CONTRIBS:
                                    print('>>> fetching contributions for user %s (%s)' %
                                          (profile['login'], profile['name']))
                                    contrib_status, contrib_data = fetch_contributions(user.login)

                                    if contrib_status == 200:
                                        profile['contributions'] = contrib_data
                                        print('>>> ok')
                                    elif contrib_status == 403:
                                        raise RateLimitExceededException(contrib_status, contrib_data)
                                    else:
                                        print('>>> unknown error when fetching contributions: %d / %s'
                                              % (contrib_status, str(contrib_data)))
                                        profile['no_contributions_reason'] = 'error when fetching contributions'
                                        n_fatal_errors += 1

                                collected_user_data[collected_data_key]['data'].append(profile)
                                input_changed = True
                                known_users.add(profile['login'])
                            else:
                                profile['no_contributions_reason'] = 'name not valid'
                                print('>>> user %s does not have a valid name: "%s"' %
                                      (profile['login'], profile['name']))

                        i_page_user += 1
                        i_user += 1
                except RateLimitExceededException:
                    sleep_on_exc(limits).sleep()
                    continue   # cont. with same page and same user
                except IOError as exc:
                    n_fatal_errors += 1
                    print('>> got request error:', str(exc))
                    print('>> will wait %d minutes' % DEFAULT_WAIT_TIME_CONN_ERROR_MINUTES)
                    sleep(DEFAULT_WAIT_TIME_CONN_ERROR_MINUTES * 60)
                    continue
                except GithubException:  # num. pages exceeded
                    print('>> page could not be fetched')
                    break
                finally:
                    if input_changed and STORE_AFTER_EACH_PAGE:
                        store_output(collected_user_data)
                        input_changed = False

                if n_fatal_errors >= MAX_FATAL_ERRORS:
                    print('>> maximum number of fatal errors reached:', n_fatal_errors)
                    break

                i_page += 1

            collected_user_data[collected_data_key]['completed_searches'].append(search_type)
            store_output(collected_user_data)
            input_changed = False

            # if n_users < 1000 then we already got all users and we don't need to re-run search with different sorting
            if n_users < 1000:
                collected_user_data[collected_data_key]['completed_searches'].append('ALL')

            if abort_script or n_users < 1000:
                break

        if input_changed or abort_script or i_sample_day >= len(country_sampledates) - 1:
            store_output(collected_user_data)

        print('')

        if abort_script:
            break

    if abort_script:
        break

#%%

print('done.')