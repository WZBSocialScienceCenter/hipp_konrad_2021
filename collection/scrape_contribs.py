"""
Obtain daily contribution counts per user for the years 2014 to 2020.

This is done via web scraping since the GitHub API only returns data for the last 365 days.

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

import re
import signal
from time import sleep

import pandas as pd
import requests
from bs4 import BeautifulSoup

from utils import load_data_from_pickle, store_pickle

#%%

# YEARS = (2020, 2017, 2016, 2015, 2014)
YEARS = (2020, )                            # specify the years to collect
COLLECTION_DATE = '2020-08-04'
INPUT_USERS_CSV = 'data/concat_users.csv'
#OUTPUT_CSV = 'data/contribs_scraped.csv'
OUTPUT_PICKLE_FMT = 'data/contribs_scraped_%s_%d.pickle'
#OUTPUT_ZIP = 'data/contribs_scraped.zip'
CACHE_FILE_RESPONSES = None   # disable caching of complete response
# CACHE_FILE_RESPONSES = 'cache/scrape_contibs_http_resp.pickle'
CACHE_FILE_RESULTS = 'cache/scrape_contribs_results_new_fmt.pickle'
SLEEP_TIME_SEC = 0
MAX_REQUESTS_TIMEOUT_SEC = 10
STORE_TO_CACHE_ITERATION = 500   # store to cache on every i'th iteration
SCRAPE_DAY_DETAILS = False  # if True, fetch data about how many contrib. were made in private repos; takes v. long!

#%%

pttrn_count_priv_contrib = re.compile(r'(\d+)\s+contributions\s+in\s+private')
abort_script = False

#%%

def get_n_private_contribs(login, contrib_date, cache):
    if cache is not None and (login, contrib_date) in cache:
        print('>> loading from cache: %s / %s' % (login, contrib_date))
        contrib_day_resp_content = cache[(login, contrib_date)]
        contrib_day_resp_ok = True
        from_cache = True
    else:
        print('>> querying: %s / %s' % (login, contrib_date))
        try:
            contrib_day_resp = requests.get(
                'https://github.com/{login}?from={date}&to={date}&tab=overview'.format(login=login, date=contrib_date),
                headers={'X-Requested-With': 'XMLHttpRequest'},
                timeout=MAX_REQUESTS_TIMEOUT_SEC
            )
            if SLEEP_TIME_SEC > 0:
                sleep(SLEEP_TIME_SEC)
        except IOError:
            print('>> IO error')
            return None, False

        contrib_day_resp_content = contrib_day_resp.content
        contrib_day_resp_ok = contrib_day_resp.ok
        from_cache = False

    if contrib_day_resp_ok:
        if not from_cache and cache is not None:
            cache[(login, contrib_date)] = contrib_day_resp_content

        contribs_day_soup = BeautifulSoup(contrib_day_resp_content, features="html.parser")

        n_private = 0
        for elem in contribs_day_soup.select('.profile-rollup-wrapper span.f4'):
            m = pttrn_count_priv_contrib.search(elem.text.strip())
            if m:
                assert n_private == 0
                n_private = int(m.group(1))
                assert n_private > 0

        print('>> OK')
        return n_private, not from_cache
    else:
        print('>> not OK')
        return None, not from_cache

def handle_abort(signum, frame):
    global abort_script
    print('received signal %d – aborting script...' % signum)
    abort_script = True


for signame in ('SIGINT', 'SIGHUP', 'SIGTERM'):
    sig = getattr(signal, signame, None)
    if sig is not None:
        signal.signal(sig, handle_abort)


#%%

print('reading user data from', INPUT_USERS_CSV)
users = pd.read_csv(INPUT_USERS_CSV, usecols=['id', 'login'])
#users = users.head(200)

n_users = len(users)

print(' > loaded %d user accounts' % n_users)

#%%

if CACHE_FILE_RESPONSES:
    cached_responses = load_data_from_pickle(CACHE_FILE_RESPONSES, {'contribs_overview': {}, 'contribs_daily': {}})
else:
    cached_responses = None

contribs_data_per_user = load_data_from_pickle(CACHE_FILE_RESULTS, {})

for i_user, row in users.iterrows():
    user_id = row.id
    login = row.login
    del row

    store_to_cache = False

    i_year = 0
    while i_year < len(YEARS) and not abort_script:
        year = YEARS[i_year]
        if (user_id, year) in contribs_data_per_user:
            print('> [%d/%d] already got results: %s / %d' % (i_user+1, n_users, login, year))
        else:
            if cached_responses is not None and (login, year) in cached_responses['contribs_overview']:
                print('> [%d/%d] loading from cache: %s / %d' % (i_user+1, n_users, login, year))
                contribs_resp_content = cached_responses['contribs_overview'][(login, year)]
                from_cache = True
                contribs_resp_ok = True
            else:
                print('> [%d/%d] querying: %s / %d' % (i_user+1, n_users, login, year))
                try:
                    contribs_resp = requests.get('https://github.com/users/{login}/contributions?to={year}-12-31'
                                                 .format(login=login, year=str(year)),
                                                 timeout=MAX_REQUESTS_TIMEOUT_SEC)
                    if SLEEP_TIME_SEC > 0:
                        sleep(SLEEP_TIME_SEC)
                except IOError:
                    print('>> IO error')
                    continue   # do not increment i_year

                contribs_resp_content = contribs_resp.content
                from_cache = False
                contribs_resp_ok = contribs_resp.ok
                store_to_cache = True

            if contribs_resp_ok:
                print('> OK')

                if not from_cache and cached_responses is not None:
                    cached_responses['contribs_overview'][(login, year)] = contribs_resp_content

                contribs_soup = BeautifulSoup(contribs_resp_content, features="html.parser")

                days_elems = contribs_soup.select('.js-calendar-graph-svg rect.day')

                if not 365 <= len(days_elems) <= 371:
                    print('> WARNING: invalid number of days: %d' % len(days_elems))

                contrib_days_rows = [[day.attrs['data-date'], int(day.attrs['data-count'])]
                                     for day in days_elems]

                contribs_data = pd.DataFrame(contrib_days_rows, columns=['date', 'count'])
                contribs_data['user_id'] = user_id

                contribs_data = contribs_data.loc[contribs_data.date.str.startswith(str(year)),
                                                  ['user_id', 'date', 'count']]
                print('> got data for %d days' % len(contribs_data))

                if not 365 <= len(contribs_data) <= 366:
                    print('> WARNING: invalid number of contrib. data: %d' % len(contribs_data))

                if SCRAPE_DAY_DETAILS:
                    count_private = []
                    for _, row in contribs_data.iterrows():
                        if row['count'] > 0:
                            n_private, store_to_cache = get_n_private_contribs(
                                login, row['date'],
                                cached_responses['contribs_overview'] if cached_responses is not None else None
                            )
                        else:
                            n_private = 0
                            store_to_cache = True

                        count_private.append(n_private)

                    assert len(count_private) == len(contribs_data)
                    contribs_data['count_private'] = count_private

                contribs_data_per_user[(user_id, year)] = {col: contribs_data[col].to_list()
                                                           for col in contribs_data.columns if col != 'user_id'}
            else:
                print('> not OK')

        i_year += 1

    if (store_to_cache and (i_user+1) % STORE_TO_CACHE_ITERATION == 0) or i_user == n_users - 1 or abort_script:
        if CACHE_FILE_RESPONSES:
            store_pickle(cached_responses, CACHE_FILE_RESPONSES, message_label='scraping responses')

        store_pickle(contribs_data_per_user, CACHE_FILE_RESULTS, message_label='processed')

    if abort_script:
        exit(1)

#%%

del cached_responses
del users

print('generating full dataset')

# need to do all the following to avoid huge memory peaks:

print('> list')
contribs_data_per_user_list = list(contribs_data_per_user.items())
del contribs_data_per_user

n_items = len(contribs_data_per_user_list)
print('> %d items in list' % n_items)

print('> dataframe')
i_added = 0
i_files = 1
partialdfs = []
while True:
    if i_added % 10000 == 0 or len(contribs_data_per_user_list) == 0:
        print('>>', round(i_added / n_items * 100, 2), '%')
        if partialdfs:
            full = pd.concat(partialdfs, sort=False)
            full.to_pickle(OUTPUT_PICKLE_FMT % (COLLECTION_DATE, i_files))
            partialdfs = []
            i_files += 1

    if len(contribs_data_per_user_list) > 0:
        (user_id, year), usercontribs = contribs_data_per_user_list.pop()
        n_vals = len(next(iter(usercontribs.values())))
        datacolumns = {
            'user_id': [user_id] * n_vals
        }
        for col, vals in usercontribs.items():
            datacolumns[col] = vals

        partialdfs.append(pd.DataFrame(datacolumns))

        i_added += 1
    else:
        break

print('done.')
