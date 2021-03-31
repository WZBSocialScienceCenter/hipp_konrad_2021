"""
Prepare user profile data for further processing.

This loads the raw GitHub user data structure and transforms it to a CSV file. It used to handle GitHub user
contributions data, too, but this is now collected separately via `scrape_contribs.py`.

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

import pickle

import pandas as pd

#%%

COLLECTION_DATE = '2020-06-17'
INPUT_FILE = 'data/collected_users_%s.pickle' % COLLECTION_DATE
OUTPUT_USERS_FILE = 'data/users_%s.csv' % COLLECTION_DATE
OUTPUT_CONTRIBS_FILE = 'data/contribs_%s.csv' % COLLECTION_DATE
REQUIRE_CONTRIBS = False

PROFILE_FIELDS = [
    'id',
    'login',
    'name',
    'company',
    'blog',
    'location',
    'email',
    'hireable',
    'bio',
    'public_repos',
    'public_gists',
    'followers',
    'following',
    'created_at',
    'updated_at'
]

#%%

print('loading data from', INPUT_FILE)

with open(INPUT_FILE, 'rb') as f:
    colldata = pickle.load(f)

#%%

print('processing data')

profiles_rows = []
contribs_rows = []
known_users = set()
for sampledate, daydata in colldata.items():
    for user in daydata['data']:
        if user['id'] in known_users or user['type'] != 'User' or not user['name'] or ' ' not in user['name']:
            continue

        try:
            contribdata = user['contributions'].get('data', {})\
                .get('user', {})\
                .get('contributionsCollection', {})\
                .get('contributionCalendar', {})
        except AttributeError:
            contribdata = None

        if contribdata or not REQUIRE_CONTRIBS:
            row = [user[f] for f in PROFILE_FIELDS]
            if contribdata:
                row.append(contribdata['totalContributions'])
            else:
                row.append(None)
            profiles_rows.append(row)

            if contribdata:
                for contribweek in contribdata['weeks']:
                    for contribday in contribweek['contributionDays']:
                        contribs_rows.append([user['id'], contribday['date'], contribday['contributionCount']])

        known_users.add(user['id'])

del colldata

#%%

profiles = pd.DataFrame(profiles_rows, columns=PROFILE_FIELDS + ['total_contrib'])
profiles['collection_date'] = COLLECTION_DATE
print('got data for %d user profiles' % len(profiles))

assert len(profiles) == len(profiles.login.unique())
assert len(profiles) == len(profiles.id.unique())
assert sum(profiles.name.isna()) == 0
assert sum(profiles.name.str.len() == 0) == 0
assert sum(~profiles.name.str.contains(' ')) == 0

print('storing user profile data to', OUTPUT_USERS_FILE)
profiles.to_csv(OUTPUT_USERS_FILE, index=False)

output_files = [OUTPUT_USERS_FILE]

#%%

if contribs_rows:
    contribs = pd.DataFrame(contribs_rows, columns=['user_id', 'date', 'num_contribs'])
    print('got %d contributions rows' % len(contribs))

    assert set(contribs.user_id.unique()) == set(profiles.id.unique())

    print('storing contributions data to', OUTPUT_CONTRIBS_FILE)
    contribs.to_csv(OUTPUT_CONTRIBS_FILE, index=False)

    output_files.append(OUTPUT_CONTRIBS_FILE)

#%%

print('done.')
