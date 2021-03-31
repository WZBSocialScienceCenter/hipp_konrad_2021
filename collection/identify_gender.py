"""
Try to identify the gender for each user profile from the given name in the `name` field by using the
[genderize.io](https://genderize.io/) API. If location data from `geocode_locations.py` is given for a user account,
make an additional query using also the country code for country-specific naming conventions. Also, in some
(specifically Eastern Asian) countries it is common to put the given name last. For these countries, also query the API
with the last name.

The obtained data can be linked with the user profile data via `id` field.

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

import pickle
from time import sleep
from collections import defaultdict

import numpy as np
import pandas as pd
pd.set_option('display.max_columns', 100)
pd.set_option('display.width', 180)
import requests.exceptions
from genderize import Genderize, GenderizeException

from accountdata import GENDERIZE_API_KEY
from utils import store_pickle, load_data_from_pickle

#%%

COLLECTION_DATE = '2020-06-17'
INPUT_FILE = 'data/users_%s.csv' % COLLECTION_DATE
INPUT_FILE_GEOCODED = 'data/geocoded_locations_%s.pickle' % COLLECTION_DATE

OUTPUT_PICKLE = 'data/users_genderizer_results_%s.pickle' % COLLECTION_DATE

CACHE_FILE = 'cache/genderize.cache'

MAX_REQUESTS_TIMEOUT_SEC = 10

GIVENNAMES_POSSIBLY_LAST = {
    'CN', 'VN', 'TW', 'HK', 'KR', 'KH', 'JP'
}

REPLACE_GEOCODES = {
    'UK': 'GB',
    'EL': 'GL'
}

DEFAULT_WAIT_TIME_SEC = 15 * 60

#%%

def wait(wait_sec):
    print('>>> will wait for %d sec (%dh %dm)'
          % (wait_sec, wait_sec // 3600, (wait_sec - (wait_sec // 3600) * 3600) // 60))
    sleep(wait_sec)

#%%

print('loading user data from', INPUT_FILE)
users = pd.read_csv(INPUT_FILE)

#%%

genderizer_results = load_data_from_pickle(CACHE_FILE, {})

#%%

print('loading geocoding results from', INPUT_FILE_GEOCODED)
with open(INPUT_FILE_GEOCODED, 'rb') as f:
    geocoding_res = pickle.load(f)

#%%

g = Genderize(api_key=GENDERIZE_API_KEY, timeout=MAX_REQUESTS_TIMEOUT_SEC)

#%%

print('processing input data')

queries_per_user = {}
user_names_rows = []
for _, u in users.iterrows():
    countrycode = None
    if not pd.isna(u['location']) and u['location']:
        geo = geocoding_res.get(u['location'], [])
        if geo:
            geo_addr_comp = geo[0].get('address_components', [])
            for comp in geo_addr_comp:
                comptypes = comp.get('types', [])
                if 'country' in comptypes and 'political' in comptypes and 'short_name' in comp:
                    countrycode = REPLACE_GEOCODES.get(comp['short_name'], comp['short_name'])
                    break

    nameparts = u['name'].split(' ')
    first = nameparts[0].strip()
    last = nameparts[-1].strip()

    user_names_rows.append([u['id'], u['name'], first, last, countrycode])
    queries_per_user[u['id']] = []

    if countrycode and countrycode in GIVENNAMES_POSSIBLY_LAST:
        queryitems = (first, last)
    else:
        queryitems = (first, )

    for npart in queryitems:
        if len(npart) > 1:
            queries_per_user[u['id']].append((None, npart))
            if countrycode and len(countrycode) == 2:
                queries_per_user[u['id']].append((countrycode, npart))


#%%

queries_genderizer = defaultdict(set)
for user_queries in queries_per_user.values():
    for country_id, name in user_queries:
        queries_genderizer[country_id].add(name)

#%%

queries_genderizer_chunks = {}
for country_id, names in queries_genderizer.items():
    chunks = np.array_split(np.array(list(names)), len(names) // 10 + 1)
    assert all(l <= 10 for l in map(len, chunks))
    queries_genderizer_chunks[country_id] = chunks

assert queries_genderizer_chunks.keys() == queries_genderizer.keys()

#%%

print('querying genderizer')

for i_country, (country_id, chunks) in enumerate(queries_genderizer_chunks.items()):
    print('> country %d/%d: %s' % (i_country + 1, len(queries_genderizer_chunks), country_id))
    i_chunk = 0
    while i_chunk < len(chunks):
        names = [name for name in chunks[i_chunk] if (country_id, name) not in genderizer_results]  # was np array
        if names:
            print('>> chunk %d/%d: will query %d new names (%s)'
                  % (i_chunk+1, len(chunks), len(names), ', '.join(names)))

            try:
                res = g.get(names, country_id=country_id, retheader=True)
            except (GenderizeException, requests.exceptions.ConnectionError,
                    requests.exceptions.Timeout, requests.exceptions.ReadTimeout) as exc:
                print('>>> got exception:', exc)
                wait(DEFAULT_WAIT_TIME_SEC)
                continue   # do not increment i_chunk

            for name, gen_res in zip(names, res['data']):
                assert name == gen_res['name']
                assert (country_id, name) not in genderizer_results
                genderizer_results[(country_id, name)] = gen_res

            requests_remaining = int(res['headers']['X-Rate-Limit-Remaining'])
            limit_reset_sec = int(res['headers']['X-Rate-Reset'])
            print('>> %d requests remaining' % requests_remaining)

            if requests_remaining <= 3:  # better be safe!
                wait(limit_reset_sec)
        else:
            print('>> no new names left in this chunk')

        i_chunk += 1

    with open(CACHE_FILE, 'wb') as f:
        pickle.dump(genderizer_results, f)

#%%

print('generating results')

final_results = defaultdict(list)
for user_id, queries in queries_per_user.items():
    for country_name in queries:
        res = genderizer_results.get(country_name, None)
        if res:
            final_results[user_id].append(res)

#%%

print('storing results for %d users' % len(final_results))

store_pickle(final_results, OUTPUT_PICKLE, 'genderizer results')

print('done.')
