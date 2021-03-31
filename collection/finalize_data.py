"""
Take intermediate raw data from `prepare_collected_data.py`, `geocode_locations.py` and `identify_gender.py`, preprocess
and join it to arrive at a final user profile dataset.

For the gender prediction data, only set the gender of a user profile when the best prediction has at least 80%
probability from at least 100 entries in the genderize.io database.

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

import pickle

import pandas as pd

pd.set_option('display.max_columns', 100)
pd.set_option('display.width', 180)

#%%
FILE_SUFFIX = '_2020-06-17'
INPUT_FILE_USERS = 'data/users%s.csv' % FILE_SUFFIX
INPUT_FILE_GEOCODED = 'data/geocoded_locations%s.pickle' % FILE_SUFFIX
INPUT_FILE_GENDERIZER = 'data/users_genderizer_results%s.pickle' % FILE_SUFFIX
OUTPUT_FILE = 'data/usersfull%s.csv' % FILE_SUFFIX

GENDER_NAME_MIN_PROB = 0.8
GENDER_NAME_MIN_COUNT = 100

#%%

print('loading user data from', INPUT_FILE_USERS)
users = pd.read_csv(INPUT_FILE_USERS)


#%%

print('loading geocoding results from', INPUT_FILE_GEOCODED)
with open(INPUT_FILE_GEOCODED, 'rb') as f:
    geocoding_res = pickle.load(f)

#%%

print('loading genderizer results from', INPUT_FILE_GENDERIZER)
with open(INPUT_FILE_GENDERIZER, 'rb') as f:
    genderizer_res = pickle.load(f)

#%%

print('processing geocoding results')

location_rows = []
for location, geocode_res in geocoding_res.items():
    if geocode_res:
        n_geocode_res = len(geocode_res)
        if n_geocode_res > 1:
            print('> multiple (%d) geocoding results for location "%s"' % (n_geocode_res, location))

        geocode_res = next(iter(geocode_res))   # only take first result

        city = None
        street = None
        street_num = None
        postal_code = None
        country_code = None
        country_name = None
        for addr_comp in geocode_res.get('address_components', []):
            addr_types = addr_comp.get('types', [])
            if 'locality' in addr_types:
                city = addr_comp['long_name']
            elif 'route' in addr_types:
                street = addr_comp['long_name']
            elif 'street_number' in addr_types:
                street_num = addr_comp['long_name']
            elif 'postal_code' in addr_types:
                postal_code = addr_comp['long_name']
            elif 'country' in addr_types:
                country_name = addr_comp['long_name']
                country_code = addr_comp['short_name']

        if street_num is not None and street is not None:
            location_level = 'building'
        elif street is not None:
            location_level = 'street'
        elif city is not None:
            location_level = 'city'
        elif country_name is not None:
            location_level = 'country'
        else:
            location_level = None

        coords = geocode_res.get('geometry', {}).get('location', {})
        location_rows.append([location, n_geocode_res, location_level, geocode_res.get('formatted_address', None),
                              street, street_num, postal_code, city, country_code, country_name,
                              coords.get('lat', None), coords.get('lng', None)])
    else:
        print('> no geocoding result for location "%s"' % location)
        location_rows.append([location, 'geocoding_failed'] + [None] * 9)

#%%

print('merging locations with user data')

loc_df = pd.DataFrame(location_rows, columns=['location', 'n_geocode_results', 'location_level',
                                              'location_formatted_address', 'street',
                                              'street_num', 'postal_code', 'city', 'country_code',
                                              'country_name', 'location_lat', 'location_lng'])

users_loc = pd.merge(users, loc_df, how='left', on='location')
assert len(users_loc) == len(users)

print('> %d of %d user profiles have geocoded locations' % (sum(~users_loc.location_lat.isna()), len(users_loc)))

#%%

print('processing genderizer results')

gender_rows = []
for _, user in users_loc.iterrows():
    gen_results = [(r['gender'], r['probability']) for r in genderizer_res.get(user.id, [])
                   if r['probability'] > GENDER_NAME_MIN_PROB and r['count'] > GENDER_NAME_MIN_COUNT]

    if gen_results:
        gender, prob = next(iter(sorted(gen_results, key=lambda x: x[1], reverse=True)))
        gender_rows.append([user.id, gender, prob])

#%%

print('merging gender data with user data')

gender_df = pd.DataFrame(gender_rows, columns=['id', 'gender', 'gender_from_name_prob'])

users_loc_gender = pd.merge(users_loc, gender_df, how='left', on='id')
assert len(users_loc_gender) == len(users)

print('> %d of %d user profiles with identified gender by name'
      % (sum(~users_loc_gender.gender.isna()), len(users_loc_gender)))

#%%

print('storing final results to', OUTPUT_FILE)

users_loc_gender.to_csv(OUTPUT_FILE, index=False)

print('done.')
