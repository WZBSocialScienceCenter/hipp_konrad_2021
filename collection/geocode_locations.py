"""
Try to obtain geographic information such as geolocation coordinates and country from the `location` field for each user profile via [Google Maps Geocoding API](https://cloud.google.com/maps-platform/).

The obtained data can be linked with the user profile data via `location` field.

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""


import logging

import pandas as pd
pd.set_option('display.max_columns', 100)
pd.set_option('display.width', 180)

from gmapswrapper import GMapsWrapper
from accountdata import GOOGLE_MAPS_API_KEY
from utils import store_pickle

# to enable logging output to console
logging.basicConfig(level=logging.INFO)
logging.getLogger('gmapswrapper').setLevel(logging.INFO)


#%%

COLLECTION_DATE = '2020-06-17'
INPUT_FILE = 'data/users_%s.csv' % COLLECTION_DATE
OUTPUT_PICKLE = 'data/geocoded_locations_%s.pickle' % COLLECTION_DATE

#%%

# "cachedir" must be a writable directory to save the cache file
gmaps = GMapsWrapper('cache', api_key=GOOGLE_MAPS_API_KEY)

#%%

print('loading user data from', INPUT_FILE)
users = pd.read_csv(INPUT_FILE)

location_queries = users.location[~users.location.isna() & (users.location.str.len() > 4)].unique().tolist()

#%%

print('will geocode %d locations' % len(location_queries))

geocode_results = gmaps.geocode(location_queries)

#%%

store_pickle(geocode_results, OUTPUT_PICKLE, message_label='geocoding')

print('done.')
