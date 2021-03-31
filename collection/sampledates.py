"""
Sample GitHub user account creation dates

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

from datetime import datetime, timedelta
from random import sample

import pandas as pd

#%%

START_DATE = '2008-04-10'   # GitHub launch date
END_DATE = '2020-05-15'
SAMPLESIZE = 100
OUTPUT_FILE = 'data/sampled_dates_%s.csv' % datetime.now().strftime('%Y-%m-%d')

print('sampling %d dates between %s and %s' % (SAMPLESIZE, START_DATE, END_DATE))

#%%

t_start = datetime.strptime(START_DATE, '%Y-%m-%d')
t_end = datetime.strptime(END_DATE, '%Y-%m-%d')
delta_days = (t_end - t_start).days

#%%

sampled_dates = []
for d in sample(range(delta_days + 1), k=SAMPLESIZE):
    dt = t_start + timedelta(days=d)
    sampled_dates.append(dt.strftime('%Y-%m-%d'))

#%%

print('writing output to file', OUTPUT_FILE)
pd.DataFrame({'date': sampled_dates}).to_csv(OUTPUT_FILE, index=False)
print('done.')