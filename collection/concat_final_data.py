"""
Load all files matching the pattern `data/usersfull*.csv` and concatenate them to a single user profiles dataset named
`concat_users.csv`. Load all files matching the pattern `data/contribs_scraped*.pickle` and concatenate them to a
ingle user contributions dataset named `concat_contribs.csv`.

Be aware that this needs lots of RAM (peak memory usage is ~8GB).

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

import sys
import glob

import pandas as pd

skip_contribs = len(sys.argv) > 1 and sys.argv[1] == 'skip_contribs'

#%%

FILTER_USERS = True            # if True, only keep users and their contributions with predicted gender
                               # and localization
# AGGREGATION = ['daily']        # set which aggregations to compute and store;
AGGREGATION = ['weekly', 'monthly']        # set which aggregations to compute and store;
                                           # options: 'none', 'daily', 'weekly', 'monthly'
FILTER_CONTRIBS_MAX_DATE = '2020-08-02'    # only retain contribution data up until this date
FILTER_CONTRIBS_MAX_COUNT = 30             # filter daily extremes

OUTPUT_USERS_CSV = 'data/concat_users.csv'
OUTPUT_CONTRIBS_CSV_FMT = 'data/concat_contribs%s.csv'    # placeholder for aggregation type

#%%

print('loading user data...')

users_dfs = []
for users_csv in sorted(glob.glob('data/usersfull*.csv')):
    print('>', users_csv)
    df = pd.read_csv(users_csv)
    users_dfs.append(df)

print('concatenating user data...')
users = pd.concat(users_dfs)
users.drop_duplicates('id', keep='last', inplace=True)

if FILTER_USERS:
    gender_na = users.gender.isna()
    country_na = users.country_code.isna()
    mask = ~gender_na & ~country_na
    print('> filter will retain %d of %d rows' % (sum(mask), len(users)))
    users = users[mask]

user_ids = set(users['id'])
assert len(users) == len(user_ids)
del users_dfs

print('%d users' % len(user_ids))
print('generating combined user data...')
users.to_csv(OUTPUT_USERS_CSV, index=False)
userinfo = users[['id', 'country_code', 'gender']]
del users

#%%

if skip_contribs:
    print('skipping contributions data')
else:
    print('loading contributions data...')
    contribs_dfs = {}

    picklefiles = glob.glob('data/contribs_scraped*.pickle')
    for i_contribs_pickle, contribs_pickle in enumerate(sorted(picklefiles)):
        print(f'> [{i_contribs_pickle+1} / {len(picklefiles)}]: {contribs_pickle}')
        df = pd.read_pickle(contribs_pickle)

        if FILTER_USERS:
            mask = df.user_id.isin(user_ids)

            if FILTER_CONTRIBS_MAX_DATE:
                mask_max_date = pd.to_datetime(df['date']) <= FILTER_CONTRIBS_MAX_DATE
                print('> %d of %d later than max. date %s' % (sum(~mask_max_date), len(df), FILTER_CONTRIBS_MAX_DATE))
                mask &= mask_max_date
            if FILTER_CONTRIBS_MAX_COUNT:
                mask_max_count = df['count'] <= FILTER_CONTRIBS_MAX_COUNT
                print('> %d of %d higher than max. count %d' % (sum(~mask_max_count), len(df), FILTER_CONTRIBS_MAX_COUNT))
                mask &= mask_max_count

            print('> filter will retain %d of %d rows' % (sum(mask), len(df)))
            df = df[mask].copy()

        for aggregtype in AGGREGATION:
            if aggregtype == 'none':
                print('> retaining unaggregated contributions')
            else:
                print('> aggregating %s contributions' % aggregtype)

                if aggregtype in {'weekly', 'monthly'}:
                    df.loc[:, 'tmp_date'] = pd.to_datetime(df.date)
                    df.loc[:, 'year'] = df.tmp_date.dt.year

                if aggregtype == 'daily':
                    # to save memory: only store non-zero values
                    df_aggreg = df.loc[df['count'] > 0, :]
                elif aggregtype == 'weekly':
                    df.loc[:, 'week'] = df.tmp_date.dt.weekofyear
                    del df['tmp_date']
                    df_weekly = df.groupby(['user_id', 'year', 'week'], as_index=False)\
                        .sum()\
                        .reset_index()
                    del df_weekly['index']
                    year_week = df_weekly.year.astype(str).str.cat(df_weekly.week.astype(str), sep=' ') + ' 1'
                    df_weekly.loc[:, 'date'] = pd.to_datetime(year_week, format='%Y %W %w').dt.strftime('%Y-%m-%d')
                    df_aggreg = df_weekly[['user_id', 'date', 'year', 'week', 'count']]
                elif aggregtype == 'monthly':
                    df.loc[:, 'month'] = df.tmp_date.dt.month
                    del df['tmp_date'], df['week']
                    df_monthly = df.groupby(['user_id', 'year', 'month'], as_index=False)\
                        .sum()\
                        .reset_index()
                    del df_monthly['index']
                    year_month = df_monthly.year.astype(str).str.cat(df_monthly.month.astype(str), sep=' ') + ' 1'
                    df_monthly.loc[:, 'date'] = pd.to_datetime(year_month, format='%Y %m %d').dt.strftime('%Y-%m-%d')
                    df_aggreg = df_monthly[['user_id', 'date', 'year', 'month', 'count']]
                else:
                    raise RuntimeError('unknown aggregation option: %s' % aggregtype)

            if aggregtype not in contribs_dfs.keys():
                contribs_dfs[aggregtype] = df_aggreg
            else:
                # important: also remove duplicates (because we collected 2020 several times) and use most recent data
                contribs_dfs[aggregtype] = pd.concat([contribs_dfs[aggregtype], df_aggreg])\
                    .sort_values(['user_id', 'date'])\
                    .drop_duplicates(['user_id', 'date'], keep='last', ignore_index=True)

    print('saving contributions data...')
    for aggregtype, df in contribs_dfs.items():
        print('>', aggregtype)
        df.to_csv(OUTPUT_CONTRIBS_CSV_FMT % aggregtype, index=False)

#%%

print('done.')
