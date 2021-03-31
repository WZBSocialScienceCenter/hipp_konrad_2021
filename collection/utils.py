"""
Utility functions.

August 2020, Markus Konrad <markus.konrad@wzb.eu>
"""

import os
import pickle
from threading import Event
from datetime import datetime

#%%

DEFAULT_WAIT_TIME_MINUTES = 15

MULTIPLE_TZ = (    # countries (w/o oversea territories etc.) with multiple timezones
    'AU',
    'BR',
    'CA',
    'CN',
    'ID',
    'MX',
    'RU',
    'US'
)


#%%

class Sleep(object):
    def __init__(self, seconds, immediate=True):
        self.seconds = seconds
        self.event = Event()
        if immediate:
            self.sleep()

    def sleep(self, seconds=None):
        if seconds is None:
            seconds = self.seconds
        self.event.clear()
        self.event.wait(timeout=seconds)

    def wake(self):
        self.event.set()


def sleep_on_exc(limits):
    if limits is None:
        wait_sec = DEFAULT_WAIT_TIME_MINUTES * 60
        print('>> got rate limit exc. – will wait %d minutes' % DEFAULT_WAIT_TIME_MINUTES)
    else:
        all_limits = [(which, getattr(limits, which).remaining, getattr(limits, which).reset)
                      for which in ('core', 'search', 'graphql')]
        min_remaining = min(all_limits, key=lambda x: x[1])
        wait_sec = (min_remaining[2] - datetime.utcnow()).seconds
        if wait_sec > 2 * DEFAULT_WAIT_TIME_MINUTES * 60:   # this may happen when reset is very close to "now"
            wait_sec = 0

        wait_sec += 10  # for safety

        print('>> got rate limit exc. (only %d remaining for %s) – will wait until %s UTC for %d sec (%dh %dm)' %
              (min_remaining[1], min_remaining[0], min_remaining[2],
               wait_sec, wait_sec // 3600, (wait_sec - (wait_sec // 3600) * 3600) // 60))

    return Sleep(wait_sec, immediate=False)


def load_data_from_pickle(fname, nonexistent_init_data=None):
    if nonexistent_init_data is None or os.path.exists(fname):
        print('loading existing data from', fname)
        with open(fname, 'rb') as f:
            return pickle.load(f)
    else:
        if nonexistent_init_data is not None:
            print('initializing with empty dataset')
        return nonexistent_init_data


def store_pickle(data, fname, message_label='', rotate_files=True):
    if message_label:
        print('storing %s results to %s' % (message_label, fname))

    if rotate_files and os.path.exists(fname):
        os.rename(fname, fname + '~')

    with open(fname, 'wb') as f:
        pickle.dump(data, f)
