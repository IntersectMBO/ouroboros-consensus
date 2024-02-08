#!/usr/bin/env python3
"""Plot missed slots and total time used by the garbage collector (in seconds)"""

import matplotlib.pyplot as pyplot
import pandas as pandas
from datetime import datetime
from dateutil import parser

slot_gc_df = pandas.read_csv("slot_gc_stats.csv", header=None)
timestamps = slot_gc_df[0].apply(lambda timestamp: parser.parse(timestamp))
missed_slots = slot_gc_df[1]
gc_time = slot_gc_df[2]

start_stop_snap_df = pandas.read_csv("store_ledger_events.csv", header=None)
start_times_timestamps = start_stop_snap_df[0].apply(
    lambda timestamp: parser.parse(timestamp)
)
end_times_timestamps = start_stop_snap_df[1].apply(
    lambda timestamp: parser.parse(timestamp)
)

pyplot.plot(timestamps, missed_slots)
pyplot.plot(timestamps, gc_time, color="purple")
pyplot.vlines(start_times_timestamps, 0, max(gc_time), colors="orange")
pyplot.vlines(end_times_timestamps, 0, max(gc_time), colors="red")
pyplot.grid()

pyplot.show()
