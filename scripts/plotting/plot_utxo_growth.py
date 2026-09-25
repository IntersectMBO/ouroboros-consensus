#!/usr/bin/env python3

"""
Plot UTxO growth in the Cardano blockchain.

This script uses the output of `db-analyser --get-block-application-metrics`.

This script assumes the CSV file path is provided as first argument.
"""

import sys
import matplotlib.pyplot as pyplot
import pandas as pandas

if len(sys.argv) != 2:
   print("no arguments passed")
   sys.exit(1)

ledger_stats = pandas.read_csv(sys.argv[1], skipinitialspace = True)

slot_numbers = ledger_stats['Slot Number']
utxo_size = ledger_stats["UTxO size (via Compact)"] / 1_000_000

# See https://github.com/IntersectMBO/cardano-ledger/wiki/First-Block-of-Each-Era
first_slot_each_era = [4492800,  # Shelley
                       16588800, # Allegra
                       23068800, # Mary
                       39916975, # Alonzo
                       43372972, # Alonzo'
                       72316896, # Babbage
                       84844885  # Babbage'
                       ]

fig, ax1 = pyplot.subplots()

pyplot.vlines(first_slot_each_era, 0, max(utxo_size), colors="red")
pyplot.grid()

utxo_size_color = 'tab:blue'
ax1.set_xlabel('Slot Number')
ax1.set_ylabel('UTxO size (MB)', color = utxo_size_color)
ax1.plot(slot_numbers, utxo_size, color = utxo_size_color)
ax1.tick_params(axis='y', color = utxo_size_color)

ax2 = ax1.twinx() # Instantiate a second axes that shares the same x-axis

map_size_color = 'tab:green'
ax2.set_ylabel('UTxO map size', color=map_size_color)
ax2.plot(slot_numbers, ledger_stats["UTxO map size"], color = map_size_color)
ax2.tick_params(axis='y', color = map_size_color)

pyplot.show()
