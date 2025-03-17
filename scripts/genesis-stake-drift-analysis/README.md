The `scrutinize-stake-drift.sh` bash script postprocesses the output of the `db-analyser --dump-stake-distributions` pass.

It yields several temporary files in the local directory, so run it in a temporary folder.

The script prints out a table that indicate how much stake the pools that have been in the top 90% of every epoch in the data had in each epoch.

The script also prints out a counterfactual table that pretends each of those pools' least-stake epochs were coincident, where that minimum iterates over every suffix of the list of epochs.
