# For example:
#
# $ db-analyser ... --dump-stake-distributions >foo.txt
# $ bash scrutinize-stake-drift.sh foo.txt

db_analyser_output_file=$1

echo "# the PoolDistrs in tabular form >tidied.txt"
cat "${db_analyser_output_file}" | tr '(,){=}"' ' ' | sed 's/KeyHash/\n/g' | awk -f tidy.awk >tidied.txt

firstEpoch=$(head -n1 tidied.txt | awk '{print $1}')
lastEpoch=$(tail -n1 tidied.txt | awk '{print $1}')
nepochs=$(expr $lastEpoch - $firstEpoch + 1)

echo "# how many epochs each pool was in >epochs.txt"
cat tidied.txt | awk '{print $4}' | sort | uniq -c >epochs.txt

echo "# histogram of epochs.txt"
cat epochs.txt | awk '{print $1}' | sort -n | uniq -c

echo "# discard pools outside of the 90% in each epoch >big.txt"
cat tidied.txt | sort -k1,1n -k5,5gr | awk '(eno != $1) { eno = $1; acc = 0 } (acc < 0.9) { acc = acc + $5; print $0 }' >big.txt

echo "# big.txt sorted by pool and then by epoch >sorted.txt"
cat big.txt | sort -k4,4 -k1,1n >sorted.txt

echo "# restrict to the pools that are in all $nepochs epochs >steady.txt"
join -1 2 -2 4 <(grep -w -e $nepochs epochs.txt) sorted.txt >steady.txt

echo "# wc -l"
wc -l tidied.txt epochs.txt sorted.txt steady.txt

echo "# head -n5"
head -n5 tidied.txt epochs.txt sorted.txt steady.txt

echo "# cumulative stake per epoch within steady.txt"
cat steady.txt | awk '{x[$3] = x[$3] + $6} END { acc = 1/0; for (k in x) { if (acc > x[k]) { kacc = k; acc = x[k] }; print k, x[k] }; print " Min is ", kacc, acc }' | sort -n

echo "# least stake for each pool in some epoch in steady.txt, for each tail of the epochs >lows.txt"
for i in $(seq $firstEpoch $lastEpoch); do
    cat steady.txt | awk -v low=$i '{pool = $1; eno = $3; stake = $6} (low <= eno && !(pool in acc && stake >= acc[pool])) { kacc[pool] = eno; acc[pool] = stake } END { for (pool in acc) { print low, acc[pool], kacc[pool], pool } }' | sort -g
done >lows.txt

echo "# sum of lows.txt for each age bound"
cat lows.txt | awk '($1 != x) {if (x) { print x, acc}; x = $1; acc = 0} {acc = acc + $2} END {print x, acc}'
