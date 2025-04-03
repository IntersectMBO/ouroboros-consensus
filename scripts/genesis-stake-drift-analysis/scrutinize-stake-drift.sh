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

echo "# discard pools outside of the 90% in each epoch >big.txt"
cat tidied.txt | sort -k1,1n -k5,5gr | awk '(eno != $1) { eno = $1; acc = 0 } (acc < 0.9) { acc = acc + $5; print $0 }' >big.txt
# cp tidied.txt big.txt # uncomment this command to use pools that were in all epochs regardless of their relative stake

echo "# for how many epochs was each pool in the top 90% >epochs.txt"
cat big.txt | awk '{print $4}' | sort | uniq -c >epochs.txt

echo "# histogram of epochs.txt"
cat epochs.txt | awk '{print $1}' | sort -n | uniq -c

echo "# big.txt sorted by pool and then by epoch >sorted.txt"
cat big.txt | sort -k4,4 -k1,1n >sorted.txt

echo "# restrict sorted.txt to the pools that are in all $nepochs epochs >steady.txt"
join -1 2 -2 4 <(grep -w -e $nepochs epochs.txt) sorted.txt >steady.txt

echo "# wc -l"
wc -l tidied.txt epochs.txt sorted.txt steady.txt

echo "# head -n5"
head -n5 tidied.txt epochs.txt sorted.txt steady.txt

echo "# cumulative stake per epoch within steady.txt"
cat steady.txt | awk '{x[$3] = x[$3] + $6} END { acc = 1/0; for (k in x) { if (acc > x[k]) { kacc = k; acc = x[k] }; print k, x[k] }; print " Min is ", kacc, acc }' | sort -n

echo "# the statistical distance between each epoch and epoch $lastEpoch"
echo "# "
echo "# see https://en.wikipedia.org/wiki/Statistical_distance#Statistically_close"
cat steady.txt | awk -v eno=$lastEpoch '(eno == $3) { print $0 }' >tmpfile-lastEpoch
for i in $(seq $firstEpoch $lastEpoch); do
    cat steady.txt | awk -v eno=$i '(eno == $3) { print $0 }' >tmpfile-$i

    paste tmpfile-lastEpoch tmpfile-$i | awk -v eno=$i '($6 > $12) { x = x + ($6 - $12) } ($6 < $12) { x = x + ($12 - $6) } END { printf("%i %.3f\n", eno, (x / 2)) }'
done
