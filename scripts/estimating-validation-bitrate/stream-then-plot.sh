# Crunch all the data.
for i in 10 100 1000; do B=$i . streaming.sh & done; wait

# Split the x-axis in half, so the plots are more legible.
#
# 125000 seconds is _currently_ the end of the x-axis if I plot the whole data set in one image.
for i in 10 100 1000; do cat catch-$i | awk '($1/1000000 <  125000/2) {print $0}' > catch1-$i & done
for i in 10 100 1000; do cat catch-$i | awk '($1/1000000 >= 125000/2) {print $0}' > catch2-$i & done
wait

# Render plot-1.png and plot-2.png.
for i in 1 2; do gnuplot -e "prefix='catch$i'" -e "suffix='-$i'" plot.gp & done
wait
