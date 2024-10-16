set terminal pngcairo transparent enhanced size 1024,512 font "sans,8"

set output 'myplot.png'

set key top left

set logscale y
set grid

set ylabel 'seconds'
set xlabel 'sample index'

set title 'pipe-valid'

plot 'catch.txt' using (column(0)):($1/1e9):(0.1) notitle with points pointtype 7 pointsize variable
