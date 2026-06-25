if (!exists('prefix')) prefix = 'catch'
if (!exists('suffix')) suffix = ''

set terminal pngcairo transparent enhanced size 32767, 1024
set output 'plot'.suffix.'.png'
set title 'See README.md.'

set xtics 500
set xlabel 'total duration of validation (s)'

unset autoscale y
set yrange [0:100]
set grid ytics
set ytics 10
set ylabel 'megabits per second'

sizes = '10 100 1000'

# FYI: words() gives length and word(,) extracts one word

plot for [i=1:words(sizes)] prefix.'-'.word(sizes, i) using ($1/1000000):($2*8 < 100 ? $2*8 : 100) title word(sizes, i).' block buffer'
