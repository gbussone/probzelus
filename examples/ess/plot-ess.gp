set datafile separator comma
set key autotitle columnhead samplen 2

set terminal png
set output 'ess.png'

set xlabel 'Step (log scale)'
set logscale x
set logscale y
set ylabel 'ESS (log scale)'
set title 'ESS'

plot 'fm_is3.csv' using 1:3:2:4 with yerrorbars lt 6 pointtype 13 title 'FM-IS', \
     'pf3.csv' using 1:3:2:4 with yerrorbars lt 3 pointtype 5 title 'PF'
