if (!exists("ex")) ex='coin'
dir='../'.ex.'/'


set datafile separator comma
set key autotitle columnhead samplen 2

set terminal png
set output ex.'-accuracy.png'

set xlabel 'Number of Particles'
set logscale x
set logscale y
set ylabel 'MSE (log scale)'
set title ex.': Accuracy'

plot dir.'particles/accuracy.csv' using 1:3:2:4 with yerrorbars lt 3 pointtype 5 title 'PF', \
     dir.'apf_is/accuracy.csv' using 1:3:2:4 with yerrorbars lt 6 pointtype 13 title 'APF-IS', \
     dir.'apf_mm/accuracy.csv' using 1:3:2:4 with yerrorbars lt 7 pointtype 15 title 'APF-MM', \
     dir.'ds/accuracy.csv' using 1:3:2:4 with yerrorbars lt 1 pointtype 11 title 'SDS'
