set datafile separator comma
set key autotitle columnhead samplen 2

set terminal pdf
set output 'split-mse-x.pdf'

set logscale y

plot 'particles/error_x.csv' using 1:3:2:4 every 10 with yerrorlines lt 3 pt 5 notitle, \
     'importance/error_x.csv' using 1:3:2:4 every 10 with yerrorlines lt 2 pt 9 notitle, \
     'apf_mm/error_x.csv' using 1:3:2:4 every 10 with yerrorlines lt 7 pt 2 notitle
