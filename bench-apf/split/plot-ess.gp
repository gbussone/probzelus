set datafile separator comma
set key autotitle columnhead samplen 2

set terminal png
set output 'split-ess.png'

set xlabel 'Pas'
set logscale y
set ylabel 'ESS (paramètres constants, échelle log)'

set title 'Split : Information (paramètres constants) par pas'

plot 'particles/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 3 pt 5 title 'PF', \
     'importance/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 2 pt 9 title 'IS', \
     'apf_is/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 4 pt 7 title 'APF-IS', \
     'apf_mm/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 7 pt 15 title 'APF-MM'
