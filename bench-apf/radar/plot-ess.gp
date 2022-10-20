set datafile separator comma
set key autotitle columnhead samplen 2

set terminal pdf
set output 'radar-ess.pdf'

set logscale y

plot 'particles/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 3 pt 5 title 'PF', \
     'importance/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 2 pt 9 title 'IS', \
     'apf_mm/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 7 pt 2 title 'APF-MM'
