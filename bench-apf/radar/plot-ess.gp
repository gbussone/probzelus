set datafile separator comma
set key autotitle columnhead samplen 2

set terminal png
set output 'radar-ess.png'

set xlabel 'Step'
set logscale y
set ylabel 'ESS (constant parameters, log scale)'

set title 'Radar: Information (constant parameters) per step'

plot 'particles/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 3 pt 5 title 'PF', \
     'importance/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 2 pt 9 title 'IS', \
     'apf_mm/metric_theta.csv' using 1:3:2:4 every 10 with yerrorlines lt 7 pt 15 title 'APF-MM'
