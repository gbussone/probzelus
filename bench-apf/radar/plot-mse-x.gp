set datafile separator comma
set key autotitle columnhead samplen 2

set terminal pdf
set output 'radar-mse-x.pdf'

set xlabel 'Pas'
set logscale y
set ylabel 'MSE (paramètres d''état, échelle log)'

set title 'Radar : Erreur (paramètres d''état) par pas'

plot 'particles/error_x.csv' using 1:3:2:4 every 10 with yerrorlines lt 3 pt 5 title 'PF', \
     'importance/error_x.csv' using 1:3:2:4 every 10 with yerrorlines lt 2 pt 9 title 'IS', \
     'apf_mm/error_x.csv' using 1:3:2:4 every 10 with yerrorlines lt 7 pt 15 title 'APF-MM'
