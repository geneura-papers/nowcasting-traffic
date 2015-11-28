set title "PETRA - Node 1011"
set xlabel "Dates: from 25-Oct-2015, 00:00h to 31-Nov-2015, 23:45h"
set ylabel "Cars detected by MOBYWIT" 
set terminal postscript eps enhanced color font 'Helvetica,12'
set output 'forecasting.eps'
set yrange [0:40]
plot "PETRA_1011_plot.txt" u 0:1 w l t "Expected Values", "PETRA_1011_plot.txt" u 0:2 w l t "Forecasted by ETS"