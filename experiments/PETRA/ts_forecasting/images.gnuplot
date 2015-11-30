set title "DGT - Node 1011"
set yrange [0:600]
plot "DGT_1011_plot.txt" u 0:1 w l t "Test Data", "DGT_1011_plot.txt" u 0:2 w l t "ETS","DGT_1011_plot.txt" u 0:3 w l,"DGT_1011_plot.txt" u 0:4 w l,"DGT_1011_plot.txt" u 0:5 w l
pause -1
