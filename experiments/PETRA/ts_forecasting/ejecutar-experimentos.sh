export HORIZ=1
R --vanilla --silent --args $HORIZ < experimentos-con-r-20151125-con-horizonte-variable.r  | grep "@vrivas"  > resultados.horizonte.$HORIZ.txt
