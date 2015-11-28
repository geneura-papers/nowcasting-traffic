library( forecast )


escribeErrores<-function ( metodo, deseadas, obtenidas, horizonte ) {
# cat( "Valores automáticos:\n" )
# cat( "===========================\n" )
# print( accuracy( metodo, deseadas ) )


# cat( "Valores calculados:\n" )
# cat( "===========================\n" )

  #cat( "Deseadas: ", deseadas, " " );

# 05-12-2013: Intentamos que prediga con horizonte cualquiera



  tama=length( obtenidas );
  deseadas<-deseadas[horizonte:tama];
  obtenidas<-obtenidas[horizonte:tama];

  errores<-accuracy( deseadas, obtenidas );

  #cat ( metodo,"/",problema, ": ");
  cat (" | ME: ",  errores[1], " | MSE: ", errores[2]^2, " | RMSE: ", errores[2] );
	cat (" | MAE mean: ", errores[3], " | MPE: ", errores[4] );
  # cat (" | MAPE: ", errores['MAPE'] ); # Lo quito porque parece que no lo calcula bien
	cat(" | MASE: ", errores['MASE'], " | ACF1: ", errores['ACF1'], " | RelRMSE: ", errores["Theil's U"] );


  tama=length( obtenidas );
  #Calculo el denominador de qt para calcular MASE porque no sale al utilizar fch
  qt_denom=0;
  for( ndat in 2:tama ) {
    qt_denom<-qt_denom+abs(deseadas[ndat]-deseadas[ndat-1] );
  }
  qt_denom<-qt_denom/(tama-1);


  absets<-array( , tama );
  abspets<-array(,tama);
  smape<-array(,tama);
  absrts<-array( , tama-1 );
  qts<-array(,tama);
  for( ndat in 1:tama ) {
    # Uso la misma nomenclatura que en De Gooijer, 2006
    yt<-deseadas[ndat];
    ft<-obtenidas[ndat];
    et<-yt-ft;
    qts[ndat]<- et/qt_denom
    if( ndat>1 ) {
      et_<-yt-deseadas[ndat-1];
      rt=et/et_;
      absrts[ndat-1]<-abs( rt  );
    }
    pet<-100*et/yt;
    absets[ndat]<-abs( et );
    abspets[ndat]<-abs( pet );
    smape[ndat]<-2*abs( deseadas[ndat]-obtenidas[ndat] )/ (deseadas[ndat]+obtenidas[ndat])
  }

  MAPE=mean( abspets ); # Aunque ya lo calcula ACCURACY , por eso no lo pongo con cat
  cat( " | MAPE (recalculado): ", MAPE );
  cat( " | MASE (recalculado): ", mean( abs( qts ) ));
  cat( " | MdAE: ", median( absets ));
  cat( " | MdAPE: ", median( abspets ));
  cat( " | sMAPE(%): ", 100*mean( smape ));
  cat( " | sMdAPE(%): ", 100*median( smape ));
  cat( " | MRAE : ", mean( absrts ) );
  cat( " | MdRAE: ", median( absrts ) );
  cat( " | GMRAE: ", mean( log( absrts )) );
  cat ( " | Obtenidas: ", obtenidas );

  cat( "\n" );
 cat( "--------------------------------------------------------------------------\n\n\n" )
}


miEts<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 dataFC<-forecast( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 #cat( "ETS:\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 #print( dataFC[1]$model );
 cat ("\n");
 cat (" @vrivas ETS | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miArima<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 aa<-auto.arima( dataTS  )
 dataFC<-forecast( aa, length( deseadas ) );
 obtenidas<-dataFC$mean;
 #cat( "ARIMA  :\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 #print( dataFC[1]$model );
 cat ("\n");
 cat (" @vrivas ARIMA | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}


miCroston<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 dataFC<-croston( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 #cat( "Croston:\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 cat (" @vrivas CROSTON | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miTheta<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 dataFC<-thetaf( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 #cat( "Theta:\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 cat (" @vrivas THETA | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miSpline<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 dataFC<-splinef( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 #cat( "Spline:\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 cat ( " @vrivas SPLINE | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}



miMean<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 dataFC<-meanf( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 #cat( "MEAN:\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 cat (" @vrivas MEAN | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}



miRWF<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 dataFC<-rwf( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 #cat( "RW (Random Walk):\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 cat (" @vrivas RW | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miGarch<-function( bbdd, horizonte ) {
 #dataTS<-ts( scan ( paste ( "./",bbdd,".keel.trn.dat", sep="" ) , comment.char="@"  ) )
 #deseadas<-scan ( paste( "./",bbdd,".keel.tst.dat", sep="" ) , comment.char="@"  )
 dataFC<-garch( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 #cat( "GARCH:\n" )
 #cat( "--------------------------------------------------------------------------\n" )
 cat( "@ GARCH |");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}


bbdd<-function( aBD, horizonte ) {
  cat( "\n\n\n" );
  cat( "==========================================================================================\n" );
  cat( " @vrivas                                 ", aBD, "\n" );
  cat( "==========================================================================================\n" );
  cat( "\n" );
  miEts( aBD, horizonte );
  miArima( aBD, horizonte );
  miCroston( aBD, horizonte );
  miTheta( aBD, horizonte );
  #miSpline( aBD, horizonte );
  miMean( aBD, horizonte );
  miRWF( aBD, horizonte );
  #miGarch( aBD, horizonte  );
}


# --------------------------------------------------------------------------------------
# MAIN
# --------------------------------------------------------------------------------------

#myarg <- commandArgs()
#myarg <- as.numeric(myarg[length(myarg)])

horizonte <- 10 #myarg;
cat ( "Horizonte: (1 para el día siguiente)", horizonte,"\n" );

path="./data_ts_forecasting/";

aBD="DGT_1011";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd( aBD, horizonte );

aBD="DGT_1021";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd( aBD, horizonte );

aBD="DGT_1031";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd( aBD, horizonte );

aBD="DGT_1051";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd( aBD, horizonte );

aBD="DGT_1061";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd( aBD, horizonte );
