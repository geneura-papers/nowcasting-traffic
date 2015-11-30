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



miEts_NoConcurso<-function( bbdd, horizonte ) {
 tmpTS<-dataTS;
 obtenidas<-vector();
 for( i in 1:length(deseadas) ) {
  dataFC<-forecast( tmpTS, h=horizonte )
  obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
  tmpTS<-c(tmpTS, deseadas[i] );
 }
 cat ("\n");
 cat (" @vrivas ETS NO CONCURSO| ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
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

miArima_NoConcurso<-function( bbdd, horizonte ) {
 tmpTS<-dataTS;
 obtenidas<-vector();
 for( i in 1:length(deseadas) ) {
  aa<-auto.arima( tmpTS  )
  dataFC<-forecast( aa,horizonte) ;
  obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
  tmpTS<-c(tmpTS, deseadas[i] );
 } 
 cat ("\n");
 cat (" @vrivas ARIMA NO CONCURSO | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miArima<-function( bbdd, horizonte ) {
 aa<-auto.arima( dataTS  )
 dataFC<-forecast( aa, length( deseadas ) );
 obtenidas<-dataFC$mean;
 cat ("\n");
 cat (" @vrivas ARIMA | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}


miCroston_NoConcurso<-function( bbdd, horizonte ) {
  tmpTS<-dataTS;
  obtenidas<-vector();
  for( i in 1:length(deseadas) ) {
    dataFC<-croston( tmpTS, h=horizonte )
    obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
    tmpTS<-c(tmpTS, deseadas[i] );
  } 
  cat (" @vrivas CROSTON NO CONCURSO| ");
  escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miCroston<-function( bbdd, horizonte ) {
 dataFC<-croston( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 cat (" @vrivas CROSTON | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miTheta_NoConcurso<-function( bbdd, horizonte ) {
  tmpTS<-dataTS;
  obtenidas<-vector();
  for( i in 1:length(deseadas) ) {
    dataFC<-thetaf( tmpTS, h=horizonte )
    obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
    tmpTS<-c(tmpTS, deseadas[i] );
  }
  cat (" @vrivas THETA NO CONCURSO | ");
  escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miTheta<-function( bbdd, horizonte ) {
 dataFC<-thetaf( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 cat (" @vrivas THETA | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}



miSpline_NoConcurso<-function( bbdd, horizonte ) {
  tmpTS<-dataTS;
  obtenidas<-vector();
  for( i in 1:length(deseadas) ) {
    dataFC<-splinef( tmpTS, h=horizonte )
    obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
    tmpTS<-c(tmpTS, deseadas[i] );
  } 
  cat ( " @vrivas SPLINE NO CONCURSO | ");
  escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miSpline<-function( bbdd, horizonte ) {
 dataFC<-splinef( dataTS, h=length( deseadas ) )
 obtenidas<-dataFC$mean;
 cat ( " @vrivas SPLINE | ");
 escribeErrores( dataFC, deseadas, obtenidas, horizonte );
}

miMean_NoConcurso<-function( bbdd, horizonte ) {
  tmpTS<-dataTS;
  obtenidas<-vector();
  for( i in 1:length(deseadas) ) {
    dataFC<-meanf( tmpTS, h=horizonte )
    obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
    tmpTS<-c(tmpTS, deseadas[i] );
  } 

 cat (" @vrivas MEAN NO CONCURSO | ");
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


miRWF_NoConcurso<-function( bbdd, horizonte ) {
  tmpTS<-dataTS;
  obtenidas<-vector();
  for( i in 1:length(deseadas) ) {
    dataFC<-rwf( tmpTS, h=horizonte )
    obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
    tmpTS<-c(tmpTS, deseadas[i] );
  } 

 cat (" @vrivas RW NO CONCURSO | ");
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

miGarch_NoConcurso<-function( bbdd, horizonte ) {
  tmpTS<-dataTS;
  obtenidas<-vector();
  for( i in 1:length(deseadas) ) {
    dataFC<-garch( tmpTS, h=horizonte )
    obtenidas<-c(obtenidas, dataFC$mean[horizonte]);
    tmpTS<-c(tmpTS, deseadas[i] );
  } 

 cat (" @vrivas GARCH NO CONCURSO | ");
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
  miSpline( aBD, horizonte );
  miMean( aBD, horizonte );
  miRWF( aBD, horizonte );
  miGarch( aBD, horizonte  );
}

bbdd_NoConcurso<-function( aBD, horizonte ) {
  cat( "\n\n\n" );
  cat( "==========================================================================================\n" );
  cat( " @vrivas NO_Concurso                                ", aBD, "\n" );
  cat( "==========================================================================================\n" );
  cat( "\n" );
  miMean_NoConcurso( aBD, horizonte );
  #miRWF_NoConcurso( aBD, horizonte );
  miEts_NoConcurso( aBD, horizonte );
  miArima_NoConcurso( aBD, horizonte );
  miTheta_NoConcurso( aBD, horizonte );
  #miCroston_NoConcurso( aBD, horizonte );
  #miSpline_NoConcurso( aBD, horizonte );  # Falla
  #miGarch_NoConcurso( aBD, horizonte  );
}



# --------------------------------------------------------------------------------------
# MAIN
# --------------------------------------------------------------------------------------

#myarg <- commandArgs()
#myarg <- as.numeric(myarg[length(myarg)])

horizonte <- 1 #myarg;
cat ( "Horizonte: (1 para el día siguiente)", horizonte,"\n" );

path="./data_ts_forecasting/";

aBD="DGT_1011";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd_NoConcurso( aBD, horizonte );

aBD="DGT_1021";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd_NoConcurso( aBD, horizonte );

aBD="DGT_1031";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd_NoConcurso( aBD, horizonte );

aBD="DGT_1051";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd_NoConcurso( aBD, horizonte );

aBD="DGT_1061";
dataTS<-ts( scan ( paste ( path,aBD,".trn", sep="" ) , comment.char="@"  ) )
deseadas<-scan ( paste( path,aBD,".tst", sep="" ) , comment.char="@"  )
bbdd_NoConcurso( aBD, horizonte );
