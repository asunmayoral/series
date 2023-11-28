#Funciones utilizadas para el artículo 1
#calculo1(): Ajusta cada configuración de un modelo en el periodo T1, y 
#           genera las predicciones para el periodo T2 de cada configuración
#calculo2():Calcula las funciones de pérdida en T1 y T2
#calculo3():Construye la matriz de errores
#calculo4():Calcula las CP y la distancia al método asociado al menor error
#calculo5():Calcula los pesos asociados a cada combinación según las cotas fijadas
#calculo6():Calcula las predicciones para el periodo T1+T2
#calcula7():Calcula las matrices de errores sMape en T2 y T3 con cada peso para 
#           cada combinación según la cota utilizada.serie:j y cota:i
#graf():Gráfico para comparar sMape en T2 y T3 para cada serie según el peso. 
#       serie:j y peso:w


calculo1<-function(j){
    # Definimos los parámetros generales
    serie<-ensayo[j]
    n_p<-12  #predicciones
    n<-length(datos[[serie]]) #nº de datos
    estac<-12 #estacionalidad
    comienzo <- start(datos[[serie]])
    
    # Seleccionamos el periodo T1(x) T2(x)
    x  <- ts(datos[[serie]][1:(n-(2*n_p))],start = comienzo, frequency = estac) #T1
    if(end(x)[2]==estac) 
    {comienzo2<-c(end(x)[1]+1,1)
    }else 
    {comienzo2<-c(end(x)[1],end(x)[2]+1)}
    x1 <- ts(datos[[serie]][(n-(2*n_p)+1):(n-n_p)], start = comienzo2,frequency = estac) #T2
    
    set.seed(846) #Fijo la semilla de aleatorización para que sea reproducible
    metodos<-list()
    #naive
    metodos[[1]]<-naive(x,h=n_p)
    #Naïve con estacionalidad
    metodos[[2]]<-snaive(x,h=n_p)
    #Suavizado exponencial
    metodos[[3]]<-ets(x,model = "ZZZ", opt.crit="lik", damped = NULL)
    metodos[[3]]<-predict(metodos[[3]],n_p)
    metodos[[4]]<-ets(x,model = "ZZZ", opt.crit="mse", damped = NULL)
    metodos[[4]]<-predict(metodos[[4]],n_p)
    metodos[[5]]<-ets(x,model = "ZZZ", opt.crit="amse", damped = NULL)
    metodos[[5]]<-predict(metodos[[5]],n_p)
    metodos[[6]]<-ets(x,model = "ZZZ", opt.crit="sigma", damped = NULL)
    metodos[[6]]<-predict(metodos[[6]],n_p)
    metodos[[7]]<-ets(x,model = "ZZZ", opt.crit="mae", damped = NULL)
    metodos[[7]]<-predict(metodos[[7]],n_p)
    metodos[[8]]<-ets(x,model = "ZZZ", opt.crit="lik", damped = TRUE)
    metodos[[8]]<-predict(metodos[[8]],n_p)
    metodos[[9]]<-ets(x,model = "ZZZ", opt.crit="mse", damped = TRUE)
    metodos[[9]]<-predict(metodos[[9]],n_p)
    metodos[[10]]<-ets(x,model = "ZZZ", opt.crit="amse", damped = TRUE)
    metodos[[10]]<-predict(metodos[[10]],n_p)
    metodos[[11]]<-ets(x,model = "ZZZ", opt.crit="sigma", damped = TRUE)
    metodos[[11]]<-predict(metodos[[11]],n_p)
    metodos[[12]]<-ets(x,model = "ZZZ", opt.crit="mae", damped = TRUE)
    metodos[[12]]<-predict(metodos[[12]],n_p)
    #arima
    a<-NULL
    metodos[[13]]<-auto.arima(x, ic="aic",approximation=T, trace=FALSE, allowdrift=F)
    a<-predict(metodos[[13]],n_p)
    metodos[[13]]$mean<-a$pred
    metodos[[14]]<-auto.arima(x, ic="bic",approximation=T, trace=FALSE, allowdrift=F)
    a<-predict(metodos[[14]],n_p)
    metodos[[14]]$mean<-a$pred
    #Theta
    metodos[[15]]<-otm.arxiv(x, h=n_p, s= NULL, g= "sAPE")
    metodos[[16]]<-dotm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead") #Dynamic Optimised Theta Model
    metodos[[17]]<-dotm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B") #Dynamic Optimised Theta Model
    metodos[[18]]<-dotm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN") #Dynamic Optimised Theta Model
    metodos[[19]]<-dstm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead") #Dynamic Standard Theta Model
    metodos[[20]]<-dstm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B") #Dynamic Standard Theta Model
    metodos[[21]]<-dstm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN") #Dynamic Standard Theta Model
    metodos[[22]]<-otm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead")  #Optimised Theta Model
    metodos[[23]]<-otm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B")  #Optimised Theta Model
    metodos[[24]]<-otm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN")  #Optimised Theta Model
    metodos[[25]]<-stm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead")  #Standard Theta Model (Fiorucci)
    metodos[[26]]<-stm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B")  #Standard Theta Model (Fiorucci)
    metodos[[27]]<-stm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN")  #Standard Theta Model (Fiorucci)
    metodos[[28]]<-stheta(x, h=n_p, s= NULL) #Standard Theta Model (Assimakopoulos)
    #STL (sólo series estacionales)
    metodos[[29]]<-stlf(x,h=n_p, method=c("ets"),robust = TRUE,allow.multiplicative.trend = TRUE)
    metodos[[30]]<-stlf(x,h=n_p, method=c("arima"),robust = TRUE,allow.multiplicative.trend = TRUE)
    metodos[[31]]<-stlf(x,h=n_p, forecastfunction=thetaf,robust = TRUE,allow.multiplicative.trend = TRUE)
    #Croston
    metodos[[32]]<-crost(x,h=n_p,type = "croston",cost = "mar",init.opt=TRUE)
    metodos[[32]]$fitted<-ts(metodos[[32]]$frc.in, start =comienzo,frequency = estac) 
    metodos[[32]]$mean<-  ts(metodos[[32]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[33]]<-crost(x,h=n_p,type = "croston",cost = "msr",init.opt=TRUE)
    metodos[[33]]$fitted<-ts(metodos[[33]]$frc.in, start =comienzo,frequency = estac) 
    metodos[[33]]$mean<-  ts(metodos[[33]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[34]]<-crost(x,h=n_p,type = "croston",cost = "mae",init.opt=TRUE)
    metodos[[34]]$fitted<-ts(metodos[[34]]$frc.in, start =comienzo,frequency = estac)
    metodos[[34]]$mean<-  ts(metodos[[34]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[35]]<-crost(x,h=n_p,type = "croston",cost = "mse",init.opt=TRUE)
    metodos[[35]]$fitted<-ts(metodos[[35]]$frc.in, start =comienzo,frequency = estac)
    metodos[[35]]$mean<-  ts(metodos[[35]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[36]]<-crost(x,h=n_p,type = "sba",cost = "mar",init.opt=TRUE)
    metodos[[36]]$fitted<-ts(metodos[[36]]$frc.in, start =comienzo,frequency = estac)
    metodos[[36]]$mean<-  ts(metodos[[36]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[37]]<-crost(x,h=n_p,type = "sba",cost = "msr",init.opt=TRUE)
    metodos[[37]]$fitted<-ts(metodos[[37]]$frc.in, start =comienzo,frequency = estac)
    metodos[[37]]$mean<-  ts(metodos[[37]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[38]]<-crost(x,h=n_p,type = "sba",cost = "mae",init.opt=TRUE)
    metodos[[38]]$fitted<-ts(metodos[[38]]$frc.in, start =comienzo,frequency = estac)
    metodos[[38]]$mean<-  ts(metodos[[38]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[39]]<-crost(x,h=n_p,type = "sba",cost = "mse",init.opt=TRUE)
    metodos[[39]]$fitted<-ts(metodos[[39]]$frc.in, start =comienzo,frequency = estac)
    metodos[[39]]$mean<-  ts(metodos[[39]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[40]]<-crost(x,h=n_p,type = "sbj",cost = "mar",init.opt=TRUE)
    metodos[[40]]$fitted<-ts(metodos[[40]]$frc.in, start =comienzo,frequency = estac)
    metodos[[40]]$mean<-  ts(metodos[[40]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[41]]<-crost(x,h=n_p,type = "sbj",cost = "msr",init.opt=TRUE)
    metodos[[41]]$fitted<-ts(metodos[[41]]$frc.in, start =comienzo,frequency = estac)
    metodos[[41]]$mean<-  ts(metodos[[41]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[42]]<-crost(x,h=n_p,type = "sbj",cost = "mae",init.opt=TRUE)
    metodos[[42]]$fitted<-ts(metodos[[42]]$frc.in, start =comienzo,frequency = estac)
    metodos[[42]]$mean<-  ts(metodos[[42]]$frc.out, start =comienzo2,frequency = estac) 
    metodos[[43]]<-crost(x,h=n_p,type = "sbj",cost = "mse",init.opt=TRUE)
    metodos[[43]]$fitted<-ts(metodos[[43]]$frc.in, start =comienzo,frequency = estac)
    metodos[[43]]$mean<-  ts(metodos[[43]]$frc.out, start =comienzo2,frequency = estac) 
    
    #Prophet
    set.seed(50)
    df<-data.frame(ds = seq(as.Date(paste(start(x)[1],"-",start(x)[2],"-","01",sep="")),
                            as.Date(paste(end(x)[1],"-",end(x)[2],"-","01",sep="")),by="m"), y = x)
    metodos[[44]]<-prophet(df,yearly.seasonality=TRUE,
                           weekly.seasonality = FALSE,
                           daily.seasonality = FALSE,seasonality.mode = "additive")
    a<-make_future_dataframe(metodos[[44]],periods = 18,freq = "month")
    b<-predict(metodos[[44]],a)
    metodos[[44]]$fitted <- ts(b$yhat[1:length(x)],start = comienzo,frequency = estac)
    metodos[[44]]$mean <- ts(b$yhat[(length(x)+1):(length(x)+n_p)],start = comienzo2,frequency = estac)
    metodos[[45]]<-prophet(df,yearly.seasonality=TRUE,
                           weekly.seasonality = FALSE,
                           daily.seasonality = FALSE,seasonality.mode = "multiplicative")
    a<-make_future_dataframe(metodos[[45]],periods = 18,freq = "month")
    b<-predict(metodos[[45]],a)
    metodos[[45]]$fitted <- ts(b$yhat[1:length(x)],start = comienzo,frequency = estac)
    metodos[[45]]$mean <- ts(b$yhat[(length(x)+1):(length(x)+n_p)],start = comienzo2,frequency = estac)
    
    #Redes neuronales NNAR: Lo ejecutamos de 2 a 10 veces según n<200 y seleccionamos la solución
    #con menor smape en T2
    set.seed(50)
    metodos[[46]]<-nnetar(x,p=estac,scale.inputs=T)
    a<-forecast(metodos[[46]],h=n_p)
    metodos[[46]]$mean<-a$mean
#    set.seed(50)
#    v=c(); pred=list()
#    for(re in 1:ci){ 
#      metodos[[46]]<-nnetar(x,p=estac,scale.inputs=T)
#      a<-forecast(metodos[[46]],h=n_p)
#      v[re]=smape(x1,a$mean);pred[[re]]=a$mean
#    } # cierro el for
#    wq=order(v)[1]
#    metodos[[46]]$mean=pred[[wq]]
    
    #TBATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)
    set.seed(50)
    metodos[[47]]<-tbats(x, biasadj = TRUE)
    metodos[[47]]$fitted<-metodos[[47]]$fitted.values
    a<-forecast(metodos[[47]],h=n_p)
    metodos[[47]]$mean<-a$mean
    
    #GRNN
    set.seed(50)
    metodos[[48]]<-grnn_forecasting(x, h = n_p, transform = "additive")
    a<-rolling_origin(metodos[[48]],h = length(x)-estac-1)
    metodos[[48]]$fitted<-a$predictions[,1]
    metodos[[48]]$mean<-metodos[[48]]$prediction
    metodos[[49]]<-grnn_forecasting(x, h = n_p, transform = "multiplicative")
    a<-rolling_origin(metodos[[49]],h = length(x)-estac-1)
    metodos[[49]]$fitted<-a$predictions[,1]
    metodos[[49]]$mean<-metodos[[49]]$prediction
    
    #MLP
    # En fitted del 14:n, las primeras n_p+2 son NA
    set.seed(50)
    metodos[[50]]<-mlp.thief(x, h=12)
    # En fitted del 5:n, las primeras 4 son vacías
    metodos[[51]]<-mlp(x, m=12, comb = c("median"),hd = c(10,5))
    a<-forecast(metodos[[51]],h=12)
    metodos[[51]]$mean<-a$mean
    metodos[[52]]<-mlp(x, m=12, comb = c("mean"),hd = c(10,5))
    a<-forecast(metodos[[52]],h=12)
    metodos[[52]]$mean<-a$mean
    
    gc()
  return(metodos)
}

calculo2<- function(j) {
  # Definimos los parámetros generales
  serie<-ensayo[j]
  n_p<-12 #predicciones
  n<-length(datos[[serie]]) #nº de datos
  estac<-12 #estacionalidad
  comienzo <- start(datos[[serie]])
  
  # Seleccionamos los periodos T1(x) y T2(x1)
  x  <- ts(datos[[serie]][1:(n-(2*n_p))],start = comienzo, frequency = estac) #T1
  if(end(x)[2]==estac) 
  {comienzo2<-c(end(x)[1]+1,1)
  }else 
  {comienzo2<-c(end(x)[1],end(x)[2]+1)}
  x1 <- ts(datos[[serie]][(n-(2*n_p)+1):(n-n_p)], start = comienzo2,frequency = estac) #T2

  #sMAPE en T1: En algunos casos es directo, pero en otros perdemos valores iniciales
  metodos.T1[[j]][[1]]$sMapeT1 <- 100*Metrics::smape(x[2:length(x)],metodos.T1[[j]][[1]]$fitted[2:length(x)])
  metodos.T1[[j]][[2]]$sMapeT1 <- 100*Metrics::smape(x[(estac+1):length(x)],metodos.T1[[j]][[2]]$fitted[(estac+1):length(x)])
  for (i in 1:24) {
    metodos.T1[[j]][[2+i]]$sMapeT1 <- 100*Metrics::smape(x,metodos.T1[[j]][[2+i]]$fitted)
  }
  for (i in 1:17) {
    metodos.T1[[j]][[26+i]]$sMapeT1 <- 100*Metrics::smape(x[2:length(x)],metodos.T1[[j]][[26+i]]$fitted[2:length(x)])
  }
  
  for (i in 1:2) {
    metodos.T1[[j]][[43+i]]$sMapeT1 <- 100*Metrics::smape(x,metodos.T1[[j]][[43+i]]$fitted)
  }
  
  metodos.T1[[j]][[46]]$sMapeT1 <- 100*Metrics::smape(x[(estac+1):length(x)],metodos.T1[[j]][[46]]$fitted[(estac+1):length(x)])
  
  metodos.T1[[j]][[47]]$sMapeT1 <- 100*Metrics::smape(x,metodos.T1[[j]][[47]]$fitted)
  
  for (i in 1:2) {
    metodos.T1[[j]][[47+i]]$sMapeT1 <- 100*Metrics::smape(x[1:(length(x)-estac-1)],metodos.T1[[j]][[47+i]]$fitted)
  }
  
  metodos.T1[[j]][[50]]$sMapeT1 <- 100*Metrics::smape(x[(estac+3):length(x)],
                                                      metodos.T1[[j]][[50]]$fitted[(estac+3):length(x)])
  metodos.T1[[j]][[51]]$sMapeT1 <- 100*Metrics::smape(x,metodos.T1[[j]][[51]]$fitted)
  metodos.T1[[j]][[52]]$sMapeT1 <- 100*Metrics::smape(x,metodos.T1[[j]][[52]]$fitted)

  #mase en T1: En algunos casos es directo, pero en otros perdemos valores iniciales
  metodos.T1[[j]][[1]]$maseT1 <- Metrics::mase(x[2:length(x)],metodos.T1[[j]][[1]]$fitted[2:length(x)],step_size=12)
  metodos.T1[[j]][[2]]$maseT1 <- Metrics::mase(x[(estac+1):length(x)],metodos.T1[[j]][[2]]$fitted[(estac+1):length(x)],step_size=12)
  for (i in 1:24) {
    metodos.T1[[j]][[2+i]]$maseT1 <- Metrics::mase(x,metodos.T1[[j]][[2+i]]$fitted,step_size=12)
  }
  for (i in 1:17) {
    metodos.T1[[j]][[26+i]]$maseT1 <- Metrics::mase(x[2:length(x)],metodos.T1[[j]][[26+i]]$fitted[2:length(x)],step_size=12)
  }
  
  for (i in 1:2) {
    metodos.T1[[j]][[43+i]]$maseT1 <- Metrics::mase(x,metodos.T1[[j]][[43+i]]$fitted,step_size=12)
  }
  
  metodos.T1[[j]][[46]]$maseT1 <- Metrics::mase(x[(estac+1):length(x)],metodos.T1[[j]][[46]]$fitted[(estac+1):length(x)],step_size=12)
  
  metodos.T1[[j]][[47]]$maseT1 <- Metrics::mase(x,metodos.T1[[j]][[47]]$fitted,step_size=12)
  
  for (i in 1:2) {
    metodos.T1[[j]][[47+i]]$maseT1 <- Metrics::mase(x[1:(length(x)-estac-1)],metodos.T1[[j]][[47+i]]$fitted,step_size=12)
  }
  
  metodos.T1[[j]][[50]]$maseT1 <- Metrics::mase(x[(estac+3):length(x)],metodos.T1[[j]][[50]]$fitted[(estac+3):length(x)])
  metodos.T1[[j]][[51]]$maseT1 <- Metrics::mase(x, metodos.T1[[j]][[51]]$fitted)
  metodos.T1[[j]][[52]]$maseT1 <- Metrics::mase(x, metodos.T1[[j]][[52]]$fitted)
  
  #sMAPE en T2
  for (i in 1:length(metodos.T1[[1]])) {
    metodos.T1[[j]][[i]]$sMapeT2 <- 100*Metrics::smape(x1,metodos.T1[[j]][[i]]$mean)
  }
  #RMSE en T2
  for (i in 1:length(metodos.T1[[1]])) {
    metodos.T1[[j]][[i]]$rmseT2 <- Metrics::rmse(x1,metodos.T1[[j]][[i]]$mean)
  }
  
  #mase en T2
  for (i in 1:length(metodos.T1[[1]])) {
    metodos.T1[[j]][[i]]$maseT2 <- Metrics::mase(x1,metodos.T1[[j]][[i]]$mean,step_size=1)
  }
  
  #OWA en T2
  for (i in 1:length(metodos.T1[[1]])) {
    metodos.T1[[j]][[i]]$owaT2 <- ((metodos.T1[[j]][[i]]$maseT2 / metodos.T1[[j]][[2]]$maseT2)+(metodos.T1[[j]][[i]]$sMapeT2 / metodos.T1[[j]][[2]]$sMapeT2))/2
  }
  
  
  return(metodos.T1[[j]])
}  

calculo3<-function(j){
  erroresT2[[j]]<-matrix(NA,nrow = length(metodos.T1[[1]]),ncol = 13)
  colnames(erroresT2[[j]])<-c("id","smapeT1","maseT1","rmse","smape",
                              "mase","owa","CP1","CP2", "dist", "w1","w2","w3")
  
  for (i in 1:length(metodos.T1[[1]])) {
    erroresT2[[j]][i,1]<-i
    erroresT2[[j]][i,2]<-metodos.T1[[j]][[i]]$sMapeT1
    erroresT2[[j]][i,3]<-metodos.T1[[j]][[i]]$maseT1
    erroresT2[[j]][i,4]<-metodos.T1[[j]][[i]]$rmseT2
    erroresT2[[j]][i,5]<-metodos.T1[[j]][[i]]$sMapeT2
    erroresT2[[j]][i,6]<-metodos.T1[[j]][[i]]$maseT2
    erroresT2[[j]][i,7]<-metodos.T1[[j]][[i]]$owaT2
  }
  dat1<-as.data.frame(round(erroresT2[[j]],3))
  dat1<-dat1 %>% distinct(smapeT1, .keep_all = TRUE)
  dat1<-dat1 %>% distinct(rmse, .keep_all = TRUE)
  erroresT2[[j]]<-as.matrix(dat1)
  return(erroresT2[[j]])
}

calculo4<-function(j){
  pca[[j]] <- prcomp(erroresT2[[j]][,2:7], scale = TRUE)
  if(sum(pca[[j]][["rotation"]][,1])<0) {erroresT2[[j]][,8]<- round(-(pca[[j]][["x"]][,1]),3)} else {erroresT2[[j]][,8]<- round(pca[[j]][["x"]][,1],3)}
  erroresT2[[j]][,9]<- round(pca[[j]][["x"]][,2],3)
  for (i in 1:dim(erroresT2[[j]])[1]) {
    erroresT2[[j]][i,10]<- round(abs(erroresT2[[j]][i,8] - min(erroresT2[[j]][,8])),3)
  }
  resultado[[1]][[j]]<-erroresT2[[j]]
  resultado[[2]][[j]]<-pca[[j]]
  return(resultado)
}

calculo4.m<-function(j){
  pca[[j]] <- prcomp(erroresT2[[j]][,2:7], scale = TRUE)
  a<-summary(pca[[j]])
  erroresT2[[j]][,8:9]<-round(pca[[j]][["x"]][,1:2],3)
  if(a[["importance"]][2,1]>0.80) {
    if(sum(pca[[j]][["rotation"]][3:6,1])<0) {for (i in 1:nrow(erroresT2[[j]])) {erroresT2[[j]][i,10]<- round(abs(erroresT2[[j]][i,8] - max(erroresT2[[j]][,8])),3)} } 
    else { for (i in 1:nrow(erroresT2[[j]])) {erroresT2[[j]][i,10]<- round(abs(erroresT2[[j]][i,8] - min(erroresT2[[j]][,8])),3)} }
  } else {
    if(sum(pca[[j]][["rotation"]][3:6,1])>0 & sum(pca[[j]][["rotation"]][1:2,2])>0) {
      m1 <- min(erroresT2[[j]][,8])
      m2 <- min(erroresT2[[j]][,9])} else if(sum(pca[[j]][["rotation"]][3:6,1])>0 & sum(pca[[j]][["rotation"]][1:2,2])<0) {
        m1 <- min(erroresT2[[j]][,8])
        m2 <- max(erroresT2[[j]][,9])} else if(sum(pca[[j]][["rotation"]][3:6,1])<0 & sum(pca[[j]][["rotation"]][1:2,2])>0) {
          m1 <- max(erroresT2[[j]][,8])
          m2 <- min(erroresT2[[j]][,9])} else {
            m1 <- max(erroresT2[[j]][,8])
            m2 <- max(erroresT2[[j]][,9])}
    
    #Manhattan
    for (i in 1:nrow(erroresT2[[j]])) {
      erroresT2[[j]][i,10]<- round(abs(erroresT2[[j]][i,8] - m1)+abs(erroresT2[[j]][i,9] - m2),3)
    }
  }
  resultado[[1]][[j]]<-erroresT2[[j]]
  resultado[[2]][[j]]<-pca[[j]]
  
  #Hay que reemplazar distancias 0 por distancias 0.001
  for (i in 1:nrow(resultado[[1]][[j]])) {
    resultado[[1]][[j]][i,10]<-ifelse(resultado[[1]][[j]][i,10]==0.000, 0.001,resultado[[1]][[j]][i,10])
  }
  comparacion_bic <- BIC(
    mlexp(resultado[[1]][[j]][,10]),
    mlinvgamma(resultado[[1]][[j]][,10]),
    mlgamma(resultado[[1]][[j]][,10])
  )
  resultado[[3]][[j]]<- comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
  return(resultado)
}

calculo5<-function(j,i){
  if(dis.ajust[j] ==1) {obj<-mlexp(resultado[[1]][[j]][,10])} else
    if(dis.ajust[j]==2) {obj<-mlgamma(resultado[[1]][[j]][,10])} else {obj<-mlinvgamma(resultado[[1]][[j]][,10])}
  
  dat<-subset(resultado[[1]][[j]],resultado[[1]][[j]][,10]<=round(qml(cota[i], obj = obj),4) )
  
  if(nrow(dat)<1) {dat<-subset(resultado[[1]][[j]], resultado[[1]][[j]][,1]==1)}
  
  #Calculo de los pesos
  a<-NULL
  b<-NULL
  for (m in 1:nrow(dat)) {
    a[m]<-round(1/dat[m,2],4) #w2: Consideramos la inversa del sMAPE de ajuste (T1) para cada método
    b[m]<-round(1/dat[m,5],4) #w3: Consideramos la inversa del sMAPE de predicción (T2) para cada método
  }
  for (m in 1:nrow(dat)) {
    dat[m,11]<- round(abs(dat[m,8])/sum(abs(dat[,8])),4)  #w1
    dat[m,12]<- round(a[m] / sum(a),4)  #w2
    dat[m,13]<- round(b[m] / sum(b),4)  #w3
  }
  resultado[[4]][[j]][[i]]<-dat
  return(resultado)
}

calculo6<-function(j){
  # Definimos los parámetros generales
  serie<-ensayo[j]
  n_p<-12 #predicciones
  n<-length(datos[[serie]]) #nº de datos
  if (n<200){ci=2}else{ci=10}
  estac<-12 #estacionalidad
  comienzo <- start(datos[[serie]])
  
  # Seleccionamos el periodo T1(x) + T2(x)
  x  <- ts(datos[[serie]][1:(n-n_p)],start = comienzo, frequency = estac) #T1
  if(end(x)[2]==estac) 
  {comienzo2<-c(end(x)[1]+1,1)
  }else 
  {comienzo2<-c(end(x)[1],end(x)[2]+1)}
  x2 <- ts(datos[[serie]][(n-n_p+1):n],start = comienzo2,frequency = estac) #T2
  
  if(j==1){m=61}else{m=846}
  if(j==1){m1=61}else{m1=50}
  set.seed(61) #Fijo la semilla de aleatorización para que sea reproducible
  metodos<-list()
  #naive
  metodos[[1]]<-naive(x,h=n_p)
  #Naïve con estacionalidad
  metodos[[2]]<-snaive(x,h=n_p)
  #Suavizado exponencial
  metodos[[3]]<-ets(x,model = "ZZZ", opt.crit="lik", damped = NULL)
  metodos[[3]]<-predict(metodos[[3]],n_p)
  metodos[[4]]<-ets(x,model = "ZZZ", opt.crit="mse", damped = NULL)
  metodos[[4]]<-predict(metodos[[4]],n_p)
  metodos[[5]]<-ets(x,model = "ZZZ", opt.crit="amse", damped = NULL)
  metodos[[5]]<-predict(metodos[[5]],n_p)
  metodos[[6]]<-ets(x,model = "ZZZ", opt.crit="sigma", damped = NULL)
  metodos[[6]]<-predict(metodos[[6]],n_p)
  metodos[[7]]<-ets(x,model = "ZZZ", opt.crit="mae", damped = NULL)
  metodos[[7]]<-predict(metodos[[7]],n_p)
  metodos[[8]]<-ets(x,model = "ZZZ", opt.crit="lik", damped = TRUE)
  metodos[[8]]<-predict(metodos[[8]],n_p)
  metodos[[9]]<-ets(x,model = "ZZZ", opt.crit="mse", damped = TRUE)
  metodos[[9]]<-predict(metodos[[9]],n_p)
  metodos[[10]]<-ets(x,model = "ZZZ", opt.crit="amse", damped = TRUE)
  metodos[[10]]<-predict(metodos[[10]],n_p)
  metodos[[11]]<-ets(x,model = "ZZZ", opt.crit="sigma", damped = TRUE)
  metodos[[11]]<-predict(metodos[[11]],n_p)
  metodos[[12]]<-ets(x,model = "ZZZ", opt.crit="mae", damped = TRUE)
  metodos[[12]]<-predict(metodos[[12]],n_p)
  #arima
  a<-NULL
  metodos[[13]]<-auto.arima(x, ic="aic",approximation=T, trace=FALSE, allowdrift=F)
  a<-predict(metodos[[13]],n_p)
  metodos[[13]]$mean<-a$pred
  metodos[[14]]<-auto.arima(x, ic="bic",approximation=T, trace=FALSE, allowdrift=F)
  a<-predict(metodos[[14]],n_p)
  metodos[[14]]$mean<-a$pred
  #Theta
  metodos[[15]]<-otm.arxiv(x, h=n_p, s= NULL, g= "sAPE")
  metodos[[16]]<-dotm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead") #Dynamic Optimised Theta Model
  metodos[[17]]<-dotm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B") #Dynamic Optimised Theta Model
  metodos[[18]]<-dotm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN") #Dynamic Optimised Theta Model
  metodos[[19]]<-dstm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead") #Dynamic Standard Theta Model
  metodos[[20]]<-dstm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B") #Dynamic Standard Theta Model
  metodos[[21]]<-dstm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN") #Dynamic Standard Theta Model
  metodos[[22]]<-otm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead")  #Optimised Theta Model
  metodos[[23]]<-otm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B")  #Optimised Theta Model
  metodos[[24]]<-otm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN")  #Optimised Theta Model
  metodos[[25]]<-stm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="Nelder-Mead")  #Standard Theta Model (Fiorucci)
  metodos[[26]]<-stm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="L-BFGS-B")  #Standard Theta Model (Fiorucci)
  metodos[[27]]<-stm(x, h=n_p, s= NULL, estimation=TRUE, opt.method="SANN")  #Standard Theta Model (Fiorucci)
  metodos[[28]]<-stheta(x, h=n_p, s= NULL) #Standard Theta Model (Assimakopoulos)
  #STL (sólo series estacionales)
  metodos[[29]]<-stlf(x,h=n_p, method=c("ets"),robust = TRUE,allow.multiplicative.trend = TRUE)
  metodos[[30]]<-stlf(x,h=n_p, method=c("arima"),robust = TRUE,allow.multiplicative.trend = TRUE)
  metodos[[31]]<-stlf(x,h=n_p, forecastfunction=thetaf,robust = TRUE,allow.multiplicative.trend = TRUE)
  
  #Croston
  metodos[[32]]<-crost(x,h=n_p,type = "croston",cost = "mar",init.opt=TRUE)
  metodos[[32]]$fitted<-ts(metodos[[32]]$frc.in, start =comienzo,frequency = estac) 
  metodos[[32]]$mean<-  ts(metodos[[32]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[33]]<-crost(x,h=n_p,type = "croston",cost = "msr",init.opt=TRUE)
  metodos[[33]]$fitted<-ts(metodos[[33]]$frc.in, start =comienzo,frequency = estac) 
  metodos[[33]]$mean<-  ts(metodos[[33]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[34]]<-crost(x,h=n_p,type = "croston",cost = "mae",init.opt=TRUE)
  metodos[[34]]$fitted<-ts(metodos[[34]]$frc.in, start =comienzo,frequency = estac)
  metodos[[34]]$mean<-  ts(metodos[[34]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[35]]<-crost(x,h=n_p,type = "croston",cost = "mse",init.opt=TRUE)
  metodos[[35]]$fitted<-ts(metodos[[35]]$frc.in, start =comienzo,frequency = estac)
  metodos[[35]]$mean<-  ts(metodos[[35]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[36]]<-crost(x,h=n_p,type = "sba",cost = "mar",init.opt=TRUE)
  metodos[[36]]$fitted<-ts(metodos[[36]]$frc.in, start =comienzo,frequency = estac)
  metodos[[36]]$mean<-  ts(metodos[[36]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[37]]<-crost(x,h=n_p,type = "sba",cost = "msr",init.opt=TRUE)
  metodos[[37]]$fitted<-ts(metodos[[37]]$frc.in, start =comienzo,frequency = estac)
  metodos[[37]]$mean<-  ts(metodos[[37]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[38]]<-crost(x,h=n_p,type = "sba",cost = "mae",init.opt=TRUE)
  metodos[[38]]$fitted<-ts(metodos[[38]]$frc.in, start =comienzo,frequency = estac)
  metodos[[38]]$mean<-  ts(metodos[[38]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[39]]<-crost(x,h=n_p,type = "sba",cost = "mse",init.opt=TRUE)
  metodos[[39]]$fitted<-ts(metodos[[39]]$frc.in, start =comienzo,frequency = estac)
  metodos[[39]]$mean<-  ts(metodos[[39]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[40]]<-crost(x,h=n_p,type = "sbj",cost = "mar",init.opt=TRUE)
  metodos[[40]]$fitted<-ts(metodos[[40]]$frc.in, start =comienzo,frequency = estac)
  metodos[[40]]$mean<-  ts(metodos[[40]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[41]]<-crost(x,h=n_p,type = "sbj",cost = "msr",init.opt=TRUE)
  metodos[[41]]$fitted<-ts(metodos[[41]]$frc.in, start =comienzo,frequency = estac)
  metodos[[41]]$mean<-  ts(metodos[[41]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[42]]<-crost(x,h=n_p,type = "sbj",cost = "mae",init.opt=TRUE)
  metodos[[42]]$fitted<-ts(metodos[[42]]$frc.in, start =comienzo,frequency = estac)
  metodos[[42]]$mean<-  ts(metodos[[42]]$frc.out, start =comienzo2,frequency = estac) 
  metodos[[43]]<-crost(x,h=n_p,type = "sbj",cost = "mse",init.opt=TRUE)
  metodos[[43]]$fitted<-ts(metodos[[43]]$frc.in, start =comienzo,frequency = estac)
  metodos[[43]]$mean<-  ts(metodos[[43]]$frc.out, start =comienzo2,frequency = estac) 
  
  #Prophet
  set.seed(61)
  df<-data.frame(ds = seq(as.Date(paste(start(x)[1],"-",start(x)[2],"-","01",sep="")),
                          as.Date(paste(end(x)[1],"-",end(x)[2],"-","01",sep="")),by="m"), y = x)
  metodos[[44]]<-prophet(df,yearly.seasonality=TRUE,
                         weekly.seasonality = FALSE,
                         daily.seasonality = FALSE,seasonality.mode = "additive")
  a<-make_future_dataframe(metodos[[44]],periods = 18,freq = "month")
  b<-predict(metodos[[44]],a)
  metodos[[44]]$fitted <- ts(b$yhat[1:length(x)],start = comienzo,frequency = estac)
  metodos[[44]]$mean <- ts(b$yhat[(length(x)+1):(length(x)+n_p)],start = comienzo2,frequency = estac)
  metodos[[45]]<-prophet(df,yearly.seasonality=TRUE,
                         weekly.seasonality = FALSE,
                         daily.seasonality = FALSE,seasonality.mode = "multiplicative")
  a<-make_future_dataframe(metodos[[45]],periods = 18,freq = "month")
  b<-predict(metodos[[45]],a)
  metodos[[45]]$fitted <- ts(b$yhat[1:length(x)],start = comienzo,frequency = estac)
  metodos[[45]]$mean <- ts(b$yhat[(length(x)+1):(length(x)+n_p)],start = comienzo2,frequency = estac)
  
  #Redes neuronales NNAR. No podemos hacer el for porque se supone que no tenemos T3
  set.seed(50)
  metodos[[46]]<-nnetar(x,p=estac,scale.inputs=T)
  a<-forecast(metodos[[46]],h=n_p)
  metodos[[46]]$mean<-a$mean
  
  #TBATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)
  set.seed(61)
  metodos[[47]]<-tbats(x, biasadj = TRUE)
  metodos[[47]]$fitted<-metodos[[47]]$fitted.values
  a<-forecast(metodos[[47]],h=n_p)
  metodos[[47]]$mean<-a$mean
  
  #GRNN
  set.seed(61)
  metodos[[48]]<-grnn_forecasting(x, h = n_p, transform = "additive")
  a<-rolling_origin(metodos[[48]],h = length(x)-estac-1)
  metodos[[48]]$fitted<-a$predictions[,1]
  metodos[[48]]$mean<-metodos[[48]]$prediction
  metodos[[49]]<-grnn_forecasting(x, h = n_p, transform = "multiplicative")
  a<-rolling_origin(metodos[[49]],h = length(x)-estac-1)
  metodos[[49]]$fitted<-a$predictions[,1]
  metodos[[49]]$mean<-metodos[[49]]$prediction
  
  #MLP
  # En fitted del 14:n, las primeras n_p+2 son NA
  set.seed(61)
  metodos[[50]]<-mlp.thief(x, h=12)
  # En fitted del 5:n, las primeras 4 son vacías
  metodos[[51]]<-mlp(x, m=12, comb = c("median"),hd = c(10,5))
  a<-forecast(metodos[[51]],h=12)
  metodos[[51]]$mean<-a$mean
  metodos[[52]]<-mlp(x, m=12, comb = c("mean"),hd = c(10,5))
  a<-forecast(metodos[[52]],h=12)
  metodos[[52]]$mean<-a$mean
  
  return(metodos)
}

calculo7.s<-function(j,i,b1,b2,b3){
  b1=b2=b3=list()  
  n_p<-12 #predicciones
  dat<-resultado[[4]][[j]][[i]]
  #Matriz predicciones métodos para T2
  pred_T2<-pred_T3<-matrix(NA,nrow=dim(dat)[1],ncol = n_p)
  for (k in 1:dim(dat)[1]) {
    for (kk in 1:length(metodos.T1[[j]])) {
      if(dat[k,1]==kk){pred_T2[k,]<-metodos.T1[[j]][[kk]]$mean; pred_T3[k,]<-metodos.T12[[j]][[kk]]$mean}
    }
  }
  a1<-a2<-a3<-b1<-b2<-b3<-rep(NA,n_p)
  for (k in 1:n_p) {
    a1[k]<-pred_T2[,k]%*%dat[,11]
    a2[k]<-pred_T2[,k]%*%dat[,12]
    a3[k]<-pred_T2[,k]%*%dat[,13]
    b1[k]<-pred_T3[,k]%*%dat[,11]
    b2[k]<-pred_T3[,k]%*%dat[,12]
    b3[k]<-pred_T3[,k]%*%dat[,13]
  }
 
  # Definimos los parámetros generales
  serie<-ensayo[j]
  estac<-12 #estacionalidad
  n<-length(datos[[serie]]) #nº de datos
  comienzo <- start(datos[[serie]])
  
  # Seleccionamos el periodo T1(x)+ T2(x)
  x  <- ts(datos[[serie]][1:(n-(2*n_p))],start = comienzo, frequency = estac) #T1
  if(end(x)[2]==estac) 
  {comienzo1<-c(end(x)[1]+1,1)
  }else 
  {comienzo1<-c(end(x)[1],end(x)[2]+1)}
  x1 <- ts(datos[[serie]][(n-(2*n_p)+1):(n-n_p)], start = comienzo1,frequency = estac) #T2
  if(end(x1)[2]==estac) 
  {comienzo2<-c(end(x1)[1]+1,1)
  }else 
  {comienzo2<-c(end(x1)[1],end(x1)[2]+1)}
  x2 <- ts(datos[[serie]][(n-n_p+1):n],start = comienzo2,frequency = estac) #T2
  
  pred_comb<-matrix(NA,nrow = length(cota),ncol = 8)
  colnames(pred_comb)<-c("serie","n.comb","w1.T2","w2.T2","w3.T2","w1.T3","w2.T3","w3.T3")
  pred_comb[i,1] <- as.integer(serie)
  pred_comb[i,2] <- as.integer(dim(dat)[1])
  pred_comb[i,3] <- round(100*smape(a1,x1),3)
  pred_comb[i,4] <- round(100*smape(a2,x1),3)
  pred_comb[i,5] <- round(100*smape(a3,x1),3)
  pred_comb[i,6] <- round(100*smape(b1,x2),3)
  pred_comb[i,7] <- round(100*smape(b2,x2),3)
  pred_comb[i,8] <- round(100*smape(b3,x2),3)

  return(pred_comb[i,])
}

calculo7.sOR<-function(j,resultadoSMAPE,ord){
	resultado[[5]]=resultadoSMAPE
	or=order(resultado[[5]][[1]][,6])[1]
	p1=or;w1=resultado[[5]][[1]][or,6]
	or=order(resultado[[5]][[1]][,7])[1]
	p2=or;w2=resultado[[5]][[1]][or,7]
	or=order(resultado[[5]][[1]][,8])[1]
	p3=or;w3=resultado[[5]][[1]][or,8]
	p1;p2;p3
	w1;w2;w3
	w=c(w1,w2,w3);or=order(w)[1]
	if(or==1){peso=p1;wi=1}
	if(or==2){peso=p2;wi=2}
	if(or==3){peso=p1;wi=3}
	wT3=c("w1.T3","w2.T3","w3.T3")
	ppeso=cota[peso]*100
	peso;wi;wT3[wi];ppeso
	ord=c()
	ord[1]=peso;ord[2]=w1;ord[3]=wT3[wi];ord[4]=ppeso
	return(ord)
}

calculo7.r<-function(j,i){
  n_p<-12 #predicciones
  dat<-resultado[[4]][[j]][[i]]
  #Matriz predicciones métodos para T2
  pred_T2<-pred_T3<-matrix(NA,nrow=dim(dat)[1],ncol = n_p)
  for (k in 1:dim(dat)[1]) {
    for (kk in 1:length(metodos.T1[[j]])) {
      if(dat[k,1]==kk){pred_T2[k,]<-metodos.T1[[j]][[kk]]$mean; pred_T3[k,]<-metodos.T12[[j]][[kk]]$mean}
    }
  }
  a1<-a2<-a3<-b1<-b2<-b3<-rep(NA,n_p)
  for (k in 1:n_p) {
    a1[k]<-pred_T2[,k]%*%dat[,11]
    a2[k]<-pred_T2[,k]%*%dat[,12]
    a3[k]<-pred_T2[,k]%*%dat[,13]
    b1[k]<-pred_T3[,k]%*%dat[,11]
    b2[k]<-pred_T3[,k]%*%dat[,12]
    b3[k]<-pred_T3[,k]%*%dat[,13]
  }
  # Definimos los parámetros generales
  serie<-ensayo[j]
  estac<-12 #estacionalidad
  n<-length(datos[[serie]]) #nº de datos
  comienzo <- start(datos[[serie]])
  
  # Seleccionamos el periodo T1(x)+ T2(x)
  x  <- ts(datos[[serie]][1:(n-(2*n_p))],start = comienzo, frequency = estac) #T1
  if(end(x)[2]==estac) 
  {comienzo1<-c(end(x)[1]+1,1)
  }else 
  {comienzo1<-c(end(x)[1],end(x)[2]+1)}
  x1 <- ts(datos[[serie]][(n-(2*n_p)+1):(n-n_p)], start = comienzo1,frequency = estac) #T2
  if(end(x1)[2]==estac) 
  {comienzo2<-c(end(x1)[1]+1,1)
  }else 
  {comienzo2<-c(end(x1)[1],end(x1)[2]+1)}
  x2 <- ts(datos[[serie]][(n-n_p+1):n],start = comienzo2,frequency = estac) #T2
  
  
  pred_comb<-matrix(NA,nrow = length(cota),ncol = 8)
  colnames(pred_comb)<-c("serie","n.comb","w1.T2","w2.T2","w3.T2","w1.T3","w2.T3","w3.T3")
  pred_comb[i,1] <- as.integer(serie)
  pred_comb[i,2] <- as.integer(dim(dat)[1])
  pred_comb[i,3] <- round(rmse(a1,x1),3)
  pred_comb[i,4] <- round(rmse(a2,x1),3)
  pred_comb[i,5] <- round(rmse(a3,x1),3)
  pred_comb[i,6] <- round(rmse(b1,x2),3)
  pred_comb[i,7] <- round(rmse(b2,x2),3)
  pred_comb[i,8] <- round(rmse(b3,x2),3)
  
  return(pred_comb[i,])
}

calculo7.m<-function(j,i){
  n_p<-12 #predicciones
  dat<-resultado[[4]][[j]][[i]]
  #Matriz predicciones métodos para T2
  pred_T2<-pred_T3<-matrix(NA,nrow=dim(dat)[1],ncol = n_p)
  for (k in 1:dim(dat)[1]) {
    for (kk in 1:length(metodos.T1[[j]])) {
      if(dat[k,1]==kk){pred_T2[k,]<-metodos.T1[[j]][[kk]]$mean; pred_T3[k,]<-metodos.T12[[j]][[kk]]$mean}
    }
  }
  a1<-a2<-a3<-b1<-b2<-b3<-rep(NA,n_p)
  for (k in 1:n_p) {
    a1[k]<-pred_T2[,k]%*%dat[,11]
    a2[k]<-pred_T2[,k]%*%dat[,12]
    a3[k]<-pred_T2[,k]%*%dat[,13]
    b1[k]<-pred_T3[,k]%*%dat[,11]
    b2[k]<-pred_T3[,k]%*%dat[,12]
    b3[k]<-pred_T3[,k]%*%dat[,13]
  }
  # Definimos los parámetros generales
  serie<-ensayo[j]
  estac<-12 #estacionalidad
  n<-length(datos[[serie]]) #nº de datos
  comienzo <- start(datos[[serie]])
  
  # Seleccionamos el periodo T1(x)+ T2(x)
  x  <- ts(datos[[serie]][1:(n-(2*n_p))],start = comienzo, frequency = estac) #T1
  if(end(x)[2]==estac) 
  {comienzo1<-c(end(x)[1]+1,1)
  }else 
  {comienzo1<-c(end(x)[1],end(x)[2]+1)}
  x1 <- ts(datos[[serie]][(n-(2*n_p)+1):(n-n_p)], start = comienzo1,frequency = estac) #T2
  if(end(x1)[2]==estac) 
  {comienzo2<-c(end(x1)[1]+1,1)
  }else 
  {comienzo2<-c(end(x1)[1],end(x1)[2]+1)}
  x2 <- ts(datos[[serie]][(n-n_p+1):n],start = comienzo2,frequency = estac) #T2
  
  
  pred_comb<-matrix(NA,nrow = length(cota),ncol = 8)
  colnames(pred_comb)<-c("serie","n.comb","w1.T2","w2.T2","w3.T2","w1.T3","w2.T3","w3.T3")
  pred_comb[i,1] <- as.integer(serie)
  pred_comb[i,2] <- as.integer(dim(dat)[1])
  pred_comb[i,3] <- round(mae(a1,x1),3)
  pred_comb[i,4] <- round(mae(a2,x1),3)
  pred_comb[i,5] <- round(mae(a3,x1),3)
  pred_comb[i,6] <- round(mae(b1,x2),3)
  pred_comb[i,7] <- round(mae(b2,x2),3)
  pred_comb[i,8] <- round(mae(b3,x2),3)
  
  return(pred_comb[i,])
}

calculo7.N<-function(j,i){
  n_p<-12 #predicciones
  dat<-resultado[[4]][[j]][[i]]
  #Matriz predicciones métodos para T2
  pred_T2<-pred_T3<-matrix(NA,nrow=dim(dat)[1],ncol = n_p)
  for (k in 1:dim(dat)[1]) {
    for (kk in 1:length(metodos.T1[[j]])) {
      if(dat[k,1]==kk){pred_T2[k,]<-metodos.T1[[j]][[kk]]$mean; pred_T3[k,]<-metodos.T12[[j]][[kk]]$mean}
    }
  }
  a1<-a2<-a3<-b1<-b2<-b3<-rep(NA,n_p)
  for (k in 1:n_p) {
    a1[k]<-pred_T2[,k]%*%dat[,11]
    a2[k]<-pred_T2[,k]%*%dat[,12]
    a3[k]<-pred_T2[,k]%*%dat[,13]
    b1[k]<-pred_T3[,k]%*%dat[,11]
    b2[k]<-pred_T3[,k]%*%dat[,12]
    b3[k]<-pred_T3[,k]%*%dat[,13]
  }
  # Definimos los parámetros generales
  serie<-ensayo[j]
  estac<-12 #estacionalidad
  n<-length(datos[[serie]]) #nº de datos
  comienzo <- start(datos[[serie]])
  
  # Seleccionamos el periodo T1(x)+ T2(x)
  x  <- ts(datos[[serie]][1:(n-(2*n_p))],start = comienzo, frequency = estac) #T1
  if(end(x)[2]==estac) 
  {comienzo1<-c(end(x)[1]+1,1)
  }else 
  {comienzo1<-c(end(x)[1],end(x)[2]+1)}
  x1 <- ts(datos[[serie]][(n-(2*n_p)+1):(n-n_p)], start = comienzo1,frequency = estac) #T2
  if(end(x1)[2]==estac) 
  {comienzo2<-c(end(x1)[1]+1,1)
  }else 
  {comienzo2<-c(end(x1)[1],end(x1)[2]+1)}
  x2 <- ts(datos[[serie]][(n-n_p+1):n],start = comienzo2,frequency = estac) #T2
  
  
  pred_comb<-matrix(NA,nrow = length(cota),ncol = 8)
  colnames(pred_comb)<-c("serie","n.comb","w1.T2","w2.T2","w3.T2","w1.T3","w2.T3","w3.T3")
  pred_comb[i,1] <- as.integer(serie)
  pred_comb[i,2] <- as.integer(dim(dat)[1])
  pred_comb[i,3] <- round(NSE(a1,x1),3)
  pred_comb[i,4] <- round(NSE(a2,x1),3)
  pred_comb[i,5] <- round(NSE(a3,x1),3)
  pred_comb[i,6] <- round(NSE(b1,x2),3)
  pred_comb[i,7] <- round(NSE(b2,x2),3)
  pred_comb[i,8] <- round(NSE(b3,x2),3)
  
  return(pred_comb[i,])
}


graf<-function(j,w) {
  x<-y1<-y2<-a<-b<-n<-NULL
  x<-resultado[[4]][[j]][,2]  #Nº comb
  y1<-resultado[[4]][[j]][,w+2] #sMape en T2 con w1
  y2<-resultado[[4]][[j]][,w+5] #sMape en T3 con w1
  
  df<-rbind(cbind(cota,x,y1,rep("T2",length(x))),cbind(cota,x,y2,rep("T3",length(x))))
  df<-as.data.frame(df);colnames(df)<-c("cota", "comb","sMape","Periodo")
  df$cota<-as.numeric(df$cota)
  df$comb<-as.integer(df$comb)
  df$sMape<-as.numeric(df$sMape)
  df$Periodo<-as.factor(df$Periodo)
  pp<-NULL
  pp<-ggplot(df, aes(x = cota, y = sMape, label=comb)) +
    geom_line(aes(colour = Periodo))+
    geom_text(hjust=0, vjust=-0.5)+
    theme_classic()
}




