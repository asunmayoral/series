t <- proc.time()
library(Metrics); library(forecast); library(forecTheta);library(tsfgrnn);library(tsintermittent); 
library(prophet);library(yager);library(dplyr);library(univariateML);library(tseries); 
library(nnfor); library(thief);library(tidyverse);library(Metrics)

## Working Directory
#setwd("E:\\alPCA_menu")  

## Data upload
# e.g.: x <- readRDS("ford.rds")
datos<-list(x=x)

## Source file of functions
f_menu = "https://raw.githubusercontent.com/asunmayoral/series/main/f_menu.r"
source(f_menu)

## The length of the test is given by the number of series
ensayo<-1:length(datos)

## Apply each method with all its configurations in period T1
metodos.T1<-list()
for (j in 1:length(ensayo)) {
  metodos.T1[[j]]<-calculo1(j)
}

## Obtains loss functions in T1 and T2
for (j in 1:length(ensayo)) {
  metodos.T1[[j]]<-calculo2(j) 
}
erroresT2<-list() 
for (j in 1:length(ensayo)) {
  erroresT2[[j]]<-calculo3(j)
}

## Calculates PCA and Manhattan distance to the method associated with the smallest error
resultado<-list()
pca<-resultado[[1]]<-resultado[[2]]<-resultado[[3]]<-list()
dis.ajust<-rep(NA,length(ensayo))
for (j in 1:length(ensayo)) {
  resultado<-calculo4.m(j) 
  if(str_detect(resultado[[3]][[j]][1,1], "mlexp")==TRUE) {dis.ajust[j]<-1} else 
    if(str_detect(resultado[[3]][[j]][1,1], "mlgamma")==TRUE) {dis.ajust[j]<-2} else {dis.ajust[j]<-3}
}

## alPCA establishes the percentiles of the adjusted distribution as cut-off points for method selection
cota<-c(0.05,0.10,0.15,0.20,0.25,0.50,0.75, 0.80, 0.85, 0.90, 0.95, 1 ) 
resultado[[4]]<-list()
for (j in 1:length(ensayo)) {  
  resultado[[4]][[j]]<-list()
  for (i in 1:length(cota)) {  
  resultado<-calculo5(j,i)
  }
}

## Obstain the predictions for the period T1+T2
metodos.T12<-list()
for (j in 1:length(ensayo)) {
  metodos.T12[[j]]<-calculo6(j)
}

## Obtain the predictions for Obtains the predictions for T3 according to the percentile chosen by the user
j=1;n_p=12;pred5=pred50=pred100=c();yu=1
	  a1<-a2<-a3<-b1<-b2<-b3<-rep(NA,n_p)
##for (j in 1:length(ensayo)) {
  for (i in 1:length(cota)) {
 
	  n_p<-12 #predicciones
	  dat<-resultado[[4]][[j]][[i]]
	  ##Matriz predicciones métodos para T2
	  pred_T2<-pred_T3<-matrix(NA,nrow=dim(dat)[1],ncol = n_p)
		  for (k in 1:dim(dat)[1]) {
			for (kk in 1:length(metodos.T1[[j]])) { 
			  #print(metodos.T12[[j]][[kk]]$mean)    ##################### OJO sí se obtienen las predicciones de los 52 metodos<<<<<<<<<<<<<<
			  if(dat[k,1]==kk){pred_T2[k,]<-metodos.T1[[j]][[kk]]$mean; pred_T3[k,]<-metodos.T12[[j]][[kk]]$mean}
			}
		  }
	  for (k in 1:n_p) {
		a1[k]<-pred_T2[,k]%*%dat[,11]
		a2[k]<-pred_T2[,k]%*%dat[,12]
		a3[k]<-pred_T2[,k]%*%dat[,13]
		b1[k]<-pred_T3[,k]%*%dat[,11];if(i==1){pred5[k]=b1[k]}; if(i==6){pred50[k]=b1[k]}; if(i==12){pred100[k]=b1[k]}
		b2[k]<-pred_T3[,k]%*%dat[,12]  
		b3[k]<-pred_T3[,k]%*%dat[,13]  
	  }
  }

## Dialogue with the user and printout of the prediction
library(svDialogs)
yu=1
while(yu<2){
perc=dlgInput(message="Ingrese un percentil (5, 50 o 100) para calcular la Predicción: ")$res
perc=as.integer(perc)
if (perc==5){cat(" Predicción correspondiente al percentil 5: ",pred5,"\n","\n")}
if (perc==50){cat(" Predicción correspondiente al percentil 50: ",pred50,"\n","\n")}
if (perc==100){cat(" Predicción correspondiente al percentil 100: ",pred5,"\n","\n")}
yu=2}
proc.time()-t

#
