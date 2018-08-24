library(tidyverse)
library(sampling)
library(samplingVarEst)
library(psych)
library(readr)
library(dplyr)
library(knitr)


viviendas<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", header = FALSE )

names(viviendas)<-c("CRIM","ZN","INDUS,","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
names(viviendas)
#Separamos la muestra
set.seed(1993)

n <- 400
N <- nrow(viviendas)
Muestra <- srswor1(n,N)
entrena <- which(Muestra == 1)
prueba <- which(Muestra == 0)

viv_entrena<-viviendas[entrena,]
viv_prueba<-viviendas[prueba,]
#Vamos a estimar para MEDV

#Comenzamos co las medidas de tendencia central 

summary(viv_entrena)
describe(viv_entrena)

#Normalizamos las variables:
viv_entrena_norm<-viv_entrena

for (i in 1:14){
  if (i != 4)
  viv_entrena_norm[,i]<-(viv_entrena_norm[,i]-mean(viv_entrena_norm[,i])/sd(viv_entrena_norm[,i]))
}


optim()
descen


