library(tidyverse)
library(sampling)
library(samplingVarEst)
library(psych)
library(readr)
library(dplyr)
library(knitr)

#Declaramos las funciones que vamos a utilizar:

descenso <- function(n, z_0, eta, h_deriv){
  z <- matrix(0,n, length(z_0))
  z[1, ] <- z_0
  for(i in 1:(n-1)){
    z[i+1, ] <- z[i, ] - eta * h_deriv(z[i, ])
  }
  z
}

grad_calc <- function(x_ent, y_ent){
  salida_grad <- function(beta){
    f_beta <- as.matrix(cbind(1, x_ent)) %*% beta
    e <- y_ent - f_beta
    grad_out <- -as.numeric(t(cbind(1, x_ent)) %*% e)
    names(grad_out) <- c('Intercept', colnames(x_ent))
    grad_out
  }
  salida_grad
}

#Para pruebas 
#iteraciones <- descenso(500000, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), 0.00005, grad_viv)
#grad_viv <- grad_calc(viv_entrena_norm[, 1:13, drop = FALSE], viv_entrena_norm[,14])

#Checar la función del error.
rss_calc <- function(datos){
  # esta función recibe los datos (x,y) y devuelve
  # una función f(betas) que calcula rss
  y <- datos$lpsa
  x <- datos$lcavol
  fun_out <- function(beta){
    y_hat <- beta[1] + beta[2]*x
    e <- (y - y_hat)
    rss <- sum(e^2)
    0.5*rss
  }
  fun_out
}

#Descargamos la información 
viviendas<-read_table2("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names =  FALSE )

names(viviendas)<-c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
names(viviendas)
#Separamos la muestra
set.seed(1923430484)

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

#Vamos a dividir el ajuste en dos casos: 
#1° No hacemos reconocimiento entre variables númericas y categoricas.


viv_entrena_norm<-as.data.frame(viv_entrena_norm)


for (i in 1:13){
    prom<-mean(viv_entrena_norm[,i] )
    #print (prom)
    ed<-sd(viv_entrena_norm[,i])
    #print(ed)
    viv_entrena_norm[,i]<-(viv_entrena_norm[,i]-prom)/ed
}
describe(viv_entrena_norm)


grad_viv <- grad_calc(viv_entrena_norm[, 1:13, drop = FALSE], viv_entrena_norm$MEDV)

grad_viv(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))

rep(0,14)
iteraciones <- descenso(500, c(rep(10,14)), 0.0007, grad_viv)
iteraciones[500,]

#Verificamos que el gradiente convergio, es decir, este se acerca a 0
grad_viv(iteraciones[500,])

#Ajustamos el modelo con la funcion lm
model_test<-lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT,data = viv_entrena_norm)
summary(model_test)

#Podemos observar que la $\beta$ son iguales, por lo que el modelo es correcto.


rss_viv <- rss_calc(viv_entrena_norm)
apply(iteraciones, 1, rss_viv)






####2° No hacemos reconocimiento entre variables númericas y categoricas.#####

for (i in 1:14){
  if (i != 4)
  viv_entrena_norm[,i]<-(viv_entrena_norm[,i]-mean(viv_entrena_norm[,i])/sd(viv_entrena_norm[,i]))
}




