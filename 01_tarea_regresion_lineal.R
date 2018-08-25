library(tidyverse)
library(sampling)
library(samplingVarEst)
library(psych)
library(readr)
library(dplyr)
library(pander)
library(knitr)

#Declaramos las funciones que vamos a utilizar:

#funcion para el descenso
descenso <- function(n, z_0, eta, h_deriv){
  z <- matrix(0,n, length(z_0))
  z[1, ] <- z_0
  for(i in 1:(n-1)){
    z[i+1, ] <- z[i, ] - eta * h_deriv(z[i, ])
  }
  z
}

#Funcion que calcula la derivada:
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

#Funcion para calcular el error:
rss_calc <- function(x,y){
  # esta función recibe los datos (x,y) y devuelve
  # una función f(betas) que calcula rss
  fun_out <- function(beta){
    y_hat <- as.matrix(cbind(1,x))%*%beta
    e <- y - y_hat
    rss <- sum(e^2)
    0.5*rss
  }
  fun_out
}

#Funcion para calcular el error
error_f <- function(preds,y){
    round(sqrt(mean((preds - y) ^ 2)),2)
  }

#Funcion para predicciones
pred<-function(x){
  function(betas){
    f_beta<-as.matrix(cbind(1,x))%*%betas
    return(f_beta)
  }
}

#Descargamos la información 
viviendas<-read_table2("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names =  FALSE )

names(viviendas)<-c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
names(viviendas)
#Separamos la muestra
set.seed(1993)
#Para usar muestra aleatoria:
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
est_des<-describe(viv_entrena)
est_des

#Normalizamos las variables:
viv_entrena_norm<-viv_entrena

#No hacemos reconocimiento entre variables númericas y categoricas.
viv_entrena_norm<-as.data.frame(viv_entrena_norm)

for (i in 1:13){
    prom<-mean(viv_entrena_norm[,i] )
    #print (prom)
    ed<-sd(viv_entrena_norm[,i])
    #print(ed)
    viv_entrena_norm[,i]<-(viv_entrena_norm[,i]-prom)/ed
}



grad_viv <- grad_calc(viv_entrena_norm[, 1:13, drop = FALSE], viv_entrena_norm$MEDV)
grad_viv(c(rep(0,14)))

iteraciones <- descenso(500, c(rep(10,14)), 0.0007, grad_viv)
iteraciones[500,]

#Verificamos que el gradiente convergio, es decir, este se acerca a 0
grad_viv(iteraciones[500,])

#Ajustamos el modelo con la funcion lm
model_test<-lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT,data = viv_entrena_norm)
summary(model_test)

#Podemos observar que la $\beta$ son iguales, por lo que el modelo es correcto.
#Procedemos a evaluar el error de entrenamiento:

#Verificamos que si disminuye.
rss_viv<- rss_calc(viv_entrena_norm[, 1:13, drop = FALSE], viv_entrena_norm$MEDV)
apply(iteraciones, 1, rss_viv)


#Finalmente, calculamos error de entrenamiento y error de prueba:
betas<-iteraciones[500,]
names(betas)<-c("Intercep",names(viv_entrena[,1:13]))

#Error de entrenamiento:
pred_entrena<-pred(viv_entrena_norm[,1:13])
e_entrena<-error_f(pred_entrena(betas),viv_entrena_norm$MEDV)

#aplicamamos la normalizacion a los datos:
viv_prueba_norm<-viv_prueba
viv_prueba_norm<-as.data.frame(viv_prueba_norm)

for (i in 1:13){
  viv_prueba_norm[,i]<-(viv_prueba_norm[,i]-est_des$mean[i])/(est_des$sd[i])
}


# Observamos como nos va con las predicciones
res_prueba<-pred(viv_prueba_norm[,1:13])
e_prueba<-error_f(res_prueba(betas),viv_prueba$MEDV)

#Observamos que los errorees encontrados son
e_entrena
#4.78
e_prueba
#4.38
