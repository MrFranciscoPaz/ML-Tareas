#Para tarea
#Se debe cargar el markdown "01-introcuccion.Rmd" y correr para poder cargar las variables del ambiente.


x <- c(1,7,10,0,0,5,9,13,2,4,17,18,1,2)

f <- function(x){
  ifelse(x < 10, 1000*sqrt(x), 1000*sqrt(10))
}


set.seed(1029384756)#280572 #2018  #1993 #1029384756 #7
error <- rnorm(length(x), 0, 500)
y <- f(x) + error
datos_entrena <- data.frame(x=x, y=y)
head(datos_entrena)
curva_1 <- geom_smooth(data=datos_entrena,
                       method = "loess", se=FALSE, color="gray", span=1, size=1.1)
curva_2 <- geom_smooth(data=datos_entrena,
                       method = "loess", se=FALSE, color="red", span=0.5, size=1.1)
curva_3 <- geom_smooth(data=datos_entrena,
                       method = "lm", se=FALSE, color="blue", size=1.1)

ggplot(datos_entrena, aes(x=x, y=y)) + geom_point() + 
  curva_1 + curva_2 + curva_3

mod_rojo <- loess(y ~ x, data = datos_entrena, span=0.5)
mod_gris <- loess(y ~ x, data = datos_entrena, span=1)
mod_recta <- lm(y ~ x, data = datos_entrena)
df_mods <- data_frame(nombre = c('recta', 'rojo','gris'))
df_mods$modelo <- list(mod_recta, mod_rojo, mod_gris)


error_f <- function(df, mod){
  function(mod){
    preds <- predict(mod, newdata = df)
    round(sqrt(mean((preds - df$y) ^ 2)))
  }
}
error_ent <- error_f(datos_entrena)

df_mods <- df_mods %>% 
  mutate(error_entrena = map_dbl(modelo, error_ent))
df_mods


set.seed(218052272)
x_0 <- sample(0:13, 100, replace = T)
error <- rnorm(length(x_0), 0, 500)
y_0 <- f(x_0) + error
datos_prueba <- data_frame(x = x_0, y = y_0)
datos_prueba

error_p <- error_f(datos_prueba)
df_mods <- df_mods %>% 
  mutate(error_prueba = map_dbl(modelo, error_p))
df_mods


df_ant$error_entrena1029384756<-df_mods$error_entrena
df_ant$error_prueba1029384756<-df_mods$error_prueba
df_ant

df_ant_guarda<-df_ant[,-2]
write.csv(df_ant_guarda,"01_tarea_cambiando_semilla_entrenamiento.csv")



