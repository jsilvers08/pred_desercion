---
title: "R_Desercion"
output: html_desercion
---
  
#Definimos el directorio de trabajo

setwd("C:/Users/julio/OneDrive - Universidad del Pacífico/Julio Silvers/Algoritmo")

#Llamada a librerías

#Librerias Generales
library(ggplot2)
library(dplyr)
library(haven)
library(caTools)
library(readxl)
library(writexl)
library(colorspace)



#Importamos data

data = read.csv("Base.csv", sep = ";")

#Se convierten a factores las variables Categóricas
data$Genero = as.factor(data$Genero)
data$Provincia = as.factor(data$Provincia)
data$Estado_civil = as.factor(data$Estado_civil)
data$emplea_ingresos = as.factor(data$emplea_ingresos)
data$niv_form_padre = as.factor(data$niv_form_padre)
data$niv_form_padre = as.factor(data$niv_form_padre)
data$Tipo_vivienda = as.factor(data$Tipo_vivienda)
data$tipo_const_vivienda = as.factor(data$tipo_const_vivienda)
data$Internet = as.factor(data$Internet)
data$cert_practicas = as.factor(data$cert_practicas)
data$Cert_ingles_A2 = as.factor(data$Cert_ingles_A2)
data$Trabaja = as.factor(data$Trabaja)
data$Colegio_especializacion = as.factor(data$Colegio_especializacion)
data$Nivel_Ingles = as.factor(data$Nivel_Ingles)
data$Sist_ingreso = as.factor(data$Sist_ingreso)
data$Tipo_Colegio = as.factor(data$Tipo_Colegio)
data$Tipo_matricula = as.factor(data$Tipo_matricula)
data$Condicion = as.factor(data$Condicion)
data$motivo_Beca_1 = as.factor(data$motivo_Beca_1)
data$motivo_Beca_2 = as.factor(data$motivo_Beca_2)

#Se muestra estadística descriptiva de todas las variables.
summary(data)

#Realizamos el Histograma de todas las variables numéricas
plot_histogram(data)


#Diagrama de barras de Continuantes
ggplot(data = data, aes(x = Continua)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 3.5, colour = "white") + 
  labs(x = "Continuantes: 0 = Deserta, 1 = Continúa", y = "No. de Propiedades")+
  theme_bw()


#Realizamos diagramas de cajas de las variables numéricas para observar la dispersión de datos
boxplot(data$Edad, horizontal = TRUE, main="Edades de los Estudiantes [en años]",col="lightgreen")
boxplot(data$ing_hogar, horizontal = TRUE, main="Ingresos en el Hogar [en $]",col="lightgreen")
boxplot(data$ing_eve, horizontal = TRUE, main="Ingresos eventuales [en $]",col="lightgreen")
boxplot(data$num_miembros_hogar, horizontal = TRUE, main="Número de miembros en el hogar",col="lightgreen")
boxplot(data$Tiempo_trab, horizontal = TRUE, main="Tiempo trabajando [en años]",col="lightgreen")
boxplot(data$Porc_Beca, horizontal = TRUE, main="Porcentajes de Beca [en %]",col="lightgreen")


#Vamos a verificar si existe previamente, una relación lineal entre la variable independiente Edad y cada una de las variables predictorias. Para esto se construye un diagrama de dispersión
#Diagrama de dispersión Cantidad de Materias aprobadas vs Edad
ggplot(data = data, aes(x=Edad, y = Continua)) + geom_point()+
  theme_bw()+ 
  labs(x="Total materias Aprob.", y="Edad")

#Diagrama de dispersión Cantidad de Materias aprobadas vs Ingresos en el hogar
ggplot(data = data, aes(x=Total_Aprob, y = ing_hogar)) + geom_point()+
  theme_bw()+ 
  labs(x="Total materias Aprob.", y="Ingresos en el hogar en $")

#Diagrama de dispersión Cantidad de Materias aprobadas vs Ingresos Eventuales
ggplot(data = data, aes(x=Total_Aprob, y = ing_eve)) + geom_point()+
  theme_bw()+ 
  labs(x="Total materias Aprob.", y="Ingresos Ecentuales en $")

#Diagrama de dispersión Cantidad de Materias aprobadas vs Numero de miembros en el hogar 
ggplot(data = data, aes(x=Total_Aprob, y = num_miembros_hogar)) + geom_point()+
  theme_bw()+ 
  labs(x="Total materias Aprob.", y="# de miembros")

#Diagrama de dispersión Cantidad de Materias aprobadas vs Tiempo trabajando 
ggplot(data = data, aes(x=Total_Aprob, y = Tiempo_trab)) + geom_point()+
  theme_bw()+ 
  labs(x="Total materias Aprob.", y="Tiempo trabajando")

#Diagrama de dispersión Cantidad de Materias aprobadas vs Porcentaje de Beca 
ggplot(data = data, aes(x=Total_Aprob, y = Porc_Beca)) + geom_point()+
  theme_bw()+ 
  labs(x="Total materias Aprob.", y="Porcentaje de Beca en %")




########################################################################
#Histograma para la variable Edad#######################################
#######################################################################
ggplot(data, aes(x = Edad)) + geom_histogram(bins = 22) + 
  labs(x = "Edad", y = "Frecuencia Absoluta") +
  geom_vline(xintercept = mean(data$Edad, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "red") + 
  geom_vline(xintercept = median(data$Edad, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "blue") +
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 1.5,
           hjust = 1.1,
           label = paste("Media =", round(mean(data$Edad, na.rm = TRUE),2)),
           col = "red",
           size = 5) + 
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 3.1,
           hjust = 1.1,
           label = paste("Mediana =", round(median(data$Edad, na.rm = TRUE),2)),
           col = "blue",
           size = 5)

#################################################

########################################################################
#Histograma para la variable Ingresos eventuales########################
#######################################################################

ggplot(data, aes(x = ing_eve)) + geom_histogram(bins = 30) + 
  labs(x = "Ingresos eventuales", y = "Frecuencia Absoluta") +
  geom_vline(xintercept = mean(data$ing_eve, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "red") + 
  geom_vline(xintercept = median(data$ing_eve, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "blue") +
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 1.5,
           hjust = 1.1,
           label = paste("Media =", round(mean(data$ing_eve, na.rm = TRUE),2)),
           col = "red",
           size = 5) + 
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 3.1,
           hjust = 1.1,
           label = paste("Mediana =", round(median(data$ing_eve, na.rm = TRUE),2)),
           col = "blue",
           size = 5)
#################################################

########################################################################
#Histograma para la variable Ingresos del hogar########################
#######################################################################

ggplot(data, aes(x = ing_hogar)) + geom_histogram(bins = 50) + 
  labs(x = "Ingresos del hogar en $", y = "Frecuencia Absoluta") +
  geom_vline(xintercept = mean(data$ing_hogar, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "red") + 
  geom_vline(xintercept = median(data$ing_hogar, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "blue") +
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 1.5,
           hjust = 1.1,
           label = paste("Media = $", round(mean(data$ing_hogar, na.rm = TRUE),2)),
           col = "red",
           size = 5) + 
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 3.1,
           hjust = 1.1,
           label = paste("Mediana = $", round(median(data$ing_hogar, na.rm = TRUE),2)),
           col = "blue",
           size = 5)
#################################################

########################################################################
#Histograma para la variable Número de miembros del Hogar###############
#######################################################################

ggplot(data, aes(x = num_miembros_hogar)) + geom_histogram(bins = 15) + 
  labs(x = "Número de miembros del Hogar", y = "Frecuencia Absoluta") +
  geom_vline(xintercept = mean(data$num_miembros_hogar, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "red") + 
  geom_vline(xintercept = median(data$num_miembros_hogar, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "blue") +
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 1.5,
           hjust = 1.1,
           label = paste("Media =", round(mean(data$num_miembros_hogar, na.rm = TRUE),2)),
           col = "red",
           size = 5) + 
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 3.1,
           hjust = 1.1,
           label = paste("Mediana =", round(median(data$num_miembros_hogar, na.rm = TRUE),2)),
           col = "blue",
           size = 5)
#################################################

########################################################################
#Histograma para la variable Porcentaje de Beca########################
#######################################################################

ggplot(data, aes(x = Porc_Beca)) + geom_histogram(bins = 22) + 
  labs(x = "Porcentaje de Beca", y = "Frecuencia Absoluta") +
  geom_vline(xintercept = mean(data$Porc_Beca, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "red") + 
  geom_vline(xintercept = median(data$Porc_Beca, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "blue") +
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 1.5,
           hjust = 1.1,
           label = paste("Media =", round(mean(data$Porc_Beca, na.rm = TRUE),2)),
           col = "red",
           size = 5) + 
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 3.1,
           hjust = 1.1,
           label = paste("Mediana =", round(median(data$Porc_Beca, na.rm = TRUE),2)),
           col = "blue",
           size = 5)
#################################################


########################################################################
#Histograma para la variable Tiempo de trabajo#########################
#######################################################################

ggplot(data, aes(x = Tiempo_trab)) + geom_histogram(bins = 22) + 
  labs(x = "Tiempo de trabajo", y = "Frecuencia Absoluta") +
  geom_vline(xintercept = mean(data$Tiempo_trab, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "red") + 
  geom_vline(xintercept = median(data$Tiempo_trab, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "blue") +
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 1.5,
           hjust = 1.1,
           label = paste("Media =", round(mean(data$Tiempo_trab, na.rm = TRUE),2)),
           col = "red",
           size = 5) + 
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 3.1,
           hjust = 1.1,
           label = paste("Mediana =", round(median(data$Tiempo_trab, na.rm = TRUE),2)),
           col = "blue",
           size = 5)
#################################################


########################################################################
#Histograma para la variable Ingresos en el hogar#########################
#######################################################################
ggplot(data, aes(x = ing_hogar)) + geom_histogram(bins = 22) + 
  labs(x = "Ingresos en el hogar en $", y = "Frecuencia Absoluta") +
  geom_vline(xintercept = mean(data$ing_hogar, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "red") + 
  geom_vline(xintercept = median(data$ing_hogar, na.rm = TRUE), lwd = 0.7, linetype = 2, color = "blue") +
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 1.5,
           hjust = 1.1,
           label = paste("Media =", round(mean(data$ing_hogar, na.rm = TRUE),2)),
           col = "red",
           size = 5) + 
  annotate("text",
           x = Inf,
           y = Inf,
           vjust = 3.1,
           hjust = 1.1,
           label = paste("Mediana =", round(median(data$ing_hogar, na.rm = TRUE),2)),
           col = "blue",
           size = 5)


#################################################
#Grafico de barras de la variable Cert_ingles_A2#########  
#################################################

ggplot(data = data, aes(x = Cert_ingles_A2)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") + 
  labs(x = "Cert_ingles_A2", y = "No. de Propiedades")+
  theme_bw()

#################################################
#################################################
#Grafico de barras de la variable Genero#########  
#################################################

ggplot(data = data, aes(x = Genero)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") + 
  labs(x = "Genero", y = "No. de Propiedades")+
  theme_bw()

#################################################


#################################################
#Grafico de barras de la variable Estado civil###  
#################################################

ggplot(data = data, aes(x = Estado_civil)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black") + 
  labs(x = "Estado civil", y = "No. de Propiedades")+
  theme_bw()

#################################################
  
  
#################################################
#Grafico de barras de la variable Provincia######  
#################################################

ggplot(data = data, aes(x = Provincia)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black") + 
  labs(x = "Provincia", y = "No. de Propiedades")+
  theme_bw()

#################################################
#Grafico de barras de la variable Emplea Ingresos######  
#################################################
ggplot(data = data, aes(x = emplea_ingresos)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black") + 
  labs(x = "Emplea ingresos", y = "No. de Propiedades")+
  theme(axis.text.x = element_text(angle=45))
  theme_bw()  

#################################################
#Grafico de barras de la variable Nivel de Formación del Padre######  
#################################################

ggplot(data = data, aes(x = niv_form_padre)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black") + 
  labs(x = "Nivel de formación del Padre", y = "No. de Propiedades")+
  theme(axis.text.x = element_text(angle=45))
  theme_bw()

  
#################################################
#Grafico de barras de la variable Nivel de Formación de la Madre######  
#################################################
  
ggplot(data = data, aes(x = niv_form_madre)) + geom_bar() +
    geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black") + 
    labs(x = "Nivel de formación de la Madre", y = "No. de Propiedades")+
    theme(axis.text.x = element_text(angle=45))
  theme_bw()
  
  

#################################################
#Grafico de barras de la variable Trabaja########
#################################################

ggplot(data = data, aes(x = Trabaja)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black") + 
  labs(x = "Trabaja", y = "No. de Propiedades")+
theme_bw()



