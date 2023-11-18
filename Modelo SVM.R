#Modelo Suport Vector Machime


#Llamada a librerías
#Librerias Generales
library(ggplot2)
library(dplyr)
library(haven)
library(caTools)
library(readxl)
library(writexl)
library(colorspace)
#Libraries for the decision tree
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(e1071)
#Librerias de Clustering
library(arules)
#Librerias de Histogramas y BoxPlots
library(DataExplorer)
#PCA
library(tidyverse)
library(scales)
library(ggfortify)
#Escalamiento de datos
library(dummy)
library(kableExtra)
library(factoextra)
library(glmnet)
library(caret)
# Cargar los datos
data <- read.csv("Base.csv", sep = ";")

#Se convierten a factores las variables Categóricas
data$Genero = as.factor(data$Genero)
data$Provincia = as.factor(data$Provincia)
data$Estado_civil = as.factor(data$Estado_civil)
data$emplea_ingresos = as.factor(data$emplea_ingresos)
data$niv_form_padre = as.factor(data$niv_form_padre)
data$niv_form_madre = as.factor(data$niv_form_madre)
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

# Dividir los datos en conjunto de entrenamiento y conjunto de prueba
set.seed(42)
split <- sample(1:nrow(data), nrow(data) * 0.8)
train <- data[split, ]
test <- data[-split, ]


model <- svm(Continua ~ Genero+Edad+Provincia+Estado_civil+emplea_ingresos
             +niv_form_padre+niv_form_madre+ing_hogar+num_miembros_hogar
             +ing_eve+Tipo_vivienda+tipo_const_vivienda+Internet+cert_practicas
             +Cert_ingles_A2+Trabaja+Tiempo_trab+Colegio_especializacion
             +Nivel_Ingles+Sist_ingreso+Tipo_Colegio+Tipo_matricula+Condicion
             +Porc_Beca+motivo_Beca_1+motivo_Beca_2, data = train, kernel = "linear", cost = 10)
model 

model <- svm(Continua ~ Genero
             +niv_form_madre+ing_hogar
             +Tipo_vivienda
             +Cert_ingles_A2
             +Sist_ingreso+Condicion, data = train, kernel = "linear", cost = 10)
model 

preds <- as.factor(preds)

# Evaluar el modelo
preds <- predict(model, test)
accuracy <- mean(preds == test$Continua)
print(paste("Accuracy:", accuracy))


test$Continua <- as.factor(test$Continua)
train$Continua <- as.factor(train$Continua)
preds <- as.factor(preds)
confusionMatrix(preds, test[["Continua"]])


# Implementar el modelo
predict_dropout <- function(data) {
  preds <- predict(model, data)
  return(preds)
}
