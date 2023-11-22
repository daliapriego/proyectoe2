#Modelo de LogIt

#1.Entorno de trabajo
rm(list=ls()); graphics.off()    
library(foreign)                 
library(dplyr) 
library(stats) 
library(tidyverse)
library(factoextra)
library(ggplot2)
library(psych)
library(klaR)

#2. Cargar BaseEE de datos

setwd("C:/Users/LAP EMI/OneDrive/Imágenes/Escritorio")
BaseEE <- read.csv("BaseEE.csv")
view(BaseEE)

# variables
# rimbo_n = número de palabras rimbombantes
# nivel_esc = nivel educativo
#           1 = Postgrado  
#           2 = Licenciatura
# sostenimiento = Tipo de sostenimiento
#           1 = Privado  
#           2 = Público
# sexo = sexo
#           1 = Hombre
#           2 = mujer
# experiencia = experiencia 
#           0 = Sin experiencia 
#           1 = Con experiencia
# lenguaje = lenguaje
#           0 = Español  
#           1 = Inglés 
# puesto_1 = tipo de puesto
#           1 = alta dirección y responsabilidad intermedia
#           2 = nivel inicial y nivel básico


#Quitar missings
BaseEE$rimbo_n=as.factor(BaseEE$rimbo_n)
BaseEE$nivel_esc=as.factor(BaseEE$nivel_esc)
BaseEE$sostenimiento=as.factor(BaseEE$sostenimiento)
BaseEE$sexo=as.factor(BaseEE$sexo)
BaseEE$experiencia=as.numeric(BaseEE$experiencia)
BaseEE$lenguaje=as.factor(BaseEE$lenguaje)
BaseEE$puesto_1=as.factor(BaseEE$puesto_1)

str(BaseEE)
#3. Etiquetar variables (metodologia)

BaseEE
BaseEE$rimbo_n<-factor(BaseEE$rimbo_n)
BaseEE$nivel_esc<-factor(BaseEE$nivel_esc, levels = c(1,2), labels = c("Postgrado", "Licenciatura"))
BaseEE$sostenimiento<-factor(BaseEE$sostenimiento, levels = c(0,1), labels = c("Privado", "Público"))
BaseEE$sexo<-factor(BaseEE$sexo, levels = c(1,2), labels = c("Hombres", "Mujer"))
BaseEE$experiencia<-factor(BaseEE$experiencia, levels = c(0,1), labels = c("Sin experiencia", "Con expereiencia"))
BaseEE$lenguaje<-factor(BaseEE$lenguaje, levels = c(0,1), labels = c("Español", "Inglés"))
BaseEE$puesto_1<-factor(BaseEE$puesto_1, levels = c(0,1), labels = c( "Alta dirección y responsabilidad intermedia", "nivel inicial y nivel básico"))


#4. Seleccionar variables (2 variables)

BaseEE<-BaseEE[c("puesto_1","lenguaje","sexo","sostenimiento","experiencia","nivel_esc","rimbo_n")]

#4.1 Variable dependiente

table(BaseEE$puesto_1)

#4.2 Covariables

table(BaseEE$rimbo_n)
table(BaseEE$nivel_esc)
table(BaseEE$sostenimiento)
table(BaseEE$sexo)
table(BaseEE$experiencia)
table(BaseEE$lenguaje)

view(BaseEE)

#5. Ajuste del modelo

regresion <- glm(puesto_1~ rimbo_n + nivel_esc + sostenimiento + sexo + experiencia + lenguaje, 
                 data = BaseEE, family = "binomial")
summary(regresion)

#6. Interpretacion

momios<-exp(coefficients(regresion))%>%round(digits = 4)%>%data.frame()
momios

