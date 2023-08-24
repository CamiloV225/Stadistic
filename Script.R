library(openxlsx)
library(dplyr)
library(corrplot)
library(randtests)
library(lmtest)
library(skedastic)

#Limpiar área de trabajo
rm(list = ls())
#Directorio de trabajo
setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\Stadistic")
data <- read.xlsx("Caso.xlsx")
set.seed(1)

muestra <- data %>% sample_n(size = 40, replace = FALSE)
corrplot(cor(muestra), method="number")

#Modelo 1
m1 <- lm(R ~ ., data = muestra)
summary(m1)
      
#Modelo 2
m2 <- lm(R ~ AGE + ED + EX0 + X, data = muestra)
summary(m2)

residuales <- m2$residuals

#Media cero
t.test(residuales)

#Normalidad
shapiro.test(residuales)

#Independencia
runs.test(residuales) #p-value = 0.5023 se cumple el supuesto de independencia

#Homogeneidad
homoge <- white(m2)
