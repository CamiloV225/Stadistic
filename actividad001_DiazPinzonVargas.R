# Cargar las bibliotecas necesarias
library(openxlsx)    # Para leer el archivo xlsx
library(dplyr)       # Para manipulación de datos
library(corrplot)    # Para graficar la matriz de correlación
library(randtests)   # Para pruebas de normalidad
library(skedastic)   # Para pruebas de homocedasticidad
library(car)         # Para pruebas de regresión
library(AICcmodavg)  # Para cálculo del AIC corregido
library(flexmix)     # Para cálculo del BIC

# Limpiar el área de trabajo
rm(list = ls())

# Establecer el directorio de trabajo (ajústalo según tu ubicación)
setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\Stadistic")

# Leer los datos desde el archivo xlsx
data <- read.xlsx("Caso.xlsx")

# Fijar la semilla para la generación de números aleatorios
set.seed(1)

# Seleccionar una muestra aleatoria de tamaño 40
muestra <- data %>% sample_n(size = 40, replace = FALSE)

# Calcular y visualizar la matriz de correlación
corrplot(cor(muestra), method = "number")

# ------------------------------
# Modelos de Regresión Lineal
# ------------------------------

# Modelo 1: Regresión lineal con todas las variables predictoras
m1 <- lm(R ~ ., data = muestra)
summary(m1)
ECM_m1 <- sum(m1$residuals^2) / length(m1$residuals)
RECM_m1 <- sqrt(ECM_m1)
BIC_m1 <- BIC(m1)

# Modelo 2: Regresión lineal con variables seleccionadas en el Modelo 1
m2 <- lm(R ~ AGE + ED + EX0 + X, data = muestra)
summary(m2)
ECM_m2 <- sum(m2$residuals^2) / length(m2$residuals)
RECM_m2 <- sqrt(ECM_m2)
BIC_m2 <- BIC(m2)

# Modelo 3: Regresión lineal con variables seleccionadas en el Modelo 2, excluyendo X
m3 <- lm(R ~ AGE + EX0 + ED, data = muestra)
summary(m3)
ECM_m3 <- sum(m3$residuals^2) / length(m3$residuals)
RECM_m3 <- sqrt(ECM_m3)
BIC_m3 <- BIC(m3)

# Modelo 4: Regresión lineal con variables seleccionadas en el Modelo 2, excluyendo ED
m4 <- lm(R ~ AGE + EX0 + X, data = muestra)
summary(m4)
ECM_m4 <- sum(m4$residuals^2) / length(m4$residuals)
RECM_m4 <- sqrt(ECM_m4)
BIC_m4 <- BIC(m4)

# Crear una lista de modelos
models <- list(m1, m2, m3, m4)

# Especificar nombres de los modelos
mod.names <- c('.', 'AGE.ED.EX0.X', 'AGE.ED.EX0', 'AGE.EX0.X')

# Calcular el AIC de cada modelo y mostrarlo en una tabla
aictab(cand.set = models, modnames = mod.names)

# ------------------------------
# Análisis de Supuestos
# ------------------------------

# Obtener los residuales del Modelo 2
residuales <- m2$residuals

# Prueba de media cero en los residuales
t.test(residuales)

# Prueba de normalidad en los residuales
shapiro.test(residuales)

# Prueba de independencia de los residuales
runs.test(residuales)  

# Prueba de homogeneidad de varianza
homoge <- white(m2)

# Cálculo de los VIF (Varianza de la Inflación) para detectar multicolinealidad
vif(m2)
hist(residuales)
