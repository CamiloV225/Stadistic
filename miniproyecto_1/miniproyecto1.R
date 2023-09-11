# Cargar las bibliotecas necesarias
library(openxlsx)
library(dplyr)
library(ggplot2)
library(flexmix)

# Parte 1: Proporción de Malformaciones por Consumo de Alcohol
setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\Stadistic\\miniproyecto_1")
set.seed(1)

# Leer los datos desde el archivo "Datos_MP1.xlsx" en la hoja "Datos_1"
datos_1 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_1")

# Calcular la proporción de presente
datos_1$Proporcion.Presente <- datos_1$Frec.Presente / (datos_1$Frec.Presente + datos_1$Frec.Ausente)

# Crear el gráfico de dispersión
ggplot(datos_1, aes(x = Alcohol, y = Proporcion.Presente)) +
  geom_point(color = "#016A70") +
  labs(
    title = "Proporción de Malformaciones por Consumo de Alcohol",
    x = "Alcohol",
    y = "Proporción de Presente"
  )

# Modelo Logit
modelo_logit <- glm(cbind(Frec.Presente, Frec.Ausente) ~ Alcohol, family = binomial(link = logit), data = datos_1)
summary(modelo_logit)
datos_1$Proporcion.Presente_Logistico <- fitted.values(modelo_logit)
predicciones_logit <- (datos_1$Frec.Presente + datos_1$Frec.Ausente) * datos_1$Proporcion.Presente_Logistico

# Modelo Probit
modelo_probit <- glm(cbind(Frec.Presente, Frec.Ausente) ~ Alcohol, family = binomial(link = "probit"), data = datos_1)
summary(modelo_probit)
datos_1$Proporcion.Presente_Probit <- fitted.values(modelo_probit)
predicciones_probit <- (datos_1$Frec.Presente + datos_1$Frec.Ausente) * datos_1$Proporcion.Presente_Probit

# Modelo Lineal
modelo_lineal <- lm(Proporcion.Presente ~ Alcohol, data = datos_1)
summary(modelo_lineal)
datos_1$Proporcion.Presente_Lineal <- fitted.values(modelo_lineal)
predicciones_lineal <- (datos_1$Frec.Presente + datos_1$Frec.Ausente) * datos_1$Proporcion.Presente_Lineal


# Crear una tabla con AIC, BIC y Devianza
tabla_resultados <- data.frame(
  Modelo = c("Modelo Logit", "Modelo Probit", "Modelo Lineal"),
  BIC = c(BIC(modelo_logit), BIC(modelo_probit), BIC(modelo_lineal)),
  AIC = c(AIC(modelo_logit), AIC(modelo_probit), AIC(modelo_lineal)),
  Devianza = c(deviance(modelo_logit), deviance(modelo_probit), deviance(modelo_lineal))
)

# Crear una tabla con las predicciones de los modelos
tabla_predicciones <- data.frame(
  Alcohol = datos_1$Alcohol,
  Prediccion_Logit = predicciones_logit,
  Prediccion_Probit = predicciones_probit,
  Prediccion_Lineal = predicciones_lineal
)

# Crear un gráfico de líneas para comparar modelos
ggplot(datos_1, aes(x = Alcohol)) +
  geom_line(aes(y = Proporcion.Presente_Logistico, color = "Logistico"), linetype = "F1", lwd = 1) +
  geom_line(aes(y = Proporcion.Presente_Probit, color = "Probit"), linetype = "dashed", lwd = 1) +
  geom_line(aes(y = Proporcion.Presente_Lineal, color = "Lineal"), linetype = "longdash", lwd = 1) +
  geom_point(aes(y = Proporcion.Presente)) +  
  labs(
    title = "Comparación de Modelos",
    x = "Alcohol",
    y = "Proporción de Presente"
  ) +
  scale_color_manual(
    values = c("Logistico" = "#A2C579", "Probit" = "#016A70", "Lineal" = "#D2DE32"),
    labels = c("Logístico", "Probit", "Lineal")
  ) +
  guides(
    color = guide_legend(title = "Modelo")
  )

# Parte 2: Número de Premios de Estudiantes de Bachillerato

# Leer los datos desde el archivo "Datos_MP1.xlsx" en la hoja "Datos_2"
datos_2 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_2")

# Tomar una muestra de 40 observaciones sin reemplazo
muestra <- datos_2 %>% sample_n(size = 40, replace = FALSE)

# Resumen estadístico de num_premios y puntaje_mat
resumen_estadistico <- data.frame(
  Variable = c("num_premios", "puntaje_mat"),
  Promedio = c(mean(muestra$num_premios, na.rm = TRUE), mean(muestra$puntaje_mat, na.rm = TRUE)),
  DE = c(sd(muestra$num_premios, na.rm = TRUE), sd(muestra$puntaje_mat, na.rm = TRUE)),
  Mediana = c(median(muestra$num_premios, na.rm = TRUE), median(muestra$puntaje_mat, na.rm = TRUE)),
  CV = c(sd(muestra$num_premios, na.rm = TRUE) / mean(muestra$num_premios, na.rm = TRUE) * 100,
         sd(muestra$puntaje_mat, na.rm = TRUE) / mean(muestra$puntaje_mat, na.rm = TRUE) * 100),
  Minimo = c(min(muestra$num_premios, na.rm = TRUE), min(muestra$puntaje_mat, na.rm = TRUE)),
  Maximo = c(max(muestra$num_premios, na.rm = TRUE), max(muestra$puntaje_mat, na.rm = TRUE))
)

# Gráfico de barras para num_premios
ggplot(muestra, aes(x = factor(num_premios))) +
  geom_bar(fill = "#016A70", color = "black") +
  labs(
    title = "Distribución de num_premios",
    x = "Número de Premios",
    y = "Frecuencia"
  )

# Boxplot para puntaje_mat
ggplot(muestra, aes(y = puntaje_mat)) +
  geom_boxplot(fill = "#016A70", color = "black") +
  labs(
    title = "Distribución de puntaje_mat",
    x = "",
    y = "Puntaje Matemáticas"
  )

# Tabla de frecuencia de la variable "programa"
table(muestra$programa)

# Gráfico de barras de la distribución de programas
ggplot(muestra, aes(x = factor(programa), fill = programa)) +
  geom_bar(stat = "count") + 
  scale_fill_manual(
    values = c("Academico" = "#A2C579", "General" = "#016A70", "Vocacional" = "#D2DE32")
  ) +
  labs(
    title = "Distribución de Programas",
    x = "Programa",
    y = "Frecuencia"
  ) +
  theme(legend.position = "none")

# Parte 3: Número de Arrestos en Equipos de Fútbol
datos_3 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_3")
muestra3 <- datos_3 %>% sample_n(size = 11, replace = FALSE)

resumen_estadistico <- data.frame(
  Variable = c("asistencia_miles", "num_arrestos", "inv_social_millones"),
  Promedio = c(mean(muestra3$asistencia_miles), mean(muestra3$num_arrestos), mean(muestra3$inv_social_millones)),
  DE = c(sd(muestra3$asistencia_miles), sd(muestra3$num_arrestos), sd(muestra3$inv_social_millones)),
  Mediana = c(median(muestra3$asistencia_miles), median(muestra3$num_arrestos), median(muestra3$inv_social_millones)),
  CV = c(sd(muestra3$asistencia_miles) / mean(muestra3$asistencia_miles) * 100,
         sd(muestra3$num_arrestos) / mean(muestra3$num_arrestos) * 100,
         sd(muestra3$inv_social_millones) / mean(muestra3$inv_social_millones) * 100),
  Minimo = c(min(muestra3$asistencia_miles), min(muestra3$num_arrestos), min(muestra3$inv_social_millones)),
  Maximo = c(max(muestra3$asistencia_miles), max(muestra3$num_arrestos), max(muestra3$inv_social_millones))
)

# Gráfico de distribución para asistencia_miles
ggplot(muestra3, aes(x = asistencia_miles)) +
  geom_histogram(fill = "#016A70", color = "black", bins = 15) +
  labs(
    title = "Distribución de asistencia_miles",
    x = "Asistencia en Miles",
    y = "Frecuencia"
  )

# Boxplot para num_arrestos
ggplot(muestra3, aes(y = num_arrestos)) +
  geom_boxplot(fill = "#016A70", color = "black") +
  labs(
    title = "Distribución de num_arrestos",
    x = "",
    y = "Número de Arrestos"
  )

# Gráfico de distribución para inv_social_millones
ggplot(muestra3, aes(x = inv_social_millones)) +
  geom_histogram(fill = "#016A70", color = "black", bins = 15) +
  labs(
    title = "Distribución de inv_social_millones",
    x = "Inversión Social en Millones",
    y = "Frecuencia"
  )

modelo_poisson <- glm(num_arrestos ~  inv_social_millones + offset(log(asistencia_miles)),family = poisson(link = "log"), data = muestra3)
summary(modelo_poisson)

modelo_lineal <- lm(num_arrestos ~  inv_social_millones + asistencia_miles, data = muestra3)
summary(modelo_lineal)

modelo_lineal_simple <- lm(num_arrestos ~  inv_social_millones, data = muestra3)
summary(modelo_lineal_simple)

muestra3$proporcion_arrestos <- muestra3$num_arrestos / muestra3$asistencia_miles

modelo_logit <- glm(proporcion_arrestos ~ asistencia_miles+ inv_social_millones, family = binomial(link = logit), data = muestra3, weights = asistencia_miles)
summary(modelo_logit)

muestra3$proporcion_arrestos_logistico <- fitted.values(modelo_logit)
predicciones_logit <- muestra3$asistencia_miles * muestra3$proporcion_arrestos_logistico

# Crear una tabla de comparación de modelos
tabla_resultados <- data.frame(
  Modelo = c("Modelo Poisson", "Modelo Lineal Completo", "Modelo Lineal Simple", "Modelo Logit"),
  AIC = c(AIC(modelo_poisson), AIC(modelo_lineal), AIC(modelo_lineal_simple), AIC(modelo_logit)),
  BIC = c(BIC(modelo_poisson), BIC(modelo_lineal), BIC(modelo_lineal_simple), BIC(modelo_logit)),
  Devianza = c(deviance(modelo_poisson), deviance(modelo_lineal), deviance(modelo_lineal_simple), deviance(modelo_logit)),
  GL = c(df.residual(modelo_poisson), df.residual(modelo_lineal), df.residual(modelo_lineal_simple), df.residual(modelo_logit))
)

# Crear una tabla con las predicciones de cada modelo
tabla_predicciones <- data.frame(
  Modelo_Poisson = modelo_poisson$fitted.values,
  Modelo_Linea_Completo = modelo_lineal$fitted.values,
  Modelo_Linea_Simple = modelo_lineal_simple$fitted.values,
  Modelo_Logit = predicciones_logit
)




