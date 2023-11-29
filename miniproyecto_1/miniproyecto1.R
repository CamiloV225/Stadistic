# Cargar las bibliotecas necesarias
library(openxlsx)
library(dplyr)
library(ggplot2)
library(flexmix)
library(MASS)

# Parte 1: Proporción de Malformaciones por Consumo de Alcohol
setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\Stadistic\\miniproyecto_1")

# Leer los datos desde el archivo "Datos_MP1.xlsx" en la hoja "Datos_1"
datos_1 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_1")

# Calcular la proporción de presente
datos_1$Proporcion.Presente <- datos_1$Frec.Presente / (datos_1$Frec.Presente + datos_1$Frec.Ausente)

# Crear el gráfico de dispersión
ggplot(datos_1, aes(x = Alcohol, y = Proporcion.Presente)) +
  geom_point(color = "#016A70") +
  labs(
    title = "Proporción de Niños con Malformaciones por Consumo de Alcohol",
    x = "Alcohol (Ponderado)",
    y = "Proporción de Niños"
  )

# Modelo Logit
modelo_logit <- glm(cbind(Frec.Presente, Frec.Ausente) ~ Alcohol, family = binomial(link = logit), data = datos_1)
summary(modelo_logit)
predicciones_logit <- (datos_1$Frec.Presente + datos_1$Frec.Ausente) * fitted.values(modelo_logit)
modelo_logit2 <- glm(Proporcion.Presente ~ Alcohol, family = binomial(link = logit), data = datos_1, weights = Frec.Presente + Frec.Ausente)
summary(modelo_logit2)


# Modelo Probit
modelo_probit <- glm(cbind(Frec.Presente, Frec.Ausente) ~ Alcohol, family = binomial(link = "probit"), data = datos_1)
summary(modelo_probit)
predicciones_probit <- (datos_1$Frec.Presente + datos_1$Frec.Ausente) * fitted.values(modelo_probit)

z <- -2.79933 + 0.11014 * 0#Reemplaza con el valor Z que necesites

# Calcula la probabilidad utilizando pnorm()
probabilidad <- pnorm(z)

# Imprime la probabilidad
print(probabilidad)

# Modelo Lineal
modelo_lineal <- lm(Proporcion.Presente ~ Alcohol, data = datos_1)
summary(modelo_lineal)
predicciones_lineal <- (datos_1$Frec.Presente + datos_1$Frec.Ausente) * fitted.values(modelo_lineal)

# Crear una tabla con AIC, BIC y Devianza
tabla_resultados <- data.frame(
  Modelo = c("Modelo Logit", "Modelo Probit", "Modelo Lineal"),
  BIC = c(BIC(modelo_logit), BIC(modelo_probit), BIC(modelo_lineal)),
  AIC = c(AIC(modelo_logit), AIC(modelo_probit), AIC(modelo_lineal)),
  Devianza = c(deviance(modelo_logit), deviance(modelo_probit), deviance(modelo_lineal))
)
# Crear una tabla con las predicciones de los modelos
predicciones <- data.frame(
  Alcohol = datos_1$Alcohol,
  Prediccion_Logit = predicciones_logit,
  Prediccion_Probit = predicciones_probit,
  Prediccion_Lineal = predicciones_lineal
)

range_alcohol <- data.frame(Alcohol = seq(min(datos_1$Alcohol), max(datos_1$Alcohol), length.out = 10))

# Obtén las predicciones de los modelos
predictions <- data.frame(
  Alcohol = range_alcohol$Alcohol,
  logit = predict(modelo_logit, newdata = range_alcohol, type = "response"),
  probit = predict(modelo_probit, newdata = range_alcohol, type = "response"),
  lineal = predict(modelo_lineal, newdata = range_alcohol, type = "response")
)

# Crea un gráfico comparativo entre las predicciones y los valores reales
ggplot() +
  geom_point(data = datos_1, aes(x = Alcohol, y = Proporcion.Presente)) +
  geom_line(data = predictions, aes(x = Alcohol, y = logit, color = "Logit"), lwd = 1) +
  geom_line(data = predictions, aes(x = Alcohol, y = probit, color = "Probit"), lwd = 1) +
  geom_line(data = predictions, aes(x = Alcohol, y = lineal, color = "Lineal"), lwd = 1) +
  labs(
    title = "Comparación de Modelos",
    x = "Alcohol (Ponderado)",
    y = "Proporción de Niños"
  ) +
  scale_color_manual(
    values = c("Logit" = "#A2C579", "Probit" = "#016A70", "Lineal" = "#D2DE32"),
  ) +
  guides(
    color = guide_legend(title = "Modelo")
  )


# Parte 2: Número de Premios de Estudiantes de Bachillerato

# Leer los datos desde el archivo "Datos_MP1.xlsx" en la hoja "Datos_2"
datos_2 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_2")

# Establecer una semilla para reproducibilidad
set.seed(1)

# Seleccionar una muestra aleatoria de tamaño 40 sin reemplazo
muestra <- datos_2 %>%
  sample_n(size = 40, replace = FALSE) 

# Resumen estadístico de num_premios y puntaje_mat
resumen <- data.frame(
  Variable = c("num_premios", "puntaje_mat"),
  Promedio = c(mean(muestra$num_premios, na.rm = TRUE), mean(muestra$puntaje_mat, na.rm = TRUE)),
  DE = c(sd(muestra$num_premios, na.rm = TRUE), sd(muestra$puntaje_mat, na.rm = TRUE)),
  Mediana = c(median(muestra$num_premios, na.rm = TRUE), median(muestra$puntaje_mat, na.rm = TRUE)),
  CV = c(
    sd(muestra$num_premios, na.rm = TRUE) / mean(muestra$num_premios, na.rm = TRUE) * 100,
    sd(muestra$puntaje_mat, na.rm = TRUE) / mean(muestra$puntaje_mat, na.rm = TRUE) * 100
  ),
  Minimo = c(min(muestra$num_premios, na.rm = TRUE), min(muestra$puntaje_mat, na.rm = TRUE)),
  Maximo = c(max(muestra$num_premios, na.rm = TRUE), max(muestra$puntaje_mat, na.rm = TRUE))
)

# Gráfico de histograma para la distribución de número de premios por programa
ggplot(muestra, aes(x = num_premios, fill = programa)) +
  geom_histogram(
    binwidth = 1, 
    position = "stack",
    colour = "black",
    lwd = 0.5
  ) +
  labs(
    title = "Distribución del Número de Premios por Programa",
    x = "Número de Premios",
    y = "Frecuencia",
    fill = "Programa"
  ) +
  scale_fill_manual(
    values = c("Academico" = "#A2C579", "General" = "#016A70", "Vocacional" = "#D2DE32")
  )

# Gráfico de dispersión para la relación entre puntaje_mat y num_premios por programa
ggplot(muestra, aes(x = puntaje_mat, y = num_premios, color = programa)) +
  geom_point() +
  labs(
    title = "Relación entre Puntaje en Matemáticas y Número de Premios",
    x = "Puntaje en Matemáticas",
    y = "Número de Premios",
    color = "Programa"
  ) +
  scale_color_manual(
    values = c("Academico" = "#A2C579", "General" = "#016A70", "Vocacional" = "#D2DE32")
  )

# Tabla de frecuencia de "programa"
tabla_programa <- table(muestra$programa)

# Gráfico de barras para la distribución de programas
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

# Ajuste del modelo de regresión de Poisson
modelo_poisson <- glm(num_premios ~ programa + puntaje_mat, family = poisson(link = "log"), data = muestra)
summary(modelo_poisson)

# Calcular el número esperado de premios utilizando los datos del modelo de regresión de Poisson ajustado
muestra$predict <- predict(modelo_poisson, type = "response")

# Gráfico que muestra el número esperado de premios según la puntuación en matemáticas y programa
ggplot(muestra, aes(x = puntaje_mat, y = predict, color = programa)) +
  geom_point(aes(y = num_premios), alpha = 0.7, position = position_jitter(h = 0.2)) +
  geom_line() +
  labs(
    title = "Resultados de la Regresión Poisson",
    x = "Puntaje en Matemáticas",
    y = "Número Esperado de Premios",
    color = "Programa"
  ) +
  scale_color_manual(
    values = c("Academico" = "#A2C579", "General" = "#016A70", "Vocacional" = "#D2DE32")
  )

# Calcular las métricas
tabla_resultados <- data.frame(
  Modelo = "Modelo Poisson",
  AIC = AIC(modelo_poisson),
  BIC = BIC(modelo_poisson),
  Devianza = deviance(modelo_poisson),
  GL = df.residual(modelo_poisson)
)

# Parte 3: Número de Arrestos en Equipos de Fútbol

# Carga de datos
datos_3 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_3")
set.seed(1)
muestra_3 <- datos_3 %>% sample_n(size = 12, replace = FALSE)

# Resumen estadístico de variables
resumen <- data.frame(
  Variable = c("asistencia_miles", "num_arrestos", "inv_social_millones"),
  Promedio = c(mean(muestra_3$asistencia_miles), mean(muestra_3$num_arrestos), mean(muestra_3$inv_social_millones)),
  DE = c(sd(muestra_3$asistencia_miles), sd(muestra_3$num_arrestos), sd(muestra_3$inv_social_millones)),
  Mediana = c(median(muestra_3$asistencia_miles), median(muestra_3$num_arrestos), median(muestra_3$inv_social_millones)),
  CV = c(sd(muestra_3$asistencia_miles) / mean(muestra_3$asistencia_miles) * 100,
         sd(muestra_3$num_arrestos) / mean(muestra_3$num_arrestos) * 100,
         sd(muestra_3$inv_social_millones) / mean(muestra_3$inv_social_millones) * 100),
  Minimo = c(min(muestra_3$asistencia_miles), min(muestra_3$num_arrestos), min(muestra_3$inv_social_millones)),
  Maximo = c(max(muestra_3$asistencia_miles), max(muestra_3$num_arrestos), max(muestra_3$inv_social_millones))
)

# Gráfico de Boxplot del número de arrestos
ggplot(muestra_3, aes(x = "", y = num_arrestos)) +
  geom_boxplot(
    fill = "#016A70",     # Color de la caja
    alpha = 0.5,          # Transparencia de la caja
    color = "black",      # Color del borde de la caja
    outlier.color = "red" # Color de los valores atípicos
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.15,
    color = "black"       # Color de las líneas de los bigotes
  ) +
  labs(
    title = "Distribución de Número de Arrestos",
    x = "",
    y = "Número de Arrestos"
  )

# Histograma de asistencia
ggplot(muestra_3, aes(x = asistencia_miles)) +
  geom_histogram(fill = "#016A70", color = "black", bins = 15) +
  labs(
    title = "Distribución de Asistencia en Miles",
    x = "Asistencia en Miles",
    y = "Frecuencia"
  )

# Gráfico de dispersión entre Inversión Social y Número de Arrestos
ggplot(data = muestra_3, aes(x = inv_social_millones, y = num_arrestos)) +
  geom_point(color = "#016A70") +
  labs(x = "Inversión Social (Millones de Pesos)", y = "Número de Arrestos") +
  ggtitle("Relación entre Inversión Social y Arrestos")

# Gráfico de dispersión entre Asistencia y Número de Arrestos
ggplot(data = muestra_3, aes(x = asistencia_miles, y = num_arrestos)) +
  geom_point(color = "#016A70") +
  labs(x = "Asistencia (Miles)", y = "Número de Arrestos") +
  ggtitle("Relación entre Asistencia y Arrestos")

# Modelos

# Modelo Binomial Negativo
modelo_binomial <- glm.nb(num_arrestos ~  inv_social_millones + offset(log(asistencia_miles)), data = muestra_3)
muestra_3$predict_binomial <- predict(modelo_binomial, type = "response")

# Modelo Poisson
modelo_poisson <- glm(num_arrestos ~  inv_social_millones + offset(log(asistencia_miles)), family = poisson(link = "log"), data = muestra_3)
muestra_3$predict_poisson <- predict(modelo_poisson, type = "response")

# Modelo Lineal
modelo_lineal <- lm(num_arrestos ~  inv_social_millones + asistencia_miles, data = muestra_3)

# Modelo Lineal Simple
modelo_lineal_simple <- lm(num_arrestos ~  inv_social_millones, data = muestra_3)
muestra_3$predict_lineal_simple <- predict(modelo_lineal)

# Crear proporción de arrestos
muestra_3$proporcion_arrestos <- muestra_3$num_arrestos / muestra_3$asistencia_miles

# Modelo Logit
modelo_logit <- glm(proporcion_arrestos ~ asistencia_miles + inv_social_millones, family = binomial(link = logit), data = muestra_3, weights = asistencia_miles)
muestra_3$predict_logit <- predict(modelo_logit, type = "response") * muestra_3$asistencia_miles

# Crear una tabla con las predicciones de cada modelo
predicciones <- data.frame(
  num_arrestos = muestra_3$num_arrestos,
  poisson = round(fitted.values(modelo_poisson), 2),
  lineal = round(fitted.values(modelo_lineal_simple), 2),
  logit = round(muestra_3$asistencia_miles * fitted.values(modelo_logit), 2),
  binomial = round(fitted.values(modelo_binomial), 2)
)

# Crear una tabla de comparación de modelos
tabla_resultados <- data.frame(
  Modelo = c("Modelo Poisson", "Modelo Lineal Simple", "Modelo Logit", "Modelo Binomial"),
  AIC = c(AIC(modelo_poisson), AIC(modelo_lineal_simple), AIC(modelo_logit), AIC(modelo_binomial)),
  BIC = c(BIC(modelo_poisson), BIC(modelo_lineal_simple), BIC(modelo_logit), BIC(modelo_binomial)),
  Devianza = c(deviance(modelo_poisson), deviance(modelo_lineal_simple), deviance(modelo_logit), deviance(modelo_binomial)),
  GL = c(df.residual(modelo_poisson), df.residual(modelo_lineal_simple), df.residual(modelo_logit), df.residual(modelo_binomial))
)


# Gráfico que muestra las predicciones de todos los modelos
ggplot(muestra_3, aes(x = inv_social_millones)) +
  geom_point(aes(y = num_arrestos), alpha = 0.7, position = position_jitter(h = 0.2)) +
  geom_line(aes(y = predict_poisson, color = "Poisson"), size = 1) +
  geom_line(aes(y = predict_binomial, color = "Binomial Negativo"), size = 1) +
  geom_line(aes(y = predict_lineal_simple, color = "Lineal Simple"), size = 1) +
  geom_line(aes(y = predict_logit, color = "Logit"), size = 1) +
  labs(
    title = "Resultados de Modelos de Regresión",
    x = "Inversión Social en Millones",
    y = "Número de Arrestos",
    color = "Modelo"
  ) +
  scale_color_manual(
    values = c("Poisson" = "#A2C579", "Binomial Negativo" = "#016A70", "Lineal Simple" = "#D2DE32", "Logit" = "#FF5733"),
    labels = c("Poisson", "Binomial Negativo", "Lineal Simple", "Logit")
  )
