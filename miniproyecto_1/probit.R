# Cargar las bibliotecas necesarias
library(openxlsx)
library(dplyr)
library(ggplot2)
library(flexmix)
library(Metrics)

# Establecer el directorio de trabajo
setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\Stadistic\\miniproyecto_1")

colores <- c("#A2C579", "#D2DE32", "#FFFFDD", "#016A70")

# Leer los datos desde el archivo "Datos_MP1.xlsx" en la hoja "Datos_1"
datos_1 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_1")
set.seed(1)

# Calcular la proporción de presente
datos_1$Proporcion.Presente <- datos_1$Frec.Presente / (datos_1$Frec.Presente + datos_1$Frec.Ausente)

# Crear el gráfico de dispersión inicial
ggplot(datos_1, aes(x = Alcohol, y = Proporcion.Presente)) +
  geom_point(color = colores[4]) +
  labs(
    title = "Proporción de Malformaciones por Consumo de Alcohol",
    x = "Alcohol",
    y = "Proporción de Presente"
  )

# Ajustar un modelo de regresión logística binomial (Modelo Logit)
modelo_logit <- glm(cbind(Frec.Presente, Frec.Ausente) ~ Alcohol, family = binomial(link = logit), data = datos_1)
summary(modelo_logit)
datos_1$Proporcion.Presente_Logistico <- fitted.values(modelo_logit)
predicciones_logit <- datos_1$Frec.Presente + datos_1$Frec.Ausente * datos_1$Proporcion.Presente_Logistico
rmse(datos_1$Proporcion.Presente, datos_1$Proporcion.Presente_Logistico)
BIC(modelo_logit)
#dispersiontest(modelo_logit)
#mod_logistico_3 <- glm(Proporcion.Presente ~ Alcohol, family = binomial(link = logit), data = datos_1, weights = Frec.Presente + Frec.Ausente)
#summary(mod_logistico_3)

# Ajustar un modelo de regresión logística binomial (Modelo Probit)
modelo_probit <- glm(cbind(Frec.Presente, Frec.Ausente) ~ Alcohol, family = binomial(link = "probit"), data = datos_1)
datos_1$Proporcion.Presente_Probit <- fitted.values(modelo_probit)
predicciones_probit <- datos_1$Frec.Presente + datos_1$Frec.Ausente * datos_1$Proporcion.Presente_Probit
rmse(datos_1$Proporcion.Presente, datos_1$Proporcion.Presente_Probit)
BIC(modelo_probit)


# Ajustar un modelo de regresión lineal
modelo_lineal <- lm(Proporcion.Presente ~ Alcohol, data = datos_1)
datos_1$Proporcion.Presente_Lineal <- fitted.values(modelo_lineal)
predicciones_lineal <- datos_1$Frec.Presente + datos_1$Frec.Ausente * datos_1$Proporcion.Presente_Lineal
rmse(datos_1$Proporcion.Presente, datos_1$Proporcion.Presente_Lineal)
BIC(modelo_lineal)

# Crear un gráfico de comparación de los tres modelos
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
    values = c("Logistico" = colores[4], "Probit" = colores[2], "Lineal" = colores[1]),
    labels = c("Logístico", "Probit", "Lineal")
  ) +
  guides(
    color = guide_legend(title = "Modelo")
  )

datos_2 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_2")
muestra <- datos_2 %>% sample_n(size = 40, replace = FALSE)

muestra %>%
  summarise(Promedio = mean(puntaje_mat, na.rm = TRUE),
            DE = sd(puntaje_mat, na.rm = TRUE),
            Mediana = median(puntaje_mat, na.rm = TRUE),
            CV = DE / Promedio * 100,
            Minimo = min(puntaje_mat, na.rm = TRUE),
            Maximo = max(puntaje_mat, na.rm = TRUE))

muestra %>%
  summarise(Promedio = mean(num_premios, na.rm = TRUE),
            DE = sd(num_premios, na.rm = TRUE),
            Mediana = median(num_premios, na.rm = TRUE),
            CV = DE / Promedio * 100,
            Minimo = min(num_premios, na.rm = TRUE),
            Maximo = max(num_premios, na.rm = TRUE))

table(muestra$programa)
ggplot(muestra, aes(x = factor(programa), fill = programa)) +
  geom_bar(stat = "count") + 
  scale_fill_manual(
    values = c("Academico" = colores[1], "General" = colores[2], "Vocacional" = colores[4])
  ) +
  labs(title = "Distribución de Programas",
       x = "Programa",
       y = "Frecuencia")+
  theme(legend.position = "none")

modelo_poisson <- glm(num_premios ~ programa + puntaje_mat ,family = poisson(link = "log"), data = datos_2)
summary(modelo_poisson)

datos_3 <- read.xlsx("Datos_MP1.xlsx", sheet = "Datos_3")
muestra3 <- datos_3 %>% sample_n(size = 11, replace = FALSE)


