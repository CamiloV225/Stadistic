library(openxlsx)
library(dplyr)
library(ggplot2)
library(flexmix)
library(MASS)
library(corrplot)
library(data.table)
library(Metrics)
library(car)

setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\Stadistic\\miniproyecto_1")
cars_df <- fread("data.csv")
names(cars_df)

#Seleccionar Variables
cars_analysis <- cars_df %>%
  dplyr::select(
    daysonmarket, 
#    frame_damaged, 
    horsepower, 
#    is_new,
    maximum_seating, 
    mileage, 
    price, 
    seller_rating,
    year
  )

#Matriz de Correlación
corrplot(cor(cars_analysis))
cor(cars_analysis)
cor.test(cars_analysis$price, cars_analysis$seller_rating)

#Análisis descriptivo 
resumen <- cars_analysis %>% 
  summarise(Promedio = mean(price, na.rm = T),
            DE = sd(price, na.rm = T),
            Mediana = median(price, na.rm = T),
            CV = DE/Promedio*100,
            Minimo = min(price, na.rm = T),
            Maximo = max(price, na.rm = T))
resumen

# Seleccionar una muestra para el entrenamiento y los tests
set.seed(1)
muestra_entrenamiento <- cars_analysis %>%
  sample_frac(size = 0.8, replace = FALSE)

muestra_prueba <- cars_analysis %>%
  anti_join(muestra_entrenamiento)

#muestra_entrenamiento_normalized <- as.data.frame(scale(muestra_entrenamiento))

#Modelos de Regresión Lineal Múltiple
modelo_lineal <- lm(price ~ ., data = muestra_entrenamiento)
summary(modelo_lineal)

#Multicolinealidad

modelo_lineal1 <- lm(price ~ year + mileage + maximum_seating + horsepower , data = muestra_entrenamiento)
summary(modelo_lineal1)
vif(modelo_lineal1)

modelo_lineal2 <- lm(price ~ year + mileage + horsepower , data = muestra_entrenamiento)
summary(modelo_lineal2)
vif(modelo_lineal2)


modelo_lineal3 <- lm(price ~ year + mileage + horsepower + daysonmarket, data = muestra_entrenamiento)
summary(modelo_lineal3)

modelo_lineal4 <- lm(price ~ year + mileage, data = muestra_entrenamiento)
summary(modelo_lineal4)


tabla_resultados <- data.frame(
  Modelo = c("modelo_lineal1", "modelo_lineal2", "modelo_lineal3"),
  BIC = c(BIC(modelo_lineal1), BIC(modelo_lineal2), BIC(modelo_lineal3)),
  AIC = c(AIC(modelo_lineal1), AIC(modelo_lineal2), AIC(modelo_lineal3)),
  RMSE = c(rmse(cars_analysis$price, predict(modelo_lineal1,cars_analysis)), rmse(cars_analysis$price, predict(modelo_lineal2,cars_analysis)), rmse(cars_analysis$price, predict(modelo_lineal3,cars_analysis))))

tabla_resultados

predictions <- data.frame(
  Price = muestra_prueba$price,
  Modelo1 = predict(modelo_lineal1, newdata = muestra_prueba, type = "response"),
  Modelo2 = predict(modelo_lineal2, newdata = muestra_prueba, type = "response"),
  Modelo3 = predict(modelo_lineal3, newdata = muestra_prueba, type = "response")
)

predictions$Index <- 1:nrow(muestra_prueba)
predictions_40 <- predictions[1:40, ]

ggplot(predictions_40, aes(x = Index, y = Price)) +
  geom_line() +
  geom_line(aes(y = Modelo1), color = "blue", size = 1)


ggplot() +
  geom_point(data = datos_1, aes(x = Price, y = Proporcion.Presente)) +
  geom_line(data = predictions, aes(x = Price, y = logit, color = "Logit"), lwd = 1) +
  geom_line(data = predictions, aes(x = Price, y = probit, color = "Probit"), lwd = 1) +
  geom_line(data = predictions, aes(x = Price, y = lineal, color = "Lineal"), lwd = 1) +
  labs(
    title = "Comparación de Modelos",
    x = "Price (Ponderado)",
    y = "Proporción de Niños"
  ) +
  scale_color_manual(
    values = c("Logit" = "#A2C579", "Probit" = "#016A70", "Lineal" = "#D2DE32"),
  ) +
  guides(
    color = guide_legend(title = "Modelo")
  )


#El modelo subestima