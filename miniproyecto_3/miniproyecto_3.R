library(fpp2)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(zoo)
library(xtable)
library(readxl)
library(tseries)

setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\Stadistic\\miniproyecto_3")

passenger_df <- read.csv('AirPassengers.csv')
passenger_df <- passenger_df %>%
  rename(
    mes = Month,
    pasajeros = X.Passengers,
  )

passenger_df$mes <- as.Date(paste0(passenger_df$mes, "-01"), format = "%Y-%m-%d")
passenger_df$mes <- format(passenger_df$mes, "%Y-%m")
passenger_ts <- ts(passenger_df$pasajeros, start = 1949, frequency=12)

# autoplot
autoplot(passenger_ts) + labs(x="Tiempo", y="Número de Pasajeros", title="Total de Pasajeros de una Aerolínea en USA")

# ggseasonplot
ggseasonplot(passenger_ts) + labs(x="Mes", y="Número de Pasajeros",  title=NULL)

# boxplot
boxplot(passenger_ts ~ cycle(passenger_ts),
        xlab = "Periodo",
        ylab = "Número de Pasajeros")


# decompose Additive
autoplot(decompose(passenger_ts))

# decompose Multiplicativo
autoplot(decompose(passenger_ts, type = "mult")) +
  labs(x = "Tiempo", title = "")

# Medias Moviles

ultimo_mes_fecha <- as.yearmon(tail(passenger_df$mes, 1), "%Y-%m")
nuevas_fechas <- seq(ultimo_mes_fecha + 1/12, length.out = 4, by = 1/12)
passenger_df <- rbind(passenger_df, data.frame(mes = as.character(nuevas_fechas, "%Y-%m"), pasajeros = NA))  


calcular_media_movil <- function(datos, n) {
  if (length(datos) < n) {
    stop("El tamaño de la ventana es mayor que la longitud de los datos.")
  }

  medias_moviles <- rep(NA, length(datos))
  
  for (i in n:length(datos)) {
    if (is.na(datos[i])) {
      # Si hay un NA en datos, toma el valor de la posición anterior de medias_moviles
      datos[i] <- medias_moviles[i]
    }
    
    # Calcular la media móvil normalmente
    medias_moviles[i + 1] <- mean(datos[i:(i - n + 1)])
  }
  medias_moviles <- medias_moviles[-length(medias_moviles)]
  
  return(medias_moviles)
}

# Calcular el promedio móvil
passenger_df <- passenger_df %>%
  mutate(
    media_movil = calcular_media_movil(pasajeros, 2)
  ) %>%
  select(mes, pasajeros, media_movil)


# Crear el gráfico
df_long <- passenger_df %>%
  gather(metric, value, -mes)
df_long$mes <- as.Date(paste0(df_long$mes, "-01"), format = "%Y-%m-%d")
pronostico <- tail(passenger_df$media_movil, 4)
pronostico_df <- data.frame(
  mes = tail(df_long$mes, 4),
  metric = rep("pronostico", 4),
  value = pronostico
)
df_long <- bind_rows(df_long, pronostico_df)
df_long <- df_long %>%
  filter(!(mes %in% c("1961-04-01", "1961-03-01", "1961-02-01", "1961-01-01") & metric == "media_movil"))


ggplot(df_long, aes(mes, value, color = metric)) +
  geom_line( size = 0.25) +
  geom_line(data = filter(df_long, metric == "pronostico"), size = 0.75) +
  labs(x = "Tiempo", y = "Número de Pasajeros", title = "") +
  scale_color_manual(values = c("red", "black", "blue"), labels = NULL) +
  guides(color = "none")

# Suavización Exponencial Simple

# Función para encontrar el alpha óptimo para la Suavización Exponencial Simple
encontrar_alpha <- function(pasajeros, h = 4, alphas = seq(.01, .99, by = .01)) {

    RMSE <- rep(NA, length(alphas))
  
  for (i in seq_along(alphas)) {
    fit <- ses(pasajeros, alpha = alphas[i], h = h)
    RMSE[i] <- sqrt(fit$model$mse)
  }
  
  alpha.fit <- tibble(alpha = alphas, RMSE = RMSE)
  
  return(alpha.fit)
}
alpha_fit <- encontrar_alpha(passenger_ts)

# Encontrar el valor de alpha que minimiza el RMSE
alpha_min <- alpha_fit %>% filter(RMSE == min(RMSE))


ggplot(alpha_fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha_min, aes(alpha, RMSE), size = 1, color = "red") +
  labs(title = "", x = "alpha", y = "RMSE") +
  theme_minimal()

# Estimación SES con el valor óptimo de alpha
estimacion_ses <- ses(passenger_ts, alpha = alpha_min$alpha, h = 4)
summary(estimacion_ses)

# Pronóstico
estimacion_ses$mean

# Gráfico de la estimación
autoplot(estimacion_ses) + 
  labs(x = "Tiempo", y = "Número de Pasajeros") +
  autolayer(fitted(estimacion_ses), series = "Ajuste") +
  ggtitle(NULL) +
  theme()


# Holt

# Función para encontrar el beta óptimo para Holt
encontrar_beta <- function(pasajeros, alpha, h = 4, betas = seq(0.0001, 0.5, by = 0.001)) {
  RMSE <- rep(NA, length(betas))
  
  for (i in seq_along(betas)) {
    fit <- holt(pasajeros, alpha = alpha, beta = betas[i], h = h)
    RMSE[i] <- sqrt(fit$model$mse)
  }
  
  beta.fit <- tibble(beta = betas, RMSE = RMSE)
  
  return(beta.fit)
}

beta_fit <- encontrar_beta(passenger_ts,alpha_min$alpha)

# Encontrar el valor de beta que minimiza el RMSE
beta_min <- beta_fit %>% filter(RMSE == min(RMSE))

# Graficar RMSE vs beta
ggplot(beta_fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta_min, aes(beta, RMSE), size = 1, color = "red") +
  labs(title = "", x = "beta", y = "RMSE") +
  theme_minimal()

# Estimar Holt con los valores óptimos de alpha y beta
estimacion_holt <- holt(passenger_ts, alpha = alpha_min$alpha, beta = beta_min$beta, h = 4)
summary(estimacion_holt)

# Pronóstico
estimacion_holt$mean

# Graficar la estimación
autoplot(estimacion_holt) + 
  labs(x = "Tiempo", y = "Número de Pasajeros") +
  autolayer(fitted(estimacion_holt), series = "Ajuste") +
  ggtitle(NULL) +
  theme()


# Holt-winter

estimacion_hw <- ets(passenger_ts, model = "MAM")

# Pronóstico
pronostico_hw <- forecast(estimacion_hw, h = 4)
summary(pronostico_hw)

# Gráfico del pronóstico
autoplot(pronostico_hw) + 
  labs(x = "Tiempo", y = "Número de Pasajeros") +
  autolayer(fitted(estimacion_hw), series = "Ajuste") +
  ggtitle(NULL) +
  theme()


# Comparación de Modelos

pronosticos <- data.frame(
  mes = tail(passenger_df$mes, 4),
  medias_moviles = round(tail(passenger_df$media_movil, 4),2),
  suavizacion_exponencial_simple = round(as.vector(estimacion_ses$mean),2),
  holt = round(as.vector(estimacion_holt$mean),2),
  holt_winters = round(as.vector(pronostico_hw$mean),2)
)
xtable(pronosticos)


rmse <- function(obs, esti){
  mse <- mean((obs - esti)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  return(rmse)
}

mape = function(obs, esti){
  MAPE = mean(abs(obs - esti)/obs, na.rm = TRUE) * 100
  return(MAPE)
}

mae <- function(obs, esti){
  mae <- mean(abs(obs - esti), na.rm = TRUE)
  return(mae)
}

tabla_comparacion <- data.frame(
  Modelo = c("Medias Moviles", "Suavización Exponencial Simple", "Holt", "Holt-winter"),
  RMSE = round(c(rmse(passenger_df$pasajeros, passenger_df$media_movil), accuracy(estimacion_ses)[2], accuracy(estimacion_holt)[2], accuracy(estimacion_hw)[2]), 2),
  MAE = round(c(mae(passenger_df$pasajeros, passenger_df$media_movil), accuracy(estimacion_ses)[3], accuracy(estimacion_holt)[3], accuracy(estimacion_hw)[3]), 2),
  MAPE = round(c(mape(passenger_df$pasajeros, passenger_df$media_movil), accuracy(estimacion_ses)[5], accuracy(estimacion_holt)[5], accuracy(estimacion_hw)[5]), 2)
)
xtable(tabla_comparacion)


predictions <- data.frame(
  mes = passenger_df$mes,
  medias_moviles = ,
  suavizacion_exponencial_simple = round(as.vector(estimacion_ses$mean),2),
  holt = round(as.vector(estimacion_holt$mean),2),
  holt_winters = round(as.vector(pronostico_hw$mean),2)
)

predictions_df <- passenger_df %>%
  slice(1:(n() - 4)) %>%
  mutate(
    SES = as.vector(fitted(estimacion_ses)),
    Holt = as.vector(fitted(estimacion_holt)),
    Holt_Winter = as.vector(fitted(estimacion_hw))
  ) %>%
  rename(Medias_Moviles = media_movil,
         Real = pasajeros)

predictions_long <- predictions_df %>%
  gather(metric, value, -mes)
predictions_long$mes <- as.Date(paste0(predictions_long$mes, "-01"), format = "%Y-%m-%d")

ggplot(data = predictions_long, aes(x = mes, y = value, color = metric)) +
  geom_line(size = 0.75) +
  labs(x = "Tiempo", y = "Número de Pasajeros", title = "") +
  scale_x_date(date_labels = "%Y") +
  scale_color_manual(values = c("SES" = "#a6033f", "Holt" = "#dbf73b", "Medias_Moviles" = "#c0cc39", "Holt_Winter" = "#eb0258", "Real" = "#2b2628")) +
  theme(legend.title = element_blank())




#Ejemplo Viviendas

# Leyendo datos desde un archivo Excel
viviendas_df <- read_excel('Viviendas.xlsx', col_types = c("date", "numeric"))
colnames(viviendas_df) <- tolower(colnames(viviendas_df))
viviendas_ts <- ts(viviendas_df$viviendas, start = 1959, frequency=12)

# Análisis exploratorio de datos (EDA)
autoplot(viviendas_ts) +
  labs(x = "Tiempo", y = "Número Viviendas", title = "")

boxplot(viviendas_ts ~ cycle(viviendas_ts),
        xlab = "Periodo",
        ylab = "Número Viviendas")


autoplot(decompose(viviendas_ts, type = "mult")) +
  labs(x = "Tiempo", title = "")

#autoplot(viviendas_ts-decompose(viviendas_ts)$trend)

# Realizar la prueba ADF en viviendas_ts
adf.test(viviendas_ts)
#H0 = No es Estacionaria p > 0.05
#Ha = Es estacionaria p < 0.05

# Crear una serie de tiempo diferenciada de primer orden
viviendas_diff1 <- diff(viviendas_ts, 1)
par(mfrow = c(1,2))
plot(viviendas_ts, ylab = "Número Viviendas", xlab= "Tiempo")
plot(viviendas_diff1, ylab = "diff(viviendas_ts, 1)", xlab= "Tiempo")

adf.test(viviendas_diff1)

#Identificar Modelo
par(mfrow = c(1,2))
acf(viviendas_diff1, main = "FAC",ylim=c(-1, 1))
pacf(viviendas_diff1, main = "FACP",ylim=c(-1, 1))

#Una forma de eliminar la estacionalidad de los datos, restamos el componente estacional de la serie original(media cero) y luego lo diferenciamos para que sea estacionario ( varianza constante e independiente).
viviendas_componentes = decompose(viviendas_ts)
viviendas_ajustada  <- viviendas_ts - viviendas_componentes$seasonal
viviendas_ajustada <- diff(viviendas_ajustada, 1)
plot(viviendas_ajustada, ylab = "Diferencia de Número de viviendas")

adf.test(viviendas_ajustada)


par(mfrow = c(1,2))
acf(viviendas_ajustada, main = "FAC",ylim=c(-1, 1))
pacf(viviendas_ajustada, main = "FACP",ylim=c(-1, 1))

#Posibles soluciones de más a menos probables: arima(1,1,1), arima(2,1,1)

modelo_sarima_1 <- arima(viviendas_ts, order=c(1,1,1), seasonal = list(order = c(1,0,0), period = 12), method="ML")
modelo_sarima_2 <- arima(viviendas_ts, order=c(2,1,1), seasonal = list(order = c(1,0,0), period = 12), method="ML")


resumen_tabla <- data.frame(
  Modelo = c("ARIMA(1,1,1)(1,0,0)", "ARIMA(2,1,1)(1,0,0)"),
  AIC = c(AIC(modelo_sarima_1), AIC(modelo_sarima_2)),
  BIC = c(BIC(modelo_sarima_1), BIC(modelo_sarima_2))
)

xtable(resumen_tabla)

#Validación de supuestos
#1. Estacionariedad

adf.test(modelo_sarima_2$residuals) 

#2. Normalidad
shapiro.test(modelo_sarima_2$residuals)

# Pronostico
pronostico_sarima <- forecast(modelo_sarima_2, h=6)

autoplot(pronostico_sarima) + 
  labs(x = "Tiempo", y = "Número de Viviendas") +
  autolayer(fitted(pronostico_sarima), series = "Ajuste") +
  ggtitle(NULL) +
  theme()


ultimo_mes_fecha <- as.yearmon(tail(viviendas_df$fecha, 1), "%Y-%m")
nuevas_fechas <- seq(ultimo_mes_fecha + 1/12, length.out = 6, by = 1/12)


pronosticos <- data.frame(
  mes = as.character(nuevas_fechas, "%Y-%m"),
  viviendas = round(as.vector(pronostico_sarima$mean), 2), 
  Lo_80 = round(head(as.vector(pronostico_sarima$lower), 6), 2),
  Hi_80 = round(head(as.vector(pronostico_sarima$upper), 6), 2),
  Lo_95 = round(tail(as.vector(pronostico_sarima$lower), 6), 2),
  Hi_95 = round(tail(as.vector(pronostico_sarima$upper), 6), 2)
)

xtable(pronosticos)







# Ejemplo Consumo

consumo_df <- read_excel('Consumo.xlsx')
colnames(consumo_df) <- tolower(colnames(consumo_df))
consumo_ts <- ts(consumo_df$consumo, start = 1970, frequency=4)

# Análisis exploratorio de datos (EDA)
autoplot(consumo_ts) +
  labs(x = "Tiempo", y = "% Cambio en Gasto de Consumo", title = "")

boxplot(as.numeric(consumo_ts) ~ cycle(consumo_ts), 
         xlab = "Periodo", ylab = "% Cambio en Gasto de Consumo", 
         main = "")


autoplot(decompose(consumo_ts, type = "mult")) +
  labs(x = "Tiempo", title = "")

# Realizar la prueba ADF 
adf.test(consumo_ts)

#Identificar Modelo
par(mfrow = c(1,2))
acf(consumo_ts, main = "FAC",ylim=c(-1, 1))
pacf(consumo_ts, main = "FACP",ylim=c(-1, 1))


#consumo_diff1 <- diff(consumo_ts, 1)
#par(mfrow = c(1,2))
#acf(consumo_diff1, main = "FAC",ylim=c(-1, 1))
#pacf(consumo_diff1, main = "FACP",ylim=c(-1, 1))

#consumo_componentes = decompose(consumo_ts)
#consumo_ajustada  <- consumo_ts - consumo_componentes$seasonal
#consumo_ajustada <- diff(consumo_ajustada, 1)
#par(mfrow = c(1,2))
#acf(consumo_ajustada, main = "FAC",ylim=c(-1, 1))
#pacf(consumo_ajustada, main = "FACP",ylim=c(-1, 1))


#Posibles soluciones de más a menos probables: arima(1,0,0), arima(0,0,1),
#arima(0,0,2), arima(1,0,1), arima(1,0,2)

modelo_ar1 <- arima(consumo_ts, order=c(1,0,0))
modelo_ma1 <- arima(consumo_ts, order=c(0,0,1))
modelo_ma2 <- arima(consumo_ts, order=c(0,0,2))
modelo_arma_1 <- arima(consumo_ts, order=c(1,0,1))
modelo_arma_2 <- arima(consumo_ts, order=c(1,0,2))


resumen_tabla <- data.frame(
  Modelo = c("ARIMA(1,0,0)", "ARIMA(0,0,1)", "ARIMA(0,0,2)", "ARIMA(1,0,1)", "ARIMA(1,0,2)"),
  AIC = c(AIC(modelo_ar1), AIC(modelo_ma1), AIC(modelo_ma2), AIC(modelo_arma_1), AIC(modelo_arma_2)),
  BIC = c(BIC(modelo_ar1), BIC(modelo_ma1), BIC(modelo_ma2), BIC(modelo_arma_1), BIC(modelo_arma_2))
)
xtable(resumen_tabla)

# Utilice la función auto.arima para encontrar automáticamente el mejor modelo ARIMA
fit.auto <- auto.arima(consumo_ts) # El mejor modelo encontrado es ARIMA(1,0,3)(1,0,1)[4]

AIC(fit.auto) # La puntuación AIC es 342,6722

BIC(fit.auto) # La puntuación BIC es 368,521

# El modelo encontrado manualmente tiene mejores estadísticas (puntuaciones AIC y BIC más bajas) que el modelo encontrado automáticamente.


#Pronostico
pronostico_arma_1 <- forecast(modelo_arma_1, h=6)
autoplot(pronostico_arma_1) + 
  labs(x = "Tiempo", y = "% Cambio en Gasto de Consumo") +
  autolayer(fitted(pronostico_arma_1), series = "Ajuste") +
  ggtitle(NULL) +
  theme()


pronosticos <- data.frame(
  año_trimestre = c("2016 Q4", "2017 Q1", "2017 Q2", "2017 Q3", "2017 Q4", "2018 Q1"),
  gasto_consumo_personal = round(as.vector(pronostico_arma_1$mean), 2), 
  Lo_80 = round(head(as.vector(pronostico_arma_1$lower), 6), 2),
  Hi_80 = round(head(as.vector(pronostico_arma_1$upper), 6), 2),
  Lo_95 = round(tail(as.vector(pronostico_arma_1$lower), 6), 2),
  Hi_95 = round(tail(as.vector(pronostico_arma_1$upper), 6), 2)
)

xtable(pronosticos)

#Validación de supuestos
#1. Estacionariedad

adf.test(modelo_arma_1$residuals)

#2. Normalidad
shapiro.test(modelo_arma_1$residuals)

#Box.test(modelo_arma_1$residuals, type="Ljung-Box")

