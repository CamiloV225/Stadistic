library(tidyverse)
library(lubridate)
library(fpp2)
library(zoo)
library(dplyr)

df <- data.frame(
  mes = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January", "February"),
  año = c(2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2014, 2014),
  ventas = c(110, 114, 123, 131, 140, 146, 157, 164, 169, 180, 184, 191, NA, NA)
)

# Crear una nueva columna "fecha"
df$fecha <- as.Date(paste(df$año, match(df$mes, month.name), "01", sep = "-"), format = "%Y-%m-%d")
df <- subset(df, select = -c(mes, año))

# Grafico de la Serie de Tiempo
ts_ventas <- ts(df$ventas, start = c(2013, 1), end = c(2014, 2), frequency = 12)
plot(ts_ventas)

#Medias Moviles 
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
  medias_moviles <- medias_moviles[1:14]
  
  return(medias_moviles)
}


calcular_media_movil(df$ventas, 4)

df <- df %>%
  mutate(
    ventas_2 = calcular_media_movil(ventas, 2),
    ventas_3 = calcular_media_movil(ventas, 3),
    ventas_4 = calcular_media_movil(ventas, 4)
  ) %>%
  select(fecha, ventas, ventas_2, ventas_3, ventas_4)

# Reorganizar los datos en formato "largo" para graficar
df_long <- df %>%
  gather(metric, value, -fecha)

# Crear el gráfico de líneas
ggplot(df_long, aes(fecha, value, color = metric)) +
  geom_line() +
  geom_point(data = filter(df_long, metric == "ventas")) +
  labs(title = "Comparación de Series", x = "Fecha", y = "Valor") +
  theme_minimal()

  




