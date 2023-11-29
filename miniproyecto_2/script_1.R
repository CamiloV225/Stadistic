# Bibliotecas para manipulación y visualización de datos
library(dplyr)
library(openxlsx)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ade4)
library(stringr)
library(scales)
library(xtable)
library(corrplot)
library(data.table)
library(hms)
library(cluster)

# Bibliotecas para manipulación de datos espaciales
library(sp)
library(sf)
library(ggiraph)
library(plotly)
library(devtools)
library(widgetframe)
library(raster)
library(ggspatial)

# Bibliotecas adicionales
library(tidyverse)
library(crosstalk)
library(maptools)



# Estableciendo el directorio de trabajo
setwd('C:/Users/sampi/OneDrive/Documentos/UAO/semestre_5/Estadistica_2/Stadistic/miniproyecto_2')
dir()

# Leyendo datos desde un archivo Excel
paises_df = read.xlsx('Datos_Paises.xlsx')

# Procesando datos
paises_df <- paises_df %>%
  rename(
    pais = País,
    x1 = X.1,
    x2 = X.2,
    x3 = X.3,
    x4 = X.4,
    x5 = X.5,
    x6 = X.6,
    x7 = X.7,
    x8 = X.8,
    x9 = X.9,
    x10 = X.10,
    x11 = X.11
  ) %>% 
  mutate(
    pais = str_split_fixed(pais, ", ", 2)[,2]
  )

rownames(paises_df) <- paises_df[,1]
paises_df <- paises_df[,-1]

# Calculando y visualizando la matriz de correlación
matriz_correlacion <- cor(paises_df)
corrplot(matriz_correlacion, method = 'square', addCoef.col = 'black',
         cl.pos = 'n', col = COL2('BrBG'))

xtable(matriz_correlacion)

# Realizando Análisis de Componentes Principales (PCA)
res.ACP <- PCA(paises_df, graph = FALSE)
fviz_eig(res.ACP, addlabels = TRUE)
xtable(res.ACP$eig)


# Realizando PCA con 2 componentes principales
res.ACP <- PCA(paises_df, graph = FALSE, ncp = 2)
fviz_pca_biplot(res.ACP, select.ind = list(cos2 = 25))

# Visualizando la proporción de varianza explicada acumulada
prop_varianza <- data.frame(pc = 1:11, accum = res.ACP$eig[,3], prop_varianza = res.ACP$eig[,2])
ggplot(prop_varianza, aes(x = pc, y = accum))+
  geom_point()+
  geom_line()+
  geom_label(aes(label = round(accum,2))) +
  theme_bw()+
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

xtable(res.ACP$svd$V)



# Visualizar la contribución de variables al primer componente principal
fviz_contrib(res.ACP, choice ="var", axes = 1)

# Visualizar la contribución de variables al segundo componente principal
fviz_contrib(res.ACP, choice ="var", axes = 2)

# Creación del indice 
indice <- res.ACP$ind$coord[,1]
hist(indice)
paises_df$indice <- rescale(indice, from = c(min(indice), max(indice)), to = c(0, 100))

cuartiles <- round(quantile(paises_df$indice, probs = c(0.25, 0.5, 0.75)), digits = 0)

paises_df$clasificacion[paises_df$indice < cuartiles[1]] <- paste("0 - ", cuartiles[1])
paises_df$clasificacion[paises_df$indice >= cuartiles[1] & paises_df$indice < cuartiles[2]] <- paste(cuartiles[1], " - ", cuartiles[2])
paises_df$clasificacion[paises_df$indice >= cuartiles[2] & paises_df$indice < cuartiles[3]] <- paste(cuartiles[2], " - ", cuartiles[3])
paises_df$clasificacion[paises_df$indice >= cuartiles[3]] <- paste(cuartiles[3], " - 100")

xtable(table(paises_df$clasificacion))


#ACM

# Leer el archivo CSV
hurtos <- fread("BASE_HURTOS.csv")
hurtos <- hurtos  %>%
  filter(DINÁMICA == "Hurto") %>%
  filter(ESTRA_MODA != "SD")

# Categorizar la hora del hecho en franjas: MAÑANA, TARDE, NOCHE
hurtos$HORA_DEL_H <- as_hms(paste0(hurtos$HORA_DEL_H, ":00"))

categorizar_hora <- function(hora) {
  if (is.na(hora) || hora == "") {
    return("SIN HORA VÁLIDA")
  } else if (hora >= as_hms("00:00:00") & hora <= as_hms("11:59:59")) {
    return("MAÑANA")
  } else if (hora >= as_hms("12:00:00") & hora <= as_hms("17:59:59")) {
    return("TARDE")
  } else {
    return("NOCHE")
  }
}

hurtos$CATEGORIA_HORA <- sapply(hurtos$HORA_DEL_H, categorizar_hora)

# Categorizar la zona de acuerdo a la comuna

hurtos$COMUNA <- as.numeric(hurtos$COMUNA)
hurtos$COMUNA[is.na(hurtos$COMUNA) == TRUE] <- 99

categorizar_zona <- function(comuna) {
  comuna_actual <- sprintf("%02d", as.numeric(comuna))  
  if (comuna_actual %in% c("01", "02", "03", "09")) {
    return("NOR_OCCIDENTE")
  } else if (comuna_actual %in% c("04", "05", "06", "07", "08")) {
    return("NOR_ORIENTE")
  } else if (comuna_actual %in% c("11", "12", "16")) {
    return("ORIENTE")
  } else if (comuna_actual %in% c("17", "18", "19", "20", "22", "10")) {
    return("SUR")
  } else if (comuna_actual %in% c("13", "14", "15", "21")) {
    return("DISTRITO_AB")
  } else {
    return("RURAL")
  }
}

hurtos$ZONA <- sapply(hurtos$COMUNA, categorizar_zona)

hurtos$CARACTERIZ <- gsub("HURTO DE ", "", hurtos$CARACTERIZ)
hurtos$CARACTERIZ <- gsub("HURTO  A ", "", hurtos$CARACTERIZ)
hurtos$CARACTERIZ <- gsub("HURTO A ", "", hurtos$CARACTERIZ)

# Seleccion de variables 
hurtos_df <- hurtos %>%
  dplyr::select(DINÁMICA, CATEGORIA_HORA, DÍA_SEMAN, MES_DEL_HE,
                ZONA, ESTRA_MODA, CARACTERIZ)

hurtos_df <- hurtos_df %>%
  rename_all(tolower) %>%
  rename(
    dia = día_seman,
    jornada = categoria_hora,
    mes = mes_del_he,
    estrato = estra_moda,
    caracterizacion = caracteriz
  )

hurtos_df <- hurtos_df %>%
  mutate_all(~str_to_title(.))

hurtos_df <- hurtos_df[, -1]

# Realizar el Análisis de Análisis Correspondencias Múltiples (ACM)
res.MCA <- MCA(hurtos_df, graph = FALSE)

fviz_mca_var(res.MCA , select.var = list(contrib = 20), repel = TRUE)

# Visualizar la contribución de las variables
fviz_contrib(res.MCA , choice ="var", axes = 1)
fviz_contrib(res.MCA , choice ="var", axes = 2)

fviz_contrib(res.MCA , choice ="var", axes=c(1,2)) #primer plano factorial



# Realizar el Análisis de Clusters 

#fviz_nbclust(res.MCA$ind$coord, FUNcluster = kmeans, method = c("silhouette"), k.max = 8, nboot = 100,)
fviz_nbclust(res.MCA$ind$coord, kmeans, method = "wss") +
  geom_vline(xintercept =3, linetype = 2)


cluster <- HCPC(res.MCA, nb.clust=3)

#Primer cluster
cluster$desc.var$category[1]
xtable(cluster$desc.var$category[[1]])

#Segundo cluster 
cluster$desc.var$category[2]
xtable(cluster$desc.var$category[[2]])

#Tercer cluster 
xtable(cluster$desc.var$category[[3]])


# Extraer el contenido del primer elemento de la lista




# Clusters en el mapa de Cali
hurtos_df$cluster <- cluster$data.clust$clust
hurtos_df$barrio <- hurtos$BARRIO_1

mode <- function(codes){
  which.max(tabulate(codes))}

barrios_cluster <- hurtos_df %>%
  group_by(barrio)%>%
  summarise(cluster = mode(cluster))

barrios_cluster$cluster <- as.character(barrios_cluster$cluster)

Mapa_Cali_Barrios <- st_read("mapa_barrios/barrios.shp")

Mapa_Cali_Barrios$BARRIO <- str_to_title(Mapa_Cali_Barrios$BARRIO)

mps_sft <- st_as_sf(Mapa_Cali_Barrios) %>%
  inner_join(., y = barrios_cluster, by = c("BARRIO" = "barrio"))

shp_sft <- st_as_sf(Mapa_Cali_Barrios)  
mps_sft <- crosstalk::SharedData$new(mps_sft, key = mps_sft$BARRIO)

ggplot(mps_sft) +
  geom_sf(data = shp_sft, fill = "white")+
  geom_sf(aes(fill = cluster))+
  theme(panel.grid.major = element_blank())+
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.1,"in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf()+
  theme_bw()

meses_ordenados <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo")

hurtos_df$mes <- factor(hurtos_df$mes, levels = meses_ordenados)





par(mfrow=c(2,2))
ggplot(hurtos_df, aes(x = mes, group = cluster, color = cluster)) +
  geom_line(stat = "count") +
  geom_point(stat = "count", size = 2) +  # Agregar puntos
  labs(title = "Dinámica de Hurtos por Mes y Cluster",
       x = "Mes",
       y = "Cantidad de Hurtos") +
  theme_minimal()


# Gráfica de Distribución de Zona por Cluster (Barras Acumuladas)
ggplot(hurtos_df, aes(x = cluster, fill = zona)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Distribución de Zona por Cluster",
       x = "Cluster",
       y = "Cantidad de Hurtos") +
  theme_minimal()

# Gráfica de Distribución de Estrato por Cluster (Barras Acumuladas)
ggplot(hurtos_df, aes(x = cluster, fill = estrato)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Distribución de Estrato por Cluster",
       x = "Cluster",
       y = "Cantidad de Hurtos") +
  theme_minimal()

ggplot(hurtos_df, aes(x = cluster, fill = jornada)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Distribución de Estrato por Cluster",
       x = "Cluster",
       y = "Cantidad de Hurtos") +
  theme_minimal() +
  theme(legend.position = "top")












library(gridExtra)

# Gráfico 1: Dinámica de Hurtos por Mes y Cluster
plot1 <- ggplot(hurtos_df, aes(x = mes, group = cluster, color = cluster)) +
  geom_line(stat = "count") +
  geom_point(stat = "count", size = 2) +
  labs(title = "Dinámica de Hurtos por Mes y Cluster",
       x = "Mes",
       y = "Cantidad de Hurtos") +
  theme_minimal()

# Gráfico 2: Distribución de Zona por Cluster (Barras Acumuladas)
plot2 <- ggplot(hurtos_df, aes(x = cluster, fill = zona)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Distribución de Zona por Cluster",
       x = "Cluster",
       y = "Cantidad de Hurtos") +
  theme_minimal()

# Gráfico 3: Distribución de Estrato por Cluster (Barras Acumuladas)
plot3 <- ggplot(hurtos_df, aes(x = cluster, fill = estrato)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Distribución de Estrato por Cluster",
       x = "Cluster",
       y = "Cantidad de Hurtos") +
  theme_minimal()

# Gráfico 4: Distribución de Jornada por Cluster (Barras Acumuladas)
plot4 <- ggplot(hurtos_df, aes(x = cluster, fill = jornada)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Distribución de Jornada por Cluster",
       x = "Cluster",
       y = "Cantidad de Hurtos") +
  theme_minimal() +
  theme(legend.position = "top")

# Organizar los gráficos en un diseño de 2x2
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
