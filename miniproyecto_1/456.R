#############################################################################################
#############################################################################################
#Analisis Multivariado
#Profesor: Johann A. Ospina 
#Tema: AnÃ¡lisis de Correspondencias Multiples (ACM)
#############################################################################################
#############################################################################################

library(FactoMineR)
library(factoextra)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(corrplot)
library(philentropy) ## calcular distancias
library(RColorBrewer) ## gráfico de perfiles
library(FactoClass) ## gráfico de perfiles


#Seleccionar las razas de perros de acuerdo con la función para la que se utilizan: compañia, caza o utilidad (salvamento, defensa, perro lazarillo o policía,
#etc.). Los datos se encuentran en el paquete FactoClass como DogBreeds.
#Para cada una de las veintisiete razas estudiadas se registran seis variables
#que miden las cualidades físicas o psíquicas de la raza:

#Variables: Categorías
#Tamaño: Pequeño Medio Grande
#Peso: Liviano Medio Pesado
#Velocidad: Baja Media Alta
#Inteligencia: Pequeña Media Grande
#Afectividad: Pequeña Grande
#Agresividad: Pequeña Grande
#Función: Compañía Caza Utilidad

#Directorio de trabajo
setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\ACM")

Datos_razas <- read.xlsx("Datos_razas.xlsx", rowNames = TRUE)



## Estadisticas descriptivas y pruebas chi cuadrado

summary(Datos_razas)

## Tabla de frecuencias
Tabla1 <- Datos_razas %>% 
  group_by(TAM)%>% 
  summarise(n = n()) %>%
  mutate(Porcentaje = round(n/sum(n)*100,1)  )

write.xlsx(Tabla1, "Tab_Tamaño.xlsx")


Datos_razas %>% 
  group_by(PES)%>% 
  summarise(n = n()) %>%
  mutate(Porcentaje = n/sum(n)*100  )

png("Figura1.png")
ggplot(data = Tabla1, aes(x = TAM, y = n)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(title="Diagrama de barras", 
       x="Tamaño", y = "Frecuencia", size=10)+
  geom_text(aes(label = paste0(n, " (",Porcentaje,"%)") ) , vjust=1.6, color="white", size=8)
dev.off()



res.ACMperros <- MCA(Datos_razas, quali.sup = 7)


fviz_mca_biplot(res.ACMperros,repel = TRUE)

#library(shiny)
#library(Factoshiny)
#MCAshiny(res.ACMperros)

### contribucciones

fviz_contrib(res.ACMperros,choice="var",axes=1)
fviz_contrib(res.ACMperros,choice="var",axes=2)
fviz_contrib(res.ACMperros,choice="var",axes=3)


### cosenos2
fviz_cos2(res.ACMperros,choice=("var"),axes=1)
fviz_cos2(res.ACMperros,choice=("var"),axes=2)
fviz_cos2(res.ACMperros,choice=("var"),axes=3)



### contribucciones individuos
fviz_contrib(res.ACMperros,choice="ind",axes=1)
fviz_contrib(res.ACMperros,choice="ind",axes=2)
fviz_contrib(res.ACMperros,choice="ind",axes=3)

### cosenos2 individuos
fviz_cos2(res.ACMperros,choice=("ind"),axes=1)
fviz_cos2(res.ACMperros,choice=("ind"),axes=2)
fviz_cos2(res.ACMperros,choice=("ind"),axes=3)

### Cluster jerarquico

clusterPerros <- HCPC(res.ACMperros,nb.clust = -1)
fviz_cluster(clusterPerros,repel = TRUE)

clusterPerros$desc.var ## la interpretacion de cada cluster
## si el pvalor es pequeño la categoria es importante en el cluster
## v.test es positivo indica que los individuos en su mayoria
## presenta la categoria. v.tets negativo lo contrario

## Cluster 1.
## En su mayoria son perros con peso pesado, fun uti, afectividad baja
## tamaño grande

## cluster 2
## en su mayoria peso mediano, velocidad alta, tamaño grande, caza

## cluster 3
## tamaño mediano, velocidad mediana, afectividad alta

## cluster 4
## tamaño pequeño, peso liviano, fun compañia, velocidad lenta.
