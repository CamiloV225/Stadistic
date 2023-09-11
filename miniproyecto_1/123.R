#Instalar lobrerias
install.packages("dplyr")
install.packages("openxlsx")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("Factoshiny")
install.packages("corrplot")
install.packages("philentropy")


#Cargar librerias
library(dplyr)
library(openxlsx)
library(FactoMineR) ## librería más comercial para Multivariada
library(factoextra) ## libreria para mejorar gráficas con ggplot
library(ggplot2)
library(corrplot)
library(philentropy) ## calcular distancias


#Directorio de trabajo
setwd("C:\\Users\\sampi\\OneDrive\\Documentos\\UAO\\semestre_5\\Estadistica_2\\ACP")

datos <- read.xlsx("Clientes.xlsx")

Tbl_descriptivas = datos %>%
  summarise(Prom = mean(edad, na.rm = T),
            Media = median(edad, na.rm = T),
            Minimo = min(edad, na.rm = T),
            Maximo = max(edad, na.rm = T),
            DE = sd(edad, na.rm = T),
            CV= round(DE/Prom*100, 2) )

df_analisis = datos[,-c(1,8)]

corrplot(cor(df_analisis), ,method = 'square', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('BrBG'),type = 'lower')

#Hipótesis sobre correlaciones
cor.test(df_analisis$edad, df_analisis$Tarjeta_credito)
cor.test(df_analisis$educacion, df_analisis$años_empleo)


#ACP 

res.ACP <- PCA(df_analisis)
res.ACP$eig
fviz_eig(res.ACP)
res.ACP <- PCA(df_analisis,ncp = 3)

fviz_pca_var(res.ACP, axes = c(1,2))
fviz_pca_var(res.ACP, axes = c(1,3))
fviz_pca_var(res.ACP, axes = c(2,3))

#Contribuciones
corrplot(res.ACP$var$cos2, is.corr=FALSE,method="number")