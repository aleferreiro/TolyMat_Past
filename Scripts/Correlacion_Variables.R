#### HACER ARBOL DE CORRELACION    ####

#Cargo paquetes necesarios
library("raster")
library("rasterVis")
library("Hmisc")
library("corrplot")

#Seteo el working directory
setwd("D:/MEGAsync/Modelado/Capa_ambientales_cortadas/WorldClim1.4/Current_2.5m_Soil/EcoRegBuf1_QGIS")

#Genero un objeto con el path hacia las capas YA CORTADAS POR EL  AREA M que quiero cargar
paths_capas <- list.files(pattern = "*.asc$",full.names = TRUE)

#Genero un stack a partir de los archivos del objeto path
bios_Stacked <- raster::stack(paths_capas)

#Extraigo n puntos al azar de los cuales posteriormente se extraeran los valores de las variables
RandomPoints <- sampleRandom(bios_Stacked,size=1000,sp=TRUE)
plot(RandomPoints)

#Extraigo los valores de las variables para los puntos al azar
extractRandomClim <- extract(bios_Stacked,RandomPoints)#extraer los valores de los 500 puntos al azar para hacer el analisis de correlacion
write.csv(extractRandomClim,"RandomClim.csv")#genero el csv con los valores de los 500 puntos

#IMPORTANTE: ANTES DE IMPORTAR EL CSV GENERADO, DEBO ELIMINAR LA COLUMNA DE X O Y GENERADAS SINO LAS CORRELACIONA
#importo el csv creado que contiene los datos de las variables desde import data en la ventana de enviroment
library(readr)
RandomClim <- read.csv("RandomClim.csv", 
                          sep= ",")
RandomClim$X <- NULL

View(RandomClim)
#convierto el data frame que cargue en una matriz para usarlo con el rcorr
MatrixBiocl<-as.matrix(RandomClim)

#Hago el arbol
png(filename="ArbolCor.png",width=21,height=18, units="cm",res=300, bg="white")
plot( varclus(cor(MatrixBiocl), similarity="pearson") )#con funcion cor del paquete stats (Ima)
dev.off()#con este comando me cierra el ploteo y ahi recien me lo guarda

#puedo hacer una matriz de correlacion cheta sino
#Hago la matriz de correlacion
Corr_matrix <- cor(MatrixBiocl, method = "pearson")

#La ploteo
corrplot(Corr_matrix, type = "full",  
         tl.col = "black", tl.srt = 45, order = "hclust")
