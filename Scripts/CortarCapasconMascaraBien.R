#### CORTAR CAPAS CON MASCARA    ####

#Cargo paquetes necesarios
library("raster")

#Seteo el working directory
setwd("D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/LIG_30S")

#Genero un objeto con el path hacia las capas que quiero cargar
paths_capas <- list.files(pattern = "*.bil$",full.names = TRUE)

#Genero un stack a partir de los archivos del objeto path
bios <- raster::stack(paths_capas)

#cargo capa que usare de modelo para modificar resolucion, en este ejemple de CHELSA, Debe tener misma
#extension y resolucion
y <- raster("D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/LGM_2.5m/MIROC/bio1.tif")

#con la funcion RESAMPLE puedo transferir valores entre dos raster diferentes en resolucion y origen.
StackResampled <- resample(bios, y, method="bilinear", )#ver si anda la resolucion

#Creo una carpeta para guardar mis raster dentro de mi directorio
dir.create("LIG_2.5m")
#Creo los rasters en mi carpeta
lapply(names(StackResampled), function(x){
  writeRaster(StackResampled[[x]], paste0("Sudam/LIG_2.5m",
                                       x,".asc"),overwrite=TRUE)})

#Cargo las mascara que usaremos
Mask <- shapefile("D:/MEGAsync/Modelado/Areas M/MaskSudam.shp")

#El crop me recorta los rasters a partir de un shape. Es recomendable antes de usar la funcion mask.
StackCropped <- raster::crop(x = bios,y = Mask)

#Ahora uso la funcion mask para cortar el stack cropeado, nuevamente usando el shapefile de mi mascara
StackMasked <- raster::mask(StackCropped, Mask)

#Creo una carpeta para guardar mis raster dentro de mi directorio
dir.create("LIG_30s_Sudam")
#Creo los rasters en mi carpeta
lapply(names(StackMasked), function(x){
  writeRaster(StackMasked[[x]], paste0("LIG_30s_Sudam/",
                                            x,".asc"),overwrite=TRUE)})

