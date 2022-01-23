##### Cargo los stacks de las variables que voy a usar en el analisis ###

options(digits = 22)

#Cargo paquetes necesarios
library("raster")

##Seteo el working directory####
setwd("D:/MEGAsync/Modelado/Capa_ambientales_cortadas/WorldClim1.4/WorldClim7_Soil_AreaMpaper")

###### Genero archivo con links a las capas cortadas por sudam#####
#Climaticas
paths_Actual <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/Current/Actual_2.5min/Sudam",
                          pattern = "*.asc$",full.names = TRUE)
paths_HTMccsm <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/HTM_2.5m/CCSM/Sudam",
                           pattern = "*.asc$",full.names = TRUE)
paths_HTMmiroc <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/HTM_2.5m/MIROC/Sudam",
                            pattern = "*.asc$",full.names = TRUE)
paths_HTMmpi <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/HTM_2.5m/MPI/Sudam",
                            pattern = "*.asc$",full.names = TRUE)
paths_LGMccsm <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/LGM_2.5m/CCSM/Sudam",
                            pattern = "*.asc$",full.names = TRUE)
paths_LGMmiroc <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/LGM_2.5m/MIROC/Sudam",
                             pattern = "*.asc$",full.names = TRUE)
paths_LGMmpi <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/LGM_2.5m/MPI/Sudam",
                           pattern = "*.asc$",full.names = TRUE)
paths_LIG <- list.files(path = "D:/BackUp-Ale/Capas/Variables_climaticas/WorldClim_1.4/LIG_2.5m",
                           pattern = "*.asc$",full.names = TRUE)
#Suelo
paths_Soil <- list.files(path = "D:/MEGAsync/Modelado/Capa_ambientales_cortadas/WorldClim1.4/WorldClim5_Soil_AreaMpaper/LGMccsm/Soil_correctas_final",
                        pattern = "*.asc$",full.names = TRUE)
paths_SoilLGM <- list.files(path = "D:/MEGAsync/Modelado/Capa_ambientales_cortadas/WorldClim1.4/WorldClim5_Soil_AreaMpaper/LGMccsm/Soil_LGM",
                         pattern = "*.asc$",full.names = TRUE)
###### Genero raster stack de cada set de variables#####
Actual <- raster::stack(paths_Actual)
HTMccsm <- raster::stack(paths_HTMccsm)
HTMmiroc <- raster::stack(paths_HTMmiroc)
HTMmpi <- raster::stack(paths_HTMmpi)
LGMccsm <- raster::stack(paths_LGMccsm)
LGMmiroc <- raster::stack(paths_LGMmiroc)
LGMmpi <- raster::stack(paths_LGMmpi)
LIG <- raster::stack(paths_LIG)
Soil <- raster::stack(paths_Soil)
SoilLGM <- raster::stack(paths_SoilLGM)

##Determinar si todas las capas climaticas tienen las mismas res, dim y crs?####
dim(Actual) == dim(HTMccsm)
dim(Actual) == dim(HTMmiroc)
dim(Actual) == dim(HTMmpi)
dim(Actual) == dim(LGMccsm)
dim(Actual) == dim(LGMmiroc)
dim(Actual) == dim(LGMmpi)
dim(Actual) == dim(LIG)

res(Actual) == res(HTMccsm)
res(Actual) == res(HTMmiroc)
res(Actual) == res(HTMmpi)
res(Actual) == res(LGMccsm)
res(Actual) == res(LGMmiroc)
res(Actual) == res(LGMmpi)
res(Actual) == res(LIG)

###Si lo tienen, usar Actual para resamplear stack de suelos ####
SoilResampled <- resample(Soil, Actual, method="bilinear" )#ver si anda la resolucion
plot(SoilResampled)
res(HTMccsm) == res(SoilResampled)
res(Actual)
res(SoilResampled)

SoilLGMResampled <- resample(SoilLGM, LGMccsm, method="bilinear" )#ver si anda la resolucion
plot(SoilLGMResampled)
res(HTMccsm) == res(SoilResampled)
res(Actual)
res(SoilResampled)


LIGResampled <- resample(LIG, Actual, method="bilinear" )#ver si anda la resolucion
plot(LIGResampled)
res(LIGResampled) == res(SoilResampled)
res(Actual)
res(SoilResampled)

### Genero un stack con todas las variables?? Darles CRS? #####
crs(Actual) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(HTMccsm) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(HTMmiroc) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(HTMmpi) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(LGMccsm) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(LGMmiroc) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(LGMmpi) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(LIGResampled) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(SoilResampled) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(SoilLGMResampled) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

Todos <- c(Actual, HTMccsm, HTMmiroc, HTMmpi, LGMccsm, LGMmiroc, LGMmpi, LIGResampled, SoilResampled, SoilLGMResampled)
StackAll <- raster::stack(Todos)

### Cargar mascara de Area M #######
Mask <- shapefile("D:/MEGAsync/Modelado/Areas M/AreaM_paper.shp")
plot(Mask)

### Cropear ####
StackAllCropped <- raster::crop(x = StackAll,y = Mask)
plot(StackAllCropped)
##Si no funca el stack de todos
ActualCropped <- raster::crop(x = Actual,y = Mask)
plot(ActualCropped)
HTMccsmCropped <- raster::crop(x = HTMccsm,y = Mask)
plot(HTMccsmCropped)
HTMmirocCropped <- raster::crop(x = HTMmiroc,y = Mask)
plot(HTMmirocCropped)
HTMmpiCropped <- raster::crop(x = HTMmpi,y = Mask)
plot(HTMmpiCropped)
LGMccsmCropped <- raster::crop(x = LGMccsm,y = Mask)
plot(LGMccsmCropped)
LGMmirocCropped <- raster::crop(x = LGMmiroc,y = Mask)
plot(LGMmirocCropped)
LGMmpiCropped <- raster::crop(x = LGMmpi,y = Mask)
plot(LGMmpiCropped)
LIGCropped <- raster::crop(x = LIGResampled,y = Mask)
plot(LIGCropped)
SoilCropped <- raster::crop(x = SoilResampled,y = Mask)
plot(SoilCropped)

### Maskear ####
StackAllMasked <- raster::mask(StackAllCropped,Mask)
plot(StackAllMasked)
##Si no funca el stack de todos
ActualMasked <- raster::mask(ActualCropped,Mask)
plot(ActualMasked)
HTMccsmMasked <- raster::mask(HTMccsmCropped,Mask)
plot(HTMccsmMasked)
HTMmirocMasked <- raster::mask(HTMmirocCropped,Mask)
plot(HTMmirocMasked)
HTMmpiMasked <- raster::mask(x = HTMmpiCropped,y = Mask)
plot(HTMmpiMasked)
LGMccsmMasked <- raster::mask(x = LGMccsmCropped,y = Mask)
plot(LGMccsmMasked)
LGMmirocMasked <- raster::mask(x = LGMmirocCropped,y = Mask)
plot(LGMmirocMasked)
LGMmpiMasked <- raster::mask(x = LGMmpiCropped,y = Mask)
plot(LGMmpiMasked)
LIGMasked <- raster::mask(x = LIGCropped,y = Mask)
plot(LIGMasked)
SoilMasked <- raster::mask(x = SoilCropped,y = Mask)
plot(SoilMasked)

### Unstack stack All#####


### Guardar capas de salida #####
dir.create("Todas")
#Creo los rasters en mi carpeta
lapply(names(StackAllMasked), function(x){
  writeRaster(StackAllMasked[[x]], paste0("Todas/",
                                      x,".asc"),overwrite=TRUE)})
