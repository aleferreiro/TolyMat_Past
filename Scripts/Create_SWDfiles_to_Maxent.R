### SAMPLE WITH DATA MAXENT ####

# HAGO EL sample.csv #####
# cargo los paquetes 
#Cargo paquetes necesarios
library("raster")
library("rasterVis")

# Genero los datos actuales -----------------------------------------------------------
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/1_Actual_RandomPart")
#Cargo el archivo de presencias actuales
OccsCurrent <- read.csv("Sp_joint.csv")
OccsCurrent$sp_name
sp_nameCurrent <- OccsCurrent[,1]
OccsCurrent2 <- OccsCurrent[,-1]
#Cargos capas ambientales actuales
paths_capas <- list.files(path = "D:/MEGAsync/Modelado/Capa_ambientales_cortadas/WorldClim1.4/WorldClim5_Soil_AreaMpaper/Actual",
                          pattern = "*.asc$",full.names = TRUE)
#Genero un stack a partir de los archivos del objeto path
StackCurrent <- raster::stack(paths_capas)

#extraigo los valores ambientales de las presencias actuales
Current_Envs <- extract(StackCurrent,OccsCurrent2)
Current_Envs
nrow(Current_Envs)
#Unos el data frame de las presencias y sus corrdenadas con sus valores ambientales
CurrentBind <- cbind(OccsCurrent, Current_Envs)
ncol(CurrentBind)
nrow(CurrentBind)

# Genero los datos pasados -----------------------------------------------------------
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/6_Pleistoceno_RandomPart/CCSM")
#Cargo el archivo de presencias fosiles
OccsPast <- read.csv("Sp_joint.csv")
sp_namePast <- OccsPast[,1]
OccsPast2 <- OccsPast[,-1]
#Cargos capas ambientales del lgm
paths_capasPast <- list.files(path = "D:/MEGAsync/Modelado/Capa_ambientales_cortadas/WorldClim1.4/WorldClim5_Soil_AreaMpaper/LGMccsm",
                          pattern = "*.asc$",full.names = TRUE)
#Genero un stack a partir de los archivos del objeto path
StackPast <- raster::stack(paths_capasPast)
#extraigo los valores ambientales de las presencias fosiles
Past_Envs <- extract(StackPast,OccsPast2)
Past_Envs
ncol(Past_Envs)
# Uno datos pasados y presentes ------------------------------------------------------
#Unos el data frame de las presencias y sus corrdenadas con sus valores ambientales
PastBind <- cbind(OccsPast, Past_Envs)
ncol(PastBind)
nrow(PastBind)
#Uno ambos archivos BIND para formar el sample.csv
TolyMatSWD<- rbind(CurrentBind, PastBind)
ncol(TolyMatSWD)
nrow(TolyMatSWD)
head(TolyMatSWD)
#Creo una carpeta para guardar el archivos
dir.create("SWD_Maxent")
write.csv(TolyMatSWD, "SWD_Maxent/TolyMatSWD_AllEnvs.csv",row.names = FALSE)

# ----------------------------------------------------
# HAGO EL background.csv #######

# Genero los datos actuales -------------------------------------------------------------
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/1_Actual_RandomPart")
#Cargos capas ambientales actuales
paths_capas <- list.files(path = "M_variables/Set_1",
                          pattern = "*.asc$",full.names = TRUE)
#Genero un stack a partir de los archivos del objeto path
StackCurrent <- raster::stack(paths_capas)

#Objeto con coord geograf yvalores ambientales
BackgroundPresent <- sampleRandom(StackCurrent,size=10000,sp=FALSE, xy=TRUE) 
plot(BackgroundPresent)
#Extraigo las coord geograficasde los puntos que corresponderan al background
BackgroundPoints <- cbind (BackgroundPresent[,1], BackgroundPresent[,2])
BackgroundPoints
nrow(BackgroundPoints)

#Le agrego al archivo con los valOres ambientales del background una columna que diga background
a <- c("background")
sp_name <- rep(a,times=10000)
CurrentBGBind <- cbind(sp_name,BackgroundPresent)
ncol(CurrentBGBind)
nrow(CurrentBGBind)
head(CurrentBGBind)
# Genero los datos pasados -----------------------------------------------------------
setwd("D:/MEGAsync/Modelado/KUENM/Tolypeutes_matacus/WorldClim5/6_Pleistoceno_RandomPart/CCSM")
#Cargos capas ambientales del lgm
paths_capasPast <- list.files(path = "M_variables/Set_1",
                              pattern = "*.asc$",full.names = TRUE)
#Genero un stack a partir de los archivos del objeto path
StackPast <- raster::stack(paths_capasPast)
#extraigo los valores ambientales del los puntos de background
PastBG_Envs <- extract(StackPast,BackgroundPoints)
PastBG_Envs
ncol(PasttBGBind)
BackgroundPast <- cbind(BackgroundPoints, PastBG_Envs)
ncol(BackgroundPast)
PastBGBind <- cbind(sp_name,BackgroundPast)
ncol(PastBGBind)

# Uno datos pasados y presentes ------------------------------------------------------
#Uno ambos archivos BIND para formar el background.csv
Background_TolyMatSWD<- rbind(CurrentBGBind, PastBGBind)
ncol(Background_TolyMatSWD)
nrow(Background_TolyMatSWD)
#Creo una carpeta para guardar el archivos
dir.create("SWD_Maxent")
write.csv(Background_TolyMatSWD, "SWD_Maxent/background_TolyMatSWD.csv", row.names = FALSE)

#Particiono los datos de ocurrencia en train y test ---------------------------------------------

