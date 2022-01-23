# Paquetes y funciones necesarias----------------------------------------------------------- 
library(kuenm)
library(raster)
library(tmap)
library(tmaptools)
library(rworldxtra)
library(sf)
library(tidyverse)
library(ntbox)

# Uso funcion que robe de "https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/"
# pero la modifico para primero calcular el valor de thrshold que quiero:
# Esta funcion tiene para calcular tres tipos de thrEsholds: MTP, P5 Y P10.
thresh = function(sdm, occs, type = "mtp"){
  occPredVals <- raster::extract(sdm, occs) # Estraigo valores de idoneidad de presencias
  if(type == "mtp"){
    thresh <- min(na.omit(occPredVals))
  } else if(type == "p5"){
    if(length(occPredVals) < 10){
      p5 <- floor(length(occPredVals) * 0.95)
    } else {
      p5 <- ceiling(length(occPredVals) * 0.95)
    }
    thresh <- rev(sort(occPredVals))[p5]
  } else if(type == "p10"){
    if(length(occPredVals) < 10){
      p10 <- floor(length(occPredVals) * 0.9)
    } else {
      p10 <- ceiling(length(occPredVals) * 0.9)
    }
    thresh <- rev(sort(occPredVals))[p10]
  }
  return(thresh)
}

# Funcion para binarizar los mapas de acuerdo al umbral calculado previamente
sdm_threshold <- function(sdm, Umbral, binary = FALSE){
  sdm_thresh <- sdm
  sdm_thresh[sdm_thresh < Umbral] <- NA
  if(binary){
    sdm_thresh[sdm_thresh >= Umbral] <- 1
  }
  return(sdm_thresh)
}

# Capas geograficas -------------------------------------------------------
# Limites provincias argentina
Prov_Arg = st_read("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_Sudam/provincia/provincia.shp")
# Limites paises sudamerica
Sudam = spData::world %>% dplyr::filter(continent == "South America")
# Cargo mapa de bioregiones de sudamerica de Morrone 2014
Bioregions = st_read("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_biologicas/BioRegions_Morrone2014/Lowenberg_Neto_2014.shp")
Bioregions_geo = st_transform(Bioregions,crs = 4326)
Bioregion_geo = Bioregions %>%  
  filter(Province_1=="Chacoan province" | Province_1=="Monte province" | Province_1=="Pampean province" 
         | Province_1=="Yungas province" | Province_1=="Cerrado province" |Province_1=="Parana Forest province" ) %>%
  st_transform(crs = 4326)
# Linea de costa del LGM
LGMcoast_geo = st_read("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_Sudam/lgm_LINEA_DE_COSTA/LGMcoastline_RayAdams_2001.gpkg")

# Area M
AreaM = st_read("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/Areas M/AreaM_paper2.shp")
# Ocurrencias usadas para la calibracion
Occs_Current = read.csv("1_Current_joint.csv")
Occs_Glacial = read.csv ("2_Glacial_joint.csv")

# 0. Working directory ----------------------------------------------------
setwd("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm")

# 1. Modelos de Current Niche y sus proyecciones -------------------------------------

# 1.1. Model calibration --------------------------------------------------
## candidate model creation 
help(kuenm_cal)

oj_complex <- "1_Current_joint.csv"
otr_complex <- "1_Current_train.csv"
mvars <- "1_Current_M_variables"
bcal <- "batch_cal1"
candir <- "1_Current_Candidate_models"
regm <- c(0.1, 0.25, 0.5, 1, 2, 5,8,10)
f_class = c("l","q","lq","lp","lqp") #Saco product solo y "qp" siguiendo Merow,2013
mxpath <- "C:/Maxent"

kuenm_cal(occ.joint = oj_complex, occ.tra = otr_complex, M.var.dir = mvars, 
          batch = bcal, out.dir = candir, max.memory = 1000, 
          reg.mult = regm, f.clas = f_class, args = NULL, 
          maxent.path = mxpath, wait = FALSE, run = TRUE)

## evaluation of candidate models 
help(kuenm_ceval)

ote_complex <- "1_Current_test.csv"
cresdir <- "1_Current_Calibration_results_b"

kuenm_ceval(path = candir, occ.joint = oj_complex, occ.tra = ote_complex, 
            occ.test = ote_complex, batch = bcal, out.eval = cresdir,
            threshold = 5, rand.percent = 50, iterations = 500,
            kept = F, selection = "OR_AICc", parallel.proc = FALSE)

# 1.2. Model projections --------------------------------------------------
help(kuenm_mod)

bfmod <- "1_Current_batch_model"
moddir <- "1_Current_Final_models"
gvars <- "G_variables"

kuenm_mod(occ.joint = oj_complex, M.var.dir = mvars, out.eval = cresdir, 
          batch = bfmod, rep.n = 10, rep.type = "Bootstrap",
          jackknife = TRUE, out.dir = moddir, max.memory = 1000, 
          out.format = "cloglog", project = TRUE, G.var.dir = gvars, 
          ext.type = "ext_clam", write.mess = TRUE, write.clamp = FALSE, 
          maxent.path = mxpath, args = NULL, wait = FALSE, run = TRUE)


# 1.3. Summary of results -------------------------------------------------
## descriptive statistics of results
help(kuenm_modstats)
#ESTA FUNCION ES UTIL SI OBTENGO MAS DE UN MODELO AL FINAL DE LA CALIBRACION!
spname <- "Tolypeutes_matacus"
modstats <- "1_Current_Final_Model_Stats"
proj_scenarios = c("Actual", "HTM", "LGM","LIG")

kuenm_modstats(sp.name = spname, fmod.dir = moddir, format = "asc", 
               project = T, statistics = c("med"), replicated = T,
               proj.scenarios = proj_scenarios, 
               ext.type = "EC", out.dir = modstats)


# 1.4. Umbralizar mapas de Distribucion Potencial ----------------------------------------- 
## Genero un grafico con mapas de presencia ausencia de la especie (y ver si 
## agrego idoneidad dentro de las presencias)
# Calculo Umbral para binarizar mapas 
# Cargo proyeccion del modelo sobre variables de calibracio, y ocurrencias de calibracion
DistribucionCurrent1 = raster("1_Current_Final_Model_Stats/Statistics_EC/Actual_med.tif")
OccsCurrent_SoloCoords = Occs_Current[,-1]

# Calculo el umbral que quiero
Current_Umbral_p5 = thresh(DistribucionCurrent1,OccsCurrent_SoloCoords, type = "p5")


DistribucionCurrent1_bin = sdm_threshold(DistribucionCurrent1, Umbral = Current_Umbral_p5, binary = F)

DistribucionHTM1 = raster("1_Current_Final_Model_Stats/Statistics_EC/HTM_med.tif")
DistribucionHTM1_bin = sdm_threshold(DistribucionHTM1, Umbral = Current_Umbral_p5, binary = F)

DistribucionLGM1 = raster("1_Current_Final_Model_Stats/Statistics_EC/LGM_med.tif")
DistribucionLGM1_bin = sdm_threshold(DistribucionLGM1, Umbral = Current_Umbral_p5, binary = F)

DistribucionLIG1 = raster("1_Current_Final_Model_Stats/Statistics_EC/LIG_med.tif")
DistribucionLIG1_bin = sdm_threshold(DistribucionLIG1, Umbral = Current_Umbral_p5, binary = F)

# Mapeo
Occs_Current_sf = Occs_Current %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326)


# 1.5. Mapas de distribucion potencial ------------------------------------
CurrentNiche_DistActual_Mapa = tm_shape(Bioregions, bbox = AreaM) + tm_polygons(col = "Province_1", 
                                                                                palette = get_brewer_pal("Greys", n = 7, contrast = c(0.15, 0.5), plot = F),
                                                                                legend.show = F) +
  tm_shape(DistribucionCurrent1_bin) + tm_raster(style = "cont",
                                                palette =get_brewer_pal("Greens", contrast = c(0.4, 0.95), plot = F),
                                                title= "Suitability",
                                                legend.format =list(text.separator="-"),
                                                legend.reverse = T) +
  tm_shape(Bioregions, bbox = AreaM) + tm_borders()+
  tm_layout(title = "Current",
            title.position = c("left", "top"),
            legend.position = c(0.7,0.05))
CurrentNiche_DistActual_Mapa
# Cuando haga los otros graficos no debo incluir los registros de presencia ni la leyenda
CurrentNiche_DistHTM_Mapa = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(DistribucionHTM1_bin) + tm_raster(style = "cont",
                                           palette =get_brewer_pal("Greens", contrast = c(0.4, 0.95), plot = F),
                                           title= "Suitability",
                                           legend.format =list(text.separator="-"),
                                           legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "HTM",
            title.position = c("left", "top"),
            legend.show = F)

CurrentNiche_DistLGM_Mapa =   tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+ 
      tm_shape(DistribucionLGM1_bin) + tm_raster(style = "cont",
                                            palette =get_brewer_pal("Greens", contrast = c(0.4, 0.9587), plot = F),
                                            title= "Suitability",
                                            legend.format =list(text.separator="-"),
                                            legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "LGM",
            title.position = c("left", "top"),
            legend.show = F)

CurrentNiche_DistLIG_Mapa = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(DistribucionLIG1_bin) + tm_raster(style = "cont",
                                            palette=get_brewer_pal("Greens", contrast = c(0.4, 0.95), plot = F),
                                            title= "Suitability",
                                            legend.format =list(text.separator="-"),
                                            legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "LIG",
            title.position = c("left", "top"),
            legend.show = F) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar()

CurrentNiche_Dist_Maps = tmap_arrange(CurrentNiche_DistLIG_Mapa,
                                      CurrentNiche_DistLGM_Mapa,
                                      CurrentNiche_DistHTM_Mapa,
                                      CurrentNiche_DistActual_Mapa,
                                      nrow = 1, asp = NULL)
CurrentNiche_Dist_Maps
# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(CurrentNiche_Dist_Maps, "CurrentNiche_Maps2.pdf")


# 2. Modelos de Current Niche y sus proyecciones -------------------------------------

# 2.1. Model calibration --------------------------------------------------
## candidate model creation 
help(kuenm_cal)

oj_complex <- "2_Glacial_joint.csv"
otr_complex <- "2_Glacial_train.csv"
mvars <- "2_Glacial_M_variables"
bcal <- "batch_cal1"
candir <- "2_Glacial_Candidate_models"
regm <- c(0.1, 0.25, 0.5, 1, 2, 5,8,10)
f_class = c("l","q","lq","lp","lqp") #Saco product solo y "qp" siguiendo Merow,2013
mxpath <- "C:/Maxent"

kuenm_cal(occ.joint = oj_complex, occ.tra = otr_complex, M.var.dir = mvars, 
          batch = bcal, out.dir = candir, max.memory = 1000, 
          reg.mult = regm, f.clas = f_class, args = NULL, 
          maxent.path = mxpath, wait = FALSE, run = TRUE)

## evaluation of candidate models 
help(kuenm_ceval)

ote_complex <- "2_Glacial_test.csv"
cresdir <- "2_Glacial_Calibration_results"

kuenm_ceval(path = candir, occ.joint = oj_complex, occ.tra = ote_complex, 
            occ.test = ote_complex, batch = bcal, out.eval = cresdir,
            threshold = 5, rand.percent = 50, iterations = 500,
            kept = F, selection = "OR_AICc", parallel.proc = FALSE)

# 2.2. Model projections --------------------------------------------------
help(kuenm_mod)

bfmod <- "2_Glacial_batch_model"
moddir <- "2_Glacial_Final_models"
gvars <- "G_variables"

kuenm_mod(occ.joint = oj_complex, M.var.dir = mvars, out.eval = cresdir, 
          batch = bfmod, rep.n = 10, rep.type = "Bootstrap",
          jackknife = TRUE, out.dir = moddir, max.memory = 1000, 
          out.format = "cloglog", project = TRUE, G.var.dir = gvars, 
          ext.type = "ext_clam", write.mess = TRUE, write.clamp = FALSE, 
          maxent.path = mxpath, args = NULL, wait = FALSE, run = TRUE)


# 2.3. Summary of results -------------------------------------------------
## descriptive statistics of results
help(kuenm_modstats)
#ESTA FUNCION ES UTIL SI OBTENGO MAS DE UN MODELO AL FINAL DE LA CALIBRACION!
spname <- "Tolypeutes_matacus"
modstats <- "2_Glacial_Final_Model_Stats"
proj_scenarios = c("Actual", "HTM", "LGM","LIG")

kuenm_modstats(sp.name = spname, fmod.dir = moddir, format = "asc", 
               project = T, statistics = c("med"), replicated = T,
               proj.scenarios = proj_scenarios, 
               ext.type = "EC", out.dir = modstats)


# 2.4. Umbralizar mapas de Distribucion Potencial ----------------------------------------- 
## Genero un grafico con mapas de presencia ausencia de la especie (y ver si 
## agrego idoneidad dentro de las presencias)
# Calculo Umbral para binarizar mapas 
# Cargo proyeccion del modelo sobre variables de calibracio, y ocurrencias de calibracion
DistribucionLGM2 = raster("2_Glacial_Final_Model_Stats/Statistics_EC/LGM_med.tif")
OccsGlacial_SoloCoords = Occs_Glacial[,-1]

# Calculo el umbral que quiero
Glacial_Umbral_p5 = thresh(DistribucionLGM2,OccsGlacial_SoloCoords, type = "p5")

DistribucionLGM2_bin = sdm_threshold(DistribucionLGM2, Umbral = Glacial_Umbral_p5, binary = F)

DistribucionHTM2 = raster("2_Glacial_Final_Model_Stats/Statistics_EC/HTM_med.tif")
DistribucionHTM2_bin = sdm_threshold(DistribucionHTM2, Umbral = Glacial_Umbral_p5, binary = F)

DistribucionCurrent2 = raster("2_Glacial_Final_Model_Stats/Statistics_EC/Actual_med.tif")
DistribucionCurrent2_bin = sdm_threshold(DistribucionCurrent2, Umbral = Glacial_Umbral_p5, binary = F)

DistribucionLIG2 = raster("2_Glacial_Final_Model_Stats/Statistics_EC/LIG_med.tif")
DistribucionLIG2_bin = sdm_threshold(DistribucionLIG2, Umbral = Glacial_Umbral_p5, binary = F)

# Mapeo
Occs_Glacial_sf = Occs_Glacial %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326)

# 2.5. Mapas de Distribucion Potencial ------------------------------------

GlacialNiche_DistActual_Mapa = tm_shape(Bioregions, bbox = AreaM) + tm_polygons(col = "Province_1", 
                                                                                palette = get_brewer_pal("Greys", n = 7, contrast = c(0.15, 0.5), plot = F),
                                                                                legend.show = F) +
  tm_shape(DistribucionCurrent2_bin) + tm_raster(style = "cont",
                                                 palette =get_brewer_pal("Reds", contrast = c(0.4, 0.95), plot = F),
                                                 title= "Suitability",
                                                 legend.format =list(text.separator="-"),
                                                 legend.reverse = T) +
  tm_shape(Bioregions, bbox = AreaM) + tm_borders()+
  tm_layout(title = "Current",
            title.position = c("left", "top"),
            legend.position = c(0.7,0.05))
GlacialNiche_DistActual_Mapa

GlacialNiche_DistHTM_Mapa = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(DistribucionHTM2_bin) + tm_raster(style = "cont",
                                             palette =get_brewer_pal("Reds", contrast = c(0.4, 0.95), plot = F),
                                             title= "Suitability",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_layout(title = "HTM",
            title.position = c("left", "top"),
            legend.show = F)

GlacialNiche_DistLGM_Mapa =   tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+ 
  tm_shape(DistribucionLGM2_bin) + tm_raster(style = "cont",
                                             palette =get_brewer_pal("Reds", contrast = c(0.4, 0.9587), plot = F),
                                             title= "Suitability",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_layout(title = "LGM",
            title.position = c("left", "top"),
            legend.show = F)

GlacialNiche_DistLIG_Mapa = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(DistribucionLIG2_bin) + tm_raster(style = "cont",
                                             palette=get_brewer_pal("Reds", contrast = c(0.4, 0.95), plot = F),
                                             title= "Suitability",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_layout(title = "LIG",
            title.position = c("left", "top"),
            legend.show = F) 

GlacialNiche_Dist_Maps = tmap_arrange(GlacialNiche_DistLIG_Mapa,
                                      GlacialNiche_DistLGM_Mapa,
                                      GlacialNiche_DistHTM_Mapa,
                                      GlacialNiche_DistActual_Mapa,
                                      nrow = 1, asp = NULL) # agregar como titulo "Glacial niche"

GlacialNiche_Dist_Maps
# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(GlacialNiche_Dist_Maps, "GlacialNiche_Maps2.pdf")

# 3. Modelos de Current Niche y sus proyecciones -------------------------------------

# 3.1. Model calibration --------------------------------------------------
## candidate model creation and evaluation of candidate models 
help("kuenm_cal_swd")

occ_joint <- "3_MultitempSWD_joint.csv"
occ_tra <- "3_MultitempSWD_train.csv"
occ_test <- "3_MultitempSWD_test.csv"
back_dir <- "3_MultitempSWD_Background"
batch_cal <- "3_MultitempSWD_batch_cal"
out_dir <- "3_MultitempSWD_Candidate_models"
regm <- c(1, 2, 5,8,10)
f_class = c("l","q","lq","lp","lqp") #Saco product solo y "qp" siguiendo Merow,2013
arguments <-  "maximumbackground=10200"# e.g., "maximumbackground=20000" for increasing the number of pixels in the bacground or
# note that some arguments are fixed in the function and should not be changed
maxent_path <- "C:/Maxent" # path de maxent
out_eval <- "3_MultitempSWD_Calibration_results"
threshold <- 5
rand_percent <- 50
iterations <- 500
kept <- FALSE
selection <- "OR_AICc"
wait <- FALSE
run <- TRUE

kuenm_cal_swd(occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, back.dir = back_dir, 
              batch = batch_cal, out.dir.models = out_dir, reg.mult = regm, f.clas = f_class,
              max.memory = 1000, args = arguments, maxent.path = maxent_path,
              selection = selection, threshold = 5,
              rand.percent = rand_percent, iterations = iterations,
              kept = kept, out.dir.eval = out_eval)

# 3.2. Model projections --------------------------------------------------
help("kuenm_mod_swd")

batch_fin <- "3_MultitempSWD_Final_models"
mod_dir <- "3_MultitempSWD_Final_Models"
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- TRUE
out_format <- "logistic"#buscar paper que hable de que salida es mas correcta
project <- TRUE
G_var_dir <- "G_variables"
extrapolation_type <- "ext_clam" # para extrapolacion "all" PORQUE ME LO PIDE AHORA 
write_mess <- TRUE
write_clamp <- TRUE
wait1 <- FALSE
run1 <- TRUE
arguments <- "maximumbackground=10200" 

kuenm_mod_swd(occ.joint = occ_joint, back.dir = back_dir, out.eval = out_eval, batch = batch_fin, 
              rep.n = rep_n, rep.type = rep_type, jackknife = jackknife,
              max.memory = 1000, out.format = out_format,
              project = project, G.var.dir = G_var_dir, ext.type = extrapolation_type, 
              write.mess = write_mess, write.clamp = write_clamp, maxent.path = maxent_path,
              args = arguments, out.dir = mod_dir, wait = FALSE, run = TRUE)


# 3.3. Summary of results -------------------------------------------------
## descriptive statistics of results
help(kuenm_modstats_swd)

spname <- "Tolypeutes_matacus"
fmod_dir <- "3_MultitempSWD_Final_Models"
statistics <- c("med")
proj_scenarios <- c("Actual", "HTM", "LGM", "LIG")
exttype <- "EC"
modstats <- "3_MultitempSWD_Final_Model_Stats"

kuenm_modstats_swd(sp.name, fmod.dir = fmod_dir, format = "asc", statistics,
                   proj.scenarios = proj_scenarios, ext.type = exttype, out.dir = modstats)

# 3.4. Umbralizar mapas de Distribucion Potencial ----------------------------------------- 
## Genero un grafico con mapas de presencia ausencia de la especie (y ver si 
## agrego idoneidad dentro de las presencias)
# Calculo Umbral para binarizar mapas 
# Cargo proyeccion del modelo sobre variables de calibracio, y ocurrencias de calibracion
DistribucionCurrent3 = raster("3_MultitempSWD_Final_Model_Stats/Statistics_EC/Actual_med.tif")
DistribucionLGM3 = raster("3_MultitempSWD_Final_Model_Stats/Statistics_EC/LGM_med.tif")

#DEFINIR COMO CALCULO EL UMBRAL???!!!!!!
# Lo que puedo hacer es calcular el p5 con el raster de idoneidad de actual obtenido y con SOLO los registros actuales,
# y a la vez calcular otro p5 usando registros glaciares y el sdm del LGM obtenido, y luego usar el menor valor como unmbral

# Calculo el umbral que quiero
Multitime_Current_Umbral_p5 = thresh(DistribucionCurrent3,OccsCurrent_SoloCoords, type = "p5")
Multitime_Glacial_Umbral_p5 = thresh(DistribucionLGM3,OccsGlacial_SoloCoords, type = "p5")
Multitime_Umbral_p5 = max(Multitime_Current_Umbral_p5, Multitime_Glacial_Umbral_p5)

DistribucionCurrent3_bin = sdm_threshold(DistribucionCurrent3, Umbral = Multitime_Umbral_p5, binary = F)

DistribucionHTM3 = raster("3_MultitempSWD_Final_Model_Stats/Statistics_EC/HTM_med.tif")
DistribucionHTM3_bin = sdm_threshold(DistribucionHTM3, Umbral = Multitime_Umbral_p5, binary = F)

DistribucionLGM3_bin = sdm_threshold(DistribucionLGM3, Umbral = Multitime_Umbral_p5, binary = F)

DistribucionLIG3 = raster("3_MultitempSWD_Final_Model_Stats/Statistics_EC/LIG_med.tif")
DistribucionLIG3_bin = sdm_threshold(DistribucionLIG3, Umbral = Multitime_Umbral_p5, binary = F)

# 3.5. Mapas de Distribucion Potencial ------------------------------------

MultitimeNiche_DistActual_Mapa =  tm_shape(Bioregions, bbox = AreaM) + tm_polygons(col = "Province_1", 
                                                                                   palette = get_brewer_pal("Greys", n = 7, contrast = c(0.15, 0.5), plot = F),
                                                                                   legend.show = F)+
  tm_shape(DistribucionCurrent3_bin) + tm_raster(style = "cont",
                                                 palette =get_brewer_pal("Blues", contrast = c(0.4, 0.95), plot = F),
                                                 title= "Suitability",
                                                 legend.format =list(text.separator="-"),
                                                 legend.reverse = T) +
  tm_shape(Bioregions, bbox = AreaM) + tm_borders()+
  tm_layout(title = "Current",
            title.position = c("left", "top"),
            legend.position = c(0.7,0.05))
MultitimeNiche_DistActual_Mapa
# Cuando haga los otros graficos no debo icnluir los registros de presencia ni la leyenda
MultitimeNiche_DistHTM_Mapa = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(DistribucionHTM3_bin) + tm_raster(style = "cont",
                                             palette =get_brewer_pal("Blues", contrast = c(0.4, 0.95), plot = F),
                                             title= "Suitability",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "HTM",
            title.position = c("left", "top"),
            legend.show = F)

MultitimeNiche_DistLGM_Mapa = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey") + 
  tm_shape(DistribucionLGM3_bin) + tm_raster(style = "cont",
                                             palette =get_brewer_pal("Blues", contrast = c(0.4, 0.9587), plot = F),
                                             title= "Suitability",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_layout(title = "LGM",
            title.position = c("left", "top"),
            legend.show = F)

MultitimeNiche_DistLIG_Mapa = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(DistribucionLIG3_bin) + tm_raster(style = "cont",
                                             palette=get_brewer_pal("Blues", contrast = c(0.4, 0.95), plot = F),
                                             title= "Suitability",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_layout(title = "LIG",
            title.position = c("left", "top"),
            legend.show = F) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar()

MultitimeNiche_Dist_Maps = tmap_arrange(MultitimeNiche_DistLIG_Mapa,
                                      MultitimeNiche_DistLGM_Mapa,
                                      MultitimeNiche_DistHTM_Mapa,
                                      MultitimeNiche_DistActual_Mapa,
                                      nrow = 1, asp = NULL)
MultitimeNiche_Dist_Maps
# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(MultitimeNiche_Dist_Maps, "MultitimeNiche_Maps1.pdf")

