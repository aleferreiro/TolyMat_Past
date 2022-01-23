# Análisis de variables limitantes (MLF)

## MESS Y MoD plots

# Paquetes y funciones a usar ----------------------------------------------------------- 
library(raster)
library(tmap)
library(tmaptools)
library(rworldxtra)
library(sf)
library(tidyverse)

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
AreaM = st_read("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/Areas M/AreaM_final.gpkg")


# 0. Working directory ----------------------------------------------------
setwd("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/MLF")

# 1. Resultados MLF --------------------------------------------------------
# Nicho actual
MLF_Current_niche_Actual = raster("TolyMat_Final/MLF_CurrentNiche_Presente.asc")
MLF_Current_niche_HTM = raster("TolyMat_Final/MLF_CurrentNiche_HTM.asc")
MLF_Current_niche_LGM = raster("TolyMat_Final/MLF_CurrentNiche_LGM.asc")
MLF_Current_niche_LGMb = crop(MLF_Current_niche_LGM, AreaM)
MLF_Current_niche_LGMb = mask(MLF_Current_niche_LGMb, AreaM)
MLF_Current_niche_LIG = raster("TolyMat_Final/MLF_CurrentNiche_LIG.asc")
# Nicho glacial
MLF_Glacial_niche_Actual = raster("TolyMat_Final/MLF_GlacialNiche_Actual.asc")
MLF_Glacial_niche_HTM = raster("TolyMat_Final/MLF_GlacialNiche_HTM.asc")
MLF_Glacial_niche_LGM = raster("TolyMat_Final/MLF_GlacialNiche_LGMccsm.asc")
MLF_Glacial_niche_LGMb = crop(MLF_Glacial_niche_LGM, AreaM)
MLF_Glacial_niche_LGMb = mask(MLF_Glacial_niche_LGMb, AreaM)
MLF_Glacial_niche_LIG = raster("TolyMat_Final/MLF_GlacialNiche_LIG.asc")
# Nicho multitemp
MLF_multitemp_niche_Actual = raster("TolyMat_Final/MLF_multitempNiche_Actual2.asc")
MLF_multitemp_niche_HTM = raster("TolyMat_Final/MLF_multitempNiche_HTM2.asc")
MLF_multitemp_niche_LGM = raster("TolyMat_Final/MLF_multitempNiche_LGM2.asc")
MLF_multitemp_niche_LGMb = crop(MLF_multitemp_niche_LGM, AreaM)
MLF_multitemp_niche_LGMb = mask(MLF_multitemp_niche_LGMb, AreaM)
MLF_multitemp_niche_LIG = raster("TolyMat_Final/MLF_multitempNiche_LIG2.asc")

# como para mejorar el script podria stackearlos a todos y crear una funcion con for
# para que me mapee todos los elementos del stack

# 2. Mapas de MLF -------------------------------------------------------------

# 2.1. Mapeo Nicho actual -----------------------------------------------------
# Actualidad
MLF_CurrentNiche_Actual_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Current_niche_Actual) + tm_raster(style = "cat",
                                              labels = c("Bio1","Bio10","Bio13","Bio15","Bio4","arcilla","crfvol","arena","limo"),
                                              palette = c("#E41A1C","#FF7F00","#377EB8","#08306b","#238b45","#A65628","#D4529A","#FFFF33","#004529"),
                                              title= "Variables",
                                              legend.format =list(text.separator="-"),
                                              legend.is.portrait = F) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "",
            title.position = c("left", "top"),
            legend.width = 2,
            legend.height = 0.1,
            legend.text.size = 6,
            legend.outside = F,
            legend.only = T) 
# HTM
MLF_CurrentNiche_HTM_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Current_niche_HTM) + tm_raster(style = "cat",
                                                 labels = c("Bio1","Bio10","Bio13","Bio15","Bio4","arcilla","crfvol","arena","limo"),
                                                 palette = c("#E41A1C","#FF7F00","#377EB8","#08306b","#238b45","#A65628","#D4529A","#FFFF33","#004529"),
                                                 title= "Variables",
                                                 legend.format =list(text.separator="-"),
                                                 legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "OCH",
            title.position = c("left", "top"),
            legend.width = 1,
            legend.text.size = 10,
            legend.show = F) 

# LGM
MLF_CurrentNiche_LGM_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Current_niche_LGMb) + tm_raster(style = "cat",
                                              labels = c("Bio1","Bio13","Bio15","Bio4","arcilla","crfvol","limo"),
                                              palette = c("#E41A1C","#377EB8","#08306b","#238b45","#A65628","#D4529A","#004529"),
                                              title= "",
                                              legend.format =list(text.separator="-"),
                                              legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "UMG",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 
# LIG
MLF_CurrentNiche_LIG_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Current_niche_LIG) + tm_raster(style = "cat",
                                              labels = c("Bio1","Bio15","Bio4","arcilla","crfvol","limo"),
                                              palette = c("#E41A1C","#08306b","#238b45","#A65628","#D4529A","#004529"),
                                              title= "Variables",
                                              legend.format =list(text.separator="-"),
                                              legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "UIG",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 

MLF_CurrentNiche_Map = tmap_arrange(MLF_CurrentNiche_LIG_Map,
                                    MLF_CurrentNiche_LGM_Map,
                                    MLF_CurrentNiche_HTM_Map,
                                    MLF_CurrentNiche_Actual_Map,
                                    nrow = 1, asp = NULL)
MLF_CurrentNiche_Map
# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(MLF_CurrentNiche_Map, "MLF_CurrentNiche_Map.pdf")


# 2.2. Mapeo MLF de nicho glaciar -----------------------------------------

# Actualidad
MLF_GlacialNiche_Actual_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Glacial_niche_Actual) + tm_raster(style = "cat",
                                                 labels = c("Bio1","Bio13","Bio4","arcilla","crfvol"),
                                                 palette = c("#E41A1C","#377EB8","#238b45","#A65628","#D4529A"),
                                                 title= "Variables",
                                                 legend.format =list(text.separator="-"),
                                                 legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "Actualidad",
            title.position = c("left", "top"),
            legend.position = c(0.65,0.05)) 
# HTM
MLF_GlacialNiche_HTM_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Glacial_niche_HTM) + tm_raster(style = "cat",
                                              labels = c("Bio1","Bio13","Bio4","arcilla","crfvol"),
                                              palette = c("#E41A1C","#377EB8","#238b45","#A65628","#D4529A"),
                                              title= "Variables",
                                              legend.format =list(text.separator="-"),
                                              legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "OCH",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 

# LGM
MLF_GlacialNiche_LGM_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Glacial_niche_LGMb) + tm_raster(style = "cat",
                                               labels = c("Bio1","Bio10","Bio13","Bio4","arcilla","crfvol"),
                                               palette = c("#E41A1C","#FF7F00","#377EB8","#238b45","#A65628","#D4529A"),
                                               legend.format =list(text.separator="-"),
                                               legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "UMG",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 
# LIG
MLF_GlacialNiche_LIG_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Glacial_niche_LIG) + tm_raster(style = "cat",
                                              labels = c("Bio1","Bio13","Bio4","arcilla","crfvol"),
                                              palette =c("#E41A1C","#377EB8","#238b45","#A65628","#D4529A"),
                                              title= "Variables",
                                              legend.format =list(text.separator="-"),
                                              legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "UIG",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 

MLF_GlacialNiche_Map = tmap_arrange(MLF_GlacialNiche_LIG_Map,
                                    MLF_GlacialNiche_LGM_Map,
                                    MLF_GlacialNiche_HTM_Map,
                                    MLF_GlacialNiche_Actual_Map,
                                    nrow = 1, asp = NULL)
MLF_GlacialNiche_Map
# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(MLF_GlacialNiche_Map, "MLF_GlacialNiche_Map.pdf")


# 2.3. Mapeo MLF de nicho multitemporal -----------------------------------------

# Actualidad
MLF_multitempNiche_Actual_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_multitemp_niche_Actual) + tm_raster(style = "cat",
                                                   labels = c("Bio1","Bio10","Bio13","Bio15","Bio4","arcilla","crfvol","arena","limo"),
                                                   palette = c("#E41A1C","#FF7F00","#377EB8","#08306b","#238b45","#A65628","#D4529A","#FFFF33","#004529"),
                                                   title= "",
                                                   legend.format =list(text.separator="-"),
                                                   legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "Actualidad",
            title.position = c("left", "top"),
            legend.position = c(0.65,0.05)) 
# HTM
MLF_multitempNiche_HTM_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_multitemp_niche_HTM) + tm_raster(style = "cat",
                                                labels = c("Bio1","Bio10","Bio13","Bio15","Bio4","arcilla","crfvol","arena","limo"),
                                                palette = c("#E41A1C","#FF7F00","#377EB8","#08306b","#238b45","#A65628","#D4529A","#FFFF33","#004529"),
                                                title= "",
                                                legend.format =list(text.separator="-"),
                                                legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "OCH",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 

# LGM
MLF_multitempNiche_LGM_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_multitemp_niche_LGMb) + tm_raster(style = "cat",
                                               labels = c("Bio1","Bio10","Bio13","Bio4","arcilla","crfvol","limo"),
                                               palette = c("#E41A1C","#FF7F00","#377EB8","#238b45","#A65628","#D4529A","#004529"),
                                               legend.format =list(text.separator="-"),
                                               legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "UMG",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 
# LIG
MLF_multitempNiche_LIG_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_multitemp_niche_LIG) + tm_raster(style = "cat",
                                              labels = c("Bio1","Bio10","Bio13","Bio4"),
                                              palette =c("#E41A1C","#FF7F00","#377EB8","#238b45"),
                                              title= "Variables",
                                              legend.format =list(text.separator="-"),
                                              legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "UIG",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05),
            legend.show = F) 

MLF_multitempNiche_Map = tmap_arrange(MLF_multitempNiche_LIG_Map,
                                    MLF_multitempNiche_LGM_Map,
                                    MLF_multitempNiche_HTM_Map,
                                    MLF_multitempNiche_Actual_Map,
                                    nrow = 1, asp = NULL)
MLF_multitempNiche_Map

# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(MLF_multitempNiche_Map, "MLF_multitempNiche_Map.pdf")


# 3. Leyenda --------------------------------------------------------------
Leyenda = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MLF_Current_niche_Actual) + tm_raster(style = "cat",
                                                 labels = c("Bio1","Bio10","Bio13","Bio15","Bio4","arcilla","crfvol","arena","limo"),
                                                 palette = c("#E41A1C","#FF7F00","#377EB8","#08306b","#238b45","#A65628","#D4529A","#FFFF33","#004529"),
                                                 title= "",
                                                 legend.format =list(text.separator="-"),
                                                 legend.is.portrait = F) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "",
            title.position = c("left", "top"),
            legend.width = 0.1,
            legend.height = 0.01,
            legend.text.size = 1,
            legend.outside = F,
            legend.only = T) 

tmap_save(Leyenda, "MLF_Leyenda.pdf")
