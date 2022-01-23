## MESS Y MoD plots

# Paquetes y funciones a usar ----------------------------------------------------------- 
library(kuenm)
library(raster)
library(tmap)
library(tmaptools)
library(rworldxtra)
library(sf)
library(tidyverse)
library(ntbox)

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

# Ocurrencias usadas para la calibracion
Occs_Current = read.csv("1_Current_joint.csv")
Occs_Glacial = read.csv ("2_Glacial_joint.csv")

# 0. Working directory ----------------------------------------------------
setwd("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm")

# 1. Mapa de MESS ----------------------------------------------------

MESS_CurrentNiche_LGM = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/WorldClim7_FinalOccData/1_CurrentNiche/Final_Models/M_0.1_F_lq_Set_2_EC/Tolypeutes_matacus_0_LGMccsm_novel_limiting.asc")
MESS_CurrentNiche_LGM[!(MESS_CurrentNiche_LGM == 0 | MESS_CurrentNiche_LGM == 2 | MESS_CurrentNiche_LGM == 4 )] <- NA # Saco areas sin ninguna variable no analoga
MESS_CurrentNiche_LGM[(MESS_CurrentNiche_LGM == 0 | MESS_CurrentNiche_LGM == 2 | MESS_CurrentNiche_LGM == 4 )] <- 1 
MESS_CurrentNiche_LGMb = crop(MESS_CurrentNiche_LGM, AreaM)
MESS_CurrentNiche_LGMb = mask(MESS_CurrentNiche_LGMb, AreaM)

# Mapa
MESS_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MESS_CurrentNiche_LGMb) + tm_raster(style = "cat",
                                             labels = c("Variable/s no analoga/s"),
                                             palette = c("#525252"),
                                             title= "",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "MESS",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05)) 

MESS_Map


# 2. Mapa de MoD -------------------------------------------------------------


MoD_CurrentNiche_LGM = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/WorldClim7_FinalOccData/1_CurrentNiche/Final_Models/M_0.1_F_lq_Set_2_EC/Tolypeutes_matacus_0_LGMccsm_novel_limiting.asc")
MoD_CurrentNiche_LGM[!(MoD_CurrentNiche_LGM == 0 | MoD_CurrentNiche_LGM == 2 | MoD_CurrentNiche_LGM == 4 )] <- NA # Saco areas sin ninguna variable no analoga
MoD_CurrentNiche_LGMb = crop(MoD_CurrentNiche_LGM, AreaM)
MoD_CurrentNiche_LGMb = mask(MoD_CurrentNiche_LGMb, AreaM)

MoD_Map = tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey")+
  tm_shape(MoD_CurrentNiche_LGMb) + tm_raster(style = "cat",
                                             labels = c("Bio1","Bio13","Bio4"),
                                             palette = get_brewer_pal("Set1", n = 8, plot = F ),
                                             title= "Variables",
                                             legend.format =list(text.separator="-"),
                                             legend.reverse = T) +
  tm_shape(Sudam, bbox = AreaM) + tm_borders()+
  tm_layout(title = "Variables Más Disímiles",
            title.position = c("left", "top"),
            legend.position = c(0.6,0.05)) 

MoD_Map

# 3. Mapas finales --------------------------------------------------------


MESSyMoD_Maps = tmap_arrange(MESS_Map,
                             MoD_Map,
                             nrow = 1, asp = NULL)
MESSyMoD_Maps
# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(MESSyMoD_Maps, "MESSyMoD_Maps.pdf")
