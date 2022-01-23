library(raster)
library(tmap)
library(tmaptools)
library(rworldxtra)
library(sf)
library(tidyverse)

# Cargo los datos para hacer el mapa -------------------------------------------
# Puntos de presencia
Occs_QGIS <- read_delim("Datos_Presencia/Final_Paper_TolyMat/Occs_QGIS.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
names(Occs_QGIS) 
class(Occs_QGIS)
# Coversion a simple feature object
Occs_sf = sf::st_as_sf(Occs_QGIS, coords = c("Longitud", "Latitud"), crs = 4326)
# Filtro registros actuales
Occs_Current = Occs_sf %>% filter(Temporalidad == "Actual")
# Filtro registros historico
Occs_Historico = Occs_sf %>% filter(Temporalidad == "Historico")
# Filtro registros holocenicos
Occs_Holoceno = Occs_sf %>% filter(Temporalidad == "Holoceno")
# Filtro registros historico
Occs_Pleistoceno = Occs_sf %>% filter(Temporalidad == "Pleistoceno")



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
  mutate(Province_1 = gsub(" province", "",Province_1)) %>% 
  st_transform(crs = 4326)
# Linea de costa del LGM
LGMcoast_geo = st_read("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_Sudam/lgm_LINEA_DE_COSTA/LGMcoastline_RayAdams_2001.gpkg")

# Area M
AreaM = st_read("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/Areas M/AreaM_final.gpkg")

# Mapa con registros actuales
OccCurrent_Mapa =  tm_shape(Bioregions, bbox = AreaM) + tm_polygons(col = "Province_1", 
                                                                    palette = get_brewer_pal("Greys", n = 7, contrast = c(0.15, 0.5), plot = F),
                                                                    legend.show = F)+
  tm_shape(Occs_Current) +  tm_symbols(size = 0.1, shape = 21,col = "black", alpha = 1) +
  tm_shape(AreaM) + tm_borders(col = "yellow", lwd = 2)+
  tm_shape(Bioregion_geo) + tm_text("Province_1", size = 0.5)+
  tm_layout(title = "Current",
            title.position = c("left", "top"),
            legend.position = c(0.7,0.05))
OccCurrent_Mapa

# Mapa con registros pasados
OccHistorico_Mapa =  tm_shape(Bioregions, bbox = AreaM) + tm_polygons(col = "Province_1", 
                                                                 palette = get_brewer_pal("Greys", n = 7, contrast = c(0.15, 0.6), plot = F),
                                                                 legend.show = F)+
  tm_shape(Occs_Historico) +  tm_symbols(col = "black", size = 0.1, shape = 21) +
  tm_shape(AreaM) + tm_borders(col = "yellow", lwd = 2)+
  tm_shape(Bioregion_geo) + tm_text("Province_1", size = 0.5)+
  tm_layout(title = "Historic",
            title.position = c("left", "top"),
            legend.position = c(0.7,0.05))
OccHistorico_Mapa

# Mapa con registros del holoceno
OccHoloceno_Mapa =  tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey") + 
  tm_shape(Occs_Holoceno) +  tm_symbols(col = "black", size = 0.1, shape = 21) +
  tm_shape(AreaM) + tm_borders(col = "yellow", lwd = 2)+
  tm_layout(title = "Holocene",
            title.position = c("left", "top"),
            legend.position = c(0.7,0.05))
OccHoloceno_Mapa

# Mapa con registros del Pleistoceno
OccPleistoceno_Mapa =  tm_shape(Sudam, bbox = AreaM) + tm_polygons(col = "lightgrey") + 
  tm_shape(Occs_Pleistoceno) +  tm_symbols(col = "black", size = 0.1, shape = 21) +
  tm_shape(AreaM) + tm_borders(col = "yellow", lwd = 2)+
  tm_layout(title = "Pleistocene",
            title.position = c("left", "top"),
            legend.position = c(0.7,0.05)) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar()
OccPleistoceno_Mapa

# Maps con ocurrencias
OccsMap = tmap_arrange(OccPleistoceno_Mapa, OccHoloceno_Mapa, OccHistorico_Mapa, OccCurrent_Mapa, 
                       nrow = 1, asp = NULL)

OccsMap

# Guardo el mapa vectorizado para darle los retoques finales en Inkscape
help("tmap_save")
tmap_save(OccsMap, "1_OccsMap.pdf")
