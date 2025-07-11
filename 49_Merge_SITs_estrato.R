#---------------------------------------------------------------------------------
#   Proyecto: Mismatch
#
#   Nombre de script: merge_SITs_estrato
#   Autor de script:  Jorge Emilio Melendez
#
#   
#  
#---------------------------------------------------------------------------------
#   Output: x
#---------------------------------------------------------------------------------


#----------------
#    SETUP
#----------------

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

library(sf)
library(dplyr)
library(lwgeom)

#############################################

SITs <- read_sf("Data/Shapefiles/SITs_2017_cut/SITs_2017.shp")
barrios <- read_sf("Data/Shapefiles/Blocks/BARRIOS CON ESTRATO.shp")

# Validamos geometrias de barrios
barrios$geometry <- lwgeom::lwgeom_make_valid(barrios$geometry)

# nos quedamos con variables de interes de barrios
barrios <- barrios %>% dplyr::select(geometry, ESTRATO)

# Creamos id para SITs y dejamos solo id y geometría
SITs <- SITs %>% mutate(id_sit = rownames(SITs)) %>%
  dplyr::select(id_sit, geometry)

# Nos quedamos solo con barrios válidos
barrios <- barrios[st_is_valid(barrios), ]

#----------------------
#    spatial merge
#----------------------

# nos aseguramos que estén en la misma proyección
barrios <- st_transform(barrios, st_crs(SITs))
barrios <- st_as_sf(barrios)

# hacemos el join espacial
barrios_in_SITs <- st_join(barrios, SITs, join = st_within)

#--------------------------------
#   Calculo de estrato por SIT
#--------------------------------

# filtramos barrios que no están dentro de una SIT
barrios_in_SITs <- barrios_in_SITs %>% filter(!is.na(id_sit))

# función robusta de moda
mode_function <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ?????? Desactivar S2 para evitar errores con st_union() en summarise()
sf::sf_use_s2(FALSE)

# por SIT, sacamos la moda del estrato de los barrios
SIT_with_estrato <- barrios_in_SITs %>%
  group_by(id_sit) %>%
  summarise(estrato_sit = mode_function(ESTRATO)) %>%
  ungroup()

# volver a activar S2 si lo deseas
sf::sf_use_s2(TRUE)

#cambiando 
SIT_with_estrato = st_drop_geometry(SIT_with_estrato)

# unimos el resultado al shapefile original
SITs <- left_join(SITs, SIT_with_estrato, by = "id_sit")

# exportamos el shapefile con estrato
st_write(SITs, "Data/Shapefiles/SITs_with_estrato/SITs_with_estrato.shp", delete_dsn = TRUE)













