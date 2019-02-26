### Paquetes ----
library(pacman)
p_load(cowplot, janitor, lubridate, readxl, sf, tidyverse)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())


### Importar datos ----

# Resultados de la consulta
resultados <- read_csv("01_datos/resultado_mesas.csv")

# Listado Nominal al 30 de septiembre de 2018
ln <- read_excel("01_datos/EM_30_Sep.xlsx", range = "A1:E2492")

### Importar shapefiles de municipios y estados ----

# Shapefile de municipios en 2018 - Fuente: INE
mpos_shp <- st_read("01_datos/shp/mpos/mpos_ine_2018.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(.,  crs = 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')

# Shapefile de estados en 2018 - Fuente: INE
edos_shp <- st_read("01_datos/shp/edos/edos_ine_2018.shp", stringsAsFactors = FALSE, quiet = TRUE)  %>% 
  st_transform(., 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')