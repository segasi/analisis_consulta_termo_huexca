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


### Preparar bases de datos para análisis ----

# Transformar tipo de variable de dia_ejercicio ----
resultados <-
  resultados %>% 
  mutate(dia_ejercicio = dmy(dia_ejercicio))

# Generar variable con clave municipal de acuerdo con la clasificación del INE ----
resultados <- 
  resultados %>% 
  mutate(cve_edo_mpo = case_when(estado == "MORELOS" & municipio == "AMACUZAC" ~ "17001",
                                 estado == "MORELOS" & municipio == "ATLATLAHUCAN" ~ "17002",
                                 estado == "MORELOS" & municipio == "AXOCHIAPAN" ~ "17003",
                                 estado == "MORELOS" & municipio == "AYALA" ~ "17004",
                                 estado == "MORELOS" & municipio == "COATETELCO" ~ "17015", # Creado en 2017. Formaba parte de Miacatlán, por lo que asumo que la Lista Nominal de este último mpo. incluye la de Coatetelco. Fuente: https://www.proceso.com.mx/515368/coatetelco-y-colonias-forman-un-nuevo-municipio-indigena-en-morelos
                                 estado == "MORELOS" & municipio == "COATLAN DEL RIO" ~ "17005",
                                 estado == "MORELOS" & municipio == "CUAUTLA" ~ "17006",
                                 estado == "MORELOS" & municipio == "CUERNAVACA" ~ "17007",
                                 estado == "MORELOS" & municipio == "EMILIANO ZAPATA" ~ "17008",
                                 estado == "MORELOS" & municipio == "HUEYAPAN" ~ "17022", # Creado en 2017. Formaba parte de Tetela del Volcán, por lo que asumo que la Lista Nominal de este último mpo. incluye la de Hueyapan. Fuente: https://www.elsoldecuernavaca.com.mx/local/hueyapan-libre-de-las-leyes-escritas-2132788.html
                                 estado == "MORELOS" & municipio == "HUITZILAC" ~ "17009",
                                 estado == "MORELOS" & municipio == "JANTETELCO" ~ "17010",
                                 estado == "MORELOS" & municipio == "JIUTEPEC" ~ "17011",
                                 estado == "MORELOS" & municipio == "JOJUTLA" ~ "17012",
                                 estado == "MORELOS" & municipio == "JONACATEPEC" ~ "17013",
                                 estado == "MORELOS" & municipio == "MAZATEPEC" ~ "17014",
                                 estado == "MORELOS" & municipio == "MIACATLAN" ~ "17015",
                                 estado == "MORELOS" & municipio == "OCUITUCO" ~ "17016",
                                 estado == "MORELOS" & municipio == "PUENTE DE IXTLA" ~ "17017",
                                 estado == "MORELOS" & municipio == "TEMIXCO" ~ "17018",
                                 estado == "MORELOS" & municipio == "TEMOAC" ~ "17033",
                                 estado == "MORELOS" & municipio == "TEPALCINGO" ~ "17019",
                                 estado == "MORELOS" & municipio == "TEPOZTLAN" ~ "17020",
                                 estado == "MORELOS" & municipio == "TETECALA" ~ "17021",
                                 estado == "MORELOS" & municipio == "TETELA DEL VOLCAN" ~ "17022",
                                 estado == "MORELOS" & municipio == "TLALNEPANTLA" ~ "17023",
                                 estado == "MORELOS" & municipio == "TLALTIZAPAN" ~ "17024",
                                 estado == "MORELOS" & municipio == "TLAQUILTENANGO" ~ "17025",
                                 estado == "MORELOS" & municipio == "TLAYACAPAN" ~ "17026",
                                 estado == "MORELOS" & municipio == "TOTOLAPAN" ~ "17027",
                                 estado == "MORELOS" & municipio == "XOCHITEPEC" ~ "17028",
                                 estado == "MORELOS" & municipio == "XOXOCOTLA" ~ "17017", # Creado en 2017. Formaba parte de Puente de Ixtla, por lo que asumo que la Lista Nominal de este último mpo. incluye la de Xoxocotla. Fuente: https://www.proceso.com.mx/515605/morelos-decreta-creacion-del-municipio-de-xoxocotla
                                 estado == "MORELOS" & municipio == "YAUTEPEC" ~ "17029",
                                 estado == "MORELOS" & municipio == "YECAPIXTLA" ~ "17030",
                                 estado == "MORELOS" & municipio == "ZACATEPEC" ~ "17031",
                                 estado == "MORELOS" & municipio == "ZACUALPAN" ~ "17032",
                                 estado == "PUEBLA" & municipio == "ACTEOPAN" ~ "21005",
                                 estado == "PUEBLA" & municipio == "ATLIXCO" ~ "21019",
                                 estado == "PUEBLA" & municipio == "ATZITZIHUACAN" ~ "21022",
                                 estado == "PUEBLA" & municipio == "CALPAN" ~ "21026",
                                 estado == "PUEBLA" & municipio == "COHUECAN" ~ "21034",
                                 estado == "PUEBLA" & municipio == "HUAQUECHULA" ~ "21070",
                                 estado == "PUEBLA" & municipio == "HUEJOTZINGO" ~ "21076",
                                 estado == "PUEBLA" & municipio == "JUAN C. BONILLA" ~ "21092",
                                 estado == "PUEBLA" & municipio == "NEALTICAN" ~ "21103",
                                 estado == "PUEBLA" & municipio == "OCOYUCAN" ~ "21107",
                                 estado == "PUEBLA" & municipio == "SAN JERONIMO TECUANIPAN" ~ "21127",
                                 estado == "PUEBLA" & municipio == "SAN PEDRO CHOLULA" ~ "21141",
                                 estado == "PUEBLA" & municipio == "SANTA ISABEL CHOLULA" ~ "21148",
                                 estado == "PUEBLA" & municipio == "TLALTENANGO" ~ "21181",
                                 estado == "PUEBLA" & municipio == "TOCHIMILCO" ~ "21188",
                                 estado == "TLAXCALA" & municipio == "HUEYOTLIPAN" ~ "29014",
                                 estado == "TLAXCALA" & municipio == "IXTACUIXTLA DE MARIANO MATAMOROS" ~ "29015",
                                 estado == "TLAXCALA" & municipio == "NATIVITAS" ~ "29023",
                                 estado == "TLAXCALA" & municipio == "PANOTLA" ~ "29024",
                                 estado == "TLAXCALA" & municipio == "SAN DAMIAN TEXOLOC" ~ "29053",
                                 estado == "TLAXCALA" & municipio == "SAN JUAN HUACTZINCO" ~ "29047",
                                 estado == "TLAXCALA" & municipio == "SANTA APOLONIA TEACALCO" ~ "29045",
                                 estado == "TLAXCALA" & municipio == "TETLATLAHUCA" ~ "29032",
                                 estado == "TLAXCALA" & municipio == "TLAXCO" ~ "29034"))


# Eliminar renglón con total nacional de ln y cambiar su tipo a numeric ----
ln <- 
  ln %>% 
  filter(entidad != "Total nacional") %>% 
  mutate(entidad = as.numeric(entidad))

# Generar columna con clave geográfica para municipios de acuerdo con la clasificación del INE ----
ln <- 
  ln %>% 
  mutate(cve_edo = str_pad(entidad, 2, pad = "0"),
         cve_mpo = str_pad(municipio, 3, pad = "0"),
         cve_edo_mpo = paste(cve_edo, cve_mpo, sep = ""))

### Unir resultados de la consulta con datos de la Lista Nominal ----
bd <- 
  resultados %>% 
  left_join(ln, by = "cve_edo_mpo") %>% 
  rename(entidad = entidad.x,           # Cambiar un par de nombrs
         municipio = municipio.x) %>% 
  select(idcasilla, entidad, estado, id_municipio, municipio, cve_edo, cve_mpo, cve_edo_mpo, casilla, everything())                  # Reordenar variables

