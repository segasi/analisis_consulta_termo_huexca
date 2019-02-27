### Paquetes ----
library(pacman)
p_load(cowplot, janitor, lubridate, readxl, sf, tidyverse)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

## Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family = "Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family = "Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family = "Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family = "Didact Gothic Regular"))


### Importar shapefiles de municipios y estados ----

# Shapefile de municipios en 2018 - Fuente: INE
mpos_shp <- st_read("01_datos/shp/mpos/mpos_ine_2018.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(.,  crs = 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')

# Shapefile de estados en 2018 - Fuente: INE
edos_shp <- st_read("01_datos/shp/edos/edos_ine_2018.shp", stringsAsFactors = FALSE, quiet = TRUE)  %>% 
  st_transform(., 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')

### Preparar shapefiles para análisis ----

# "Limpiar" nombres en shapefiles ----
mpos_shp <- clean_names(mpos_shp)
edos_shp  <- clean_names(edos_shp) 

# Generar clave municipal ----
mpos_shp <- 
  mpos_shp %>% 
  mutate(cve_edo = str_pad(entidad, 2, pad = "0"),
         cve_mpo = str_pad(municipio, 3, pad = "0"),
         cve_edo_mpo = paste(cve_edo, cve_mpo, sep = ""))


### Importar datos ----

# Resultados de la consulta
resultados <- read_csv("01_datos/resultado_mesas.csv")

# Listado Nominal al 30 de septiembre de 2018
ln <- read_excel("01_datos/EM_30_Sep.xlsx", range = "A1:E2492")


### Preparar bases de datos para análisis ----

# Transformar tipo de variable de dia_ejercicio ----
resultados <-
  resultados %>% 
  mutate(dia_ejercicio = dmy(dia_ejercicio))

# Generar columna con clave geográfica para municipios de acuerdo con la clasificación del INE ----
resultados <- 
  resultados %>% 
  mutate(cve_edo = str_pad(entidad, 2, pad = "0"),
         cve_mpo = str_pad(municipio, 3, pad = "0"),
         cve_edo_mpo = paste(cve_edo, cve_mpo, sep = ""))

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

# "Limpiar" nombres de variables ----
ln <- 
  ln %>% 
  clean_names()

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
         municipio = municipio.x,
         cve_edo = cve_edo.x,
         cve_mpo = cve_mpo.x) %>% 
  select(idcasilla, entidad, estado, id_municipio, municipio, cve_edo, cve_mpo, cve_edo_mpo, casilla, everything())                  # Reordenar variables


# Verificar que nombres de municipios sean iguales
bd %>% 
  select(municipio, nombre_del_municipio, cve_edo_mpo) %>% 
  mutate(municipio = str_trim(municipio),
         nombre_del_municipio = str_trim(nombre_del_municipio), 
         iguales = ifelse(municipio == nombre_del_municipio, 1, 0)) %>% 
  arrange(iguales) %>% 
  print(n = Inf)

# Nota: en 14 renglones los nombres de los municipios de la base de datos de resultados no coinciden con los de la base de datos del INE. Esto se debe a los siguientes motivos: (i) en la base de datos de resultados usan el nombre "TLALTIZAPAN" y en la del INE "TLALTIZAPAN DE ZAPATA"; (ii) en la base de datos de resultados usan el nombre "ZACUALPAN" y en la del INE "ZACUALPAN DE AMILPAS"; (iii) en la base de datos incluyen los municipios de Coatetelco, Xoxocotla y Hueyapan, y en la base de datos del INE estos municipios pertenecen a Miactlán, Puente de Ixtla y Tetela del Volcán, respectivamente


### Unir bd con shapefile de mpos y generar nuevo objeto llamado bd_shp ----
bd_shp <- 
  bd %>% 
  group_by(cve_edo_mpo) %>% 
  summarise(estado = last(estado),
            municipio = last(municipio),
            total_si = sum(votos_si),
            total_no = sum(votos_no),
            total_mpo = sum(total),
            ln_mpo = mean(lista_nominal)) %>% 
  ungroup() %>% 
  mutate(por_si = round((total_si/total_mpo)*100, 1),
         por_no = round((total_no/total_mpo)*100, 1),
         por_part = round((total_mpo/ln_mpo)*100, 1)) %>%
  left_join(mpos_shp, by = "cve_edo_mpo") %>% 
  st_as_sf() 


### Calcular totales y porcentajes es por municipio ----
bd_mpo <- 
  bd %>% 
  group_by(cve_edo_mpo) %>% 
  summarise(estado = last(estado),
            municipio = last(municipio),
            total_si = sum(votos_si),
            total_no = sum(votos_no),
            total_mpo = sum(total),
            ln_mpo = mean(lista_nominal)) %>% 
  ungroup() %>% 
  mutate(por_si = round((total_si/total_mpo)*100, 1),
         por_no = round((total_no/total_mpo)*100, 1),
         por_part = round((total_mpo/ln_mpo)*100, 1))

### Gráfica: Número de muncipios en los que la participación ciudadana fue ____ ----
bd_mpo %>% 
  mutate(nivel_part = case_when(por_part < 2.5 ~ "Menor a 2.5%",
                                por_part >= 2.5 & por_part < 5 ~ "Entre 2.5% y 4.9%",
                                por_part >= 5 & por_part < 7.5 ~ "Entre 5% y 7.4%",
                                por_part >= 7.5 & por_part < 10 ~ "Entre 7.5% y 9.9%",
                                por_part >= 10 & por_part < 12.5 ~ "Entre 10% y 12.5%")) %>% 
  count(nivel_part, sort = T)  %>% 
  mutate(nivel_part = fct_relevel(nivel_part, "Menor a 2.5%", "Entre 2.5% y 4.9%", "Entre 5% y 7.4%", "Entre 7.5% y 9.9%", "Entre 10% y 12.5%")) %>% 
  ggplot(aes(nivel_part, n)) +
  geom_col(fill = "#a50300", alpha = 0.9) +
  geom_text(aes(label = n), family = "Didact Gothic Regular", fontface = "bold", size = 8, vjust = 1.8, color = "white") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("número de muncipios en los que la participación ciudadana fue ..."), width = 70),
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Segob e INE\n\nNota: tres de los 60 municipios considerados en el ejercicio de participación son de reciente creación, por lo que para el INE su Lista Nominal sigue siendo\nparte de la del municipio que integraban antes. Por ello, la gráfica solo reporta datos de 57 municipios.") +
  tema +
  theme(panel.grid = element_blank(), 
        plot.caption = element_text(margin = margin(10, 0, 0, 0)),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_blank(),
        axis.title = element_blank()) +
  ggsave(filename = "numero_mpos_por_rango_participacion.png", path = "03_graficas/", width = 14, height = 9, dpi = 200)

### Gráfica: % de participación en los mpos. en los que se instaló al menos una casilla en la consulta de la Termoeléctrica Huexca
bd_mpo %>% 
  mutate(acronimo_edo = case_when(estado == "MORELOS" ~ "Mor.",
                                  estado == "PUEBLA" ~ "Pue.",
                                  estado == "TLAXCALA" ~ "Tlax."),
         etiqueta_mpo = paste(municipio, " (", acronimo_edo, ")", sep = "")) %>% 
  ggplot(aes(fct_reorder(str_to_title(etiqueta_mpo), por_part), por_part)) +
  geom_col(fill = "#a50300") +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 2.5)) +
  coord_flip() +
  labs(title = str_wrap(str_to_upper("porcentaje de participación en los municipios en los que se instaló al menos una casilla en el ejercicio participativo de la Termoeléctrica Huexca"), width = 55),
       x = NULL,
       y = "\n% de participación ",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Segob e INE") +
  tema +
  theme(plot.title = element_text(size = 24),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "por_participacion_por_mpo.png", path = "03_graficas/", width = 14, height = 17, dpi = 200)


### Mapa: Porcentaje de ciudadanos que votaron "sí" en el ejercicio participativo de la Termoeléctrica Huexca ----
bd_shp %>% 
  ggplot() +
  geom_sf(data = edos_shp, color = "grey40", fill = "grey93", size = 0.5) +
  geom_sf(aes(fill = por_si), color = "grey70") +
  geom_sf(data = edos_shp, color = "grey40", fill = NA, size = 0.5) +
  annotate("point", x = -98.879267, y = 18.801463, colour = "steelblue", size = 3) +
  coord_sf(xlim = c(-99.7, -97.8), 
           ylim = c(18.3, 19.7),
           datum = NA) +
  scale_fill_gradient(low = "white", high = "tomato", breaks = c(4, 46, 95.9), limits = c(0, 95.9), labels = c("4% (min)", "46", "96% (máx.)")) +
  labs(title = str_wrap(str_to_upper("porcentaje de ciudadanos que votaron \"sí\" en el ejercicio participativo de la Termoeléctrica Huexca"), width = 60),
       fill = "% que votó \"Sí\"\n",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Segob e INE") +
  theme_void() +
  theme(plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), color = "grey20", family = "Trebuchet MS Bold"),
        plot.caption = element_text(size = 20, face = "bold", color = "grey20", family = "Didact Gothic Regular", hjust = 0),
        plot.background = element_rect(fill = "grey93"),
        panel.grid = element_blank(), 
        legend.position = c(0.85, 0.2),
        legend.title = element_text(face = "bold", size = 20, family = "Trebuchet MS Bold"),
        legend.text = element_text(face = "bold", size = 18, family = "Didact Gothic Regular")) +
  ggsave(filename = "mapa_porcentaje_si_por_mpo.png", path = "03_graficas/", width = 12.7, height = 11.67, dpi = 200)


### Mapa: porcentaje de participación municipal en la consulta -----
bd_shp %>% 
  ggplot() +
  geom_sf(data = edos_shp, color = "grey40", fill = "grey93", size = 0.5) +
  geom_sf(aes(fill = por_part), color = "grey70") +
  geom_sf(data = edos_shp, color = "grey40", fill = NA, size = 0.5) +
  annotate("point", x = -98.879267, y = 18.801463, colour = "steelblue", size = 3) +
  coord_sf(xlim = c(-99.7, -97.8), 
           ylim = c(18.2, 19.7),
           datum = NA) +
  scale_fill_gradient(low = "white", high = "tomato", breaks = c(0, 6, 12.1), limits = c(0, 12.1),  labels = c("0% (min.)", "6%", "12.1% (máx.)")) +
  labs(title = str_wrap(str_to_upper("porcentaje de participación municipal en el ejercicio participativo de la Termoeléctrica Huexca"), width = 60),
       fill = "% de participación\n", 
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuentes: Segob e INE") +
  theme_void() +
  theme(plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), color = "grey20", family = "Trebuchet MS Bold"),
        plot.caption = element_text(size = 20, face = "bold", color = "grey20", family = "Didact Gothic Regular", hjust = 0),
        plot.background = element_rect(fill = "grey93"),
        panel.grid = element_blank(), 
        legend.position = c(0.85, 0.2),
        legend.title = element_text(face = "bold", size = 20, family = "Trebuchet MS Bold"),
        legend.text = element_text(face = "bold", size = 18, family = "Didact Gothic Regular")) +
  ggsave(filename = "mapa_porcentaje_participacion_por_mpo.png", path = "03_graficas/", width = 11.73, height = 11.67, dpi = 200)
