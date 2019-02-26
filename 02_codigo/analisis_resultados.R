### Paquetes ----
library(pacman)
p_load(cowplot, janitor, lubridate, readxl, sf, tidyverse)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())
