# assignment 4
# giovanni martello
# prof Ricardo Dahis


# limpando o environment
rm(list = ls())

# bibliotecas necessarias
pacman::p_load(terra, spData, geobr, ggplot2, tidyverse)

# tema para os graficos
tema = theme(panel.background = element_rect(fill = "white"),
             panel.grid = element_line(color = "grey95"),
             legend.key = element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 9),
             )


# instalacao geobr
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
# library(geobr)

#diretorio
path = "C:\\Users\\Giovanni Machado\\assignment-4"

# importando os dados 
# arquivo tif de cobertura vegetal
my_rast = rast(file.path(path,"brasil_coverage_2020.tif"))

# arquivos de municipalidades
mun = read_municipality(year =2020)
rio_de_janeiro_nv = mun %>% filter(abbrev_state == "RJ") # formato sf
rio_de_janeiro = vect(rio_de_janeiro_nv) # formato spatvector

## crop and mask
cr = terra::crop(my_rast, rio_de_janeiro)
ms = terra::mask(cr, rio_de_janeiro)

## extract
ex = terra::extract(ms, rio_de_janeiro)

# consolidando por municipio no estado do rio de janeiro
# total de pixels
total = ex %>% 
  group_by(ID) %>% summarise(pixels_total = n())

# pixels que representam floresta
forest = ex %>% 
  filter(brasil_coverage_2020 %in% c(3,4,5,49)) %>% 
  group_by(ID) %>%
  summarise(forest_pixel = n())

# percentual de pixel florestal por municipio
mun_florest = 
  total %>% left_join(forest, by = "ID") %>% 
  mutate(percentual = forest_pixel/pixels_total)

# voltando para o shapefile

rio_de_janeiro_nv = 
  rio_de_janeiro_nv %>% 
  mutate(ID = 1:92) %>% 
  inner_join(mun_florest, by = "ID")

# mapa
ggplot(rio_de_janeiro_nv) +
  geom_sf(aes(fill = percentual), color = NA) +
  labs(title = "Cobertura Vegetal Percentual",
       subtitle = "por municipio do estado do Rio de Janeiro",
       x = NULL, y = NULL)+
  tema + scale_fill_viridis_c(name = "Cobertura \n Vegetal")

ggsave("cobertura_vegetal_rj.pdf", path = path, plot = last_plot())






