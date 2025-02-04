##########################################################
# Taller 1
##########################################################

#### PUNTO 1 ###

#### PUNTO 2 ###

#Limpiar
rm(list = ls())
cat("\014")

#Cargar paquetes
require(pacman)
p_load(rio, tidyverse, skimr, here, gridExtra, corrplot, stargazer, MASS, rvest)

#Directorio
wd<-here()
setwd(wd)
rm(wd)

#Importar base
db <- readRDS("stores/datos_GEIH.rds") %>% 
  as_tibble()

#Limpiar a población objetivo
db<- db %>% 
  filter(age > 18 & dsi==0)

#Crear variable categórica para género
db<- db %>% 
  mutate(gender=ifelse(sex==1,0,1))


#### PUNTO 3 ###

#### PUNTO 4 ###

#### PUNTO 5 ###
