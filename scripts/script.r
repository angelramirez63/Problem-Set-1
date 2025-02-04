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

  #Editar variable categórica para género, 1 sea igual a mujer
db<- db %>% 
  mutate(gender=ifelse(sex==1,0,1))

  #Imputar valores faltantes de variables categóricas de interés
  # Calculando la moda. Para maxEducLevel
mode_edu <- as.numeric(names(sort(table(db$maxEducLevel), decreasing = TRUE)[1]))

  # Imputar el valor faltante. 
db <- db  %>%
  mutate(maxEducLevel = ifelse(is.na(maxEducLevel) == TRUE, mode_edu , maxEducLevel))

  # Para clase, 1 si vive en zona urbana
mode_clas <- as.numeric(names(sort(table(db$clase), decreasing = TRUE)[1]))

db <- db  %>%
  mutate(clase = ifelse(is.na(clase) == TRUE, mode_clas , clase))

  # Para formal, 1 si el trabajador tiene un empleo formal
mode_for <- as.numeric(names(sort(table(db$formal), decreasing = TRUE)[1]))

db <- db  %>%
  mutate(formal = ifelse(is.na(formal) == TRUE, mode_for , formal))

#### PUNTO 3 ###

#### PUNTO 4 ###

#### PUNTO 5 ###
