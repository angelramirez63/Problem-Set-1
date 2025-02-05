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
  # Por el momento tenemos edad, máximo nivel educativo, formalidad del empleo, zona urbana o rural, y total de horas trabajadas la semana previa
  # Calculando la moda. Para maxEducLevel
mode_edu <- as.numeric(names(sort(table(db$maxEducLevel), decreasing = TRUE)[1]))

  # Imputar el valor faltante. 
db <- db  %>%
  mutate(maxEducLevel = ifelse(is.na(maxEducLevel) == TRUE, mode_edu , maxEducLevel))

  # Para clase, 1 si vive en zona urbana. No hay missing values
is.na(db$clase) %>% table()

  # Para formal, 1 si el trabajador tiene un empleo formal
is.na(db$formal) %>% table()
mode_for <- as.numeric(names(sort(table(db$formal), decreasing = TRUE)[1]))

db <- db  %>%
  mutate(formal = ifelse(is.na(formal) == TRUE, mode_for , formal))

#Para la variable age no se encontraron missing values
is.na(db$age) %>% table()

#Para el total de horas trabajadas
is.na(db$totalHoursWorked) %>% table()
db <- db  %>%
  mutate(totalHoursWorked = ifelse(is.na(totalHoursWorked) == TRUE, median(db$totalHoursWorked, na.rm = TRUE) , totalHoursWorked))

#Ahora se limpiarán outliers haciendo uso de la desviación estándar



#### PUNTO 3 ###

#### PUNTO 4 ###

#### PUNTO 5 ###
