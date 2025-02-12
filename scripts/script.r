#### PUNTO 1 ###

#### PUNTO 2 ###

  #Limpiar
rm(list = ls())
cat("\014")

  #Cargar paquetes
require(pacman)
p_load(rio, tidyverse, dplyr skimr, here, gridExtra, corrplot, stargazer, MASS, rvest)

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

  #Observar la cantidad de missing values de cada variable
  missing_values<-colSums(is.na(db))
missing_tab<-data.frame(
  Miss_val=missing_values
  )
missing_tab

  # Eliminar variables donde todos los valores son NA
db <- db %>%
  select_if(~ !all(is.na(.)))

####Se imputarán las variables categóricas agrupados por si el individuo está empleado y por su estrato###

    # Función para calcular la moda (el valor más frecuente)
calcular_moda <- function(x) {
  tabla <- table(x)
  moda <- names(tabla)[which.max(tabla)]
  return(moda)
}

    #Se reemplazan los valores de las variables categóricas por la moda agrupada si está empleado y su estrado
db_limpio <- db %>%
  group_by(dsi, estrato1) %>%
  mutate(
    p6090 = ifelse(is.na(p6090) | p6090 == 9, as.numeric(calcular_moda(p6090)), p6090),
    p6100 = ifelse(is.na(p6100) | p6100 == 9, as.numeric(calcular_moda(p6100)), p6100),
    p6210 = ifelse(is.na(p6210) | p6210 == 9, as.numeric(calcular_moda(p6210)), p6210),
    p6510 = ifelse(is.na(p6510) | p6510 == 9, as.numeric(calcular_moda(p6510)), p6510),
    p6545 = ifelse(is.na(p6545) | p6545 == 9, as.numeric(calcular_moda(p6545)), p6545),
    p6580 = ifelse(is.na(p6580) | p6580 == 9, as.numeric(calcular_moda(p6580)), p6580),
    p6585s1 = ifelse(is.na(p6585s1) | p6585s1 == 9, as.numeric(calcular_moda(p6585s1)), p6585s1),
    p6585s2 = ifelse(is.na(p6585s2) | p6585s2 == 9, as.numeric(calcular_moda(p6585s2)), p6585s2),
    p6585s3 = ifelse(is.na(p6585s3) | p6585s3 == 9, as.numeric(calcular_moda(p6585s3)), p6585s3),
    p6585s4 = ifelse(is.na(p6585s4) | p6585s4 == 9, as.numeric(calcular_moda(p6585s4)), p6585s4),
    p6590 = ifelse(is.na(p6590) | p6590 == 9, as.numeric(calcular_moda(p6590)), p6590),
    p6600 = ifelse(is.na(p6600) | p6600 == 9, as.numeric(calcular_moda(p6600)), p6600),
    p6610 = ifelse(is.na(p6610) | p6610 == 9, as.numeric(calcular_moda(p6610)), p6610),
    p6620 = ifelse(is.na(p6620) | p6620 == 9, as.numeric(calcular_moda(p6620)), p6620),
    p7500s1 = ifelse(is.na(p7500s1) | p7500s1 == 9, as.numeric(calcular_moda(p7500s1)), p7500s1),
    p7500s2 = ifelse(is.na(p7500s2) | p7500s2 == 9, as.numeric(calcular_moda(p7500s2)), p7500s2),
    p7500s3 = ifelse(is.na(p7500s3) | p7500s3 == 9, as.numeric(calcular_moda(p7500s3)), p7500s3),
    p7510s1 = ifelse(is.na(p7510s1) | p7510s1 == 9, as.numeric(calcular_moda(p7510s1)), p7510s1),
    p7510s2 = ifelse(is.na(p7510s2) | p7510s2 == 9, as.numeric(calcular_moda(p7510s2)), p7510s2),
    p7510s3 = ifelse(is.na(p7510s3) | p7510s3 == 9, as.numeric(calcular_moda(p7510s3)), p7510s3),
    p7510s5 = ifelse(is.na(p7510s5) | p7510s5 == 9, as.numeric(calcular_moda(p7510s5)), p7510s5),
    p7510s6 = ifelse(is.na(p7510s6) | p7510s6 == 9, as.numeric(calcular_moda(p7510s6)), p7510s6),
    p7510s7 = ifelse(is.na(p7510s7) | p7510s7 == 9, as.numeric(calcular_moda(p7510s7)), p7510s7)
  ) %>%
  ungroup()

    #Ahora realizamos lo mismo, pero con las variables categóricas faltantes
db_limpio <- db %>%
  group_by(dsi, estrato1) %>%
  mutate(
    p6100 = ifelse(is.na(p6100), calcular_moda(p6100), p6100),
    oficio = ifelse(is.na(oficio), calcular_moda(oficio), oficio),
    relab = ifelse(is.na(relab), calcular_moda(relab), relab),
    p6510s2 = ifelse(is.na(p6510s2), calcular_moda(p6510s2), p6510s2),
    p6545s2 = ifelse(is.na(p6545s2), calcular_moda(p6545s2), p6545s2),
    p6580s2 = ifelse(is.na(p6580s2), calcular_moda(p6580s2), p6580s2),
    p6585s1a2 = ifelse(is.na(p6585s1a2), calcular_moda(p6585s1a2), p6585s1a2),
    p6585s2a2 = ifelse(is.na(p6585s2a2), calcular_moda(p6585s2a2), p6585s2a2),
    p6585s3a2 = ifelse(is.na(p6585s3a2), calcular_moda(p6585s3a2), p6585s3a2),
    p6630s1 = ifelse(is.na(p6630s1), calcular_moda(p6630s1), p6630s1),
    p6630s2 = ifelse(is.na(p6630s2), calcular_moda(p6630s2), p6630s2),
    p6630s3 = ifelse(is.na(p6630s3), calcular_moda(p6630s3), p6630s3),
    p6630s4 = ifelse(is.na(p6630s4), calcular_moda(p6630s4), p6630s4),
    p6630s6 = ifelse(is.na(p6630s6), calcular_moda(p6630s6), p6630s6),
    p6870 = ifelse(is.na(p6870), calcular_moda(p6870), p6870),
    p6920 = ifelse(is.na(p6920), calcular_moda(p6920), p6920),
    p7040 = ifelse(is.na(p7040), calcular_moda(p7040), p7040),
    p7050 = ifelse(is.na(p7050), calcular_moda(p7050), p7050),
    p7090 = ifelse(is.na(p7090), calcular_moda(p7090), p7090),
    p7110 = ifelse(is.na(p7110), calcular_moda(p7110), p7110),
    p7120 = ifelse(is.na(p7120), calcular_moda(p7120), p7120),
    p7140s1 = ifelse(is.na(p7140s1), calcular_moda(p7140s1), p7140s1),
    p7140s2 = ifelse(is.na(p7140s2), calcular_moda(p7140s2), p7140s2),
    p7150 = ifelse(is.na(p7150), calcular_moda(p7150), p7150),
    p7160 = ifelse(is.na(p7160), calcular_moda(p7160), p7160),
    p7472 = ifelse(is.na(p7472), calcular_moda(p7472), p7472),
    ina = ifelse(is.na(ina), calcular_moda(ina), ina),
    cclasnr2 = ifelse(is.na(cclasnr2), calcular_moda(cclasnr2), cclasnr2),
    cclasnr3 = ifelse(is.na(cclasnr3), calcular_moda(cclasnr3), cclasnr3),
    cclasnr4 = ifelse(is.na(cclasnr4), calcular_moda(cclasnr4), cclasnr4),
    cclasnr6 = ifelse(is.na(cclasnr6), calcular_moda(cclasnr6), cclasnr6),
    cclasnr7 = ifelse(is.na(cclasnr7), calcular_moda(cclasnr7), cclasnr7),
    cclasnr8 = ifelse(is.na(cclasnr8), calcular_moda(cclasnr8), cclasnr8),
    cclasnr11 = ifelse(is.na(cclasnr11), calcular_moda(cclasnr11), cclasnr11),
    maxEducLevel = ifelse(is.na(maxEducLevel), as.numeric(calcular_moda(maxEducLevel)), maxEducLevel),
    regSalud = ifelse(is.na(regSalud), calcular_moda(regSalud), regSalud),
    cotPension = ifelse(is.na(cotPension), calcular_moda(cotPension), cotPension),
    formal = ifelse(is.na(formal), calcular_moda(formal), formal),
    informal = ifelse(is.na(informal), calcular_moda(informal), informal),
    microEmpresa = ifelse(is.na(microEmpresa), calcular_moda(microEmpresa), microEmpresa),
    sizeFirm = ifelse(is.na(sizeFirm), calcular_moda(sizeFirm), sizeFirm)
  ) %>%
  ungroup()



##########################################################################################################################

  #Imputar valores faltantes de variables categóricas de interés
  # Por el momento tenemos edad, máximo nivel educativo, formalidad del empleo, ingreso por horas extra, ingreso total, zona urbana o rural, y total de horas trabajadas la semana previa
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

#Para el ingreso total, no hay missing values
is.na(db$ingtot) %>% table()

#Para el ingreso por horas extra
db <- db %>%  rename(ingextra = p6510s1)
is.na(db$ingextra) %>% table()
db <- db  %>%
  mutate(ingextra = ifelse(is.na(ingextra) == TRUE, median(db$ingextra, na.rm = TRUE) , ingextra))

#Ahora se limpiarán outliers haciendo uso del cuartil 0,975 y 0,025

#Para ingreso total
low <- quantile(db$ingtot, 0.025, na.rm = TRUE)  # Percentil 2.5
up <- quantile(db$ingtot, 0.975, na.rm = TRUE)   # Percentil 97.5

# Truncar los valores de ingtot
db <- db %>%
  mutate(ingtot = ifelse(ingtot <= low, low, 
                             ifelse(ingtot >= up, up, ingtot)))





#### PUNTO 3 ###

#### PUNTO 4 ###

#### PUNTO 5 ###
