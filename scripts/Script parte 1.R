#Problem Set 1 -----------------------------------------------------------------
rm(list = ls())

#0. Preparación ----------------------------------------------------------------

# Librerías 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret)


# Crear el directorio 
wd <- here()
setwd(wd)
rm(wd)

#2.1. Obtener datos ------------------------------------------------------------


# Link para el web scrapping
link_GEIH <- "https://ignaciomsarmiento.github.io/GEIH2018_sample"

datos_GEIH <-data.frame()

for(page in 1:10) {
# Obtenemos la tabla con los datos de cada página
tabla <- read_html(paste0(link_GEIH, "/pages/geih_page_", page, ".html")) %>%
  html_table() %>%
  as.data.frame()

# Agregamos la nueva tabla a nuestra base de datos
datos_GEIH <- bind_rows(datos_GEIH, tabla)

#Registro de progreso
print(paste("Página:", page))
}

export(datos_GEIH, 'stores/datos_GEIH.rds')

#2.2. Limpieza de datos ---------------------------------------------------------

rm(list = ls())
db <- readRDS("stores/datos_GEIH.rds") %>% 
  as_tibble()

#i) la condicción para conservar la observación es:  age == edad_personas , ocu === dummy_si_la_persona esta ocupada

db_limpia <- db %>% 
  filter(wap == 1 & dsi == 1) 

#Eliminar variables de solo missings o que no tienen variación

db_limpia <- db_limpia %>% select_if(~ !all(is.na(.)) & length(unique(.))>1) %>%
  select(!directorio, !secuencia_p, !orden)

#Convertir variables categoricas en factores

db_limpia <- db_limpia %>%
  mutate(across(c(cclasnr11, cclasnr2, cclasnr3, cclasnr4,cclasnr6, cclasnr7, 
                  cclasnr8, college, cotPension, cuentaPropia, formal, informal, 
                  microEmpresa, sex, estrato1, maxEducLevel, oficio, p6050, p6090, 
                  p6100, p6210, p6210s1, p6240, p6510, p6510s2, p6545, p6545s2, 
                  p6580, p6580s2, p6585s1, p6585s1a2, p6585s2, p6585s2a2, p6585s3, 
                  p6585s3a2, p6585s4, p6585s4a2, p6590, p6600, p6610, p6620, 
                  p6630s1, p6630s2, p6630s3,p6630s4, p6630s6, p6920, p7040, 
                  p7050, p7505, regSalud, relab, sizeFirm), as.factor))


#Renombramos variables para facilitar el manejo de datos
db_limpia <- db_limpia %>% rename(parentesco_jefe = p6050, segur_social = p6090, 
                                  regim_segur_social =p6100, nivel_educ = p6210, 
                                  grad_aprob = p6210s1, actividad_prin = p6240, 
                                  tiempo_trabaj = p6426, cotiza_pens = p6920, 
                                  ingreso_laboral = p6500, horas_ocup_prin = p7040, 
                                  subempleado = p7050, tipo_contrato = p7090, 
                                  afiliado_pension = p7110, afiliado_salud = p7120, 
                                  recibe_ing_hor_ext = p6510, ingreso_hor_ext = p6510s1)

# Eliminamos las variables para las cuales más del 60% de las observaciones son faltantes
missing_percent <- colMeans(is.na(db_limpia)) * 100
db_limpia <- db_limpia[, missing_percent <= 60]
library(dplyr)
library(caret)


# Eliminamos variables altamente correlacionadas
db_numerica <- db_limpia %>%
  select(where(is.numeric)) %>%
  select(!starts_with("y_"))

# Calcular la matriz de correlación, ignorando NAs
cor_matrix <- cor(db_numerica, use = "pairwise.complete.obs")

  # Revisar si hay valores NA en la matriz de correlación
if (any(is.na(cor_matrix))) {
  print("¡Atención! Hay valores NA en la matriz de correlación.")
} else {
  # Identificar pares de variables con correlación ≥ 0.9
  highly_correlated <- which(abs(cor_matrix) >= 0.9999, arr.ind = TRUE)
  highly_correlated <- highly_correlated[highly_correlated[,1] != highly_correlated[,2], ]
  
  # Obtener nombres de variables correlacionadas
  correlated_vars <- unique(rownames(highly_correlated))
  
  # Mantener solo la primera variable de cada grupo
  vars_to_keep <- unique(highly_correlated[,1])  # Índices de variables a conservar
  vars_to_remove <- setdiff(correlated_vars, colnames(db_numerica)[vars_to_keep])  # Variables a eliminar
  
  # Filtrar la base de datos
  db_numerica_clean <- db_numerica[, !names(db_numerica) %in% vars_to_remove]
  
  # Reunir con variables no numéricas
  db_limpia <- bind_cols(db_numerica_clean, db_limpia %>% select(where(negate(is.numeric))))
  
  #Imprimimos la lista de variables que eliminamos
  print(paste("Variables eliminadas:", paste(vars_to_remove, collapse = ", ")))
  
  #Eliminamos los elementos que no vamos a necesitar después
  rm(db_numerica, db_numerica_clean, highly_correlated, correlated_vars, missing_percent,
     vars_to_keep, vars_to_remove)

}


#Imputamos missings
db_limpia <- kNN(db_limpia, k=3)
"kNN se demora mucho, entonces vale la pena hacer otras aproximaciones a la imputación
de variables primero y luego llenar las que faltan usando kNN"
missing_percent2 <- colMeans(is.na(db_limpia)) * 100

#Creamos la variable de resultado: el logarítmo natural del salario
db_limpia$ln_sal <- log(db_limpia$y_ingLab_m_ha) 



#3. Modelo de regresión lineal -------------------------------------------------
db_limpia$age_2 <- db_limpia$age^2
modelo1 <- lm(ln_sal ~ age + I(age^2), data = db_limpia)
mar <- summary(margins(modelo1))

stargazer(modelo1, type = "latex", title = "Resultados Modelo 1", out = "Views/mod1.txt", digits = 5)

