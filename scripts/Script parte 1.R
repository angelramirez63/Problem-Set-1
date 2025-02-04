#Problem Set 1 -----------------------------------------------------------------
rm(list = ls())

#0. Preparación ----------------------------------------------------------------

# Librerías 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,  visdat, margins, stargazer)

# Obtener el nombre de usuario del sistema operativo
usuario <- Sys.info()["user"]

# Crear el directorio condicional dependiendo del usuario
if (usuario == "Natalia") {
  setwd("C:/Users/Natalia/OneDrive - Universidad de los Andes/Documentos/2025-1/Big Data y Machine Learning/Problem-Set-1")
} else if (usuario == "Pepito") {
  setwd("directorio de pepito") 
} else {
  setwd("")  # Fijar otro directorio
}

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

#Eliminar variables de solo missings o que no tienen variación

datos_GEIH <- datos_GEIH %>% select_if(~ !all(is.na(.)) & length(unique(.))>1)

#Seleccionamos variables
datos_GEIH <- datos_GEIH %>% select(-p6100)

#Renombramos variables para facilitar el manejo de datos
datos_GEIH<- datos_GEIH %>% rename(parentesco_jefe = p6050, segur_social = p6090,
                                   nivel_educ = p6210, grad_aprob = p6210s1,
                                   actividad_prin = p6240, tiempo_trabaj = p6426)

#Para las variables categoricas imputamos los missings con la categoría más común
# we can use skim as a dataset. 

db_miss <- skim(datos_GEIH) %>% select( skim_variable, n_missing)

#Creamos la variable de resultado: el logarítmo natural del salario
datos_GEIH$ln_sal <- log(datos_GEIH$y_ingLab_m_ha) 


#3. Modelo de regresión lineal -------------------------------------------------
datos_GEIH$age_2 <- datos_GEIH$age^2
modelo1 <- lm(ln_sal ~ age + I(age^2), data = datos_GEIH)
mar <- summary(margins(modelo1))

stargazer(modelo1, type = "latex", title = "Resultados Modelo 1", out = "Views/mod1.txt", digits = 5)

