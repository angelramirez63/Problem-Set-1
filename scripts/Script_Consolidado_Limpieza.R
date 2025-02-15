#Problem Set 1 -----------------------------------------------------------------
rm(list = ls())

#0. Preparación ----------------------------------------------------------------

# Librerías 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, dplyr)


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
  filter(wap == 1 & ocu == 1) 

"El método que usamos da lo mismo que el que usaron en la complementaria:
db_limpia2 <- db %>% filter(totalHoursWorked>0)"

#Eliminar variables de solo missings o que no tienen variación

db_limpia <- db_limpia %>% select_if(~ !all(is.na(.)) & length(unique(.))>1) %>%
  select(!directorio, !secuencia_p, !orden)

# Eliminamos las variables para las cuales más del 60% de las observaciones son faltantes
missing_percent <- colMeans(is.na(db_limpia)) * 100
db_limpia <- db_limpia[, missing_percent <= 60]

#Imputar variables categoricas con la categoría más común:

#Función para calcular la moda (el valor más frecuente)
calcular_moda <- function(x) {
  tabla <- table(x)
  moda <- names(tabla)[which.max(tabla)]
  return(moda)
}

db_limpia <- db_limpia %>%
  group_by(formal, estrato1) %>%
  mutate(
    p6100 = ifelse(is.na(p6100) | p6100 == 9, as.numeric(calcular_moda(p6100)), p6100),
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
    p6620 = ifelse(is.na(p6620) | p6620 == 9, as.numeric(calcular_moda(p6620)), p6620)
  ) %>%
  ungroup()

#Ahora realizamos lo mismo, pero con las variables categóricas faltantes
db_limpia <- db_limpia %>%
  group_by(formal, estrato1) %>%
  mutate(
    p6630s1 = ifelse(is.na(p6630s1), as.numeric(calcular_moda(p6630s1)), p6630s1),
    p6630s2 = ifelse(is.na(p6630s2), as.numeric(calcular_moda(p6630s2)), p6630s2),
    p6630s3 = ifelse(is.na(p6630s3), as.numeric(calcular_moda(p6630s3)), p6630s3),
    p6630s4 = ifelse(is.na(p6630s4), as.numeric(calcular_moda(p6630s4)), p6630s4),
    p6630s6 = ifelse(is.na(p6630s6), as.numeric(calcular_moda(p6630s6)), p6630s6),
    p6920 = ifelse(is.na(p6920), as.numeric(calcular_moda(p6920)), p6920),
    maxEducLevel = ifelse(is.na(maxEducLevel), as.numeric(calcular_moda(maxEducLevel)), maxEducLevel),
    regSalud = ifelse(is.na(regSalud), as.numeric(calcular_moda(regSalud)), regSalud),
    cotPension = ifelse(is.na(cotPension), as.numeric(calcular_moda(cotPension)), cotPension)
  ) %>%
  ungroup()

# Lista de variables a procesar. Se omitieron variables que estaban muy 
#concentrados en cero y hacian que el límite superior tuviera este mismo valor
vars <- c(
  "p6500", "p6585s2a1", "p6585s3a1", "p6590s1", "p6630s1a1", "p6630s2a1", 
  "p6630s3a1", "p6630s4a1", "p7070", "impa", "isa", "ie", "y_salary_m", "y_salary_m_hu", "y_ingLab_m",
  "y_primaServicios_m", "y_ingLab_m_ha", "y_total_m", "y_total_m_ha"
)

# Aplicar el proceso a cada variable en el loop
for (var in vars) {
  # Calcular el percentil 97.5% de la variable
  up <- quantile(db_limpia[[var]], 0.975, na.rm = TRUE)
  
  # Reemplazar valores mayores o iguales al percentil 97.5%
  db_limpia <- db_limpia %>%
    mutate(!!sym(var) := ifelse(test = (.data[[var]] >= up), 
                                yes = up, 
                                no = .data[[var]]))
}



#Convertir variables categoricas en factores

db_limpia <- db_limpia %>%
  mutate(across(c(college, cotPension, cuentaPropia, formal, informal, microEmpresa, 
                  sex, estrato1, maxEducLevel, oficio, p6050, p6090, p6100, p6210, 
                  p6210s1, p6240, p6545, p6580, p6585s1, p6585s2, p6585s3, p6585s4, 
                  p6590, p6600, p6610, p6620, p6630s1, p6630s2, p6630s3,p6630s4, 
                  p6630s6, p6920, p7040, p7505, regSalud, relab, sizeFirm), as.factor))

#Renombramos variables para facilitar el manejo de datos
db_limpia <- db_limpia %>% rename(parentesco_jefe = p6050, segur_social = p6090, 
                                  regim_segur_social =p6100, nivel_educ = p6210, 
                                  grad_aprob = p6210s1, actividad_prin = p6240, 
                                  tiempo_trabaj = p6426, cotiza_pens = p6920, 
                                  ingreso_laboral = p6500, horas_ocup_prin = p7040, 
                                  tipo_contrato = p7090, recibe_ing_hor_ext = p6510,
                                  ingreso_hor_ext = p6510s1)


#Imputamos missings
db_limpia <- kNN(db_limpia, k=3)
"kNN se demora mucho, entonces vale la pena hacer otras aproximaciones a la imputación
de variables primero y luego llenar las que faltan usando kNN"
missing_percent2 <- colMeans(is.na(db_limpia)) * 100


#Imputación variables de ingreso salarial: 

#i) Visualización: de las distribuciones de las variables en el vecto "variables" para decidir si imputar por la+
#---------------- media o la mediana. 
#Nota: Se agrupa por estrato porque si en promedio una vivienda es la mayor fuente de riqueza y de deuda simultaneamente 
# el estrato de la vivienda que aspectos de la ubicación y la construcción de la vivienda tambien refleja el patrimonio de las personas. 
#Fuente: https://www.imf.org/en/Blogs/Articles/2024/12/04/housings-unique-role-in-lives-and-economies-demands-greater-understanding

variables <- c("y_salary_m", "y_salary_m_hu", "y_ingLab_m", "y_ingLab_m_ha", "y_total_m", "y_total_m_ha") 


for (variable in variables) {
  
  plot <- ggplot(db_limpia, aes_string(variable)) +
    geom_histogram(color = "#000000", fill = "#0099F8") +
    geom_vline(xintercept = median(db_limpia[[variable]], na.rm = TRUE), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(db_limpia[[variable]], na.rm = TRUE), linetype = "dashed", color = "blue") +  
    ggtitle(paste("Distribución", as.character(variable), sep = " ")) +
    theme_classic() +
    theme(plot.title = element_text(size = 18))
  
  print(plot)
  
}

#Las distrubiciones tienen colas izquierdas pesadas y colas derechas largas. Por lo tanto, se decide imputar usando la media. 


#ii)Imputar las variables ingreso salarial: 

variables <- c("y_salary_m", "y_salary_m_hu", "y_ingLab_m", "y_ingLab_m_ha", "y_total_m", "y_total_m_ha") 

db <- db %>% 
  group_by(estrato1) %>% 
  mutate(across(all_of(variables), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>% 
  ungroup()



#Creamos la variable de resultado: el logarítmo natural del salario
db_limpia$ln_sal <- log(db_limpia$y_ingLab_m_ha) 



#3. Modelo de regresión lineal -------------------------------------------------
db_limpia$age_2 <- db_limpia$age^2
modelo1 <- lm(ln_sal ~ age + I(age^2), data = db_limpia)
mar <- summary(margins(modelo1))

stargazer(modelo1, type = "latex", title = "Resultados Modelo 1", out = "Views/mod1.txt", digits = 5)
