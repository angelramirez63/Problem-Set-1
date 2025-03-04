"

                      Data pre-proccessing - Taller 1
                      -------------------------------
"


"
------------------------------
0) Alistar ambiente de trabajo:
------------------------------
"

###Borrar ambiente y consola: 
rm(list = ls())
cat("\014")


###Cargar paquetes: 
if(!require(pacman)) install.packages("pacman") ; require(pacman)


p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer, # tables/output to TEX.
       here) # referenciar ubicación de archivos


###Cargar datos:

#i) here(): es una función que se usa para referenciar la ubicación de los archivos. 
#para que funcionen deben abrir el problem set-1 como proyecto de R

#ii) Definir el directorio 
wd <- here()
setwd(wd)
rm(wd)

#iii) Cargar los datos: 
db <- readRDS("stores/datos_GEIH.rds") %>% 
  as_tibble()


###Conservar indiviudos mayores de 18 años que tienen un empleo: 

#iv) la condicción para conservar la observación es:  age == edad_personas , ocu === dummy_si_la_persona esta ocupada

db <- db %>% 
  filter(age >= 18 & ocu == 1) 


"
1) Limpieza general - remover variables con más del 59% de NA o con el mismo valor:
-----------------------------------------------------------------------------------

El objetivo de esta primera parte es limpiar la mayor cantidad de variables posible que comportan 
atributos comunes. Por ejemplo, las que solo tienen missing values o tienen el mismo valor para todas las observaciones
"


#i) Variables que solo tienen missing values o que tienen el mismo valor para todas las observaciones: 
#Se removieron 21 variables que cumplen está condición
db_clean_process <- db %>% select_if(~ !all(is.na(.)) & length(unique(.))>1)

#ii) Visualización missing después de la primera limpieza e identificación de las variables que tienen
#90% de missing values: 
#Hay 40 variables que cumplen esta condición que se van a ser removidas: 
db_miss <- skim(db_clean_process) %>% 
  select( skim_variable, n_missing, complete_rate) %>% 
  arrange(-n_missing) %>% 
  mutate(Not_complete_10 = ifelse(complete_rate < 0.1, 1, 0)) 

db_clean_process <- db_clean_process %>% select(-iof3ies, -y_accidentes_m, -p6585s4a2, -y_subEducativo_m, -cclasnr3, 
                                                -isaes, -cclasnr7, -iof2es, -cclasnr11, -iof6es, -p6545s2, -y_primas_m, 
                                                -y_vivienda_m, -iof3hes, -cclasnr8, -y_otros_m, -cclasnr6, -iof1es, -cclasnr4, 
                                                -iees, -p6580s2, -y_bonificaciones_m, -y_viaticos_m, -p6585s1a2, -y_auxilioAliment_m, 
                                                -y_salarySec_m, -hoursWorkActualSecondJob, -y_primaVacaciones_m, -y_primaNavidad_m, 
                                                -p7050, -p6510s2, -y_horasExtras_m, -y_especie_m, -cclasnr2, -impaes, -p7500s1, -p7500s2, 
                                                -p7500s3, -p7110, -p7120)
skim(db_clean_process)

#iii) Visualización missing depués de la 2 limpieza e identificación variables con al menos 41% missing values: 
#hay 21 variables que cumplen está condición que van a ser removidas 
#la razón por que no use el 60% es porque hay 4 variables salariales que están completas el 59%, que son: y_salary_m, y_salary_m_hu, 
#y_ingLab_m, y_ingLab_m_ha 
db_miss  <- db_miss %>% 
  filter(Not_complete_10 == 0) %>% 
  arrange(-n_missing) %>% 
  mutate(Not_complete_59 = ifelse(complete_rate < 0.59, 1, 0)) %>%
  select(-Not_complete_10)

db_clean_process <- db_clean_process %>% select(-c(ingtotes, p6585s3a2, y_subFamiliar_m, p7140s1, p7140s2, p7150, p7160, 
                                                   p6750, y_gananciaNeta_m, y_gananciaIndep_m, y_gananciaIndep_m_hu, p6585s2a2, 
                                                   y_auxilioTransp_m, p6760, p7510s1, p7510s2, p7510s3, p7510s5, p7510s6, p7510s7, 
                                                   y_primaServicios_m))


"
2) Imputación de valores para las variables de ingreso salarial: 
---------------------------------------------------------------
"


#i) Visualización: de las distribuciones de las variables en el vecto "variables" para decidir si imputar por la+
#---------------- media o la mediana. 
#Nota: Se agrupa por estrato porque si en promedio una vivienda es la mayor fuente de riqueza y de deuda simultaneamente 
# el estrato de la vivienda que aspectos de la ubicación y la construcción de la vivienda tambien refleja el patrimonio de las personas. 
#Fuente: https://www.imf.org/en/Blogs/Articles/2024/12/04/housings-unique-role-in-lives-and-economies-demands-greater-understanding

variables <- c("y_salary_m", "y_salary_m_hu", "y_ingLab_m", "y_ingLab_m_ha", "y_total_m", "y_total_m_ha") 


for (variable in variables) {
  
  plot <- ggplot(db, aes_string(variable)) +
    geom_histogram(color = "#000000", fill = "#0099F8") +
    geom_vline(xintercept = median(db[[variable]], na.rm = TRUE), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(db[[variable]], na.rm = TRUE), linetype = "dashed", color = "blue") +  
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
























