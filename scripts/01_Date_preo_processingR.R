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

"
--------------------------------------------
1) Limpieza y pre procesamiento de los datos:
--------------------------------------------
"

###Conservar indiviudos mayores de 18 años que tienen un empleo: 

#i) la condicción para conservar la observación es:  age == edad_personas , ocu === dummy_si_la_persona esta ocupada

db <- db %>% 
        filter(age >= 18 & ocu == 1) 


###Visualización missing values: 

#ii) Organizar la base de datos en orden descendente dependiendo del número de NA 

db_miss <- skim(db) %>% 
  select( skim_variable, n_missing, complete_rate) %>% 
  arrange(-n_missing)

#iii) Conservamos las variables que tienen missing values para su analísis: 

#Notas: 
#119 variables tienen al menos un NA 
#12 de las 119 variables tienen NA para todas las observaciones

db_miss <- db_miss %>%
  filter(n_missing > 0)  
head(db_miss,20)

#De las 12 variables que solo son NA: 
#P550: Es para centros poblados y Bogotá no es un centro poblado (rm)
#y_gananciaNetaAgro_m: (?) 
#P7310: Categoríca de dummy de si la persona habia trabajado antes (rm) - se puede aproximar usando la edad si se necesita
#P7350: es una pregunta para desocupados (rm)
#P7422: es una pregunta para desocupados (rm)
#P7422s1: es una pregunta complementaria a P7422 (rm)
#P7472: pregunta para desocupados (rm)
#P7472s1: pregunta complementaria a P7472 (rm)
#ina: pregunta si la persona esta inactiva y solo nos interesan los empleados (rm)
#imdi: pregunta para desocupados (rm)
#cclasnr5: pregunta para desocupados (rm)
#imdies: pregunta para desocupados (rm)
#iof3ies: para valores extremos o NA

#Remover 11 de las 12 variables: 

db_miss <- db_miss %>% filter(complete_rate >0)
db <- db %>%
      select(-c(p550,p7310,p7422, p7422s1, p7472,p7472s1, ina, imdi,  cclasnr5,imdies, iof3ies))



