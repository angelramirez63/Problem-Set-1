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
--------------------------------------------
1) Limpieza y pre procesamiento de los datos:
--------------------------------------------
"


"
1.1) Visualización previa de algunas variables - missing values:
---------------------------------------------------------------
"

db_sub1<- db %>% select( directorio, secuencia_p, orden, estrato1, sex, age, oficio, orden, totalHoursWorked,
                         dsi, ie , formal, informal, sizeFirm , regSalud, maxEducLevel, ingtot,
                         ingtotes,ingtotob, y_salary_m, y_salary_m_hu)

#i) Visualizar las variables por tipo de dato:
vis_dat(db_sub1)

#ii) Visualizar las valores que son NA: 
vis_miss(db_sub1)

#Conclusión las variables que toca imputar más son en especifico: regSalud - regimen de salud categoríca; 
#ingtotes - ingreso total imputado(valor de la compensaciones que recibe un empleado de su empleador); y_salary_m - salario mensual; 
# y_total_m - ingresos totales mensuales; y_ingLab_m_ha - ingresos salariales por hora 

#iii) Correlación entre las variables: 
db_corr <-  db_sub1 %>%  select(which(apply(db_sub1, 2, sd) > 0))
M <- cor(db_corr)
corrplot(M) 
#Los trabajadores formales tienden a trabajar en firmas más grandes 



"
1.2) Imputación de la variable salario por hora: 
------------------------------------------------
"

#i)Variables categorícas que se van usar para obtener los grupos: 

db <-  db %>%  mutate(across(c(cclasnr11, cclasnr2, cclasnr3, cclasnr4,cclasnr6, cclasnr7, 
                        cclasnr8, college, cotPension, cuentaPropia, formal, informal, 
                        microEmpresa, sex, estrato1, maxEducLevel, oficio, p6050, p6090, 
                        p6100, p6210, p6210s1, p6240, p6510, p6510s2, p6545, p6545s2, 
                        p6580, p6580s2, p6585s1, p6585s1a2, p6585s2, p6585s2a2, p6585s3, 
                        p6585s3a2, p6585s4, p6585s4a2, p6590, p6600, p6610, p6620, 
                        p6630s1, p6630s2, p6630s3,p6630s4, p6630s6, p6920, p7040, 
                        p7050, p7505, regSalud, relab, sizeFirm), as.factor))

#ii) Gráfica distribuión de las variables en la lista llamada "variables": 

variables <- c("ingtot", "ingtotes", "ingtotob", "y_salary_m", "y_salary_m_hu")

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

#Las distribuciones de las variables tienen  una cola derecha my larga por lo cual es mas apropiado usar la mediana para cada uno de los grupos

#iii) imputar los valores faltantes para la mediana por grupos de las variables de ingreso: 


for (variable in variables) {
  
     db <- db %>% 
          group_by(estrato1) %>% 
          mutate( variable = ifelse(is.na(variable) == T, median(variable, na.rm = T), variable)) %>% 
          ungroup()
}


db <- db %>% 
        group_by(estrato1) %>% 
        mutate(y_salary_m_hu = ifelse(is.na(y_salary_m_hu) == T, median(y_salary_m_hu, na.rm = T), y_salary_m_hu)) %>% 
        ungroup()



"
1.2) Missing values: 
-------------------
"


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

##Limpieza de variables - Parte 1:
##-------------------------------

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

db_miss <- db_miss %>% filter(complete_rate > 0)
db <- db %>%
      select(-c(p550,p7310,p7422, p7422s1, p7472,p7472s1, ina, imdi,  cclasnr5,imdies, iof3ies))


##Limpieza de variables - Parte 2:
##-------------------------------


#clase: identificador urbano rural, comon la encuesta se hizo en bogotá todo es urbano (rm)
#ocu: por construcción sabemos que todas las personas están ocupadas (rm)
#ina: como solo dejamos ocupados non hay inactivos por construcción (rm)
#depto: todos están en Bogotá (rm)
#informal: es redundante con la variable formal (rm)
#dominio: todos están en Bogotá (rm)
#dsi: indicador de empleado (rm) 
























