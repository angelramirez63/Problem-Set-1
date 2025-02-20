
#-----------------------  Punto 4 The Gender Earning Gap -----------------------


#0) Alistar ambiente de trabajo ------------------------------------------------
rm(list = ls())
cat("\014")

#Cargar paquetes: 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(tidyverse #tidy up data
       ,stargazer #show regression results 
       ,here #make commons paths to ease co working 
       ,skimr #summary statistics 
       ,boot #bootstrapping 
       ,rio #export data 
       )

#Definir directorio de trabajo:  
wd <- here()
setwd(wd)
rm(wd)

#Cargar datos: 
db <- readRDS("stores/datos_modelos.rds") %>% 
  as_tibble()

#Establecer semilla: 
set.seed(123)


#1) Organizar y limpiar datos antes de estimar el modelo -----------------------

##1.1)Female variable####
db <- db %>% 
      mutate(female = ifelse(sex == 0, yes = 1 , no = 0)) %>%
      select(-sex)


##1.2) Visualizar variable salario nominal por hora####

#Función de densidad de la variable ingreso salarial por hora: 
#La distribución tiene una cola izquierda pesada y una larga cola derecha 
density_plot_y_ingLab_m_ha <- ggplot(db, aes(x = db$y_ingLab_m_ha)) +
                              geom_density(fill = "blue", alpha = .2) + 
                              geom_line(stat = "density") + 
                              geom_vline(xintercept = median(db$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "red") +
                              geom_vline(xintercept = mean(db$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "gray") + 
                              ggtitle("Densidad ingreso salarial por horas")


##1.3) Outliers####
#En ambos casos hay bastantes outliers. Estos podrían ser parte del procesos de generar de datos 
#Teniendo en cuenta la desigualdad que hay en Colombia, pero no quiero hacerle overfitting a esos datos extremos

#Agrupados por estrato: 
box_plot_y_ingLab_m_ha<- ggplot(db , mapping = aes(as.factor(estrato1),  y_ingLab_m_ha)) + 
                         geom_boxplot() + 
                         ggtitle("Box plot salarial por horas por estrato")
  
#Sin agrupar: 
box_plot_y_ingLab_m_ha_no_group<- ggplot(db , mapping = aes( y_ingLab_m_ha)) + 
  geom_boxplot() + 
  ggtitle("Box plot salarial")



#Regla de la desviación estándar:
low <- mean(db$y_total_m_ha) - 2* sd(db$y_ingLab_m_ha)
up <- mean(db$y_ingLab_m_ha) + 2* sd(db$y_total_m_ha)



density_plot_sigma_rule <-  ggplot(db, aes(x = db$y_ingLab_m_ha)) +
                            geom_density(fill = "blue", alpha = .2) + 
                            geom_line(stat = "density") + 
                            geom_vline(xintercept = low, linetype = "dashed", color = "red") +
                            geom_vline(xintercept = up, linetype = "dashed", color = "gray") + 
                            ggtitle("Densidad ingreso salarial por horas")


##1.4) Caracterízar outliers####

#Guardar outliers en la base de datos: 
#i) Son 613 observaciones que corresponden al 6,2% de las observaciones
db_outliers <- db %>% 
               filter(y_ingLab_m_ha < low | y_ingLab_m_ha > up )

#ii) ingreso salarial por hora: son personas con un ingreso salarial mínimo de 23917 es decir que están en la cota superior 
# de la regla de las dos desviaciones estándar 
min(db_outliers$y_total_m_ha)

#iii) Características outliers 
summary(db_outliers)
#Seguridad social: son 610 con seguridad social -> son formales 

#Educación: Superior o universitaria para 606

#Tamaño de la empresa y cuenta propia: son empleados de grandes empresas

#Género: el grupo esta balanceado entre mujeres y hombres

#Edad: La mayoría tienen entre 35 y 51 años con un promedio de 42

#Conclusión: no es aleatorio que las personas esten a dos desviaciones estándar de la media



#2) Estimar el primero modelo sin controles ------------------------------------

#Nota: por ahora voy a estimar el modelo con los outliers 

                      
modelo4a <- lm(ln_sal ~ female, data = db)
stargazer(modelo4a, type = "text")


#3) Estimar el segundo modelo con controles y FWL ------------------------------

#Literatura para pensar en los controles: https://pubs.aeaweb.org/doi/pdfplus/10.1257/jel.20160995 
#La variable de interés es female

##3.1) Modelo####
modelo4b <- lm(ln_sal ~ female + nivel_educ + age + sizeFirm + formal + horas_ocup_prin + oficio  ,data = db)
stargazer(modelo4b, type = "text", omit = "oficio")

##3.2) Estimar el modelo por partialling-out/FWL####

#(i) Regresión auxiliar de female en los controles: representa la parte de female que no esta explicada por los controles
db <- db %>% mutate(female_resid = lm(female ~ nivel_educ + age + sizeFirm + formal + horas_ocup_prin + oficio  ,data = db)$residuals)

#(ii) Regresión auxiliar de ln_sal en los controles: representa la parte de ln_sal que no esta explicada por los controles
db <- db %>% mutate(ln_sal_resid = lm(ln_sal ~ nivel_educ + age + sizeFirm + formal + horas_ocup_prin + oficio  ,data = db)$residuals) 

#(iii) regresar los residuales de la variable resultado (ii) en los residuales de la variable de interés (iii): 
modelo_4b_fwl <- lm(ln_sal_resid ~ female_resid, data = db)
stargazer(modelo4b, modelo_4b_fwl, type = "text", omit = c("oficio", "nivel_educ", "age", "sizeFirm", "formal", "horas_ocup_prin"))



##3.3) Estimar el modelo por partialling-out y los SE haciendo bootstrapping####


#(i) Función que hace la estimación del modelo por partialling-out 
#Nota: siempre por el índice igual a 2 
partialling_out <- function (data, index) {
  
  db_resid <- data.frame(row.names = 1:nrow(data))
  
  db_resid$x_resid <- lm(female ~ nivel_educ + age + sizeFirm + formal + horas_ocup_prin + oficio, data = data, subset = index )$residuals
  
  db_resid$y_resid <- lm(ln_sal ~ nivel_educ + age + sizeFirm + formal + horas_ocup_prin + oficio, data = data, subset = index)$residuals
  
  coef(lm( y_resid ~ x_resid, data = db_resid, subset = index))[2]
}

"
Nota: Tengo que comentar esta función 
"

#Prueba función partialling_out:
#partialling_out(db, 1:nrow(db))


#(ii) Bootstrapping usando el paquete boot

#R = 1000
female_boostrap_se_R1000 <-boot(db,partialling_out, R = 1000)
export(female_boostrap_se_R1000, 'stores/female_boostrap_se_R1000.rds')

#R = 10000
#Nota: Este bootstrap se demora bastante corriendo por lo que es mejor cargar los resultados guardados en stores
female_boostrap_se_R10000 <-boot(db,partialling_out, R = 10000)
export(female_boostrap_se_R10000, 'stores/female_boostrap_se_R10000.rds')



#4) Gráfico salario con la edad por género -------------------------------------







































