
#-----------------------  Punto 4 The Gender Earning Gap -----------------------


#Alistar ambiente de trabajo ---------------------------------------------------
rm(list = ls())
cat("\014")

#Cargar paquetes: 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(tidyverse, stargazer, here, skimr)

#Definir directorio de trabajo:  
wd <- here()
setwd(wd)
rm(wd)

#Cargar datos: 
db <- readRDS("stores/datos_modelos.rds") %>% 
  as_tibble()


#Organizar y limpiar datos antes de estimar el modelo --------------------------

##Female variable####
db <- db %>% 
      mutate(female = ifelse(sex == 0, yes = 1 , no = 0)) %>%
      select(-sex)


##Visualizar variable salario nominal por hora####

#i) Función de densidad de la variable ingreso salarial por hora: 
#La distribución tiene una cola izquierda pesada y una larga cola derecha 
density_plot_y_ingLab_m_ha <- ggplot(db, aes(x = db$y_ingLab_m_ha)) +
                              geom_density(fill = "blue", alpha = .2) + 
                              geom_line(stat = "density") + 
                              geom_vline(xintercept = median(db$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "red") +
                              geom_vline(xintercept = mean(db$y_ingLab_m_ha, na.rm = TRUE), linetype = "dashed", color = "green") + 
                              ggtitle("Densidad ingreso salarial por horas")


##ii) Outliers####
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
mean_y_ingLab_m_ha <- mean(db$y_ingLab_m_ha)
sd_y_ingLab_m_ha <- sd(db$y_total_m_ha)
lower_bound <- mean_y_ingLab_m_ha - 2*sd_y_ingLab_m_ha
upper_bound <- mean_y_ingLab_m_ha + 2*sd_y_ingLab_m_ha

#Normalizar la variable: 
db$y_ingLab_m_ha_std <- (db$y_total_m_ha - mean_y_ingLab_m_ha)/sd_y_ingLab_m_ha












