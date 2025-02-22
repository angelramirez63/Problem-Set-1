# Estadísticas descriptivas ----------------------------------------------------
rm(list = ls())

## 0. Preparación --------------------------------------------------------------

### Librerías 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, dplyr)


### Crear el directorio 
wd <- here()
setwd(wd)
rm(wd)

## Cargar datos
db <- readRDS("stores/datos_modelos.rds") %>% 
  as_tibble()

## Estadísticas descriptivas ---------------------------------------------------
#Visualización: de las distribuciones de las variables de ingreso 

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

# Calcular el salario por hora promedio por edad
df_summary <- db_limpia %>%
  group_by(age) %>%
  summarise(salario_promedio = mean(y_ingLab_m_ha, na.rm = TRUE))

# Crear el gráfico
ggplot(df_summary, aes(x = age, y = salario_promedio)) +
  geom_line(color = "blue", size = 0.7) +  # Línea azul
  geom_point(color = "darkblue", size = 1.5) +  # Puntos rojos
  labs(
    title = "Salario por hora promedio por edad",
    x = "Edad",
    y = "Salario por hora"
  ) +
  theme_minimal()  # Estilo limpio y profesional
