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

#Tabla de estadísticas descriptivas

interest_vars <- db %>% select(y_ingLab_m, y_ingLab_m_ha, ln_sal, 
                               age, female, microEmpresa, formal, 
                               totalHoursWorked, hoursWorkUsual,
                               grad_aprob, college, segundo_trabajo) %>%
  as.data.frame() %>%
  mutate(across(c(college, microEmpresa, formal, female, segundo_trabajo), 
                ~ as.integer(levels(.))[.]))

stargazer(interest_vars, summary = TRUE, type = "latex", 
          title = "Estadísticas descriptivas",
          out = "Views/desc_est.txt", digits = 3)

#Punto 3 -------------------------------------------------------------

# Modelo
modelo1 <- lm(ln_sal ~ age + I(age^2), data = db)
mar <- summary(margins(modelo1))

stargazer(modelo1, type = "latex", title = "Resultados Modelo 1", 
          out = "Views/mod1.txt", digits = 5)

# Calcular el salario por hora promedio por edad
mean_sal_age <- db %>%
  group_by(age) %>%
  summarise(salario_promedio = mean(y_ingLab_m_ha, na.rm = TRUE))

# Crear el gráfico
mean_sal_age_plot <- ggplot(mean_sal_age, aes(x = age, y = salario_promedio)) +
  geom_col(fill = "cornflowerblue", width = 0.7) +  # Barras azules
  labs(
    title = "Salario por hora promedio por edad",
    x = "Edad",
    y = "Salario por hora"
  ) +
  theme_classic() + # Fondo blanco
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")  # Centrar título y ponerlo en negrita
  )

mean_sal_age_plot

ggsave("views/salario_por_edad.png", width = 8, height = 5, dpi = 300)


mean_sal_age <- db %>%
  group_by(age, female) %>%
  summarise(salario_promedio = mean(y_ingLab_m_ha, na.rm = TRUE), .groups = "drop")

ggplot(mean_sal_age, aes(x = age, y = salario_promedio)) +
  geom_col(fill = "cornflowerblue", width = 0.7) +  
  labs(
    title = "Salario por hora promedio por edad",
    x = "Edad",
    y = "Salario por hora"
  ) +
  theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  facet_wrap(~ female, labeller = as_labeller(c(`0` = "Hombres", `1` = "Mujeres")))

ggplot(db, aes(x = age, y = ln_sal)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Línea suavizada
  labs(
    title = "Logaritmo del salario por edad",
    x = "Edad",
    y = "Logaritmo del salario"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
