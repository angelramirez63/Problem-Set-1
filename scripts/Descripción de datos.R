# Estadísticas descriptivas ----------------------------------------------------
rm(list = ls())

## 0. Preparación --------------------------------------------------------------

### Librerías 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, 
       dplyr, boot)


### Crear el directorio 
wd <- here()
setwd(wd)
rm(wd)

## Cargar datos
db <- readRDS("stores/datos_modelos.rds") %>% 
  as_tibble()

## Estadísticas descriptivas ---------------------------------------------------
#Visualización: de las distribuciones de las variables de ingreso 
db <- db %>% mutate(y_ingLab_m_k =y_ingLab_m/1000)
variables <- c("y_ingLab_m_k", "y_ingLab_m_ha") 
labels <- c("Salario mensual (miles)", "Salario por hora")

# Iteramos con índice para asociar cada variable con su label correspondiente
for (i in seq_along(variables)) {
  variable <- variables[i]
  label <- labels[i]
  
  plot <- ggplot(db, aes_string(variable)) +
    geom_histogram(color = "#000000", fill = "cornflowerblue") +
    geom_vline(xintercept = median(db[[variable]], na.rm = TRUE), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(db[[variable]], na.rm = TRUE), linetype = "dashed", color = "blue") +  
    ggtitle(paste("Distribución:", label)) +
    xlab(label) + 
    ylab("Conteo") +
    theme_classic() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"))
  
  print(plot)
  ggsave(filename = paste0("views/", variable, ".png"), width = 7, height = 6, dpi = 300)
  
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
          out = "Views/desc_est.txt", digits = 2)

#Punto 3 -----------------------------------------------------------------------

## Modelo ----------------------------------------------------------------------
modelo1 <- lm(ln_sal ~ age + I(age^2), data = db)

stargazer(modelo1, type = "latex", title = "Resultados Modelo 1", 
          out = "Views/mod1.txt", digits = 3)

#Efecto marginal de la edad

mar <- summary(margins(modelo1))

#Efectio marginal en la media
mar_en_media=modelo1$coefficients[2]+2*modelo1$coefficients[3]*mean(db$age)

#Efecto marginal promedio
db <- db %>% mutate(mar_edad=modelo1$coefficients[2]+2*modelo1$coefficients[3]*age)

mar_promedio=mean(db$mar_edad)

## Hallar edad pico del salario

edad_pico_1=-modelo1$coefficients[2]/(2*modelo1$coefficients[3])


## Intervalo de confianza con bootstrap

set.seed(9500)

#Función que define el estimador que queremos obtener en el bootstrap
bootsedad <- function(data, index) { 

  modelo <- lm(ln_sal ~ age + I(age^2), db, subset = index)
  
  edad_pico=-modelo$coefficients[2]/(2*modelo$coefficients[3])
  
  return(edad_pico)
}

edad_pico_dist <- boot(data = db, bootsedad, R = nrow(db))
edad_pico_dist

ci_edad_pico <- boot.ci(boot.out = edad_pico_dist, conf = c(0.95, 0.99), type = "all")

## Visualización ---------------------------------------------------------------
## Calcular el salario por hora promedio por edad
mean_sal_age <- db %>%
  group_by(age) %>%
  summarise(salario_promedio = mean(y_ingLab_m_ha, na.rm = TRUE))

# Crear el gráfico de barras de salario promedio por edad
mean_sal_age_plot <- ggplot(mean_sal_age, aes(x = age, y = salario_promedio)) +
  geom_col(fill = "royalblue1", width = 0.7) +  # Barras azules
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

ggsave("views/salario_por_edad.png", width = 7, height = 6, dpi = 300)

#Gráfico de barras de salario promedio por edad y por género

mean_sal_age <- db %>%
  group_by(age, female) %>%
  summarise(salario_promedio = mean(y_ingLab_m_ha, na.rm = TRUE), .groups = "drop")

ggplot(mean_sal_age, aes(x = age, y = salario_promedio)) +
  geom_col(fill = "royalblue1", width = 0.7) +  
  labs(
    title = "Salario por hora promedio por edad y género",
    x = "Edad",
    y = "Salario por hora"
  ) +
  theme_classic() +  
  theme(plot.title = element_text(size =16, hjust = 0.5, face = "bold")) +
  facet_wrap(~ female, labeller = as_labeller(c(`0` = "Hombres", `1` = "Mujeres")))

ggsave("views/salario_por_edad_genero.png", width = 10, height = 5, dpi = 300)

## Scatter de ln(Salario) vs edad.
ggplot(db, aes(x = age, y = ln_sal)) +
  geom_point(color = "royalblue1", alpha = 0.4) +  
  geom_smooth(method = "loess", color = "brown2", se = FALSE) +  # Línea suavizada
  labs(
    title = "Logaritmo del salario por edad",
    x = "Edad",
    y = "Logaritmo del salario"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("views/salario_por_edad_scatter.png", width = 7, height = 6, dpi = 300)
