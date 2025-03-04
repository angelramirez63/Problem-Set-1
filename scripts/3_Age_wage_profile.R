# Estadísticas descriptivas ----------------------------------------------------
rm(list = ls())

## 0. Preparación --------------------------------------------------------------

### Librerías 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, 
       dplyr, boot, kableExtra, knitr)


### Crear el directorio 
wd <- here()
setwd(wd)
rm(wd)

## Cargar datos
db <- readRDS("stores/datos_modelos.rds") %>% 
  as_tibble()

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

error_estandar <- sd(edad_pico_dist$t)
print(error_estandar)

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
# Generamos predicciones para cada edad en la base de datos con intervalos de confianza
predicciones <- predict(modelo1, newdata = db, interval = "confidence")

# Agregamos los valores de predicción e intervalos de confianza a la base de datos
db$predicted_ln_sal <- predicciones[, "fit"]
db$lwr <- predicciones[, "lwr"]
db$upr <- predicciones[, "upr"]

# Graficamos
ggplot(db, aes(x = age, y = ln_sal)) +
  geom_point(aes(color = "Valores observados"), alpha = 0.4) +  # Puntos de dispersión
  geom_line(aes(y = predicted_ln_sal, color = "Predicción del modelo"), size = 1) +  # Línea del modelo cuadrático
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "Intervalo de confianza"), alpha = 0.2) +  # Banda de confianza
  scale_color_manual(name = "",
                     values = c("Valores observados" = "royalblue1", 
                                "Predicción del modelo" = "brown2")) +
  scale_fill_manual(name = "", 
                    values = c("Intervalo de confianza" = "brown2")) +
  labs(
    x = "Edad",
    y = "Logaritmo del salario"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("views/salario_por_edad_scatter.png", width = 7, height = 6, dpi = 300)