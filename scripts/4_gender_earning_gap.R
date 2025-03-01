
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

#1.1) Volver female una variable númerica para poder usarla como variable de respuesta usando lm()

db <- db %>% 
      mutate(female = as.numeric(female)) %>%
      mutate(female = ifelse(female == 2, 1, 0))




#2) Estimar el primero modelo sin controles  -----------------------------------


modelo4a <- lm(ln_sal ~ female, data = db)
stargazer(modelo4a, type = "text", star.cutoffs = NA, notes.append = FALSE)


#3) Estimar el segundo modelo con controles y FWL ------------------------------
#La variable de interés es female

##3.1) Modelo####
modelo4b <- lm(ln_sal ~ female + nivel_educ + age + sizeFirm + formal + oficio + estrato1 ,data = db)
stargazer(modelo4a, modelo4b, 
          type = "text", 
          omit =  c("Constant", "oficio", "nivel_educ", "age", "sizeFirm", "formal", "estrato1"), 
          star.cutoffs = NA, 
          notes.append = FALSE)

##3.2) Estimar el modelo por partialling-out/FWL####

#(i) Regresión auxiliar de female en los controles: representa la parte de female que no esta explicada por los controles
#(ii) Regresión auxiliar de ln_sal en los controles: representa la parte de ln_sal que no esta explicada por los controles
db_fwl <- db %>% mutate(female_resid = lm(female ~ nivel_educ + age + sizeFirm + formal + oficio + estrato1 ,data = db)$residuals) %>%
                 mutate(ln_sal_resid = lm(ln_sal ~ nivel_educ + age + sizeFirm + formal + oficio + estrato1 ,data = db)$residuals) 


#(iii) Dejar en la base solo las variables de interés 
db_fwl <- db_fwl %>% 
                 select(female_resid, ln_sal_resid) %>%
                 rename(female = female_resid)

#(iv) regresar los residuales de la variable resultado (ii) en los residuales de la variable de interés (iii): 
modelo_4b_fwl <- lm(ln_sal_resid ~ female, data = db_fwl)
stargazer(modelo4b, modelo_4b_fwl, 
          type = "text", 
          omit = c("Constant", "oficio", "nivel_educ", "age", "sizeFirm", "formal", "horas_ocup_prin", "estrato1"), 
          star.cutoffs = NA, 
          notes.append = FALSE)


#(iv) agregar los valores predichos a la base de datos: 
#Usamos los errores predichos por el modelo 4b porque es el que genera el mejor ajuste 
db$ln_sal_predicted <- predict(modelo4b)


##3.3) Estimar el modelo por partialling-out y los SE haciendo bootstrapping####


#(i) Función que hace la estimación del modelo por partialling-out 
#Nota: siempre por el índice igual a 2 

partialling_out <- function (data, index) {
  
  #Función partialling-out: Esta función hace partilling-out para estimar el si existe una brecha de género en el salario por hora. Se usan como
  #                          controles el nivel de educación, la edad, el tamaño de la firma, si es formal o informal, el oficio/ocupación de 
  #                          la persona y el estrato
  
  
  #Parámetros: 
        
              #db: base de datos con la información de la GEIH 
              #index: número de observaciones en db. El índice facilita el uso de esta función con paquete boot 
  
  
  #Return: el coeficiente de la variable female obtenido al hacer partillaing out 
  
  
  db_resid <- data.frame(row.names = 1:nrow(data))
  
  db_resid$x_resid <- lm(female ~ nivel_educ + age + sizeFirm + formal + oficio + estrato1, data = data, subset = index )$residuals
  
  db_resid$y_resid <- lm(ln_sal ~ nivel_educ + age + sizeFirm + formal + oficio + estrato1, data = data, subset = index)$residuals
  
  coef(lm( y_resid ~ x_resid, data = db_resid, subset = index))[2]
}


#Prueba función partialling_out:
#partialling_out(db, 1:nrow(db))


#(ii) Bootstrapping usando el paquete boot

#R = 1000
female_boostrap_se_R1000 <-boot(db,partialling_out, R = 1000)
export(female_boostrap_se_R1000, 'stores/female_boostrap_se_R1000.rds')

female_boostrap_se_R1000 <- readRDS("stores/female_boostrap_se_R1000.rds")

#R = 10000
#Nota: Este bootstrap se demora bastante corriendo por lo que es mejor cargar los resultados guardados en stores
female_boostrap_se_R10000 <-boot(db,partialling_out, R = 10000)
export(female_boostrap_se_R10000, 'stores/female_boostrap_se_R10000.rds')

female_boostrap_se_R10000 <- readRDS("stores/female_boostrap_se_R10000.rds")


##3.4) Agregar en la tabla de regresión los bootstrap se####

#Guardar en un objecto los erroes estándar de los modelos: 
se_modelo4a <- summary(modelo4a)$coefficients[, "Std. Error"]
se_modelo4b <- summary(modelo4b)$coefficients[, "Std. Error"]
se_modelo4b_fwl <- summary(modelo_4b_fwl)$coefficients[, "Std. Error"]
se_modelo4b_fwl_boot <- c(NA, 0.01526995) #Este es el resultado de female_boostrap_se_R_10000

#Incluirlos en la tabla y darle formato a la tabla: 
resultados_modelos <- stargazer(modelo4a ,modelo4b, modelo_4b_fwl, modelo_4b_fwl,
                      type = "text", 
                      omit = c("Constant","oficio", "nivel_educ", "age", "sizeFirm", "formal", "horas_ocup_prin", "estrato1"), # Nos mostrar controles
                      star.cutoffs = NA, # No mostrar asteriscos para la significancia 
                      notes.append = FALSE, # No incluir notas 
                      se = list(se_modelo4a, se_modelo4b, se_modelo4b_fwl, se_modelo4b_fwl_boot), #Incluir errores estándar especificados por nosotros
                      column.labels = c("Sin condicionar", "Condicionada", "FWL", "FWL Boot SE"),  #Ponerle nombres a los modelos
                      dep.var.labels.include = F, 
                      dep.var.caption = "", # Remover titulo que dice "Dependent variable:"
                      title =  "Brecha salarial por género", #Ponerle título a la tabla
                      covariate.labels = c("Mujer"), # Renombrar covariables 
                      keep.stat = c("n", "rsq", "ser")) # Conservar las estadísticas de interés 

  

#4) Gráfico salario por la edad por género -------------------------------------


##4.1) Colapsar base de datos a nivel de género y año####
#Nota: Se colapsa la base a nivel de género y año para la media del salario por horas 
average_salary_per_age_db  <- db %>% 
                              select(age, female, y_ingLab_m_ha) %>%
                              group_by(age, female) %>%
                              summarise(mean_salary_per_hour = mean(y_ingLab_m_ha))

##4.2) Hacer un gráfico preliminar de la relación ingreso/edad por género####
salary_plot_1 <-ggplot(data = average_salary_per_age_db, mapping = aes( x = age , y = mean_salary_per_hour, group = female, color = female)) +
  geom_line() 

#Nota: hay unos picos irregulares en 70, 78 y 80 años con salarios por hora de 41253.7 que es el valor que se uso
#para truncar las valores máximos de los outliers del salario por hora. 



##4.3) Caracterizar picos encontrados en 4.2#### 
#Identificar outliers por edad y salario por hora:
#Para ver los picos de ingreso después de los 60 que están por fuera del 
#intervalo de confianza de 95% para el salario por hora
db_outliers <- db %>% 
               select(y_ingLab_m_ha, age, female) %>%
               filter(y_ingLab_m_ha >30000) %>%
               filter( age > 60) %>%
               mutate(flag = ifelse(y_ingLab_m_ha > 41253, 1, 0))

#Nota: Los picos en 78 y 80 años son porque solo hay una persona con 78 años y dos con 80 años que tiene valores de 41253.7 para el salario

##4.4) Conservar las observaciones que esten en el intervalo de confianza de 95%####
#Para el salario por hora
#Quitamos los años con menos de 25 observaciones para no seguir tan de cerca a pocas observaciones para un año
#table(db$female, db$age)

db_sin_outliers <- db %>% 
                   filter(y_ingLab_m_ha < 24297) %>%
                   filter(age < 62) %>% 
                   mutate (female = as.factor(female))


##4.5) Volver a hacer el gráfico####


#i) Colapsar la base a nivel de edad y salario por género (otra vez)
average_salary_per_age_db  <- db_sin_outliers %>% 
                              select(age, female, ln_sal_predicted) %>%
                              group_by(age, female) %>%
                              summarise(mean_salary_per_hour = mean(ln_sal_predicted))

#ii) Gráfico ingreso relación ingreso/edad por género (otra vez)
salary_plot_2 <-ggplot(data = average_salary_per_age_db, mapping = aes( x = age , y = mean_salary_per_hour, group = female, color = female)) +
                geom_smooth(method = "gam", fill="#69b3a2" ,se = T, level = 0.95) + 
                scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65)) +
                labs(title = "Dinámica promedio salario por hora con la edad" , x = "Edad", y = "Promedio salario por hora", color = "") + 
                scale_color_manual(labels = c("Hombres", "Mujeres"), values = c("coral2", "deepskyblue")) + 
                theme_minimal()

ggsave("views/dinamica_promedio_salario_por_hora_con_la_edad.png")

salary_plot_3 <-ggplot(data = average_salary_per_age_db, mapping = aes( x = age , y = mean_salary_per_hour, group = female, color = female)) +
                  geom_line() +
                  geom_point() + 
                  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65)) +
                  labs(title = "Dinámica promedio salario por hora con la edad" , x = "Edad", y = "Promedio salario por hora", color = "") + 
                  scale_color_manual(labels = c("Hombres", "Mujeres"), values = c("coral2", "deepskyblue")) + 
                  theme_minimal()

#5) Calcular edades picos por género:#### 




  
#Anexo visualizar outliers------------------------------------------------------


##A.1) Visualizar variable salario nominal por hora####

#Función de densidad de la variable ingreso salarial por hora: 
#La distribución tiene una cola izquierda pesada y una larga cola derecha 
density_plot_ln_sal <- ggplot(db, aes(x = db$ln_sal)) +
  geom_density(fill = "blue", alpha = .2) + 
  geom_line(stat = "density") + 
  geom_vline(xintercept = median(db$ln_sal, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(db$ln_sal, na.rm = TRUE), linetype = "dashed", color = "gray") + 
  ggtitle("Densidad ingreso salarial por horas")


##A.2) Outliers####
#En ambos casos hay bastantes outliers. Estos podrían ser parte del procesos de generar de datos 
#Teniendo en cuenta la desigualdad que hay en Colombia, pero no quiero hacerle overfitting a esos datos extremos

#Agrupados por estrato: 
box_plot_y_ingLab_m_ha<- ggplot(db , mapping = aes(as.factor(estrato1),  ln_sal)) + 
  geom_boxplot() + 
  ggtitle("Box plot salarial por horas por estrato")

#Sin agrupar: 
box_plot_y_ingLab_m_ha_no_group<- ggplot(db , mapping = aes(ln_sal)) + 
  geom_boxplot() + 
  ggtitle("Box plot salarial")



#Regla de la desviación estándar:
low <- mean(db$ln_sal) - 2* sd(db$ln_sal)
up <- mean(db$ln_sal) + 2* sd(db$ln_sal)



density_plot_sigma_rule <-  ggplot(db, aes(x = db$ln_sal)) +
  geom_density(fill = "blue", alpha = .2) + 
  geom_line(stat = "density") + 
  geom_vline(xintercept = low, linetype = "dashed", color = "red") +
  geom_vline(xintercept = up, linetype = "dashed", color = "gray") + 
  ggtitle("Densidad ingreso salarial por horas")


##A.3) Caracterízar outliers####

#Guardar outliers en la base de datos: 
#i) Son 613 observaciones que corresponden al 6,2% de las observaciones
db_outliers <- db %>% 
  filter(y_ingLab_m_ha < low | y_ingLab_m_ha > up)

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












