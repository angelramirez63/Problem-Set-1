#### Preparación
library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, dplyr)

rm(list = ls())

#### Importar base
db <- readRDS("stores/datos_modelos.rds") %>% 
  as_tibble()

#### Crear esperiencia potencial
db <- db %>%
  mutate(
    age = as.numeric(as.character(age)),  # Convertir age a numérico
    grad_aprob = as.numeric(as.character(grad_aprob)),  # Convertir grad_aprob a numérico
    exp_pot = age - grad_aprob - 5  # Calcular la nueva variable
  )

table(db$exp_pot)

table(db$grad_aprob)

#### Renombrar variables

db <- db %>%
  rename(
    ##### Ingresos laborales
    ing_primas = p6545s1, ing_bonif = p6580s1, ing_lab_secundario = p7070,    
    
    ##### Subsidios
    subs_alimentacion = p6585s1a1, subs_transporte = p6585s2a1,         
    subs_familiar = p6585s3a1, subs_educ = p6585s4a1,             
    
    ##### Ingresos en especie
    val_alimentos_especie = p6590s1, val_vivienda_especie = p6600s1,         
    val_transporte_especie = p6610s1, val_otros_especie = p6620s1,             
    
    ##### Primas y bonificaciones adicionales
    prima_servicios = p6630s1a1,           
    prima_navidad = p6630s2a1, prima_vacaciones = p6630s3a1,             
    viaticos_bonif = p6630s4a1, bonif_anuales = p6630s6a1,         
    
    ##### Ingresos no laborales
    ing_arriendos = p7500s1a1,              
    ing_pension = p7500s2a1, ing_pension_alimenticia = p7500s3a1,    
    ing_otros_hogares = p7510s1a1, ing_otros_hogares_extranjero = p7510s2a1, 
    ing_ayudas_instituciones = p7510s3a1, ing_intereses_dividendos = p7510s5a1, 
    ing_cesantias = p7510s6a1, ing_otras_fuentes = p7510s7a1  
  )

### a) 

### Crear semilla

set.seed(9963)

inTrain <- createDataPartition(
  y = db$ln_sal,  ## the outcome data are needed
  p = .70, ## The percentage of training data
  list = FALSE
)

training <- db %>% 
  filter(row_number() %in% inTrain)

testing  <- db %>% 
  filter(!row_number() %in% inTrain)

### b)

#### Primer modelo

form_1 <- ln_sal ~ age + I(age^2)

modelo1a <- lm(form_1, data = training)

predictions <- predict(modelo1a, testing)
score1a<- RMSE(predictions, testing$ln_sal )

score1a

#### Segundo modelo

form_2 <- ln_sal ~ female

modelo2a <- lm(form_2, data = training)

predictions <- predict(modelo2a, testing)
score2a<- RMSE(predictions, testing$ln_sal )

score2a

#### Tercer modelo

form_3 <- ln_sal ~ female + nivel_educ + age + I(age^2) + sizeFirm + formal + 
  oficio + estrato1

modelo3a <- lm(form_3, data = training)

predictions <- predict(modelo3a, testing)
score3a<- RMSE(predictions, testing$ln_sal )

score3a

#### Cuarto modelo

form_4 <- ln_sal ~ female + nivel_educ + age + I(age^2) + sizeFirm + 
  formal + oficio + estrato1 + exp_pot + I(exp_pot^2) + 
  totalHoursWorked

modelo4a <- lm(form_4, data = training)

predictions <- predict(modelo4a, testing)
score4a<- RMSE(predictions, testing$ln_sal )

score4a

#### Quinto modelo

form_5 <- ln_sal ~ female:parentesco_jefe + nivel_educ:poly(age,4,raw=TRUE) + 
  sizeFirm:formal + oficio + estrato1 + poly(exp_pot,4,raw=TRUE) + totalHoursWorked

modelo5a <- lm(form_5, data = training)

predictions <- predict(modelo5a, testing)
score5a<- RMSE(predictions, testing$ln_sal )

score5a

#### Sexto modelo

form_6 <- ln_sal ~ female:parentesco_jefe + nivel_educ:poly(age,4,raw=TRUE) + 
  sizeFirm:formal + oficio + estrato1 + poly(exp_pot,4,raw=TRUE) +
  totalHoursWorked + subs_alimentacion + subs_transporte + subs_familiar + subs_educ +
  prima_servicios + prima_navidad + prima_vacaciones + 
  viaticos_bonif + bonif_anuales

modelo6a <- lm(form_6, data = training)

predictions <- predict(modelo6a, testing)
score6a<- RMSE(predictions, testing$ln_sal )

score6a

#### Séptimo modelo

form_7 <- ln_sal ~ female:parentesco_jefe + nivel_educ:poly(age,4,raw=TRUE) + 
  sizeFirm:formal + oficio + estrato1 + 
  poly(exp_pot,4,raw=TRUE):poly(totalHoursWorked,4,raw=TRUE) + 
  subs_alimentacion + subs_transporte + subs_familiar + 
  subs_educ + prima_servicios + prima_navidad + prima_vacaciones + 
  viaticos_bonif + bonif_anuales + ing_primas + ing_bonif + ing_lab_secundario + 
  ing_arriendos + ing_pension + ing_pension_alimenticia + 
  ing_otros_hogares + ing_otros_hogares_extranjero + 
  ing_ayudas_instituciones + ing_intereses_dividendos + 
  ing_cesantias + ing_otras_fuentes + val_alimentos_especie + 
  val_vivienda_especie + val_transporte_especie + val_otros_especie


modelo7a <- lm(form_7, data = training)

predictions <- predict(modelo7a, testing)
score7a<- RMSE(predictions, testing$ln_sal )

score7a

#### Octavo modelo

form_8 <- ln_sal ~ female:parentesco_jefe + nivel_educ:poly(age,8,raw=TRUE) + 
  sizeFirm:formal + oficio + estrato1 + 
  poly(exp_pot,8,raw=TRUE):poly(totalHoursWorked,8,raw=TRUE) + ing_primas + 
  ing_bonif + ing_lab_secundario + ing_arriendos + ing_pension + 
  ing_pension_alimenticia + ing_otros_hogares + ing_otros_hogares_extranjero + 
  ing_ayudas_instituciones + ing_intereses_dividendos + ing_cesantias + 
  ing_otras_fuentes + val_alimentos_especie + val_vivienda_especie + 
  val_transporte_especie + val_otros_especie + subs_alimentacion + 
  subs_transporte + subs_familiar + subs_educ + prima_servicios + 
  prima_navidad + prima_vacaciones + viaticos_bonif + bonif_anuales

modelo8a <- lm(form_8, data = training)

predictions <- predict(modelo8a, testing)
score8a<- RMSE(predictions, testing$ln_sal )

score8a

### c)

#### Resultados de los modelos
scores<- data.frame( Modelo= c(1, 2, 3, 4, 5, 6, 7, 8),
                     RMSE= c(score1a, score2a, score3a, score4a, 
                             score5a, score6a, score7a, score8a)
)

scores

stargazer(scores[, c("Modelo", "RMSE")], type = "latex", title = "Resultados de Modelos",
          summary = FALSE, digits = 4, out = "tabla_modelos.tex",
          omit.summary.stat = c("n"))

#### gráfica de los errores de predicción por modelo

RMSE_vsa   <-  c(score1a, score2a, score3a, score4a, score5a, score6a, 
                 score7a, score8a)

scores<- data.frame( Modelo= c(1, 2, 3, 4, 5, 6, 7, 8),
                     Aproximación= c("Validation set"),
                     RMSE= c(RMSE_vsa)
)             

ggplot(scores, aes(x = Modelo, y = RMSE, col = Aproximación)) + 
  geom_line(size = 0.5) +
  theme_bw() +
  ggtitle("Desempeño de los modelos") +
  theme(plot.title = element_text(hjust = 0.5)) 

#### Outliers de la muestra

##### Distribución de los errores del modelo con mejor predicción

predictions <- predict(modelo6a, testing)

testing$errores <- testing$ln_sal - predictions

ggplot(testing, aes(x = errores)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(testing$errores, na.rm = TRUE), linetype = "dashed", color = "red") +
  theme_minimal()

##### Encontrar outliers con cuartiles

Q1 <- quantile(testing$errores, 0.25)
Q3 <- quantile(testing$errores, 0.75)
IQR <- Q3 - Q1  

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

db_outliers <- testing %>%
  filter(errores < limite_inferior | errores > limite_superior)

##### Encontrar outliers con desviaciones

media_errores <- mean(testing$errores, na.rm = TRUE)
sd_errores <- sd(testing$errores, na.rm = TRUE)

limite_inferior <- media_errores - 3 * sd_errores
limite_superior <- media_errores + 3 * sd_errores

db_outliers <- testing %>%
  filter(errores < limite_inferior | errores > limite_superior)

##### Revisamos si los outliers tienen valores atípicos en las variables independientes

var_cont <- c( "age", "exp_pot", "totalHoursWorked", 
               "subs_alimentacion", "subs_transporte", "subs_familiar", 
               "subs_educ", "prima_servicios", "prima_navidad", 
               "prima_vacaciones","viaticos_bonif", "bonif_anuales")   


var_categ <- c("female", "parentesco_jefe", "nivel_educ", "sizeFirm", 
               "formal", "oficio", "estrato1")

##### Para la base completa, categoricas
for (variable in var_categ) {
  
  plot <- ggplot(db, aes_string(x = variable)) +
    geom_bar(color = "#000000", fill = "#0099F8") +
    ggtitle(paste("Frecuencia de", as.character(variable))) +
    theme_classic() +
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))  # Rotación para mejor visibilidad
  
  print(plot)
}

##### Para los outliers, categóricas
for (variable in var_categ) {
  
  plot <- ggplot(db_outliers, aes_string(x = variable)) +
    geom_bar(color = "#000000", fill = "#0099F8") +
    ggtitle(paste("Frecuencia de", as.character(variable))) +
    theme_classic() +
    theme(plot.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))  # Rotación para mejor visibilidad
  
  print(plot)
}

##### Para la base completa, continuas
for (variable in var_cont) {
  
  plot <- ggplot(db, aes_string(variable)) +
    geom_histogram(color = "#000000", fill = "#0099F8") +
    geom_vline(xintercept = median(db_outliers[[variable]], na.rm = TRUE), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(db_outliers[[variable]], na.rm = TRUE), linetype = "dashed", color = "blue") +  
    ggtitle(paste("Distribución", as.character(variable), sep = " ")) +
    theme_classic() +
    theme(plot.title = element_text(size = 18))
  
  print(plot)
  
}

##### Para los outliers, continuas
for (variable in var_cont) {
  
  plot <- ggplot(db_outliers, aes_string(variable)) +
    geom_histogram(color = "#000000", fill = "#0099F8") +
    geom_vline(xintercept = median(db_outliers[[variable]], na.rm = TRUE), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(db_outliers[[variable]], na.rm = TRUE), linetype = "dashed", color = "blue") +  
    ggtitle(paste("Distribución", as.character(variable), sep = " ")) +
    theme_classic() +
    theme(plot.title = element_text(size = 18))
  
  print(plot)
  
}

##### La variable de formalidad tiene una distribución diferente

summary(db_outliers$formal)

### d)

##### Realización con comandos

ctrl <- trainControl(
  method = "LOOCV") ## input the method Leave One Out Cross Validation

##### Modelo 6

ctrl$verboseIter <- TRUE  # Enable progress printing
modelo6b <- train(form_6,
                  data = db,
                  method = 'lm', 
                  trControl = ctrl)

score6b<-RMSE(modelo6b$pred$pred, db$ln_sal)

##### Modelo 7

ctrl$verboseIter <- TRUE  # Enable progress printing
modelo7b <- train(form_6,
                  data = db,
                  method = 'lm', 
                  trControl = ctrl)

score7b<-RMSE(modelo7b$pred$pred, db$ln_sal)

##### Revisión adicional con k-fold

ctrl <- trainControl(
  method = "cv", ## Define the method for cross validation 
  number = 10) ## the number of folds. 

###### Modelo 6

set.seed(69205)  

modelo6c <- train(form_6,  ## define the functional form (i.e the variable to predict and the features)
                  data = db,  # the data frame
                  method = 'lm',  # the method
                  trControl= ctrl)  # input our cross validation method. 

modelo6c$resample

score6c<- mean(modelo6c$resample$RMSE)

###### Modelo 7

set.seed(69205)  

modelo7c <- train(form_7,  ## define the functional form (i.e the variable to predict and the features)
                  data = db,  # the data frame
                  method = 'lm',  # the method
                  trControl= ctrl)  # input our cross validation method. 

modelo7c$resample

score7c<- mean(modelo7c$resample$RMSE)

###### Modelo 8

set.seed(69205)  

modelo8c <- train(form_8,  ## define the functional form (i.e the variable to predict and the features)
                  data = db,  # the data frame
                  method = 'lm',  # the method
                  trControl= ctrl)  # input our cross validation method. 

modelo8c$resample

score8c<- mean(modelo8c$resample$RMSE)

score8b <- NA

##### Tabla de los errores de Validation set y LOOCV

scores<- data.frame( Model= c(6, 7, 8),
                     RMSE_vsa= c(score6a, score7a, score8a), 
                     RMSE_loocv= c(score6b, score7b, score8b),
                     RMSE_kfold= c(score6c, score7c, score8c)
)

stargazer(scores, type = "latex", summary = FALSE, rownames = FALSE)

#### Estadístico de influencia

##### Modelo 1

full_model1 <- lm(form_1,
                  data = db )

hatvalues1 <- hatvalues(full_model1)
leverage1 <- mean(hatvalues1)

##### Modelo 2

full_model2 <- lm(form_2,
                  data = db )

hatvalues2 <- hatvalues(full_model2)
leverage2 <- mean(hatvalues2)

##### Modelo 3

full_model3 <- lm(form_3,
                  data = db )

hatvalues3 <- hatvalues(full_model3)
leverage3 <- mean(hatvalues3)

##### Modelo 4

full_model4 <- lm(form_4,
                  data = db )

hatvalues4 <- hatvalues(full_model4)
leverage4 <- mean(hatvalues4)

##### Modelo 5

full_model5 <- lm(form_5,
                  data = db )

hatvalues5 <- hatvalues(full_model5)
leverage5 <- mean(hatvalues5)

#### Modelo 6

full_model6 <- lm(form_6,
                  data = db )

hatvalues6 <- hatvalues(full_model6)
leverage6 <- mean(hatvalues6)

##### Modelo 7

full_model7 <- lm(form_7,
                  data = db )

hatvalues7 <- hatvalues(full_model7)
leverage7 <- mean(hatvalues7)

##### Modelo 8

full_model8 <- lm(form_8,
                  data = db )

hatvalues8 <- hatvalues(full_model8)
leverage8 <- mean(hatvalues8)