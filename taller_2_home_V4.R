# Taller 2
# En este modelo se regulariza y se cambia la forma de estimar. se sigue el cuaderno del profesor
# Este modelo correo bien, pero solamente tiene una especificacion muy simple. En este Scrip se hace
# modelos mas complejos
# Limpiar todo

rm(list = ls())

# Librerias

library(pacman)

p_load(tidyverse, readr, skimr, fastDummies, caret, glmnet, MLmetrics)

# Importar datos

train_hogares <- read_csv("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/Taller 2/train_hogares.csv")
train_personas <- read_csv("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/Taller 2/train_personas.csv")
test_hogares <- read_csv("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/Taller 2/test_hogares.csv")
test_personas <- read_csv("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/Taller 2/test_personas.csv")

sample_submission <- read_csv("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/Taller 2/sample_submission.csv")

## ----- TRAIN HOGARES: Extrayendo variables de condiciones del jefe de hogar de la base train_personas

cond_jefe_hog_train <- train_personas %>%
  group_by(id) %>%
  filter(Orden ==1) %>%
  select(Estrato1, P6020, P6040, P6050, P6090, P6210, P6426, P6430, P6800, P7090, P6870)

summary(cond_jefe_hog_train)

## * Uniendo condiciones del jefe de hogar con la base train_hogares

train_hogares <- left_join(train_hogares, cond_jefe_hog_train)

colnames(train_hogares)

## --- TEST HOGARES: Extrayendo variables de condiciones del jefe de hogar de la base test_personas

cond_jefe_hog_test <- test_personas %>%
  group_by(id) %>%
  filter(Orden ==1) %>%
  select(P6020, P6040, P6050, P6090, P6210, P6426, P6430, P6800, P7090, P6870)

summary(cond_jefe_hog_test)

## * Uniendo condiciones del jefe de hogar con la base test_hogares

test_hogares <- left_join(test_hogares, cond_jefe_hog_test)

colnames(test_hogares)

## Eliminar las varaibles no relevantes

train_hogares <- select(train_hogares, -P5100, -P5130, -P5140)  # En train_hogares

test_hogares <- select(test_hogares, -P5100, -P5130, -P5140)  # En test_hogares

# * Veamos los NAs de la base

train_hogares <- na.omit(train_hogares)

sapply(train_hogares, function(x) sum(is.na(x)))
sapply(test_hogares, function(x) sum(is.na(x)))

## * Imputando datos faltantes

# *** En test_hogares
test_hogares$P6090 %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test_hogares$P7090 %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test_hogares$P6430 %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test_hogares$P6090[is.na(test_hogares$P6090)] <- 1
test_hogares$P7090[is.na(test_hogares$P7090)] <- 2
test_hogares$P6430[is.na(test_hogares$P6430)] <- 4
test_hogares$P6426[is.na(test_hogares$P6426)] <- mean(test_hogares$P6426, na.rm = TRUE)
test_hogares$P6800[is.na(test_hogares$P6800)] <- mean(test_hogares$P6800, na.rm = TRUE)
test_hogares$P6870[is.na(test_hogares$P6870)] <- mean(test_hogares$P6870, na.rm = TRUE)

sapply(test_hogares, function(x) sum(is.na(x)))

## ----------------------------------------------------------------------------------------
## Seleccionando variables para los modelos

train_hogares_final <- train_hogares %>%
  select(Clase, Ingtotug, P5000, P5090, Nper, Pobre, P6020, P6040, P6090, P6210,
         P6426, P6430, P6800, P7090,
         P6870)

test_hogares_final <- test_hogares %>%
  select(Clase, P5000, P5090, Nper, P6020, P6040, P6090, P6210,
         P6426, P6430, P6800, P7090,
         P6870)
colnames(train_hogares_final)
colnames(test_hogares_final)

sapply(train_hogares_final, function(x) sum(is.na(x)))
sapply(test_hogares_final, function(x) sum(is.na(x)))

#-------------------------------------------------------------------------------------------
## Renombrando Variables
## Para entrenamiento
train_hogares_final<-train_hogares_final %>% 
  rename(cabecera_resto = Clase,
         income = Ingtotug,
         cuartos_hogar = P5000,
         vivienda_propia = P5090,
         personas_hogar= Nper,
         hombre = P6020,
         edad = P6040,
         entidad_salud =P6090,
         nivel_educativo = P6210,
         tiempo_trabajando = P6426,
         tipo_de_trabajo = P6430,
         horas_trabajo = P6800,
         mas_trabajo = P7090,
         tam_emp = P6870
         )


##Para testeo:

test_hogares_final<-test_hogares_final %>% 
  rename(cabecera_resto = Clase,
         cuartos_hogar = P5000,
         vivienda_propia = P5090,
         personas_hogar= Nper,
         hombre = P6020,
         edad = P6040,
         entidad_salud =P6090,
         nivel_educativo = P6210,
         tiempo_trabajando = P6426,
         tipo_de_trabajo = P6430,
         horas_trabajo = P6800,
         mas_trabajo = P7090,
         tam_emp = P6870
         )
#-------------------------------------------------------------------------------------------

#TRAIN DICOTOMAS, CATEGORIAS-FACTORES Y CONTINUAS:

#pasamos variables dicotomas a 0 y 1

train_hogares_final<-train_hogares_final %>%
  mutate(entidad_salud = if_else(entidad_salud == 1, 1, 0), 
         cabecera_resto = if_else(cabecera_resto == 1, 1, 0),
         hombre = if_else(hombre == 1, 1, 0),
         mas_trabajo = if_else(mas_trabajo == 1, 1, 0)
  )


#Variables categóricas a factores - Labels Factores
train_hogares_final<-train_hogares_final%>%
  mutate(cabecera_resto=factor(cabecera_resto, levels=c(0, 1), labels=c("resto", "cabecera")),
         vivienda_propia=factor(vivienda_propia, levels=c(1,2,3,4,5,6), labels=c("propia_pagada", "propia_pagando", "arriendo", "usufructo", "posesión_sin_título", "otra")),
         sexo= factor(hombre, levels=c(0,1), labels=c("mujer", "hombre")),
         pobre=factor(Pobre, levels=c(0,1), labels=c("No pobre","Sí pobre")),
         entidad_salud=factor(entidad_salud, levels=c(0,1), labels=c("No salud","Sí salud")),
         nivel_educativo=factor(nivel_educativo, levels=c(1,2,3,4,5,6), labels=c("ninguno", "pre_esc", "primaria", "secundaria", "media", "superior")),
         tipo_de_trabajo=factor(tipo_de_trabajo, levels=c(1,2,3,4,5,6,7,8,9), labels=c("obre_part", "obre_gob", "empl_dom", "cuet_propia", "empleador", "trab_fam", "trab_emp", "jornal", "otro")),
         mas_trabajo=factor(mas_trabajo, levels=c(0,1), labels=c("Si más trabajo","No más trabajo"))
         )

colnames(train_hogares_final)

summary(train_hogares_final)
#-------------------------------------------------------------------------------------------

#TEST DICOTOMAS, CATEGORIAS-FACTORES:

#pasamos variables dicotomas a 0 y 1

test_hogares_final<-test_hogares_final %>%
  mutate(entidad_salud = if_else(entidad_salud == 1, 1, 0), 
         cabecera_resto = if_else(cabecera_resto == 1, 1, 0),
         hombre = if_else(hombre == 1, 1, 0),
         mas_trabajo = if_else(mas_trabajo == 1, 1, 0)
  )


#Variables categóricas a factores - Labels Factores
test_hogares_final<-test_hogares_final%>%
  mutate(cabecera_resto=factor(cabecera_resto, levels=c(0, 1), labels=c("resto", "cabecera")),
         vivienda_propia=factor(vivienda_propia, levels=c(1,2,3,4,5,6), labels=c("propia_pagada", "propia_pagando", "arriendo", "usufructo", "posesión_sin_título", "otra")),
         sexo= factor(hombre, levels=c(0,1), labels=c("mujer", "hombre")),
         entidad_salud=factor(entidad_salud, levels=c(0,1), labels=c("No salud","Sí salud")),
         nivel_educativo=factor(nivel_educativo, levels=c(1,2,3,4,5,6), labels=c("ninguno", "pre_esc", "primaria", "secundaria", "media", "superior")),
         tipo_de_trabajo=factor(tipo_de_trabajo, levels=c(1,2,3,4,5,6,7,8,9), labels=c("obre_part", "obre_gob", "empl_dom", "cuet_propia", "empleador", "trab_fam", "trab_emp", "jornal", "otro")),
         mas_trabajo=factor(mas_trabajo, levels=c(0,1), labels=c("Si más trabajo","No más trabajo"))
  )

colnames(test_hogares_final)

summary(test_hogares_final)

sapply(train_hogares_final, function(x) sum(is.na(x)))
sapply(test_hogares_final, function(x) sum(is.na(x)))

# Imputar datos faltantes en la variable nivel_educativo

train_hogares_final$nivel_educativo %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test_hogares_final$nivel_educativo %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train_hogares_final$nivel_educativo[is.na(train_hogares_final$nivel_educativo)] <- "primaria"
test_hogares_final$nivel_educativo[is.na(test_hogares_final$nivel_educativo)] <- "primaria"

train_hogares_final <- train_hogares_final %>% mutate(edad_2 = I(edad^2))
test_hogares_final <- test_hogares_final %>% mutate(edad_2 = I(edad^2))

sapply(train_hogares_final, function(x) sum(is.na(x)))  # No tiene NAs
sapply(test_hogares_final, function(x) sum(is.na(x)))   # No tiene NAs

#-------------------------------------------------------------------------------------------
## BASE PARA INCOME REGRESSION MODEL

# Ahora procedemos a dummyficar la base train_hogares_final

train_hogares_s<- model.matrix(~ income + cabecera_resto + hombre + edad + edad_2 + nivel_educativo + tiempo_trabajando +
                                 tipo_de_trabajo + horas_trabajo + tam_emp, train_hogares_final) %>%
  as.data.frame()

# Ahora procedemos a dummyficar la base test_hogares_final

test_hogares_s<- model.matrix(~ cabecera_resto + hombre + edad + edad_2 + nivel_educativo + tiempo_trabajando +
                                tipo_de_trabajo + horas_trabajo + tam_emp, test_hogares_final) %>%
  as.data.frame()

#-------------------------------------------------------------------------------------------
# Estandarizamos las variables continuas

variables_numericas <- c("edad", "edad_2", "tiempo_trabajando", "horas_trabajo","tam_emp")

escalador <- preProcess(train_hogares_s[,variables_numericas],
                        method = c("center", "scale"))

train_hogares_s[, variables_numericas] <- predict(escalador, train_hogares_s[, variables_numericas])

test_hogares_s[, variables_numericas] <- predict(escalador, test_hogares_s[, variables_numericas])

sapply(train_hogares_s, function(x) sum(is.na(x)))  # No tiene NAs
sapply(test_hogares_s, function(x) sum(is.na(x)))   # No tiene NAs
#------------------------------------------------------------------------------------------
# Quitamos de la base la variable nivel_educativono_sabe

#train_hogares_s <- select(train_hogares_s, -nivel_educativono_sabe)  # En train_hogares_s

#test_hogares_s <- select(test_hogares_s, -nivel_educativono_sabe)  # En test_hogares_s


#-------------------------------------------------------------------------------------------
# MODELO INCOME REGRESSION


#y <- train_hogares_final$income
#X <- model.matrix(~ cabecera_resto + hombre + edad + edad_2 + nivel_educativo + tiempo_trabajando +
                  # tipo_de_trabajo + horas_trabajo + tam_emp -1, train_hogares_final)

#head(X)



# ------- Regresion Lineal -------------------

modelo_reg <- lm("income+1 ~ -1 +.", data = train_hogares_s)
summary(modelo_reg)

df_coeficientes_reg <- modelo_reg$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes_reg %>%
  filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(coeficiente)), 
             y = coeficiente)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Coeficientes del modelo de regresión", 
       x = "Variables",
       y = "Coeficientes") +
  theme_bw()



# ------ Regresion Ridge ---------------------
grid=10^seq(10,-2,length=100)
grid

y_train <- train_hogares_s$income
X_train <- select(train_hogares_s, -income)
X_test <- test_hogares_s

ridge1<-glmnet(x=X_train,
               y=y_train,
               alpha=0, #0 is ridge, 1 is lasso
               lambda=grid,
               standardize = FALSE)
coef(ridge1)

# poniendo los coeficientes en un data frame

#Put coefficients in a data frame, except the intercept
coefs_ridge<-data.frame(t(as.matrix(coef(ridge1)))) %>% select(-X.Intercept.)
#add the lambda grid to to data frame
coefs_ridge<- coefs_ridge %>% mutate(lambda=grid)              

#ggplot friendly format
coefs_ridge<- coefs_ridge %>% pivot_longer(cols=!lambda,
                                           names_to="variables",
                                           values_to="coefficients")



ggplot(data=coefs_ridge, aes(x = lambda, y = coefficients, color = variables)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes Ridge", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")

#-------------- Regresion Lasso ---------------------------------------

grid=10^seq(10,-2,length=100)

lasso1<-glmnet(x=X_train,
               y=y_train,
               alpha=1, #0 is ridge, 1 is lasso
               lambda=grid,
               standardize = FALSE)

#Put coefficients in a data frame, except the intercept
coefs_lasso<-data.frame(t(as.matrix(coef(lasso1)))) %>% select(-X.Intercept.)
#add the lambda grid to to data frame
coefs_lasso<- coefs_lasso %>% mutate(lambda=grid)              

#ggplot friendly format
coefs_lasso<- coefs_lasso %>% pivot_longer(cols=!lambda,
                                           names_to="variables",
                                           values_to="coefficients")



ggplot(data=coefs_lasso, aes(x = lambda, y = coefficients, color = variables)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes Lasso", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")


## ------- Elejimos los Hiper parametros -----------------

# Minimos Cuadrados Ordinario (OLS)
#modelo_reg <- lm("income ~ -1 +.", data = train_hogares_s)
p_load(caret)

set.seed(123)
fitControl <- trainControl(## 5-fold CV, 10 better
  method = "cv",
  number = 8)

fmla<-formula(income ~ cabecera_resto + edad + edad_2 + hombre + edad:hombre + edad_2:hombre +
                nivel_educativo + hombre:nivel_educativo + tiempo_trabajando + edad:tiempo_trabajando + 
                edad_2:tiempo_trabajando + tipo_de_trabajo + hombre:tipo_de_trabajo + horas_trabajo +
                hombre:horas_trabajo + tam_emp)

linear_reg<-train(fmla,
                  data=train_hogares_final,
                  method = 'lm', 
                  trControl = fitControl,
                  preProcess = c("center", "scale")
) 


linear_reg
summary(linear_reg)

y_hat_reg <- predict(linear_reg, newdata = test_hogares_final)

#------ Modelo Ridge --------------------------#

ridge<-train(fmla,
             data=train_hogares_final,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 0, #Ridge
                                    lambda = seq(0.001,0.02,by = 0.001)),
             preProcess = c("center", "scale")
) 

plot(ridge$results$lambda,
     ridge$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE)"
)

ridge$bestTune

coef_ridge<-coef(ridge$finalModel, ridge$bestTune$lambda)
coef_ridge

modelo_ridge<-train(fmla,
             data=train_hogares_final,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 0, #Ridge
                                    lambda = 0.02),
             preProcess = c("center", "scale")
) 

y_hat_ridge <- predict(modelo_ridge, newdata = test_hogares_final)

## Modelo Lasso

lasso<-train(fmla,
             data=train_hogares_final,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 1, #lasso
                                    lambda = seq(0.001,0.02,by = 0.001)),
             preProcess = c("center", "scale")
) 

plot(lasso$results$lambda,
     lasso$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE) Lasso"
)

lasso$bestTune

coef_lasso<-coef(lasso$finalModel, lasso$bestTune$lambda)
coef_lasso

modelo_lasso<-train(fmla,
             data=train_hogares_final,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 1, #lasso
                                    lambda = 0.02),
             preProcess = c("center", "scale")
) 

y_hat_lasso <- predict(modelo_lasso, newdata = test_hogares_final)

## Elastic Net

EN<-train(fmla,
          data=train_hogares_final,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1), #grilla de alpha
                                 lambda = seq(0.001,0.02,by = 0.001)),
          preProcess = c("center", "scale")
) 

EN$bestTune

coef_EN<-coef(EN$finalModel,EN$bestTune$lambda)
coef_EN

modelo_EN<-train(fmla,
          data=train_hogares_final,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = 0.1, #grilla de alpha
                                 lambda = 0.02),
          preProcess = c("center", "scale")
) 

y_hat_EN <- predict(modelo_EN, newdata = test_hogares_final)

## Tabla: Coeficientes de los modelos

coefs_df<-cbind(coef(linear_reg$finalModel),as.matrix(coef_ridge),as.matrix(coef_lasso),as.matrix(coef_EN))
colnames(coefs_df)<-c("OLS","RIDGE","LASSO","ELASTIC_NET")
round(coefs_df,4)

RMSE_df<-cbind(linear_reg$results$RMSE,ridge$results$RMSE[which.min(ridge$results$lambda)],lasso$results$RMSE[which.min(lasso$results$lambda)],EN$results$RMSE[which.min(EN$results$lambda)])
colnames(RMSE_df)<-c("OLS","RIDGE","LASSO","EN")
RMSE_df

#-----------------------------------------------------

# Estimando pobreza desde las predicciones
# y_hat_reg
# y_hat_ridge
# y_hat_lasso
# y_hat_EN

test_hogares <- cbind(test_hogares, y_hat_EN)

test_hogares <- test_hogares %>% mutate(ingreso = y_hat_EN)
#test_hogares <- test_hogares %>% mutate(ingreso = exp(y_hat_EN))

test_hogares<- test_hogares %>% mutate(pobre1=ifelse(ingreso<Lp,1,0))

pobre <- test_hogares$pobre1

sample_submission <- select(sample_submission, -pobre)

sample_submission <- cbind(sample_submission, pobre)

table(sample_submission$pobre)

# ----------
setwd("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/Taller 2")

write.csv(sample_submission, file="predichos.csv")
