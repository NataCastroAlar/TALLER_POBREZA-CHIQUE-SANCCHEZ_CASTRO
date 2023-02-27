############################################################
# Scrip Problem Set 2 ->    GRUPO 6                        #
# authors: Victor Chique    BIG DATA AND MACHINE LEARNING  #
#          Natalia Castro                                  #    
#          Victor Sanchez                                  #
############################################################

                ########################
                ##  PREDICTING POVERTY##
                ########################


#######################
##  1.BASE DE DATOS  ##
#######################

# Limpiar todo

rm(list = ls())

##install.packages("caret")

# Librerias

library("pacman", "readr", "skimr")
library(caret)
p_load("tidyverse", "readr", "skimr")

# Importar datos

train_hogares <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 2/Data/train_hogares.csv")
train_personas <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 2/Data/train_personas.csv")
test_hogares <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 2/Data/test_hogares.csv")
test_personas <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 2/Data/test_personas.csv")
sample_submission <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 2/Data/sample_submission.csv")

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

sapply(train_hogares, function(x) sum(is.na(x)))
sapply(test_hogares, function(x) sum(is.na(x)))

## * Imputando datos faltantes

# *** En train_hogares
train_hogares$P6090 %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train_hogares$P7090 %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train_hogares$P6430 %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train_hogares$P6090[is.na(train_hogares$P6090)] <- 1
train_hogares$P7090[is.na(train_hogares$P7090)] <- 2
train_hogares$P6430[is.na(train_hogares$P6430)] <- 4
train_hogares$P6426[is.na(train_hogares$P6426)] <- mean(train_hogares$P6426, na.rm = TRUE)
train_hogares$P6800[is.na(train_hogares$P6800)] <- mean(train_hogares$P6800, na.rm = TRUE)
train_hogares$P6870[is.na(train_hogares$P6870)] <- mean(train_hogares$P6870, na.rm = TRUE)

sapply(train_hogares, function(x) sum(is.na(x)))

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
  select(id, Clase, Ingtotug, P5000, P5090, Nper, Pobre, P6020, P6040, P6090, P6210,
         P6426, P6430, P6800, P7090,
         P6870)

test_hogares_final <- test_hogares %>%
  select(id, Clase, P5000, P5090, Nper, P6020, P6040, P6090, P6210,
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
         hombre= factor(hombre, levels=c(0,1), labels=c("mujer", "hombre")),
         pobre=factor(Pobre, levels=c(0,1), labels=c("No pobre","Sí pobre")),
         entidad_salud=factor(entidad_salud, levels=c(0,1), labels=c("No salud","Sí salud")),
         nivel_educativo=factor(nivel_educativo, levels=c(1,2,3,4,5,6,7), labels=c("ninguno", "pre_esc", "primaria", "secundaria", "media", "superior", "no_sabe")),
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
         hombre= factor(hombre, levels=c(0,1), labels=c("mujer", "hombre")),
         entidad_salud=factor(entidad_salud, levels=c(0,1), labels=c("No salud","Sí salud")),
         nivel_educativo=factor(nivel_educativo, levels=c(1,2,3,4,5,6,7), labels=c("ninguno", "pre_esc", "primaria", "secundaria", "media", "superior", "no_sabe")),
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

sapply(train_hogares_final, function(x) sum(is.na(x)))  # No tiene NAs
sapply(test_hogares_final, function(x) sum(is.na(x)))   # No tiene NAs

#-------------------------------------------------------------------------------------------
## BASE PARA INCOME REGRESSION MODEL

# Ahora procedemos a dummyficar la base train_hogares_final

train_hogares_s<- model.matrix(~ income + cabecera_resto + cuartos_hogar + vivienda_propia + personas_hogar +
                                 hombre + edad + entidad_salud + nivel_educativo + tiempo_trabajando +
                                 tipo_de_trabajo + horas_trabajo + mas_trabajo + tam_emp + pobre, train_hogares_final) %>%
  as.data.frame()

# Ahora procedemos a dummyficar la base test_hogares_final

test_hogares_s<- model.matrix(~ cabecera_resto + cuartos_hogar + vivienda_propia + personas_hogar +
                                hombre + edad + entidad_salud + nivel_educativo + tiempo_trabajando +
                                tipo_de_trabajo + horas_trabajo + mas_trabajo + tam_emp, test_hogares_final) %>%
  as.data.frame()

#-------------------------------------------------------------------------------------------
# Estandarizamos las variables continuas

variables_numericas <- c("cuartos_hogar", "personas_hogar", "edad", "tiempo_trabajando",
                         "horas_trabajo","tam_emp")

escalador <- preProcess(train_hogares_s[,variables_numericas],
                        method = c("center", "scale"))

train_hogares_s[, variables_numericas] <- predict(escalador, train_hogares_s[, variables_numericas])

test_hogares_s[, variables_numericas] <- predict(escalador, test_hogares_s[, variables_numericas])

sapply(train_hogares_s, function(x) sum(is.na(x)))  # No tiene NAs
sapply(test_hogares_s, function(x) sum(is.na(x)))   # No tiene NAs

#############################
##  2.CLASIFICATION MODEL  ##
#############################

# Mutación de factor variable pobre

pobre<-train_hogares_final$Pobre # definimos objeto pobre

train_hogares_s<- train_hogares_s%>% mutate(pobre=factor(pobre,levels=c(0,1),labels=c("No","Si")))

table(train_hogares_s$pobre)

# Plot para revisar variables relevantes

plot(pobre ~ cabecera_resto, data=train_hogares_final, col=c(8,2), ylab="Pobre")
plot(pobre ~ vivienda_propia, data=train_hogares_final, col=c(8,2), ylab="Pobre")
plot(pobre ~ cuartos_hogar, data=train_hogares_final, col=c(8,2), ylab="Pobre")
plot(pobre ~ tipo_de_trabajo, data=train_hogares_final, col=c(8,2), ylab="Pobre")

head(train_hogares_s)
summary(train_hogares_s)

#Estimando el modelo Logit

mylogit <- glm(pobre~income+cabecera_restocabecera+cuartos_hogar+vivienda_propiapropia_pagando+vivienda_propiaarriendo+vivienda_propiausufructo+
                 +vivienda_propiaposesión_sin_título+vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosuperior+horas_trabajo, family = "binomial", data = train_hogares_s)
summary(mylogit,type="text")

#Predicción train
pred<-predict(mylogit,newdata = train_hogares_s, type = "response")
summary(pred)

#Missclasification rates

#Rule 1
rule<- 1/2
pred>rule
table(pobre)
sum(pred>rule)


sum( (pred>rule)[pobre==0] )/sum(pobre==0) #False positive rate
sum( (pred>rule)[pobre==1] )/sum(pobre==1) #True positive rate

#Rule 2
rule<- 1/5
pred>rule
table(pobre)
sum(pred>rule) #nuestro modelo predice 25.508 pobres#


sum( (pred>rule)[pobre==0] )/sum(pobre==0) #False positive rate
sum( (pred>rule)[pobre==1] )/sum(pobre==1) #True positive rate

## ROC curve

roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  FPR <- colMeans(!Q[y==levels(y)[1],])
  TPR <- colMeans(Q[y==levels(y)[2],])
  plot(1-FPR, TPR, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}

roc(p=pred, y=pobre, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[pobre==0]), 
       y=mean((pred>.2)[pobre==1]), 
       cex=4, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[pobre==0]), 
       y=mean((pred>.5)[pobre==1]), 
       cex=4, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")



# Logit Caret

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...)) 



ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats, 
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(1410)

#Modelo 1

mylogit_caret <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                         vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                         vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                         vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+horas_trabajo, 
                       data = train_hogares_s, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial", 
                       metric = 'ROC')

mylogit_caret

#Modelo 2

mylogit_caret2 <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                          vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                          vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                          vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                          nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                          tipo_de_trabajoempleador+horas_trabajo, 
                       data = train_hogares_s,
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial", 
                       metric = 'ROC')

mylogit_caret2

#Modelo 3 ##¡PENDIENTE CUANDO TENGA Y_HAT

mylogit_caret_inc <- train(pobre~income+cabecera_restocabecera+cuartos_hogar+
                         vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                         vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                         vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                         nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                         tipo_de_trabajoempleador+horas_trabajo, 
                       data = train_hogares_s, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial", 
                       metric = 'ROC')

mylogit_caret_inc


#Out of sample prediction

# Modelo 1

predictTest_logit <- data.frame (
  predict(mylogit_caret, newdata = test_hogares_s, type="prob"),
  pred = predict(mylogit_caret, newdata = test_hogares_s, type = "raw")
)

head(predictTest_logit)
summary(predictTest_logit)

# Modelo 2

predictTest_logit2 <- data.frame (
  predict(mylogit_caret2, newdata = test_hogares_s, type="prob"),
  pred = predict(mylogit_caret2, newdata = test_hogares_s, type = "raw")
)

head(predictTest_logit2)
summary(predictTest_logit2)

# Modelo 3 #Pendiente cuando tenga la base

predictTest_logit <- data.frame (
  predict(mylogit_caret, newdata = test_hogares_s, type="prob"),
  pred = predict(mylogit_caret, newdata = test_hogares_s, type = "raw")
)

head(predictTest_logit)
summary(predictTest_logit) ## PENDIENTE CON BASE DE Y_HAT

#Sample submision model 1 

test_hogares <- cbind(test_hogares,predictTest_logit2)
test_hogares <- rename(test_hogares, pobre = pred)


sample_submission_1 <- test_hogares %>% 
  select(id, pobre)

table(sample_submission_1$pobre)
table(sample_submission$pobre)
table(train_hogares$Pobre)

library(dplyr)

sample_submission_1 <- sample_submission_1 %>%
  mutate(pobre = ifelse(pobre=="Si", 1,0))

setwd("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 2/Data")

write.csv(sample_submission_1, file="sample_submissionM1.csv", row.names = F)

#------LDA-----#

lda_fit = train(pobre~cuartos_hogar+edad+personas_hogar+tiempo_trabajando+
                  horas_trabajo+tam_emp, 
                data=train_hogares_s, 
                method="lda",
                trControl = ctrl,
                metric = 'ROC')

lda_fit

predictTest_lda <- data.frame(
  predict(lda_fit, newdata = test_hogares_s, type = "prob"),         ## predicted class probabilities
  pred = predict(lda_fit, newdata = test_hogares_s, type = "raw")    ## predicted class labels
)

head(predictTest_lda) 

## Revisar para subirlo 

###--------DESBALANCE DE CLASES-------####
 

#Cargar librerías 
require("pacman")
p_load("tidyverse")
 
prop.table(table(train_hogares_s$pobre))  
    ## La muestra desbalanceada datos de clase minoritaria estan en un 20% desbalance entre leve y moderado. 
#Division de la muestra
 
#First Split- Training set
 
set.seed(156)
split1 <- createDataPartition(train_hogares_s$pobre, p = .7)[[1]]
length(split1)
head(split1, n=20)
 
other     <- train_hogares_s[-split1,]
training  <- train_hogares_s[ split1,]

# Evaluación y prueba (testing)
set.seed(934)
split2 <- createDataPartition(other$pobre, p = 1/3)[[1]]
evaluation  <- other[ split2,]
testing     <- other[-split2,]


dim(training)
dim(testing)
dim(evaluation)

# Usamos Modelos 1 y 2 - para entrenarlo

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...)) 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats, 
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(1410)

#Modelo 1

mylogit_caret <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                         vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                         vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                         vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+horas_trabajo, 
                       data = training, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial", 
                       metric = 'ROC')

mylogit_caret

# Modelo 2

mylogit_caret2 <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                          vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                          vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                          vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                          nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                          tipo_de_trabajoempleador+horas_trabajo, 
                        data = training,
                        method = "glm",
                        trControl = ctrl,
                        family = "binomial", 
                        metric = 'ROC')

mylogit_caret2

##-----TUNNING-------###

##Modelo tunning - para maximizar capacidad predictiva del modelo

#logit - Podemos targetear metricas: p.e

#Aca nuestro target sera Specificity
set.seed(1410)
mylogit_caret2 <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                         vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                         vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                         vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                         nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                         tipo_de_trabajoempleador+horas_trabajo, 
                       data = training, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial", 
                       metric = 'Spec')

mylogit_caret2 ##No cambia mucho pues el modelo es bastante Naive Bayes

#Lasso ## 
#En lasso si debe cambiar pues aca debemos elegir paramentro lambda que optimiza una metrica
#Targetamos Specificity

lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300


set.seed(1410)

mylogit_lasso_spec <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                              vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                              vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                              vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                              nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                              tipo_de_trabajoempleador+horas_trabajo,
                            data = training, 
                            method = "glmnet",
                            trControl = ctrl,
                            family = "binomial", 
                            metric = "Spec",
                            tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                            preProcess = c("center", "scale")
)
mylogit_lasso_spec

##Targetamos Sensitivity

lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300


set.seed(1410)
mylogit_lasso_spec <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                              vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                              vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                              vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                              nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                              tipo_de_trabajoempleador+horas_trabajo,
                            data = training, 
                            method = "glmnet",
                            trControl = ctrl,
                            family = "binomial", 
                            metric = "Spec",
                            tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                            preProcess = c("center", "scale")
)
mylogit_lasso_spec

###------ACURACCY-----###

#Lasso
lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300


set.seed(1410)

mylogit_lasso_acc <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                             vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                             vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                             vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                             nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                             tipo_de_trabajoempleador+horas_trabajo,
                           data = training, 
                           method = "glmnet",
                           trControl = ctrl,
                           family = "binomial", 
                           metric = "Accuracy",
                           tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                           preProcess = c("center", "scale")
)
mylogit_lasso_acc

##------ROC----##

set.seed(1410)
mylogit_lasso_roc <- train(
  pobre~cabecera_restocabecera+cuartos_hogar+
    vivienda_propiapropia_pagando+vivienda_propiaarriendo+
    vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
    vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
    nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
    tipo_de_trabajoempleador+horas_trabajo, 
  data = training, 
  method = "glmnet",
  trControl = ctrl,
  family = "binomial", 
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
  preProcess = c("center", "scale")
)
mylogit_lasso_roc

###-----ALTERNATIVE CUT OFF------####

evalResults <- data.frame(pobre = evaluation$pobre)

evalResults$Roc <- predict(mylogit_lasso_roc,
                           newdata = evaluation,
                           type = "prob")[,1]

head(evalResults)

p_load("pROC")

rfROC <- pROC::roc(evalResults$pobre, evalResults$Roc, levels = rev(levels(evalResults$pobre)))
rfROC

rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh

evalResults<-evalResults %>% mutate(hat_def_05=ifelse(evalResults$Roc>0.5,"Si","No"), 
                                    hat_def_rfThresh=ifelse(evalResults$Roc>rfThresh$threshold,"Si","No"))

with(evalResults,table(pobre,hat_def_05))


with(evalResults,table(pobre,hat_def_rfThresh))

####--------REMUESTREO--------####

##Upsampling

set.seed(1103)
upSampledTrain <- upSample(x = training,
                           y = training$pobre,
                           ## keep the class variable name the same:
                           yname = "Pobre")
dim(training)

dim(upSampledTrain)

table(upSampledTrain$pobre)

set.seed(1410)
mylogit_lasso_upsample <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                                  vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                                  vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                                  vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                                  nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                                  tipo_de_trabajoempleador+horas_trabajo, 
                                data = upSampledTrain, 
                                method = "glmnet",
                                trControl = ctrl,
                                family = "binomial", 
                                metric = "ROC",
                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                                preProcess = c("center", "scale")
)
mylogit_lasso_upsample

##Downsampling

set.seed(1103)
downSampledTrain <- downSample(x = training,
                               y = training$pobre,
                               ## keep the class variable name the same:
                               yname = "Pobre")
dim(training)

dim(downSampledTrain)

table(downSampledTrain$pobre)

mylogit_lasso_doownsample <- train(pobre~cabecera_restocabecera+cuartos_hogar+
                                  vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                                  vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                                  vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                                  nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                                  tipo_de_trabajoempleador+horas_trabajo, 
                                data = downSampledTrain, 
                                method = "glmnet",
                                trControl = ctrl,
                                family = "binomial", 
                                metric = "ROC",
                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                                preProcess = c("center", "scale")
)
mylogit_lasso_doownsample

##Smote

p_load("smotefamily")

predictors<-c("cabecera_restocabecera","cuartos_hogar",
                "vivienda_propiapropia_pagando","vivienda_propiaarriendo",
                "vivienda_propiausufructo","vivienda_propiaposesión_sin_título",
                "vivienda_propiaotra","personas_hogar","hombrehombre","edad","nivel_educativosecundaria",
                "nivel_educativomedia","horas_trabajo","tipo_de_trabajoobre_gob","tipo_de_trabajocuet_propia",
                "tipo_de_trabajoempleador","horas_trabajo")
head( training[predictors])

smote_output = SMOTE(X = training[predictors],
                    target = training$pobre)
smote_data = smote_output$data
table(training$pobre)
table(smote_data$class)

set.seed(1410)

mylogit_lasso_smote <- train(class~cabecera_restocabecera+cuartos_hogar+
                                     vivienda_propiapropia_pagando+vivienda_propiaarriendo+
                                     vivienda_propiausufructo+vivienda_propiaposesión_sin_título+
                                     vivienda_propiaotra+personas_hogar+hombrehombre+edad+nivel_educativosecundaria+
                                     nivel_educativomedia+horas_trabajo+tipo_de_trabajoobre_gob+tipo_de_trabajocuet_propia+
                                     tipo_de_trabajoempleador+horas_trabajo, 
                                   data = smote_data, 
                                   method = "glmnet",
                                   trControl = ctrl,
                                   family = "binomial", 
                                   metric = "ROC",
                                   tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                                   preProcess = c("center", "scale")
)
mylogit_lasso_smote

class(mylogit_caret)
class(mylogit_lasso_roc)
testResults <- data.frame(pobre = testing$pobre)

testResults$logit<- predict(mylogit_caret,
                            newdata = testing,
                            type ="prob")[,1]

testResults$logit2<- predict(mylogit_caret2,
                            newdata = testing,
                            type ="prob")[,1]

