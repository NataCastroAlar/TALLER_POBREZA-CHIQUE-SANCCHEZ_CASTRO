
rm(list = ls())

library(readr)
library(pacman)
library(stargazer)
p_load(tidyverse,rvest,skimr, caret, MLmetrics, smotefamily, AER) 
train_hogares <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/train_hogares.csv")
train_personas <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/train_personas.csv")
test_hogares <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/test_hogares.csv")
test_personas <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/test_personas.csv")


#base_train<-merge(train_hogares, train_personas, by="id")

#-------------------------------------------------------------------------------------------
## ----- TRAIN HOGARES: Extrayendo variables de condiciones del jefe de hogar de la base train_personas

cond_jefe_hog_train <- train_personas %>%
  group_by(id) %>%
  filter(Orden ==1) %>%
  select(P6020, P6040, P6050, P6090, P6210, P6426, P6430, P6800, P7090, P6870)

summary(cond_jefe_hog_train)

## * Uniendo condiciones del jefe de hogar con la base train_hogares

train_hogares_final<- left_join(train_hogares, cond_jefe_hog_train)

colnames(train_hogares_final)
summary(train_hogares_final)

## variable ingreso y cálculo de pobreza

colnames(train_hogares_final)
colnames(train_personas)[1:2]

sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)

train_hogares_final<-left_join(train_hogares_final,sum_ingresos)
colnames(train_hogares_final)

head(train_hogares_final[c("id","Ingtotugarr","Ingtot_hogar")])

table(train_hogares_final$Pobre) #La mayoría es no pobre

#-------------------------------------------------------------------------------------------
## --- TEST HOGARES: Extrayendo variables de condiciones del jefe de hogar de la base test_personas

cond_jefe_hog_test <- test_personas %>%
  group_by(id) %>%
  filter(Orden ==1) %>%
  select(P6020, P6040, P6050, P6090, P6210, P6426, P6430, P6800, P7090, P6870)

summary(cond_jefe_hog_test)

## * Uniendo condiciones del jefe de hogar con la base test_hogares

test_hogares_final <- left_join(test_hogares, cond_jefe_hog_test)

colnames(test_hogares_final)

#-------------------------------------------------------------------------------------------
## Renombrando Variables
## Para entrenamiento
train_hogares_final<-train_hogares_final %>% 
  rename(cuartos_hogar = P5000,
         cuartos_hogar_ocup = P5010,
         vivienda_propia = P5090,
         arriendo = P5140,
         personas_hogar= Nper,
         personas_unidad_gasto = Npersug,
         ingreso_total_unidad_gasto = Ingtotug,
         hombre = P6020,
         edad = P6040,
         parentesco_jefe = P6050,
         entidad_salud =P6090,
         nivel_educativo = P6210,
         tiempo_trabajando = P6426,
         tipo_de_trabajo = P6430,
         horas_trabajo = P6800,
         mas_trabajo = P7090,
         tam_emp = P6870,
         cabecera_resto = Clase
  )

##Para testeo:
test_hogares_final<-test_hogares_final %>% 
  rename(cuartos_hogar = P5000,
         cuartos_hogar_ocup = P5010,
         vivienda_propia = P5090,
         arriendo = P5140,
         personas_hogar= Nper,
         personas_unidad_gasto = Npersug,
         hombre = P6020,
         edad = P6040,
         parentesco_jefe = P6050,
         entidad_salud =P6090,
         nivel_educativo = P6210,
         tiempo_trabajando = P6426,
         tipo_de_trabajo = P6430,
         horas_trabajo = P6800,
         mas_trabajo = P7090,
         tam_emp = P6870,
         cabecera_resto = Clase
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
         cuartos_hogar=factor(cuartos_hogar),
         personas_hogar=factor(personas_hogar),
         entidad_salud=factor(entidad_salud, levels=c(0,1), labels=c("No salud","Sí salud")),
         nivel_educativo=factor(nivel_educativo),
         tipo_de_trabajo=factor(tipo_de_trabajo),
         mas_trabajo=factor(mas_trabajo, levels=c(0,1), labels=c("Si más trabajo","No más trabajo")),
         tam_emp=factor(tam_emp)
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
         cuartos_hogar=factor(cuartos_hogar),
         personas_hogar=factor(personas_hogar),
         entidad_salud=factor(entidad_salud, levels=c(0,1), labels=c("No salud","Sí salud")),
         nivel_educativo=factor(nivel_educativo),
         tipo_de_trabajo=factor(tipo_de_trabajo),
         mas_trabajo=factor(mas_trabajo, levels=c(0,1), labels=c("Si más trabajo","No más trabajo")),
         tam_emp=factor(tam_emp)
  )

colnames(test_hogares_final)

summary(test_hogares_final)
#--------------------------------------------
##--------------------------------------------
###BASE TRAIN PARA MODELO

base_modelo<-select(train_hogares_final, pobre, Ingtot_hogar, cabecera_resto, vivienda_propia, hombre, cuartos_hogar,
                    personas_hogar, entidad_salud, nivel_educativo, tipo_de_trabajo, tam_emp,edad, tiempo_trabajando,
                    mas_trabajo )

##CREACION DE DUMMYS Y ESTANDARIZACION DE VARIABLES CONITNUAS

p_load("caret")
train_dummy<-dummyVars(formula= ~ .+ I(edad^2) - 1, data = base_modelo, fullrank=T) 

#Nueva base con las dummies:
train_dummy_final<-data.frame(predict(train_dummy, newdata=base_modelo))

#Eliminamos una de las variables dummies de cada variable para que quede como base (multicolinealidad):
multicol<-lm(formula=pobre.Sí.pobre ~ . , train_dummy_final)
summary(multicol)
multicol_NA<-names(multicol$coefficients[is.na(multicol$coefficients)])
multicol_NA
train_final<-train_dummy_final%>%
  data.frame()%>%
  select(-all_of(multicol_NA))

#Estandarizamos variables numéricas para Lasso y Ridge:

train_estandarizada<-train_final
glimpse(train_estandarizada)

variables_numericas<-c("Ingtot_hogar", "edad", "tiempo_trabajando", "I.edad.2.")
escalador<-preProcess(train_estandarizada[,variables_numericas],
                      method = c("center", "scale"))

train_estandarizada[,variables_numericas]<- predict(escalador, train_estandarizada[,variables_numericas])


##verificamos estandarización: por ejemplo la media de la variable edad es cero y la desviación estandar 1. 
mean(train_estandarizada$edad)
sd(train_estandarizada$edad)

##Recodificamos la variable pobre para que quede categórica:

train_estandarizada<-train_estandarizada%>%
  rename(pobre = pobre.Sí.pobre)

train_estandarizada<-train_estandarizada%>%
  mutate(pobre=factor(pobre, levels=c(0,1), labels=c("No pobre", "Si pobre")))

train_estandarizada<-train_estandarizada[-c(1)]
#----------------------------------------------
##--------------------------------------------
###BASE TEST PARA MODELO

base_test_modelo<-select(test_hogares_final, cabecera_resto, vivienda_propia, hombre, cuartos_hogar,
                    personas_hogar, entidad_salud, nivel_educativo, tipo_de_trabajo, tam_emp,edad, tiempo_trabajando,
                    mas_trabajo )

##CREACION DE DUMMYS Y ESTANDARIZACION DE VARIABLES CONITNUAS

p_load("caret")
test_dummy<-dummyVars(formula= ~ .+ I(edad^2) - 1, data = base_test_modelo, fullrank=T) 

#Nueva base con las dummies:
test_dummy_final<-data.frame(predict(test_dummy, newdata=base_test_modelo))

#Eliminamos una de las variables dummies de cada variable para que quede como base (multicolinealidad):
multicol_test<-lm(formula=pobre.Sí.pobre ~ . , test_dummy_final)
summary(multicol)
multicol_test_NA<-names(multicol$coefficients[is.na(multicol$coefficients)])
multicol_test_NA
test_final<-test_dummy_final%>%
  data.frame()%>%
  select(-all_of(multicol_test_NA))

#Estandarizamos variables numéricas para Lasso y Ridge:

train_estandarizada<-train_final
glimpse(train_estandarizada)

variables_numericas<-c("Ingtot_hogar", "edad", "tiempo_trabajando", "I.edad.2.")
escalador<-preProcess(train_estandarizada[,variables_numericas],
                      method = c("center", "scale"))

train_estandarizada[,variables_numericas]<- predict(escalador, train_estandarizada[,variables_numericas])


##verificamos estandarización: por ejemplo la media de la variable edad es cero y la desviación estandar 1. 
mean(train_estandarizada$edad)
sd(train_estandarizada$edad)

##Recodificamos la variable pobre para que quede categórica:

train_estandarizada<-train_estandarizada%>%
  rename(pobre = pobre.Sí.pobre)

train_estandarizada<-train_estandarizada%>%
  mutate(pobre=factor(pobre, levels=c(0,1), labels=c("No pobre", "Si pobre")))

train_estandarizada<-train_estandarizada[-c(1)]


##----------------------------------------------------------------------
##MODELOS

summary(train_estandarizada)
glimpse(train_estandarizada)
sum(is.na(train_estandarizada$pobre))

set.seed(1234)

  
modelo_1<-train(pobre ~ .,
                data=train_estandarizada,
                method= "glm",
                trControl = ctrl,
                family = "binomial",
                metric = 'ROC')
  
  
modelo_2<-train(x=select(train_estandarizada, -pobre),
                y=train_estandarizada$pobre,
                preProcess=NULL,
                method="glmnet")

#Predicción en entrenamiento y en testeo:

pobre_hat_train<- predict(modelo_2, train_estandarizada)


set.seed(1234)
logit_caret_train<-train(pobre ~.- pobre.Sí.pobre - pobre.No.pobre,
                                 data=train_dummy_final,
                                 method = "glm",
                                 trControl=ctrl,
                                 family ="binomial",
                                 metric = 'ROC')



##Gráfica pobres - no pobres
ggplot(train_hogares_final, aes(x=pobre)) +
  geom_bar(fill = "green")+
  theme_bw()+
  labs(x="", y= "Frecuencia", title="Porcentaje de personas en estado de pobreza")

prop.table(table(train_hogares_final$pobre)) #Porcentaje de hogares en situación de pobreza - Desbalance

glimpse(train_hogares_final) #variables de la base

summary(train_hogares_final$parentesco_jefe)
glimpse(train_hogares_final$Depto)



##



  
  
##Outliers
##Cuartos hogar
#Calculamos el top 1% y lo elimnamos- top (top one percent):  
top_cuartos_hogar<-quantile(train_hogares_final$cuartos_hogar, .99)
top_cuartos_hogar

train_hogares_final<-train_hogares_final%>%
  filter(cuartos_hogar<top_cuartos_hogar)%>%
  dim()
  
  
train_logit<-glm(pobre~ cuartos_hogar, data = train_hogares_final, family = "binomial"))
  
  
  
  
  
  
  
  







##
