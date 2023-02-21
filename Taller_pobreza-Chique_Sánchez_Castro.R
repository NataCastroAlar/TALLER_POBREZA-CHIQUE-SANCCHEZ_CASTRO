
# Limpiar todo

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

##BASE VICTOR

## ----- TRAIN HOGARES: Extrayendo variables de condiciones del jefe de hogar de la base train_personas

cond_jefe_hog_train <- train_personas %>%
  group_by(id) %>%
  filter(Orden ==1) %>%
  select(Estrato1, P6020, P6040, P6050, P6090, P6210, P6426, P6430, P6800, P7090, P6870)

summary(cond_jefe_hog_train)

## * Uniendo condiciones del jefe de hogar con la base train_hogares

train_hogares_final<- left_join(train_hogares, cond_jefe_hog_train)

colnames(train_hogares_final)
summary(train_hogares_final)


## --- TEST HOGARES: Extrayendo variables de condiciones del jefe de hogar de la base test_personas

cond_jefe_hog_test <- test_personas %>%
  group_by(id) %>%
  filter(Orden ==1) %>%
  select(P6020, P6040, P6050, P6090, P6210, P6426, P6430, P6800, P7090, P6870)

summary(cond_jefe_hog_test)

## * Uniendo condiciones del jefe de hogar con la base test_hogares

test_hogares_final <- left_join(test_hogares, cond_jefe_hog_test)

colnames(test_hogares)

## Renombrando Variables

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


## variable ingreso y calculo de pobreza

colnames(train_hogares_final)
colnames(train_personas)[1:2]

sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)

train_hogares_final<-left_join(train_hogares_final,sum_ingresos)
colnames(train_hogares_final)

head(train_hogares_final[c("id","Ingtotugarr","Ingtot_hogar")])

table(train_hogares_final$Pobre) #La mayoría es no pobre

#pasamos variables dicotomas a 0 y 1

train_hogares_final<-train_hogares_final %>%
  mutate(entidad_salud = if_else(entidad_salud == 1, 1, 0), 
         cabecera_resto = if_else(cabecera_resto == 1, 1, 0),
         hombre = if_else(hombre == 1, 1, 0),
         mas_trabajo = if_else(mas_trabajo == 1, 1, 0)
         )


#Variables categóricas - Labels Factores
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

##--------------------------------------------
###BASE PARA MODELO

base_modelo<-select(train_hogares_final, pobre, Ingtot_hogar, cabecera_resto, vivienda_propia, hombre, cuartos_hogar,
                    personas_hogar, entidad_salud, nivel_educativo, tipo_de_trabajo, tam_emp,edad, tiempo_trabajando,
                    mas_trabajo )



######MODELO LOGISTICO

##CREACION DE DUMMYS



p_load("caret")
train_dummy<-dummyVars("~ .", data = train_hogares_final)
head(train_dummy)



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
