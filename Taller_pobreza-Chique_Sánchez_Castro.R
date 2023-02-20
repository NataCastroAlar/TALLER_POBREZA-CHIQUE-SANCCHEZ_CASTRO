
library(readr)
library(pacman)
library(stargazer)
p_load(tidyverse,rvest,skimr) 
train_hogares <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/train_hogares.csv")
train_personas <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/train_personas.csv")
base_train<-merge(train_hogares, train_personas, by="id")


##Renombramos variables
base_train<-base_train %>% 
  rename(cuartos_hogar = P5000,
         vivienda_propia = P5090,
         arriendo_estimado_vivienda = P5130,
         arriendo = P5140,
         personas_hogar= Nper,
         personas_unidad_gasto = Npersug,
         ingreso_total_unidad_gasto = Ingtotug,
         estrato= Estrato1,
         hombre = P6020,
         edad = P6040,
         parentesco_jefe = P6050,
         entidad_salud =P6090,
         nivel_educativo = P6210,
         tipo_de_trabajo = P6430,
         horas_trabajo= P6800,
         tiene_segunda_actividad=P7040, 
         )

#Variables - Labels Factores
base_train<-base_train%>%
  mutate(ciudad_cabecera=factor(Clase.x, levels=c(1, 2), labels=c("cabecera", "resto")),
         capital_cabe_otro=factor(Dominio.x, levels=c(1, 2), labels=c("cabecera", "resto")),
         vivienda_propia=factor(vivienda_propia, levels=c(1,2,3,4,5,6), labels=c("propia_pagada", "propia_pagando", "arriendo", "usufructo", "posesión_sin_título", "otra")),
         hombre= factor(hombre, levels=c(1,2), labels=c("hombre", "mujer")),
         parentesco_jefe= factor(parentesco_jefe, levels=c(1,2), labels=c("hombre", "mujer"))
           )

#Base Final:
train_final<-base_train%>%
  select(Pobre, Lp, ingreso_total_unidad_gasto, cuartos_hogar, vivienda_propia, arriendo, arriendo_estimado_vivienda,
         personas_hogar, personas_unidad_gasto, ingreso_total_unidad_gasto, estrato, hombre, edad, parentesco_jefe,
         entidad_salud, nivel_educativo, tipo_de_trabajo, horas_trabajo, tiene_segunda_actividad, ciudad_cabecera,
         capital_cabe_otro, vivienda_propia)