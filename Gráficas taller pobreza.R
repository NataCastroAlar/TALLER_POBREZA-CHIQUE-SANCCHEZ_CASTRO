
##GRAFICAS TALLER 2: POBREZA. GRUPO 6. CHIQUE - SANCHEZ - CASTRO
# Limpiar todo

rm(list = ls())

# Librerias

library(pacman)

p_load(tidyverse, readr, skimr, fastDummies, caret, glmnet, MLmetrics)

# Importar datos

train_hogares <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/train_hogares.csv")
train_personas <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/train_personas.csv")
test_hogares <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/test_hogares.csv")
test_personas <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Datos Pobreza Colombia/test_personas.csv")


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


names(train_hogares_final)

ggplot(data = train_hogares_final,
       mapping = aes(x = horas_trabajo, y = income, color=Pobre)) +
  geom_point()
##-------------------------
##--------------------------
#Cuadro Estadísticas descriptivas

library(stargazer)
p_load(stargazer, tidyverse,rvest,skimr)

stargazer(as.data.frame(train_hogares_final), type="text", title= "Estadísticas",
          digits=3, summary.stat=c("min", "p25", "mean", "p75", "max", "median", "sd"))


##Gráfica Ingreso - Horas de Trabajo
names(train_hogares_final)

ggplot(train_hogares_final,
       mapping = aes(x = horas_trabajo, y = income, color=Pobre)) +
  geom_point(aes(colour = factor(Pobre)), size=3)+
  labs(title = "Ingreso - Horas de trabajo",
       y = "Ingreso")

##--------------------------------
##--------------------------------

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

####----------------------------------------------------------
###-----------------------------------------------------------
##GRAFICOS DE BARRAS 


prop.table(table(train_hogares_final$Pobre))

#Pobres
ggplot(train_hogares_final, aes(x=Pobre)) + 
  geom_bar(aes(x=Pobre), fill="steelblue")+
  labs(title = "Personas en Situación de Pobreza")+
  scale_x_discrete(limit=c(0,1), labels = c("No Pobre", "Pobre"))

#Cabecera-Entidad Salud
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(cabecera_resto), fill=entidad_salud))

#Cabecera
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(cabecera_resto), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Cabecera y Resto", x="")+
  labs(fill="Pobre")
 
#Cuartos Hogar 
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(cuartos_hogar), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Cuartos Hogar", x="")+
  labs(fill="Pobre")+
  scale_x_discrete(limit = c(1,2,3,4,5,6,7,8))

#Género
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(sexo), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Género", x="")+
  labs(fill="Pobre")+
  coord_flip()

#Nivel educativo
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(nivel_educativo), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Nivel Educativo", x="")+
  labs(fill="Pobre")+
  coord_flip()

ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(nivel_educativo), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Nivel Educativo", x="")+
  labs(fill="Pobre")+
  scale_x_discrete(limit = c("ninguno", "primaria", "secundaria", "media", "superior"))+
  coord_flip()

#Tipo de trabajo
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(tipo_de_trabajo), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Tipo de Trabajo", x="")+
  labs(fill="Pobre")+
  coord_flip()

#Vivienda Propia
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(vivienda_propia), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Vivienda Propia", x="")+
  labs(fill="Pobre")+
  coord_flip()


#Entidad de salud
ggplot(train_hogares_final) + 
  geom_bar(aes(x =as.factor(entidad_salud), fill=as.factor(Pobre)))+
  labs(title = "Pobres - No Pobres: Tiene Servicio de Salud", x="")+
  labs(fill="Pobre")


##---------------------------------------------------------


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


#-----------------------------------------


###GRAFICAS#####
###Personas en situación de Pobreza

ggplot(train_hogares_final, aes(x=Pobre))+
  geom_bar(fill="darkblue")+
  theme_bw()+
  labs(x="", y="frecuencia", title= "Personas en Situación de Pobreza")

prop.table(table(train_hogares_final$Pobre))

##Gráficas de correlación de variables continuas

library(GGally)

##Gráfica logaritmo ingreso

train_hogares_final<-train_hogares_final%>%
  mutate(log_ingreso=log(income))

ggplot(train_hogares_final, aes(x = log_ingreso, fill = Pobre )) + 
  geom_histogram()+
  labs(title = "Ingreso",
       y = "Frecuencia")

ggpairs(train_hogares_final, columns=c(2,8, 11), ggplot2::aes(colour = pobre))

colnames(train_hogares_final)

class(train_hogares_final$Pobre)

##Gráficas de variables categóricas.
library(gridExtra)
p_load(gridExtra)

variables_categoricas <- names(train_hogares_final[,sapply(train_hogares_final, is.factor)])
variables_categoricas

for (var in variables_categoricas) {
  
  p2 <- ggplot(train_hogares_final, aes(y = .data[[var]])) +
    geom_bar(aes(x = (..count..)/sum(..count..)),
             fill = "darkblue") +
    labs(title = paste("Distribución de la variable", var),
         x = "Proporción (%)") +
    scale_x_continuous(labels = scales::percent) +
    theme_bw() 
  
  grid.arrange(p2, ncol = 1)
}




