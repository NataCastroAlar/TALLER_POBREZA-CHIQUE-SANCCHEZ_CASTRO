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

# Librerias

library("pacman", "readr", "skimr")

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

## ----- Uniendo condiciones del jefe de hogar con la base train_hogares

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

## Eliminando variables con muchos NAs

# * Test_hogares
test_hogares$P5100 <- NULL 
test_hogares$P5130 <- NULL
test_hogares$P5140 <- NULL

test_hogares<-na.omit(test_hogares)

# * Train_hogares
train_hogares$P5100 <- NULL 
train_hogares$P5130 <- NULL
train_hogares$P5140 <- NULL

train_hogares<-na.omit(train_hogares)

head(test_hogares)
str(test_hogares)
summary(test_hogares)
skim(test_hogares)

head(train_hogares)
str(train_hogares)
summary(train_hogares)
skim(train_hogares)

## Renombrando Variables

train_hogares<-train_hogares %>% 
  rename(cuartos_hogar = P5000,
         cuartos_hogar_ocup = P5010,
         vivienda_propia = P5090,
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
         tam_emp = P6870
  )

test_hogares<-test_hogares %>% 
  rename(cuartos_hogar = P5000,
         cuartos_hogar_ocup = P5010,
         vivienda_propia = P5090,
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
         tam_emp = P6870
  )




