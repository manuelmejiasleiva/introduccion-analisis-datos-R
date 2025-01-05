###############################################################
###################  Ejercicios - Sesión 2 #################### 
###############################################################


# 1. Introducción ---------------------------------------------------------

# En este ejercicio, vamos a preparar los datos de la Encuesta de Educación y Hogares de Andalucía del año 2010. 
# Esta encuesta proporciona información sobre los hijos, padres y hogares. 
# Puedes obtener más información en el siguiente enlace: 
# <https://www.juntadeandalucia.es/institutodeestadisticaycartografia/descarga/encSocial/2010/encSocial2010.htm>
#   
# El objetivo de este ejercicio es limpiar una base de datos y prepararla para su posterior análisis. 
# Para ello, vamos a obtener una base de datos que incluya las siguientes variables:
#   
# - Rendimiento educativo: puntuación en el test de matemáticas.
# 
# - Variables sociodemográficas básicas: sexo del adolescente y cohorte de nacimiento.
# 
# - Variables de contexto socioeconómico: nivel educativo de los padres.
# 
# - Una variable de pesos poblacionales.



# 2.  Primeros pasos ------------------------------------------------------

## 2.1.


## 2.2.
rm(list = ls())

## 2.3.
setwd("~/Desktop/Intro-R/Dia2")


## 2.4. 
library(tidyverse)

## 2.5.
esoc <- read_csv("ESOC.csv")


# 3.  Limpiando los datos -------------------------------------------------

## 3.1.


## 3.2.
df_esoc <- esoc %>%
  select(SEXO_EGO,SUBP,STUDIOSC,RMATE,FEIR)

## 3.3.1.
df_esoc <- df_esoc %>% 
  transmute(
    sexo = case_when(
      SEXO_EGO == 1 ~ "chico",
      SEXO_EGO == 6 ~ "chica"),
    sexo = as.factor(sexo),
    # 3.3.2.
    cohorte = case_when(
      SUBP == 1 ~ "1994",
      SUBP == 2 ~ "1998"),
    cohorte = as.factor(cohorte),
    # 3.3.3.
    estudios_padres = case_when(
      STUDIOSC %in% c(1:3) ~ "Bajo",
      STUDIOSC %in% c(4,5) ~ "Medio",
      STUDIOSC %in% c(6) ~ "Alto"),
    estudios_padres = as.factor(estudios_padres),
    # 3.3.4.
    test_mates = as.numeric(if_else(RMATE == -1, NA, RMATE)),
    # 3.3.5.
    pesos = as.numeric(FEIR)) 



# 4.  Análisis descriptivo de los datos -----------------------------------

## 4.1.
df_esoc %>% 
  summarise_all(~sum(is.na(.)))

df_esoc <- df_esoc %>% 
  drop_na()

## 4.2.
df_secundaria <- df_esoc %>%
  filter(cohorte == "1994") 

df_primaria <- df_esoc %>%
  filter(cohorte == "1998") 


## 4.3.
df_secundaria %>%
  summarise(mean(test_mates))

df_primaria %>%
  summarise(mean(test_mates))

## 4.4.
df_esoc %>%
  filter(cohorte == "1994") %>%
  group_by(sexo) %>%
  summarise(media = weighted.mean(test_mates, w = pesos))


## 4.5.
df_esoc %>%
  filter(cohorte == "1998") %>%
  group_by(sexo,estudios_padres) %>%
  summarise(media = weighted.mean(test_mates, w = pesos)) %>%
  arrange(desc(media))

df_esoc %>%
  filter(cohorte == "1994") %>%
  group_by(sexo,estudios_padres) %>%
  summarise(media = weighted.mean(test_mates, w = pesos)) %>%
  arrange(desc(media))
