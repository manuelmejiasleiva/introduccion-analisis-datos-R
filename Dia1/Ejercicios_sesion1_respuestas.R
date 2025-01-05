###############################################################
###################  Ejercicios - Sesión 1 #################### 
###############################################################


# 1. Introducción ---------------------------------------------------------


## La documentación sobre las variables que contiene esta base de datos 
## se encuentra disponible en el siguiente enlace: 
## https://vincentarelbundock.github.io/Rdatasets/doc/AER/GSOEP9402.html



# 2.  Primeros pasos ------------------------------------------------------


## 2.1.

rm(list = ls())

## 2.2.
library(tidyverse)
library(janitor)

## 2.3.
setwd("Desktop/Intro-R/Dia1")

## 2.4.
data <- read_csv("GSOEP9402.csv")


# 3. Observando la estructura de los datos --------------------------------

## 3.1.
head(data)

## 3.2.
glimpse(data)

## 3.3.
colSums(is.na(data))


# 4. Análisis exploratorio de los datos -----------------------------------

## 4.1.
mean(data$meducation)


## 4.2.
median(data$income)


## 4.3.
aggregate(meducation ~ school, data = data, mean)


## 4.4.
aggregate(income ~ school, data = data, mean)



## 4.5.
data %>% 
  tabyl(school, marital) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()