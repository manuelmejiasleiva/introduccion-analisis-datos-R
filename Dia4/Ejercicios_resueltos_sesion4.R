
# Ejercicios resueltos - Sesión 4 -----------------------------------------

##Información sobre la base de datos: https://github.com/marespadafor/panelmadrid/blob/main/panel_information.md

# Ejercicio 1: regresión lineal simple ------------------------------------

## Punto 1: cargar librerías, definir directorio, cargar datos y primer resumen estadístico

library(tidyverse) #librería para limpieza de datos
library(haven) #abrir archivo .dta
library(ggeffects) #para obtener y visualizar las probabilidades pronosticadas


setwd("~/Desktop/Intro-R/Dia4")

dfescuelas <- read_dta("madrid_schools.dta")

summary(dfescuelas)

#variable dependiente: "dfescuelas$eso1718" (porcentaje de estudiantes matriculados en ESO)
#variable independiente: "average_household_income" (ingresos medio de los hogares en cada distrito)



## Punto 2: gráfico de dispersión

dfescuelas %>%
  ggplot(aes(x = average_household_income, y = eso1718)) +
  geom_point() +
  geom_smooth(method = "loess")



## Punto 3 y 4: modelo de regresión lineal simple

modelo_simple <- lm(eso1718 ~ average_household_income,
                    data = dfescuelas)
summary(modelo_simple) #salida para interpretar el modelo

## Punto 5: predicción 
ggeffects::ggeffect(modelo_simple, terms = "average_household_income") %>%
  plot()




# Ejercicio 2: regresión lineal multivariante -----------------------------


## Punto 1: modelo de regresión multivariante (con interacción)

###Previamente se aconseja limpiar la variable de tipo de colegio. Por ejemplo:
dfescuelas <- dfescuelas %>%
  mutate(tipo_colegio = case_when(
    type2 == "PË\u0099blico" ~ "Publico",
    type2 %in% c("Privado Concertado","Privado") ~ "Privado-concertado"),
    tipo_colegio = as.factor(tipo_colegio))



model_interaccion <- lm(eso1718 ~ average_household_income*tipo_colegio,
                        data = dfescuelas)
summary(model_interaccion)



## Punto 2: predicción

ggeffect(model_interaccion,
         terms = c("average_household_income","tipo_colegio")) %>%
  plot() #añadiendo con el pipe la función de plot pinta los valores predichos
