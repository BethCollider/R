#Cargas librerias o paquetes
library(tidyverse)
library(dplyr)
library(stringr)

#Cargar bases de datos 
library(readr)
setwd("~/Curso_R")
datos <- read.csv("Ejercicio1.csv")
dat <- read.csv("BaseDatosCurso.csv")

#Analisis de la base de datos
dat %>%
  
  filter( str_detect(Esp, "Pinus"))%>%
  mutate(AB = ((3.141516/4)*(Dn^2)/10000))%>%
  arrange(Sitio, desc(AB))%>%
  group_by(Sitio)
