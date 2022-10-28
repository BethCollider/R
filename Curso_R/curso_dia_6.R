#Cargas librerias o paquetes
library(tidyverse)
library(dplyr)
library(stringr)


#Cargar bases de datos 
library(readr)
library("googlesheets4")

#Para poder interpolar valores que no estan en la base de datos
library(imputeTS)



sitio <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit#gid=0", sheet = 1)
UM <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit#gid=0", sheet = 2)
Sup <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit#gid=0", sheet = 3)
modelos <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit#gid=0", sheet = 4)

#glimpse(sitio)

#Buscar valores na
valorNA <- filter(sitio, is.na(Dn)|is.na(At))
View(valorNA)

#cuando existen na en la base de datos se suguiere interpolar los valores
sitio1<- sitio %>% 
na_interpolation(method="linear")


#Determinar las diferentes especies dentro de la base de datos

sitio %>% 
distinct(Esp) %>% 
arrange(Esp)

sitio1_<- sitio1 %>% 
  mutate(Grupo = case_when(str_detect(Esp , "Pinus|Juniperus")~ "Conifere", TRUE ~ "Latifoliada"))

EspSit <- sitio1 %>% 
  distinct(Esp)

EspMode <- modelos %>% 
  distinct(Especie)


sitio1_ %>% 
  inner_join(modelos, by = c("Esp"="Especie")) %>% 
  mutate(vol = a0*Dn^a1*At^a2) %>% 
  group_by(Sitio,Esp) %>% 
  summarise(Vt = sum(vol))
