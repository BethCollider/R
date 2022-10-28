#Cargas librerias o paquetes
library(tidyverse)
library(dplyr)
library(stringr)

#Cargar archivos de excel en R
#library(readxl)

#Cargar bases de datos 
library(readr)
setwd("~/Curso_R")
##datos <- read.sheet"BaseDatosCurso_5.xlsx", sheet = 1)
dat <- read.csv("BaseDatosCurso.csv")
dat_ <-  read.csv("um.csv")
sup <- read.csv("sup.csv")
ecu <- read.csv("ecu.csv")


#Unir dos tablas por Sitio
dat_n <- inner_join(dat, dat_ , by = "Sitio")

dat_n %>%
  filter(str_detect(Esp, "Pinus|Quercus")) %>%
  mutate(AB = (3.141516/40000)*(Dn^2))%>%
  group_by(Esp)%>%
  summarise(ABE = sum(AB))


## unir base de datos dat_ y ecu

vol <- inner_join(dat_n, ecu , by = c ("Esp" = "Especie"))

#Volumen total 

vol %>%
mutate (VT =vol$a0*vol$Dn^vol$a1*vol$At*vol$a2)



