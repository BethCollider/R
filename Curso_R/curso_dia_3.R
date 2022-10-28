#Cargas librerias o paquetes
library(tidyverse)
library(dplyr)
library(stringr)

#Cargar bases de datos 
library(readr)
setwd("~/Curso_R")
datos <- read.csv("Ejercicio1.csv")
dat <- read.csv("BaseDatosCurso.csv")
dat_ <-  read.csv("um.csv")


# ISW por especie
dat_ %>%
  
  mutate (AB = (3.141516/40000)*(Dn^2))%>%
  group_by(Esp) %>%
  summarize(ABN = n()) %>%
  mutate( ABNR = ABN / sum(ABN))%>%
  mutate(ISW = (sum(ABNR)*log(ABNR))*-1)

#Unir dos tablas por Sitio
 dat_n <- inner_join(dat, dat_ , by = "Sitio")
 
 
 #ISW por sitio
 dat_n %>%
   
   mutate (AB = (3.141516/40000)*(Dn^2))%>%
   group_by(Sitio) %>%
   summarize(ABN = n()) %>%
   mutate( ABNR = ABN / sum(ABN))%>%
   mutate(ISW = (sum(ABNR)*log(ABNR))*-1)
 
 #ISW por UM
 dat_n %>%
   
   mutate (AB = (3.141516/40000)*(Dn^2))%>%
   group_by(UM) %>%
   summarize(ABN = n()) %>%
   mutate( ABNR = ABN / sum(ABN))%>%
   mutate(ISW = (sum(ABNR)*log(ABNR))*-1)
 



            



  