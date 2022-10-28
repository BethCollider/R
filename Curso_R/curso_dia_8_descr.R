#Cargas librerias o paquetes
library(tidyverse)
library(dplyr)
library(stringr)


#Cargar bases de datos 
library(readr)
library("googlesheets4")

#Para poder interpolar valores que no estan en la base de datos
library(imputeTS)

#cargar archivos de googlesheet

parcelas1 <- read_sheet("https://docs.google.com/spreadsheets/d/1kXH3t5rsc6MznhBGTzgbwfUrBPMwyIHKxWLbh1tmQtY/edit?usp=sharing", sheet = 1)
parcelas2 <- read_sheet("https://docs.google.com/spreadsheets/d/1kXH3t5rsc6MznhBGTzgbwfUrBPMwyIHKxWLbh1tmQtY/edit?usp=sharing", sheet = 2)
parcelas3 <- read_sheet("https://docs.google.com/spreadsheets/d/1kXH3t5rsc6MznhBGTzgbwfUrBPMwyIHKxWLbh1tmQtY/edit?usp=sharing", sheet = 3)

#inclui sitios separadas para comparar los valores
ME_1 <- parcelas1 %>% 
  filter(str_detect(CLAVE_SITIO , "ME_10_00001"))

ME_2 <- parcelas1 %>% 
  filter(str_detect(CLAVE_SITIO , "ME_10_00002"))
ME_10 <- parcelas1 %>% 
  filter(str_detect(CLAVE_SITIO , "ME_10_01813"))

ME_5 <- parcelas1 %>% 
  filter(str_detect(CLAVE_SITIO , "ME_10_00005"))

#--------------------------------------------------

      #---------Graficas-----------------
#Graficas Sitio ME_10_00001

grafica_ <- ggplot(ME_1, aes(DIAMETRO, ALTURA, color = str_c (GENERO, " ", ESPECIE))) +
 geom_point(size = 3)+
  labs(color = "ESPECIE")
grafica_

#GRAFICAS CON TODOS LOS SITIOS
grafica_S <- ggplot(parcelas1, aes(x, y, color = str_c (GENERO, " ", ESPECIE))) +
  geom_point(aes())+
  theme(axis.title = element_text(size = 18))+
  coord_equal(expand = FALSE, clip = "off")+
  labs(color = "ESPECIE")+
  lims(x = c(0,50), y = c(0,50))+
  theme_bw()+
  facet_wrap(~CLAVE_SITIO)

grafica_S


#sHANNON para comparar
ME_1 %>% 
  group_by(GENERO, ESPECIE) %>% 
  summarise(Ind = n()) %>% 
  ungroup() %>% 
  mutate(Pi=Ind/sum(Ind)) %>% 
  summarise(Shannon = -1*sum(Pi*log(Pi))) 
print(ME_1)

 ME_2 %>% 
  group_by(GENERO, ESPECIE) %>% 
  summarise(Ind = n()) %>% 
  ungroup() %>% 
  mutate(Pi=Ind/sum(Ind)) %>% 
  summarise(Shannon = -1*sum(Pi*log(Pi)))


ME_10 %>% 
  group_by(GENERO, ESPECIE) %>% 
  summarise(Ind = n()) %>% 
  ungroup() %>% 
  mutate(Pi=Ind/sum(Ind)) %>% 
  summarise(Shannon = -1*sum(Pi*log(Pi))) 


ME_5 %>% 
  group_by(GENERO, ESPECIE) %>% 
  summarise(Ind = n()) %>% 
  ungroup() %>% 
  mutate(Pi=Ind/sum(Ind)) %>% 
  summarise(Shannon = -1*sum(Pi*log(Pi))) 

#Shannon en conjunto de sitios

#-----el indice de Shannon es considerado como una metrica de la varianza de la distribucion de la abundancia de especies
  #----- por otra parte indica la probabilidad de un encuntro al azar en dos individuos marquen que sea de la misma especie

Clave<- parcelas1 %>% 
  
 group_by(CLAVE_SITIO, GENERO, ESPECIE) %>% 
  summarise(Ind = n()) %>% 
  ungroup() %>% 
  group_by(CLAVE_SITIO) %>% 
  mutate(Pi=Ind/sum(Ind)) %>% 
  summarise(Shannon = sum(-1*sum(Pi*log(Pi))))

print(Clave)

#---interpretando los resultados valores por arriba de 3 se consideran como sitios diversos
#------Tomando esta refrencia para describir lo sitios, ME_10_0005 presenta un sitio  mas diversas pero con menos dominancia


#----fuentes https://ciespinosa.github.io/AlphaDiversidad/medidas-de-diversidad.html


