ibrary("googlesheets4")
library(tidyverse)


sitio <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 1)
UM <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 2)
Sup <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 3)
modelos <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 4)
claves <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 5)
glimpse(sitio)

filter(sitio, Sitio==1)

filter(sitio, Sitio==1|Sitio==7)
filter(sitio, Sitio%in%c(1,7))

v1 <- filter(sitio, str_detect(Esp, "Pinus|Quercus"))

sitios1 <- filter(sitio, Sitio==1)
sitios2 <- filter(sitio, Sitio==2)

sitios12 <- bind_rows(sitios1, sitios2)
sitios1 <- rename(sitios1, Lugar="Sitio")

bind_cols(sitios1, sitios12)

filter(sitio, Sitio==1)

sitio %>% 
  filter(Sitio==1)

arrange(sitio, desc(Sitio))


sitio %>% 
  mutate(AreaBasal=pi/40000*Dn^2)

mutate(sitio, AreaBasal=pi/40000*Dn^2)

v1 <- select(sitio, -Sitio:-Dn)


x <- c(1:10)

sitio %>% 
  filter(str_detect(Esp, "Pinus")) %>% 
  mutate(AreaBasal=pi/40000*Dn^2) %>% 
  group_by(Esp) %>% 
  summarise(AreabasalEsp=sum(AreaBasal))

sitios1 <- inner_join(sitio, modelos, by=c("Esp"="Especie"))

anti_join(sitio, modelos, by=c("Esp"="Especie")) %>% 
  distinct(Esp)

sitios1 <- sitio %>% 
  filter(Dom!=9) %>% 
  inner_join(modelos, by=c("Esp"="Especie")) %>% 
  mutate(AreaBasal=pi/40000*Dn^2,
         Volumen=a0*Dn^a1*At^a2) %>% 
  select(-a0:-a2)

ifelse(1==2,1,
       ifelse(1==1, 2))

sitio %>% 
  inner_join(claves, by=c("Esp"="Especie"))

sitios1 <- sitio %>% 
  filter(Dom!=9) %>% 
  inner_join(modelos, by=c("Esp"="Especie")) %>% 
  mutate(AreaBasal=pi/40000*Dn^2,
         Volumen=a0*Dn^a1*At^a2) %>% 
  select(-a0:-a2)


sitios1resumen <- sitios1 %>%  
  group_by(Sitio) %>% 
  summarise(AreaBasal=sum(AreaBasal),
            Volumen=sum(Volumen)) %>% 
  summarise(AreaBasal=mean(AreaBasal),
            Volumen=mean(Volumen))

sitios1resumenEsp <- sitios1 %>%
  mutate(nsitios=n_distinct(Sitio)) %>% 
  group_by(Esp) %>% 
  summarise(AreaBasal=sum(AreaBasal/nsitios),
            Volumen=sum(Volumen/nsitios)) %>% 
  mutate(AreaBasaltotal=sum(AreaBasal),
         Volumentotal=sum(Volumen))


sum(c(10,15,20,25,30))/5
sum(c(10,15,20,25,30)/5)

#Para exportar datos a hojas de trabajo
#write.csv(tabla en R, "nombre.csv", row.names = F)