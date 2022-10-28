library("googlesheets4")
library(tidyverse)
library(readxl)



sitio <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 1)
UM <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 2)
Sup <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 3)
modelos <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 4)
claves <- read_sheet("https://docs.google.com/spreadsheets/d/12NQBFHx5DYiHoQ9Ams6RI9pbvKhjD1_BKzEUiSDN2r0/edit?usp=sharing", sheet = 5)


glimpse(sitio)
valoresNA <- filter(sitio, is.na(Dn)|is.na(At))

sitio <- sitio %>% 
  na_interpolation()

ggplot(sitio, aes(x=Dn, y=At))+
  geom_jitter()+
  geom_smooth() +
  theme_bw()

sitio %>% 
  distinct(Esp) %>% 
  arrange(Esp)

sitio <- sitio %>% 
  mutate(Grupo=case_when(str_detect(Esp, "Pinus|Juniperus")~"Coníferas",
                         TRUE ~ "Latifoliada"))

EspSit <- sitio %>% 
  distinct(Esp)

EspMod <- modelos %>% 
  distinct(Especie)

anti_join(EspSit, EspMod, by=c("Esp"="Especie"))  


sitio1 <- sitio %>% 
  inner_join(modelos, by=c("Esp"="Especie")) %>% 
  mutate(vol=a0*Dn^a1*At^a2) %>% 
  select(-a0:-a2) %>% 
  group_by(Sitio) %>% 
  summarise(Vol=sum(vol)) %>% 
  ungroup() %>% 
  summarise(Vol=mean(Vol))
