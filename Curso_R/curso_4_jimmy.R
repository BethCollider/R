df <- base de datos sitios
df1 <- base
glimpse(df)
df %>% 
  group_by(Esp) %>% 
  summarise(Ind=n()) %>% 
  mutate(Pi=Ind/(sum(Ind))) %>% 
  ungroup() %>% 
  summarise(shannon=-1*sum(Pi*log(Pi)))

df %>% 
  group_by(Sitio, Esp) %>% 
  summarise(Ind=n()) %>% 
  mutate(Pi=Ind/(sum(Ind))) %>% 
  group_by(Sitio) %>% 
  summarise(shannon=-1*sum(Pi*log(Pi)))

df %>% 
  inner_join(df1) %>% 
  group_by(UM, Esp) %>% 
  summarise(Ind=n()) %>% 
  mutate(Pi=Ind/(sum(Ind))) %>% 
  group_by(UM) %>% 
  summarise(shannon=-1*sum(Pi*log(Pi)))

##exportar excel a r
library(readxl)
read_excel()

#Para exportar datos a hojas de trabajo
#write.csv(tabla en R, "nombre.csv", row.names = F)