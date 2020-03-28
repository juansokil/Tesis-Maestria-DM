#################LEVANTA LIBRERIAS#####################
library(readr)
library(tidytext)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(ggridges)
library(tidyr)
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(udpipe)
library(igraph)
library(ggplot2)
library(scales)
library(stringr)
library(ggraph)
library(stringr)
library(rworldmap)
library(cluster)
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dendextend)
library(rlang)
library(matrixStats)




base_completa <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/LDA/base_completa.csv",   "\t", escape_double = FALSE, col_types = cols(X1 = col_skip(), id_1 = col_skip()), trim_ws = TRUE)
paises_colaboracion <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/LDA/paises_colaboracion.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
completo <-  base_completa %>% left_join(paises_colaboracion, c('id'='ut'))

#indices2 <- read_delim("https://raw.githubusercontent.com/juansokil/Scripts-Tesis/master/resultados/total_indices.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2 <- read_delim("https://raw.githubusercontent.com/juansokil/Scripts-Tesis/master/resultados/indices2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2$especializacion_pct = indices2$especializacion*100

#############CALCULAR LA IMPORTANCIA DE LOS TOPICOS############## TOTAL #########
base_total <- completo %>%
  select(-Titulo) %>%
  select(id, Año, ISO3, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.01, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO3, continente, subcontinente)) %>%
  #filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
  #filter(!is.na(participacion)) %>%
  group_by(topico) %>%
  summarize(total=mean(participacion, na.rm=TRUE))


#############CALCULAR LA IMPORTANCIA DE LOS TOPICOS POR PAIS############## ULTIMOS TRES AÑOS #########
manualcolors<-c('Topic06'='forestgreen', 
                'Topic96' = 'gainsboro',
                'Topic21'= 'red2', 
                'Topic43' ='orange',
                'Topic52' = 'tan4', 
                'Topic03'= 'cornflowerblue', 
                'Topic12' = 'darkblue',
                'Topic63' = 'darkolivegreen4',
                'Topic41' = 'indianred1',
                'Topic91' = 'mediumorchid1')


#####ESTE ES EL TOPICO ORIGINAL - SIN NINGUN TIPO DE AJUSTE (EL QUE NO SE USA MAS)####
#completo <- completo %>% left_join(indicesmatch, by=c('ISO2'='ISO2'))

#topicos_relevantes_pais <- completo %>%
#  filter(especializacion_pct > 0) %>%
#  select(-Titulo) %>%
#  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
##mutate_all(funs(replace(., . <= 0.001, NA))) %>%
#gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
#  filter(!is.na(participacion)) %>%
#  #filter(Año == 2004 | Año == 2005 | Año == 2006) %>%
#  filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
#  filter(topico %in% c('Topic06','Topic21','Topic43','Topic03','Topic96','Topic63','Topic41')) %>%
#  group_by(topico, ISO2, continente, subcontinente) %>%
#  summarize(total=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id))  %>%
#  arrange(ISO2, desc(total))  

#topic_continente <- topicos_relevantes_pais %>%
#  group_by(continente) %>%
#  summarise(topico_continente = first(topico))

#topic_subcontinente <- topicos_relevantes_pais %>%
#  group_by(subcontinente) %>%
#  summarise(topic_subcontinente = first(topico))

          

######################EJEMPLO MAPA - NO UTILIZADO######################

######MAPAS# PAIS####
#mapped_data <- joinCountryData2Map(bla2, joinCode = "ISO2", nameJoinColumn = "ISO2")
#mapped_data
#par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
#mapParams <-mapCountryData(mapped_data, nameColumnToPlot="topico_pais.x", colourPalette=c('forestgreen', 'gainsboro','red2', 'orange','tan4', 'cornflowerblue', 'darkblue'), 
#                           mapTitle = "", 
#                           missingCountryCol="white", oceanCol="lightblue", addLegend=TRUE)
#, mapRegion='eurasia')



#############CALCULAR LA IMPORTANCIA DE UN TOPICO POR REGION - VERSION 2020##############
############EN ESTE CASO LA ESPECIALIZACION POR TOPICO SE AJUSTA A LA ESPECIALIZACION POR PAIS####
indicesmatch <- indices2 %>%
  select(ISO2, especializacion_pct) %>%
  mutate(especializacion_pct = especializacion_pct /100)

#####LE SUMO GLOBAL GENDER GAP####
completo <- completo %>% left_join(indicesmatch, by=c('ISO2'='ISO2'))
completo$agrup <- 1

topicos_relevantes <- completo %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"), especializacion_pct)  %>%
  filter(continente %in%  c('Europa','Asia', 'America', 'Africa', 'Oceania')) %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
filter(especializacion_pct > 0 ) %>%
  filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
  gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente,especializacion_pct)) %>%
  mutate(importancia_topico=(participacion * especializacion_pct)) %>%
  #filter(participacion > 0) %>%
  #filter(!is.na(participacion)) %>%
  #group_by(ISO3, topico) %>%
  group_by(ISO2, topico,continente,subcontinente) %>%
  summarize(importancia_topico=mean(importancia_topico, na.rm=TRUE)*100, participacion=mean(participacion, na.rm=TRUE)*100, especializacion_pct=mean(especializacion_pct, na.rm=TRUE)*100, cantidad=n_distinct(id))  %>%
  group_by(continente, subcontinente, ISO2, topico) %>%
  #group_by(continente) %>%
  summarize(importancia_topico=mean(importancia_topico, na.rm=TRUE), participacion=mean(participacion, na.rm=TRUE)/100, especializacion_pct=mean(especializacion_pct, na.rm=TRUE)/100, cantidad=cantidad) %>%
  mutate(importancia_topico_pond = (importancia_topico / especializacion_pct)/100) %>%
  filter (topico %in% c('Topic03',  'Topic06',  'Topic08',  'Topic09',   'Topic11',  'Topic12',  'Topic16',  
                        'Topic17',  'Topic21',  'Topic30',  'Topic32',   'Topic33',  'Topic36',  'Topic38',  
                        'Topic41',  'Topic43',  'Topic48',  'Topic52',   'Topic55',  'Topic59',  'Topic62',  
                        'Topic72',  'Topic73',  'Topic84',  'Topic90',  'Topic92',  'Topic94',  'Topic96',  'Topic97',  'Topic99'))
  
paises <- completo %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.01, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
  #filter(!is.na(participacion)) %>%
  group_by(ISO2) %>%
  summarize(cantidad=n_distinct(id)) %>%
  filter(cantidad > 4)


####CONTROL######
  topicos_relevantes %>%
  group_by (continente) %>%
  filter (ISO2 =='USA') %>%
  summarize(sum(importancia_topico_pond), sum(participacion), mean(especializacion_pct))

topicos_relevantes$subcontinente <- as.factor(topicos_relevantes$subcontinente)
topicos_relevantes$continente <- as.factor(topicos_relevantes$continente) 
topicos_relevantes$ISO2 <- as.factor(topicos_relevantes$ISO2) 
topicos_relevantes$pais <-  topicos_relevantes$ISO2


##################A PARTIR DEL TOPICO PONDERADO - REALIZO LOS RANKINGS###############
####################################################
####ARMADO DE RANKINGS########NIVEL PAIS ###########

cc <- topicos_relevantes%>%
  select(continente, subcontinente, ISO2, topico, importancia_topico_pond, pais) %>%
  #filter(ISO2 %in% list(paises$ISO2))  %>%
  spread(key=topico, value=importancia_topico_pond) %>% column_to_rownames(var = "pais")

cc <- cc %>% left_join(paises) %>%
  filter (cantidad >= 5)

cccc <- data.frame(cc$ISO2, cc$cantidad)

#write.table(topicos_relevantes, file = "topicos_relevantes.txt", sep = "\t ", dec = ",", row.names = FALSE,col.names = TRUE)

rankings <- as.data.frame(rowRanks(as.matrix(cc[4:33])*-1))

completo_rankings <- cbind(cc[1:3], rankings)
new_names <- names(cc[4:33])
variable_order <- c('Topic96',  'Topic21',  'Topic43',  'Topic52',  'Topic03',  
                    'Topic08',  'Topic12',  'Topic84',  'Topic06',  'Topic41',  
                    'Topic73',  'Topic92',  'Topic32',  'Topic62',  'Topic17',
                    'Topic55',  'Topic72',  'Topic90',  'Topic09',  'Topic11',  
                    'Topic30',  'Topic33',  'Topic16',  'Topic48',  'Topic59',  
                    'Topic94',  'Topic97',  'Topic99',  'Topic36',  'Topic38')

completo_rankings %>%
  rename_at(vars(starts_with('V')), ~ new_names) %>%
  gather(key=topic, value = value , -c('continente','subcontinente','ISO2')) %>%
  mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
  ggplot(aes(x=topic, y=value)) + 
  scale_y_continuous(breaks = seq(1, 30, by = 1)) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7, outlier.colour = "red", outlier.shape = 20, outlier.size = 3) +
  coord_flip() 

stats <- completo_rankings %>%
  rename_at(vars(starts_with('V')), ~ new_names) %>%
  gather(key=topic, value = value , -c('continente','subcontinente','ISO2')) %>%
  mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
  group_by(topic)%>%
  summarize(Minimo=min(value), Maximo=max(value), Promedio=mean(value), Sd=sd(value), Mediana=median(value))


manualcolors<-c('Africa Austral'='forestgreen', 
                'Africa Central'= 'red2', 
                'Africa del Norte' ='orange',
                'Africa Occidental'= 'cornflowerblue', 
                'Africa Oriental' = 'gainsboro',
                'America Central' = 'darkolivegreen4',
                'America del Norte' = 'indianred1',
                'America del Sur' = 'tan4', 
                'Asia Central' = 'darkblue',
                'Asia del Sur' = 'mediumorchid1',
                'Asia Occidental' = 'seagreen',
                'Asia Oriental' = 'yellowgreen', 
                'Australia y Nueva Zelanda' = 'lightsalmon',
                'Caribe' = 'tan3',
                'Europa del Norte' = "tan1",
                'Europa del Sur' = 'darkgray', 
                'Europa Occidental' = 'wheat4',
                'Europa Oriental' = 'chartreuse',  
                'Sudeste Asiatico' = 'moccasin'
)
###Detalle de algunos topicos -Mejores###
completo_rankings %>%
  rename_at(vars(starts_with('V')), ~ new_names) %>%
  gather(key=topic, value = value , -c('continente','subcontinente','ISO2')) %>%
  filter(topic %in% c('Topic21')) %>%
  mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
  ggplot(aes(x=topic, y=value, label=ISO2, fill=subcontinente)) + 
  scale_fill_manual(values = manualcolors) +
  scale_y_continuous(breaks = seq(1, 30, by = 1)) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7, outlier.colour = "red", outlier.shape = 20, outlier.size = 3) +
  geom_label(fontface = "bold", size=3, alpha=0.8, position=position_jitter()) +
  coord_flip() 

###Detalle de algunos topicos - Peores### 
completo_rankings %>%
  rename_at(vars(starts_with('V')), ~ new_names) %>%
  gather(key=topic, value = value , -c('continente','subcontinente','ISO2')) %>%
  filter(topic %in% c('Topic96', 'Topic21','Topic43')) %>%
  mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
  ggplot(aes(x=topic, y=value, label=ISO2, fill=subcontinente)) + 
  scale_fill_manual(values = manualcolors) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7, outlier.colour = "red", outlier.shape = 20, outlier.size = 3) +
  geom_label(fontface = "bold", size=2, alpha=0.8, position=position_jitter()) +
  coord_flip() 





write.csv(completo_rankings, 'ranking_paises.csv')
  


##################A PARTIR DEL TOPICO PONDERADO - REALIZO LOS RANKINGS###############
####################################################
####ARMADO DE RANKINGS########NIVEL REGION #########
topicos_relevantes <- completo %>%
  select(-Titulo) %>%
  select(id, Año, ISO3, continente, subcontinente, starts_with("Topic"), especializacion_pct)  %>%
  filter(continente %in%  c('Europa','Asia', 'America', 'Africa', 'Oceania')) %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
filter(especializacion_pct > 0 ) %>%
  filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
  gather(key = "topico", value = "participacion", -c(id, Año, ISO3, continente, subcontinente,especializacion_pct)) %>%
  mutate(importancia_topico=(participacion * especializacion_pct)) %>%
  #filter(participacion > 0) %>%
  #filter(!is.na(participacion)) %>%
  #group_by(ISO3, topico) %>%
  group_by(ISO3, topico,continente,subcontinente) %>%
  summarize(importancia_topico=mean(importancia_topico, na.rm=TRUE)*100, participacion=mean(participacion, na.rm=TRUE)*100, especializacion_pct=mean(especializacion_pct, na.rm=TRUE)*100)  %>%
  group_by(continente, subcontinente, topico) %>%
  #group_by(continente) %>%
  summarize(importancia_topico=mean(importancia_topico, na.rm=TRUE), participacion=mean(participacion, na.rm=TRUE)/100, especializacion_pct=mean(especializacion_pct, na.rm=TRUE)/100) %>%
  mutate(importancia_topico_pond = (importancia_topico / especializacion_pct)/100) %>%
  filter (topico %in% c('Topic03',  'Topic06',  'Topic08',  'Topic09',   'Topic11',  'Topic12',  'Topic16',  
                        'Topic17',  'Topic21',  'Topic30',  'Topic32',   'Topic33',  'Topic36',  'Topic38',  
                        'Topic41',  'Topic43',  'Topic48',  'Topic52',   'Topic55',  'Topic59',  'Topic62',  
                        'Topic72',  'Topic73',  'Topic84',  'Topic90',  'Topic92',  'Topic94',  'Topic96',  'Topic97',  'Topic99'))


####CONTROL######
topicos_relevantes$subcontinente <- as.factor(topicos_relevantes$subcontinente)
topicos_relevantes$continente <- as.factor(topicos_relevantes$continente) 

cc <- topicos_relevantes %>%
  select(continente, subcontinente, topico, importancia_topico_pond) %>%
  spread(key=topico, value=importancia_topico_pond) 

cc <- cc %>% left_join(paises) %>%
  filter (cantidad >= 5)

cccc <- data.frame(cc$ISO2, cc$cantidad)

rankings <- as.data.frame(rowRanks(as.matrix(cc[3:32])*-1))
completo_rankings <- as.data.frame(cbind(as.data.frame(cc[1:2]), rankings))


new_names <- names(cc[3:32])
variable_order <- c('Topic96',  'Topic21',  'Topic43',  'Topic52',  'Topic03',  
                    'Topic08',  'Topic12',  'Topic84',  'Topic06',  'Topic41',  
                    'Topic73',  'Topic92',  'Topic32',  'Topic62',  'Topic17',
                    'Topic55',  'Topic72',  'Topic90',  'Topic09',  'Topic11',  
                    'Topic30',  'Topic33',  'Topic16',  'Topic48',  'Topic59',  
                    'Topic94',  'Topic97',  'Topic99',  'Topic36',  'Topic38')

###BOXPLOT EXPECIFICO###

manualcolors<-c('Africa Austral'='forestgreen', 
                'Africa Central'= 'red2', 
                'Africa del Norte' ='orange',
                'Africa Occidental'= 'cornflowerblue', 
                'Africa Oriental' = 'gainsboro',
                'America Central' = 'darkolivegreen4',
                'America del Norte' = 'indianred1',
                'America del Sur' = 'tan4', 
                'Asia Central' = 'darkblue',
                'Asia del Sur' = 'mediumorchid1',
                'Asia Occidental' = 'seagreen',
                'Asia Oriental' = 'yellowgreen', 
                'Australia y Nueva Zelanda' = 'lightsalmon',
                'Caribe' = 'tan3',
                'Europa del Norte' = "tan1",
                'Europa del Sur' = 'darkgray', 
                'Europa Occidental' = 'wheat4',
                'Europa Oriental' = 'chartreuse',  
                'Sudeste Asiatico' = 'moccasin'
)


completo_rankings %>%
  rename_at(vars(starts_with('V')), ~ new_names) %>%
  gather(key=topic, value = value , -c('continente','subcontinente')) %>%
  mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
  #filter(topic %in% c('Topic96', 'Topic21','Topic43')) %>%
  ggplot(aes(x=topic, y=value, color=subcontinente)) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7, outlier.shape = NA) +
  geom_point(size=2) +
  scale_fill_manual(values = manualcolors, drop = FALSE) +
  scale_colour_manual(values = manualcolors, drop = FALSE) +
  coord_flip()
  theme(legend.title=element_blank(), legend.position="bottom") 
  

  
  
  ##################A PARTIR DEL TOPICO PONDERADO - REALIZO LOS RANKINGS###############
  ####################################################
  ####ARMADO DE RANKINGS########NIVEL CONTINENTE #####
  topicos_relevantes <- completo %>%
    select(-Titulo) %>%
    select(id, Año, ISO3, continente, subcontinente, starts_with("Topic"), especializacion_pct)  %>%
    filter(continente %in%  c('Europa','Asia', 'America', 'Africa', 'Oceania')) %>%
    #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
  #mutate_all(funs(replace(., . <= 0.001, NA))) %>%
  filter(especializacion_pct > 0 ) %>%
    filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
    gather(key = "topico", value = "participacion", -c(id, Año, ISO3, continente, subcontinente,especializacion_pct)) %>%
    mutate(importancia_topico=(participacion * especializacion_pct)) %>%
    #filter(participacion > 0) %>%
    #filter(!is.na(participacion)) %>%
    #group_by(ISO3, topico) %>%
    group_by(ISO3, topico,continente,subcontinente) %>%
    summarize(importancia_topico=mean(importancia_topico, na.rm=TRUE)*100, participacion=mean(participacion, na.rm=TRUE)*100, especializacion_pct=mean(especializacion_pct, na.rm=TRUE)*100)  %>%
    group_by(continente,  topico) %>%
    #group_by(continente) %>%
    summarize(importancia_topico=mean(importancia_topico, na.rm=TRUE), participacion=mean(participacion, na.rm=TRUE)/100, especializacion_pct=mean(especializacion_pct, na.rm=TRUE)/100) %>%
    mutate(importancia_topico_pond = (importancia_topico / especializacion_pct)/100) %>%
    filter (topico %in% c('Topic03',  'Topic06',  'Topic08',  'Topic09',   'Topic11',  'Topic12',  'Topic16',  
                          'Topic17',  'Topic21',  'Topic30',  'Topic32',   'Topic33',  'Topic36',  'Topic38',  
                          'Topic41',  'Topic43',  'Topic48',  'Topic52',   'Topic55',  'Topic59',  'Topic62',  
                          'Topic72',  'Topic73',  'Topic84',  'Topic90',  'Topic92',  'Topic94',  'Topic96',  'Topic97',  'Topic99'))
  
  cc <- topicos_relevantes %>%
    select(continente, topico, importancia_topico_pond) %>%
    spread(key=topico, value=importancia_topico_pond) 
  
  
  rankings <- as.data.frame(rowRanks(as.matrix(cc[2:31])*-1))
  
  completo_rankings <- as.data.frame(cbind(as.data.frame(cc[1]), rankings))
  View(completo_rankings)
  new_names <- names(cc[2:32])
  variable_order <- c('Topic96',  'Topic21',  'Topic43',  'Topic52',  'Topic03',  
                      'Topic08',  'Topic12',  'Topic84',  'Topic06',  'Topic41',  
                      'Topic73',  'Topic92',  'Topic32',  'Topic62',  'Topic17',
                      'Topic55',  'Topic72',  'Topic90',  'Topic09',  'Topic11',  
                      'Topic30',  'Topic33',  'Topic16',  'Topic48',  'Topic59',  
                      'Topic94',  'Topic97',  'Topic99',  'Topic36',  'Topic38')

  
  continentcolors<-c('Africa'='gray11', 
                     'America' = 'red2',
                     'Asia' = 'yellow3',
                     'Oceania' ='forestgreen',
                     'Europa' = "cornflowerblue")
  

  completo_rankings %>%
    rename_at(vars(starts_with('V')), ~ new_names) %>%
    gather(key=topic, value = value , -c('continente')) %>%
    mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
    #filter(topic %in% c('Topic96', 'Topic21','Topic43')) %>%
    ggplot(aes(x=topic, y=value, color=continente, fill=continente)) +
    geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7, outlier.shape = NA) +
    geom_point(size=2) +
    scale_fill_manual(values = continentcolors, drop = FALSE) +
    scale_colour_manual(values = continentcolors, drop = FALSE) +
    coord_flip()
  
  stats <- completo_rankings %>%
    rename_at(vars(starts_with('V')), ~ new_names) %>%
    gather(key=topic, value = value , -c('continente')) %>%
    mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
    group_by(topic)%>%
    summarize(Minimo=min(value), Maximo=max(value), Promedio=mean(value), Sd=sd(value), Mediana=median(value))
  

  completo_rankings %>%
    rename_at(vars(starts_with('V')), ~ new_names) %>%
    gather(key=topic, value = value , -c('continente')) %>%
    mutate(topic = fct_rev(factor(topic, levels = variable_order))) %>%
  ggplot(aes(x = continente, y = value, group = topic)) +
    geom_line(aes(color = continente, alpha = 1), size = 2) +
    geom_point(aes(color = continente, alpha = 1), size = 4) +
    scale_y_reverse(breaks = 1:nrow(df.rankings))

  
  
  