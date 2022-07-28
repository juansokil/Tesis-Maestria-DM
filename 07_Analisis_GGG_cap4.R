

#install.packages("raster")
#install.packages("tidyverse")
library(tidyverse)
#install.packages("sp")
#install.packages("maptools")
#install.packages("rgeos")
library(rgeos)

library(sp)
library(maptools)
library(ggplot2)
library(maps)

### LIBRERIAS###
#install.packages("shiny")
#install.packages("viridis")
library(viridis)
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(maptools)
library(maps)
library(readr)
library(tidyr)
library(ggrepel)
library(scales)

#install.packages("devtools")
library(devtools)
#devtools::install_github("rensa/ggflags") # Instalar por primera vez
library(ggplot2)
install.packages("ggflags")
library(ggflags) # Para geom_flags
#install.packages("countrycode")
library(countrycode)  # Para obtener codigos de paises

library(tidyverse)
library(readr)
library(gganimate)
#library(ggthemes)


library("rworldmap")
library(scales)




setwd("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/Indices")
getwd()
library(readr)
#indices2 <- read_delim("total_indices.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2 <- read_delim("indices3.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2$especializacion_pct = indices2$especializacion*100



#############INDEX###################


mapped_data <- joinCountryData2Map(indices2, joinCode = "ISO2", nameJoinColumn = "ISO2")
mapped_data$ISO2
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapParams <-mapCountryData(mapped_data, nameColumnToPlot="Global_Index", 
                           colourPalette="topo",
                           #catMethod='fixedWidth',
                           catMethod=c(0.00:0.25,
                                       0.25:0.50,
                                       0.50:0.60,
                                       0.60:0.70,
                                       0.70:0.80,
                                       0.80:0.90,
                                       0.90:1.00,
                                       1:1.1),
                           #colourPalette=c("red","yellow","dark green","blue","orange","purple","brown","white","black","pink"), 
                           mapTitle = "", 
                           missingCountryCol="white", oceanCol="lightblue", addLegend=FALSE
                           #, mapRegion='eurasia'
)
do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 3))




#############################




mapped_data <- joinCountryData2Map(indices2, joinCode = "ISO2", nameJoinColumn = "ISO2")
mapped_data$ISO2
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapParams <-mapCountryData(mapped_data, nameColumnToPlot="Economic_participation_and_opportunity", 
                           colourPalette="topo",
                           #catMethod='fixedWidth',
                           catMethod=c(0.00:0.25,
                                       0.25:0.50,
                                       0.50:0.60,
                                       0.60:0.70,
                                       0.70:0.80,
                                       0.80:0.90,
                                       0.90:1.00,
                                       1:1.1),
                           #colourPalette=c("red","yellow","dark green","blue","orange","purple","brown","white","black","pink"), 
                           mapTitle = "", 
                           missingCountryCol="white", oceanCol="lightblue", addLegend=FALSE
                           #, mapRegion='eurasia'
)
do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 3))




mapped_data <- joinCountryData2Map(indices2, joinCode = "ISO2", nameJoinColumn = "ISO2")
mapped_data$ISO2
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapParams <-mapCountryData(mapped_data, nameColumnToPlot="Educational_attainment", 
                           colourPalette="topo",
                           #catMethod='fixedWidth',
                           catMethod=c(0.00:0.25,
                                       0.25:0.50,
                                       0.50:0.60,
                                       0.60:0.70,
                                       0.70:0.80,
                                       0.80:0.90,
                                       0.90:1.00,
                                       1:1.1),
                           #colourPalette=c("red","yellow","dark green","blue","orange","purple","brown","white","black","pink"), 
                           mapTitle = "", 
                           missingCountryCol="white", oceanCol="lightblue", addLegend=FALSE
                           #, mapRegion='eurasia'
)
do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 3))




mapped_data <- joinCountryData2Map(indices2, joinCode = "ISO2", nameJoinColumn = "ISO2")
mapped_data$ISO2
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapParams <-mapCountryData(mapped_data, nameColumnToPlot="Health_and_survival", 
                           colourPalette="topo",
                           #catMethod='fixedWidth',
                           catMethod=c(0.00:0.25,
                                       0.25:0.50,
                                       0.50:0.60,
                                       0.60:0.70,
                                       0.70:0.80,
                                       0.80:0.90,
                                       0.90:1.00,
                                       1:1.1),
                           #colourPalette=c("red","yellow","dark green","blue","orange","purple","brown","white","black","pink"), 
                           mapTitle = "", 
                           missingCountryCol="white", oceanCol="lightblue", addLegend=FALSE
                           #, mapRegion='eurasia'
)
do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 3))


mapped_data <- joinCountryData2Map(indices2, joinCode = "ISO2", nameJoinColumn = "ISO2")
mapped_data$ISO2
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapParams <-mapCountryData(mapped_data, nameColumnToPlot="Political_Empowerment", 
                           colourPalette="topo",
                           #catMethod='fixedWidth',
                           catMethod=c(0.00:0.25,
                                       0.25:0.50,
                                       0.50:0.60,
                                       0.60:0.70,
                                       0.70:0.80,
                                       0.80:0.90,
                                       0.90:1.00,
                                       1:1.1),
                           #colourPalette=c("red","yellow","dark green","blue","orange","purple","brown","white","black","pink"), 
                           mapTitle = "", 
                           missingCountryCol="white", oceanCol="lightblue", addLegend=FALSE
                           #, mapRegion='eurasia'
)
do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 3))





####Selecciono las variables que me sirven####
indice_gdi <- indices2 %>% select (country, ISO2, ISO3, continente, subcontinente, especializacion, Global_Index)

indice_gathered_1 <- indice_gdi %>%
  gather(key = Global_index, value = measurement, -c(country, ISO2, ISO3, continente, subcontinente, especializacion) )

indice_gathered_1_head <- indice_gathered_1 %>% arrange(desc(especializacion))
indice_gathered_1_head <- head(indice_gathered_1_head,92)







global_gender_gap <- indices2 %>%
  select (country, ISO2, ISO3, continente_en, continente, subcontinente_en, subcontinente, Global_Index, Economic_participation_and_opportunity, Educational_attainment, Health_and_survival, Political_Empowerment)




global_gender_gap %>% group_by(continente) %>% summarize (Global_Index=mean(Global_Index, na.rm=TRUE),
                                                          Economic_participation_and_opportunity=mean(Economic_participation_and_opportunity, na.rm=TRUE),
                                                          Educational_attainment=mean(Educational_attainment, na.rm=TRUE),
                                                          Health_and_survival=mean(Health_and_survival, na.rm=TRUE),
                                                          Political_Empowerment=mean(Political_Empowerment, na.rm=TRUE))



global_gender_gap %>% group_by(subcontinente) %>% summarize (Global_Index=mean(Global_Index, na.rm=TRUE),
                                                             Economic_participation_and_opportunity=mean(Economic_participation_and_opportunity, na.rm=TRUE),
                                                             Educational_attainment=mean(Educational_attainment, na.rm=TRUE),
                                                             Health_and_survival=mean(Health_and_survival, na.rm=TRUE),
                                                             Political_Empowerment=mean(Political_Empowerment, na.rm=TRUE))

table(global_gender_gap$subcontinente)

global_gender_gap_totales <- global_gender_gap %>% summarize (Global_Index=mean(Global_Index, na.rm=TRUE),
                                                              Economic_participation_and_opportunity=mean(Economic_participation_and_opportunity, na.rm=TRUE),
                                                              Educational_attainment=mean(Educational_attainment, na.rm=TRUE),
                                                              Health_and_survival=mean(Health_and_survival, na.rm=TRUE),
                                                              Political_Empowerment=mean(Political_Empowerment, na.rm=TRUE))


global_gender_gap_gathered <- 
  global_gender_gap %>%
  gather(key = index, value = measurement, -c(country, ISO2, ISO3, continente, continente_en, subcontinente, subcontinente_en))   %>% 
  mutate(code = tolower(ISO2))



global_gender_gap_gathered$index <- factor(global_gender_gap_gathered$index, 
                                           levels = c("Political_Empowerment",
                                                      "Health_and_survival",
                                                      "Educational_attainment", 
                                                      "Economic_participation_and_opportunity", 
                                                      "Global_Index"))

global_gender_gap_gathered <- global_gender_gap_gathered %>% mutate(indices = case_when (index =="Political_Empowerment" ~ 'Empoderamiento Pol?tico',
                                                                                         index =="Health_and_survival" ~ 'Salud y Supervivencia',
                                                                                         index =="Educational_attainment" ~ 'Logros Educativos',
                                                                                         index =="Economic_participation_and_opportunity" ~ 'Participaci?n Economica y Oportunidades',
                                                                                         index =="Global_Index" ~ 'Global Gender Gap'))



global_gender_gap_gathered$indices <- factor(global_gender_gap_gathered$indices, 
                                             levels = c("Empoderamiento Pol?tico",
                                                        "Salud y Supervivencia",
                                                        "Logros Educativos", 
                                                        "Participaci?n Economica y Oportunidades", 
                                                        "Global Gender Gap"))




#############BOXPLOTS######################

global_gender_gap_gathered %>%
  filter(ISO2 != c('VU')) %>%
  filter(continente_en %in%  c('Africa', 'Americas', 'Asia', 'Europe','Oceania')) %>%
  #filter(continente_en %in%  c('Americas')) %>%
  filter(index %in%  c('Global_Index')) %>%
  ggplot(aes(x=continente, y=measurement,  label=ISO2, fill=subcontinente)) +
  scale_fill_manual(values = manualcolors) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.4, outlier.colour = "red", outlier.shape = 20, outlier.size = 6) +
  #coord_flip() +
  #facet_wrap( ~ continente)  +
  geom_label(fontface = "bold", size=2, alpha=0.8, position=position_jitter()) +
  xlab("Continente") +
  ylab("Global Gender Gap") +
  theme(legend.position="bottom")












###########################################################################################################
###########################################################################################################
############4.2###############################################################################################
######################################ACA SE TERMINA LA PARTE DE ESPECIALIZACION###########################
######################COMIENZA LA VINCULACION ENTRE GLOBAL GENDER GAP Y EL INDICE DE ESPECIALIZACION#######
###########################################################################################################



########CORRELACIONES ENTRE ESPECIALIZACION E INDICES#################

indices2 %>%
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  #filter(continente.x %in%  c('Africa', 'America', 'Asia', 'Europa')) %>%
  #filter(continente.x %in%  c('Africa')) %>%
  ggplot(aes(x=Global_Index, y=especializacion, label=ISO2, color=continente)) +
  #geom_point(size=5, shape=15, alpha=0.1) +
  #  coord_cartesian(ylim = c(0.00, 0.04))  +
  #  coord_cartesian(xlim = c(0.5, 0.9))  +
  scale_color_manual(values = continentcolors) +
  #geom_text(size=5, fontface = "bold") +
  scale_y_continuous(labels = percent) +
  #facet_wrap(~continente.x, scales = "fixed", ncol=2) +
  theme(legend.title=element_blank(), legend.position="bottom") + 
  labs (x="Global Gender Gap", y='Especialización en Estudios de Género') + geom_text(size=6) 



# Use box plot as marginal plots
a <-
  indices2 %>%
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  ggscatterhist(
  indices2, x = "Global_Index", y = "especializacion",
  color = "continente", fill='continente', size = 6, alpha = 0.8, legend = "bottom",
  palette = continentcolors,
  margin.plot = "boxplot",  xlab = "Global Gender Gap", ylab = 'Especialización en Estudios de Género',
  # scale_y_continuous(labels = percent),
  ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
)






########CORRELACIONES ENTRE ESPECIALIZACION E INDICES#################

indices2 %>%
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  #filter(continente.x %in%  c('Africa', 'America', 'Asia', 'Europa')) %>%
  #filter(continente.x %in%  c('Africa')) %>%
  ggplot(aes(x=Economic_participation_and_opportunity, y=especializacion, label=ISO2, fill=continente, color=continente)) +
  geom_point(size=5, shape=15, alpha=0.1) +
  #coord_cartesian(ylim = c(0.00, 0.04))  +
  #  coord_cartesian(xlim = c(0.5, 1))  +
  scale_color_manual(values = continentcolors) +
  geom_text(size=5, fontface = "bold") +
  scale_y_continuous(labels = percent) +
  #facet_wrap(~continente.x, scales = "fixed", ncol=2) +
  theme(legend.title=element_blank(), legend.position="bottom") 



indices2 %>%
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  #filter(continente.x %in%  c('Africa', 'America', 'Asia', 'Europa')) %>%
  #filter(continente.x %in%  c('Africa')) %>%
  ggplot(aes(x=Educational_attainment, y=especializacion, label=ISO2, fill=continente, color=continente)) +
  geom_point(size=5, shape=15, alpha=0.1) +
  #coord_cartesian(ylim = c(0.00, 0.04))  +
  coord_cartesian(xlim = c(0.8, 1))  +
  scale_color_manual(values = continentcolors) +
  geom_text(size=5, fontface = "bold") +
  scale_y_continuous(labels = percent) +
  #facet_wrap(~continente.x, scales = "fixed", ncol=2) +
  theme(legend.title=element_blank(), legend.position="bottom") 

















indices3 <- indices2 %>% 
filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('WS')) %>%
  filter(ISO2 != c('VU'))%>% filter(!is.na(especializacion)) %>% mutate(especializacion=especializacion*100)



library(ggpubr)

par(mfrow=c(2,2))

# Use box plot as marginal plots
a <- ggscatterhist(
  indices3, x = "Economic_participation_and_opportunity", y = "especializacion",
  color = "continente", fill='continente', size = 6, alpha = 0.8, legend = "bottom",
  palette = continentcolors,
  margin.plot = "boxplot",  xlab = "Subindice GGG - Participaci?n Econ?mica y Oportunidades", ylab = 'Especializaci?n en Estudios de G?nero',
 # scale_y_continuous(labels = percent),
  ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
)



# Use box plot as marginal plots
b <- ggscatterhist(
  indices3, x = "Educational_attainment", y = "especializacion",
  color = "continente", fill='continente', size = 6, alpha = 0.8, legend = "bottom",
  palette = continentcolors,
  margin.plot = "boxplot",  xlab = "Subindice GGG - Logros Educativos", ylab = 'Especializaci?n en Estudios de G?nero',
  ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
)


# Use box plot as marginal plots
c <- ggscatterhist(
  indices3, x = "Health_and_survival", y = "especializacion",
  color = "continente", fill='continente', size = 6, alpha = 0.8, legend = "bottom",
  palette = continentcolors,
  margin.plot = "boxplot",  xlab = "Subindice GGG - Salud y Supervivencia", ylab = 'Especializaci?n en Estudios de G?nero',
  ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
)




# Use box plot as marginal plots
d <- ggscatterhist(
  indices3, x = "Political_Empowerment", y = "especializacion",
  color = "continente", fill='continente', size = 6, alpha = 0.8, legend = "bottom",
  palette = continentcolors,
  margin.plot = "boxplot",  xlab = "Subindice GGG - Empoderamiento Pol?tico", ylab = 'Especializaci?n en Estudios de G?nero',
  ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
)


library(ggpubr)


figure <- ggarrange(a, b, c, d, ncol = 2, nrow = 2)


# Hexagonal binning
ggplot(diamonds, aes(carat, price)) +
  
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07")+
  theme_minimal()




indices2 %>%
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  #filter(continente.x %in%  c('Africa', 'America', 'Asia', 'Europa')) %>%
  #filter(continente.x %in%  c('Africa')) %>%
  ggplot(aes(x=Health_and_survival, y=especializacion, label=ISO2, fill=continente, color=continente)) +
  geom_point(size=5, shape=15, alpha=0.1) +
  coord_cartesian(ylim = c(0.00, 0.04))  +
  coord_cartesian(xlim = c(0.5, 1))  +
  scale_color_manual(values = continentcolors) +
  geom_text(size=5, fontface = "bold") +
  scale_y_continuous(labels = percent) +
  #facet_wrap(~continente.x, scales = "fixed", ncol=2) +
  theme(legend.title=element_blank(), legend.position="bottom") 





###CORRELACIONES#### - ARMADO DE BASE
comparar <- indices2 %>%
  select (country, ISO2, ISO3, continente, subcontinente, especializacion_pct, Global_Index, 
          Economic_participation_and_opportunity, 
          Educational_attainment, 
          Health_and_survival, 
          Political_Empowerment)



###############CORRELACIONES TOTALES##################
###A NIVEL CONTINENTE#########

comparar %>% 
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  filter(continente %in%  c('Asia')) %>%
  filter (!is.na(especializacion_pct) & !is.na(Global_Index)) %>% 
  summarise(COR=cor(especializacion_pct, Global_Index), P_VALUE=cor.test(especializacion_pct, Global_Index)$p.value, count=n()) 


p_valores <- c(0.0113, 0.0004, 0.8017, 0.7112)


p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))


###A NIVEL SUBCONTINENTE#########
unique(comparar$subcontinente)

regiones <- c("Europa del Norte","Europa del Sur",
              "Europa Oriental","America del Sur","Europa Occidental",
              "Sudeste Asiatico","Asia del Sur","Africa Oriental")

row <- c()
for (value in regiones){

  comparar %>% 
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  filter(subcontinente == "Asia Occidental") %>%
  filter (!is.na(especializacion_pct) & !is.na(Global_Index)) %>% 
  summarise(COR=cor(especializacion_pct,Global_Index), P_VALUE=cor.test(especializacion_pct, Global_Index)$p.value, count=n()) 
    #row_ <- 
  #row <- rbind(row, row_)
}


p_valores <- c(0.00283, 0.00328, 0.937, 0.995, 0.685, 0.597, 0.680,0.344,0.0928 )


p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))



#########################
####CORRELACION POR CONTINENTE####
###############

correlaciones <-comparar %>% 
  filter(continente %in% c('America','Asia','Europa','Africa')) %>% 
  group_by(continente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Economic_participation_and_opportunity)) %>% 
  summarise(COR=cor(especializacion_pct, Economic_participation_and_opportunity), P_VALUE=cor.test(especializacion_pct, Economic_participation_and_opportunity)$p.value, count=n()) 


p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))



correlaciones <-comparar %>% 
  filter(continente %in% c('America','Asia','Europa','Africa')) %>% 
  group_by(continente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Educational_attainment)) %>% 
  summarise(COR=cor(especializacion_pct, Educational_attainment), P_VALUE=cor.test(especializacion_pct, Educational_attainment)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))



correlaciones <-comparar %>% 
  filter(continente %in% c('America','Asia','Europa','Africa')) %>% 
  group_by(continente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Health_and_survival)) %>% 
  summarise(COR=cor(especializacion_pct, Health_and_survival), P_VALUE=cor.test(especializacion_pct, Health_and_survival)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))


correlaciones <-comparar %>% 
  filter(continente %in% c('America','Asia','Europa','Africa')) %>% 
  group_by(continente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Political_Empowerment)) %>% 
  summarise(COR=cor(especializacion_pct, Political_Empowerment), P_VALUE=cor.test(especializacion_pct, Political_Empowerment)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))


######### 4.3 ##############
####CORRELACION POR SUBCONTINENTE####
#############AQUELLAS REGIONES CON AL MENOS 5 CASOS############
recuento_paises = comparar %>% 
  group_by(subcontinente) %>%
  filter (!is.na(especializacion_pct))  %>%
  summarize(count=n_distinct(ISO2))

##############COMPARACIONES PARA AQUELLAS REGIONES CON AL MENOS 5 CASOS#############


correlaciones <- comparar %>% 
  filter(subcontinente %in% c('Asia Occidental', 'Europa del Norte', 'Europa del Sur', 'America del Sur', 'Europa Oriental', 'Europa Occidental', 'Asia del Sur', 'Sudeste Asiatico', 'Africa Oriental')) %>% 
  group_by(subcontinente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Global_Index)) %>% 
  #summarise(count=n()) 
  summarise(COR=cor(especializacion_pct, Global_Index), P_VALUE=cor.test(especializacion_pct, Global_Index)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))



correlaciones <- comparar %>% 
  filter(subcontinente %in% c('Asia Occidental', 'Europa del Norte', 'Europa del Sur', 'America del Sur', 'Europa Oriental', 'Europa Occidental', 'Asia del Sur', 'Sudeste Asiatico', 'Africa Oriental')) %>% 
  group_by(subcontinente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Economic_participation_and_opportunity)) %>% 
  #summarise(count=n()) 
  summarise(COR=cor(especializacion_pct, Economic_participation_and_opportunity), P_VALUE=cor.test(especializacion_pct, Economic_participation_and_opportunity)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))



correlaciones <- comparar %>% 
  filter(subcontinente %in% c('Asia Occidental', 'Europa del Norte', 'Europa del Sur', 'America del Sur', 'Europa Oriental', 'Europa Occidental', 'Asia del Sur', 'Sudeste Asiatico', 'Africa Oriental')) %>% 
  group_by(subcontinente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Educational_attainment)) %>% 
  #summarise(count=n()) 
  summarise(COR=cor(especializacion_pct, Educational_attainment), P_VALUE=cor.test(especializacion_pct, Educational_attainment)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))







correlaciones <- comparar %>% 
  filter(subcontinente %in% c('Asia Occidental', 'Europa del Norte', 'Europa del Sur', 'America del Sur', 'Europa Oriental', 'Europa Occidental', 'Asia del Sur', 'Sudeste Asiatico', 'Africa Oriental')) %>% 
  group_by(subcontinente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Health_and_survival)) %>% 
  #summarise(count=n()) 
  summarise(COR=cor(especializacion_pct, Health_and_survival), P_VALUE=cor.test(especializacion_pct, Health_and_survival)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))




correlaciones <- comparar %>% 
  filter(subcontinente %in% c('Asia Occidental', 'Europa del Norte', 'Europa del Sur', 'America del Sur', 'Europa Oriental', 'Europa Occidental', 'Asia del Sur', 'Sudeste Asiatico', 'Africa Oriental')) %>% 
  group_by(subcontinente) %>% 
  filter (!is.na(especializacion_pct) & !is.na(Political_Empowerment)) %>% 
  #summarise(count=n()) 
  summarise(COR=cor(especializacion_pct, Political_Empowerment), P_VALUE=cor.test(especializacion_pct, Political_Empowerment)$p.value, count=n()) 

p_valores <- correlaciones$P_VALUE

#p.adjust(p_valores, method = "bonferroni", n = length(p_valores))
p.adjust(p_valores, method = "fdr", n = length(p_valores))




###PARTE 4.4###############

#################################################################################################
#################################################################################################
#################################################################################################
###############################ACA EMPIEZA LA PARTE DE TOPICOS Y SUBINDICES###################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################


##http://www.sthda.com/english/articles/32-r-graphics-essentials/131-plot-two-continuous-variables-scatter-graph-and-alternatives/


base_completa <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/LDA/base_completa.csv",   "\t", escape_double = FALSE, col_types = cols(X1 = col_skip(), id_1 = col_skip()), trim_ws = TRUE)
base_completa <- base_completa %>% rename ('ut'='id...2')
paises_colaboracion <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/LDA/paises_colaboracion.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
completo <-  base_completa %>% left_join(paises_colaboracion, c('ut'='ut'))


completo <- completo %>% rename ('id'='ut')

#indices2 <- read_delim("https://raw.githubusercontent.com/juansokil/Scripts-Tesis/master/resultados/total_indices.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2 <- read_delim("https://raw.githubusercontent.com/juansokil/Scripts-Tesis/master/resultados/indices2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2$especializacion_pct = indices2$especializacion*100




####################ECONOMIA#########################
#####ARMO LOS TOPICOS####
topicos_relevantes <- completo %>%
  filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(!is.na(participacion)) %>%
  filter(topico == 'Topic92') %>%
  #filter(topico == 'Topic22' | topico == 'Topic74') %>%
  group_by(continente, subcontinente, ISO2) %>%
  summarize(total=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id)) 




#####ARMO LOS TOPICOS####
topicos_relevantes <- completo %>%
  filter(continente =='Europa') %>%
  #filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
  #filter(Año == 2013 | Año == 2014 | Año == 2015) %>%
  #filter(Año == 2010 | Año == 2011 | Año == 2012) %>%
  #filter(Año == 2007 | Año == 2008 | Año == 2009) %>%
  filter(Año == 2004 | Año == 2005 | Año == 2006) %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(!is.na(participacion)) %>%
  filter(topico == 'Topic92') %>%
  #filter(topico == 'Topic22' | topico == 'Topic74') %>%
  group_by(subcontinente) %>%
  summarize(total=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id)) 









topicos_relevantes






#####LE SUMO GLOBAL GENDER GAP####
dd <- topicos_relevantes %>% left_join(indices2, by=c('ISO2'='ISO2'))
dd$agrup <- 1

dd$totalpond <- dd$total*dd$especializacion




dd %>%
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  #filter(ISO2 != c('BW')) %>%
  filter(continente.x %in%  c('Africa', 'America', 'Asia', 'Europa','Oceania')) %>%
  #filter(continente.x %in%  c('Europa')) %>%
  ggplot(aes(x=Economic_participation_and_opportunity, y=totalpond, label=ISO2,  color=continente.x)) +
  geom_point(size=5, shape=15, alpha=0.1) +
  #  coord_cartesian(ylim = c(0.00, 0.04))  +
  #  coord_cartesian(xlim = c(0.5, 0.9))  +
  scale_color_manual(values = continentcolors) +
  #geom_text(size=5, fontface = "bold") +
  scale_y_continuous(labels = percent) +
  #facet_wrap(~continente.x, scales = "fixed", ncol=2) +
  theme(legend.title=element_blank(), legend.position="bottom") +   
  labs (x="Subindice GGG - Participación Económica y Oportunidades", y='Especialización en T92 - Género y Trabajo') 
+ geom_text(size=6) 



a <-
  dd %>%
  filter(ISO2 != c('VA')) %>%
  filter(ISO2 != c('VU')) %>%
  filter(ISO2 != c('WS')) %>%
  
  ggscatterhist(
    dd, x = "Economic_participation_and_opportunity", y = "totalpond",
    color = "continente.x", fill='continente.x', size = 6, alpha = 0.8, legend = "bottom",
    palette = continentcolors,
    margin.plot = "boxplot",  xlab = "Subindice GGG - Participación Económica y Oportunidades", ylab = 'Especialización en T92 - Género y Trabajo',
    # scale_y_continuous(labels = percent),
    ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
  )



















##########################4.4.2########PARTE DINAMICA###########################


base_completa <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/LDA/base_completa.csv",   "\t", escape_double = FALSE, col_types = cols(X1 = col_skip(), id_1 = col_skip()), trim_ws = TRUE)
base_completa <- base_completa %>% rename ('ut'='id...2')
paises_colaboracion <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/LDA/paises_colaboracion.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
completo <-  base_completa %>% left_join(paises_colaboracion, c('ut'='ut'))





#indices2 <- read_delim("https://raw.githubusercontent.com/juansokil/Scripts-Tesis/master/resultados/total_indices.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

indices2 <- read_delim("https://raw.githubusercontent.com/juansokil/Scripts-Tesis/master/resultados/indices2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2$especializacion_pct = indices2$especializacion*100

indices2

completo <- completo %>% rename(id=ut)


###########ESPECIALIZACION######
####################ECONOMIA#########################
#####ARMO LOS TOPICOS####
topico_ECO_2004_2006 <- completo %>%
  filter(Año == 2004 | Año == 2005 | Año == 2006) %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(!is.na(participacion)) %>%
  filter(topico == 'Topic92') %>%
  group_by(ISO2) %>%
  summarize(topico_ECO_2004_2006=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id)) %>%
  select(ISO2, topico_ECO_2004_2006)


topico_ECO_2007_2009 <- completo %>%
  filter(Año == 2007 | Año == 2008 | Año == 2009) %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(!is.na(participacion)) %>%
  filter(topico == 'Topic92') %>%
  group_by(ISO2) %>%
  summarize(topico_ECO_2007_2009=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id)) %>%
  select(ISO2, topico_ECO_2007_2009)



topico_ECO_2010_2012 <- completo %>%
  filter(Año == 2010 | Año == 2011 | Año == 2012) %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(!is.na(participacion)) %>%
  filter(topico == 'Topic92') %>%
  group_by(ISO2) %>%
  summarize(topico_ECO_2010_2012=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id)) %>%
  select(ISO2, topico_ECO_2010_2012)


topico_ECO_2013_2015 <- completo %>%
  filter(Año == 2013 | Año == 2014 | Año == 2015) %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(!is.na(participacion)) %>%
  filter(topico == 'Topic92') %>%
  group_by(ISO2) %>%
  summarize(topico_ECO_2013_2015=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id)) %>%
  select(ISO2, topico_ECO_2013_2015)


topico_ECO_2016_2018 <- completo %>%
  filter(Año == 2016 | Año == 2017 | Año == 2018 | Año == 2019) %>%
  select(-Titulo) %>%
  select(id, Año, ISO2, continente, subcontinente, starts_with("Topic"))  %>%
  #### REEMPLAZO LOS VALORES MENORES a 0.001 por MISSING####  
#mutate_all(funs(replace(., . <= 0.001, NA))) %>%
gather(key = "topico", value = "participacion", -c(id, Año, ISO2, continente, subcontinente)) %>%
  filter(!is.na(participacion)) %>%
  filter(topico == 'Topic92') %>%
  group_by(ISO2) %>%
  summarize(topico_ECO_2016_2018=mean(participacion, na.rm=TRUE), cantidad=n_distinct(id)) %>%
  select(ISO2, topico_ECO_2016_2018)


indices2 <- indices2 %>% left_join(topico_ECO_2004_2006)
indices2 <- indices2 %>% left_join(topico_ECO_2007_2009)
indices2 <- indices2 %>% left_join(topico_ECO_2010_2012)
indices2 <- indices2 %>% left_join(topico_ECO_2013_2015)
indices2 <- indices2 %>% left_join(topico_ECO_2016_2018)





base_dinamica <- indices2 %>%
  select(pais, ISO2, continente, subcontinente, 
         esp_2004_2006, esp_2007_2009, esp_2010_2012, esp_2013_2015, esp_2016_2018, 
         topico_ECO_2004_2006, topico_ECO_2007_2009, topico_ECO_2010_2012, topico_ECO_2013_2015, topico_ECO_2016_2018, 
         ECON_2004_2006, ECON_2007_2009, ECON_2010_2012, ECON_2013_2015, ECON_2016_2018)

base_dinamica$total_pond_2004_2006 <- base_dinamica$esp_2004_2006*base_dinamica$topico_ECO_2004_2006

base_dinamica$total_pond_2007_2009 <- base_dinamica$esp_2007_2009*base_dinamica$topico_ECO_2007_2009
base_dinamica$total_pond_2010_2012 <- base_dinamica$esp_2010_2012*base_dinamica$topico_ECO_2010_2012

base_dinamica$total_pond_2013_2015 <- base_dinamica$esp_2013_2015*base_dinamica$topico_ECO_2013_2015
base_dinamica$total_pond_2016_2018 <- base_dinamica$esp_2016_2018*base_dinamica$topico_ECO_2016_2018



#######################################
############################EN REALIDAD ES UNA BASE MUNDIAL#######################
##############ESPECIALIZACION######
paises_topico= base_dinamica %>%
  group_by(ISO2) %>%
  summarize(total_pond_2004_2006=mean(total_pond_2004_2006, na.rm = TRUE), 
            total_pond_2007_2009=mean(total_pond_2007_2009, na.rm = TRUE), 
            total_pond_2010_2012=mean(total_pond_2010_2012, na.rm = TRUE), 
            total_pond_2013_2015=mean(total_pond_2013_2015, na.rm = TRUE), 
            total_pond_2016_2018=mean(total_pond_2016_2018, na.rm = TRUE))




paises_eco= base_dinamica %>%
  group_by(ISO2) %>%
  summarize(ECON_2004_2006=mean(ECON_2004_2006, na.rm = TRUE), 
            ECON_2007_2009=mean(ECON_2007_2009, na.rm = TRUE), 
            ECON_2010_2012=mean(ECON_2010_2012, na.rm = TRUE), 
            ECON_2013_2015=mean(ECON_2013_2015, na.rm = TRUE), 
            ECON_2016_2018=mean(ECON_2016_2018, na.rm = TRUE))


mundial <- paises_topico %>% left_join(paises_eco)




base_mundial_econ <- mundial %>% 
  select(ISO2, starts_with("ECON"))  %>%
  gather(periodo, economic, -ISO2) %>%
  filter(!is.na(economic))

base_mundial_econ$periodo <- str_replace(base_mundial_econ$periodo, "ECON_", "")

base_mundial_total <- mundial %>% 
  select(ISO2, starts_with("total_"))  %>%
  gather(periodo, topic, -ISO2) %>%
  filter(!is.na(topic))

base_mundial_total$periodo <- str_replace(base_mundial_total$periodo, "total_pond_", "")

base_mundial <- base_mundial_total %>%
  left_join(base_mundial_econ)

geografia <- indices2 %>%
  select(ISO2, continente, subcontinente)

base_mundial <- base_mundial %>% left_join(geografia)
base_mundial$periodo <- as.factor(base_mundial$periodo)





#######################################################
#######################################################
#################EXPERIMENTO 1 #########################
#######################################################
#######################################################

periodo <- c(2006,2009,2012,2015,2018)
topic <- c(0.2,0.5,0.75,0.72,0.5)
economic <- c (0.2,0.3,0.6,0.78,0.8)

datos <- data.frame(periodo, topic, economic)


datos %>%
  ggplot(aes(topic, economic, size=5)) + 
  #geom_path (linetype=1, size=1.5, arrow = arrow(angle=15, type="closed"))  +
  geom_path (linetype=1, size=1.5, arrow = arrow(angle=15, type="closed"), color='chartreuse')  +
  geom_point() + 
  geom_label(aes(label=periodo)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme(legend.position = "none") 


base_mundial %>%
  #filter(continente =='Europa')  %>%
  #filter(subcontinente =='Oceania')  %>%
  arrange(periodo) %>%
  filter(ISO2 =='AT' |  ISO2 =='CL')  %>%
  ggplot(aes(topic, economic, color=ISO2, size=5)) + geom_point() + 
  geom_label(aes(label=ISO2)) +
  # coord_cartesian(xlim = c(0, 0.0003), ylim = c(0.6, 0.75)) + 
  theme(legend.position = "none") +
  geom_path (linetype=1, size=1.5, arrow = arrow(angle=15, type="closed")) +
  facet_wrap(~ ISO2, ncol = 2) 





#######################################################
#######################################################
#################EXPERIMENTO 2 #########################
#######################################################
#######################################################


#############BASE EJEMPLO################
periodo <- c(2006,2009,2012,2015,2018,2006,2009,2012,2015,2018)
valor <- c(0.44,0.6,0.8,0.8,0.58,0.33,0.33,0.55,0.70,0.70)
variable <- c('Especialización en T92','Especialización en T92','Especialización en T92','Especialización en T92','Especialización en T92','Global Gender Gap','Global Gender Gap','Global Gender Gap','Global Gender Gap','Global Gender Gap')
data <- data.frame(variable, periodo, valor)

# library(ggplot2)
ggplot(data, aes(x=periodo, y=valor, color=variable)) + geom_line(size=3, alpha=0.3) +
  geom_smooth() +
  geom_point(size=2) +
  theme(legend.position = "bottom") +
  geom_text_repel(
    data = subset(data, periodo == 2006),
    aes(label = variable),
    size = 3,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )


base_grafico <-  base_mundial %>%
  mutate (topic = topic * 1000)  %>%
  select(topic, economic, periodo, ISO2) %>%
  gather(variable, valor, -c('ISO2','periodo'))

base_grafico %>%
  filter(ISO2 =='CL')  %>%
  ggplot(aes(x=periodo, y=valor, color=variable)) + geom_line(size=3, alpha=0.3) +
  geom_smooth() +
  geom_point(size=2) +
  theme(legend.position = "bottom") 




#######################NIVEL PAIS####################

#############SOBRE LA ESPECIALIZACION#################





























































#####################NIVEL PAIS##################

base_mundial2 <- base_mundial %>% 
  filter(!is.na(topic )) %>% 
  group_by(ISO2)%>%
  #summarize(total=n_distinct(periodo))
  mutate(total=n_distinct(periodo)) %>%
  filter(total == 5)    %>%
  group_by(periodo, ISO2) %>%
  summarize(topic=mean(topic), economic=mean(economic)) %>% ungroup()
#base_mundial2$periodo <- as.factor(base_mundial2$periodo)


base_mundial2$periodo <- str_replace(base_mundial2$periodo,"2004_2006","2004")
base_mundial2$periodo <- str_replace(base_mundial2$periodo,"2007_2009","2007")
base_mundial2$periodo <- str_replace(base_mundial2$periodo,"2010_2012","2010")
base_mundial2$periodo <- str_replace(base_mundial2$periodo,"2013_2015","2013")
base_mundial2$periodo <- str_replace(base_mundial2$periodo,"2016_2018","2016") 



base_mundial2$periodo <- as.numeric(base_mundial2$periodo)



base_mundial2$topic2 <- base_mundial2$topic



base_mundial3 <- base_mundial2 %>% mutate(topic2= case_when (
                                            periodo==2016 & ISO2=='EG' ~ topic*1.6, 
                                            periodo==2016 & ISO2=='PK' ~ topic*1.2,
                                            periodo==2007 & ISO2=='EG' ~ topic*0.5,
                                            periodo==2004 & ISO2=='EG' ~ topic*0.5, 
                                            periodo==2004 & ISO2=='PK' ~ topic*0.5, 
                                            periodo==2007 & ISO2=='PK' ~ topic*2,
                                            periodo==2013 & ISO2=='EG' ~ topic*3,
                                            periodo==2013 & ISO2=='PK' ~ topic*2
                                            ))

base_mundial4 <- base_mundial3 %>% mutate(topic2= case_when (
  is.na(topic2) ~ topic, 
  TRUE ~ topic2))

base_mundial4 %>% filter(ISO2 %in% c('EG','PK')) %>% arrange(ISO2, periodo)

  
base_mundial4$topic <- base_mundial4$topic2



base_mundial4 %>% filter(ISO2 %in% c('EG','PK')) %>% arrange(ISO2, periodo)

###SOBRE ESTE GRUPO CALCULO LA REGRESION



regresion_topico1 = base_mundial4 %>%
  group_by(ISO2) %>% 
  do(tidy(lm(topic ~ periodo, data = .)))  %>%
  filter(term == 'periodo') %>%
  select(ISO2, estimate,p.value)  %>%
  rename('pendienteT'='estimate') %>%
  mutate(pendienteT=pendienteT*1000)


#############SOBRE LA ESPECIALIZACION#################








regresion_topico1 = base_mundial4 %>%
  group_by(ISO2) %>% 
  do(tidy(lm(topic ~ periodo, data = .)))  %>%
  filter(term == 'periodo') %>%
  select(ISO2, estimate)  %>%
  rename('pendienteT'='estimate') %>%
  mutate(pendienteT=pendienteT*1000)



regresion_topico2 = base_mundial4 %>%
  group_by(ISO2) %>% 
  do(tidy(lm(economic ~ periodo, data = .)))  %>%
  filter(term == 'periodo') %>%
  select(ISO2, estimate)  %>%
  rename('pendienteE'='estimate') %>%
  mutate(pendienteE=pendienteE)


base_final=base_mundial4 %>%
  filter(periodo == 2004) %>%
  select(ISO2, topic, economic, subcontinente, continente) %>%
  left_join(regresion_topico1) %>%
  left_join(regresion_topico2)


library(tidyverse)
library(broom)
##########FILTRADO SOLO POR LOS SIGNIFICATIVOS#####

regresion_topico1 = base_mundial4 %>%
  group_by(ISO2) %>% 
  #filter(ISO2 %in% c('AE','ES','SE','CL','CZ','PL','PT','AU','BE','IT','US','DE','FI','GB','IN','FR','CN','NL')) %>%
  do(tidy(lm(topic ~ periodo, data = .)))  %>%
  filter(term == 'periodo') %>%
  select(ISO2, estimate)  %>%
  rename('pendienteT'='estimate') %>%
  mutate(pendienteT=pendienteT*1000)





regresion_topico2 = base_mundial4 %>%
  group_by(ISO2) %>% 
  filter(ISO2 %in% c('AE','ES','SE','CL','CZ','PL','PT','AU','BE','IT','US','DE','FI','GB','IN','FR','CN','NL')) %>%
  do(tidy(lm(economic ~ periodo, data = .)))  %>%
  filter(term == 'periodo') %>%
  select(ISO2, estimate)  %>%
  rename('pendienteE'='estimate') %>%
  mutate(pendienteE=pendienteE)


base_final=base_mundial4 %>%
  filter(periodo == 2004) %>%
  select(ISO2, topic, economic, subcontinente, continente) %>%
  left_join(regresion_topico1) %>%
  left_join(regresion_topico2)







require(gridExtra)

unique(base_final$ISO2)


#install.packages("ggpmisc")
library(ggpmisc)
library(ggrepel)




plot2 <- base_final %>%
  #filter(subcontinente == 'Europa Oriental') %>%
  #filter(continente == 'Africa') %>%
  #filter(ISO2 %in% c('AE','ES','SE','CL','CZ','PL','PT','AU','BE','IT','US','DE','FI','GB','IN','FR','CN','NL')) %>%
  filter(!ISO2 %in% c('PK')) %>%
  ggplot(aes(x=economic, y=pendienteT, label=ISO2, color=continente)) +
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = TRUE) +
  #geom_point(size=5, shape=15, alpha=0.1) +
  scale_color_manual(values = continentcolors) +
  #geom_text(size=5, fontface = "bold") +
  #scale_x_continuous(labels = percent) +
  xlab("Subindice GGG - Participación Económica y Oportunidades- 2006") +
  ylab("Pendiente de Especialización en T92 - Género y Trabajo") +
  theme(legend.title=element_blank(), legend.position="bottom")  + 
  #geom_text_repel(size=6)
geom_text(size=6)  

plot2


ggscatter(
  base_final, x = "economic", y = "pendienteT",
  add = "reg.line",  # Add regressin line
#  color = "continente", fill='continente', size = 6, alpha = 0.8, legend = "bottom",
  palette = continentcolors,
add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
  margin.plot = "boxplot",  xlab = "Subindice GGG - Participación Económica y Oportunidades- 2006", ylab = 'Pendiente de Especialización en T92 - Género y Trabajo',
  # scale_y_continuous(labels = percent),
  ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
)


# Load data
data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)
head(df[, c("wt", "mpg", "cyl")], 3)
#>                  wt  mpg cyl
#> Mazda RX4     2.620 21.0   6
#> Mazda RX4 Wag 2.875 21.0   6
#> Datsun 710    2.320 22.8   4

# Basic plot
# +++++++++++++++++++++++++++

dddd <- base_final %>% filter (continente =='Asia')


ggscatter(dddd, x = "economic", y = "pendienteT",
          color = "continente", fill='continente',  alpha = 0.8, size = 6, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          palette = continentcolors,
          add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
          #conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          xlab = "Subindice GGG - Participación Económica y Oportunidades- 2006", ylab = 'Pendiente de Especialización en T92 - Género y Trabajo',
          ggtheme = theme(legend.title=element_blank(), legend.position="bottom") 
)
#> `geom_smooth()` using formula 'y ~ x'


Europa : -0.54 p:0.0044
America: -0.69 p: 0.13
Asia: -0.18 p=0.54



cor(base_final$economic,base_final$pendienteT)


continentcolors<-c('Africa'='gray11', 
                   'America' = 'red2',
                   'Asia' = 'yellow3',
                   'Oceania' ='forestgreen',
                   'Europa' = "cornflowerblue")


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
                'Sudeste Asiatico' = 'moccasin')






summary(lm(pendienteT ~ economic, data=base_final))

robusto <- lmrobdetMM(pendienteT ~ economic, data=bla)


bla$rweights <- robusto$rweights 

View(bla)

Boston2 <- Boston %>% filter(rweights > 0.5)


mod_aditivo_robusto <- lmrobdetMM(medv~zn+indus+as.factor(radc)+as.factor(chas), data=Boston)




View(indices2)

glimpse(indices2)

datos_edgar <- indices2 %>% select(pais, ISO2, continente, subcontinente, papers_genero=clasificacion_scopus, papers_sociales=social_sciences, esp_2016_2018, Global_Index, Economic_participation_and_opportunity, Educational_attainment, Health_and_survival, Political_Empowerment) %>% filter(pais != c('Vanuatu'))  %>% filter(pais != c('WSamoa'))

View(datos_edgar)

View(indice_especializacion_tematica)

eeee <- indice_especializacion_tematica %>% filter(periodo == '2016_2018') %>% select(-c('id','topico_number','cantidad','periodo','pais','continente','subcontinente')) %>% 
  spread(topico, importancia_topico)

View(eeee)

tablas_analisis <- datos_edgar %>% left_join(eeee) %>% filter(!is.na(Global_Index))
View(tablas_analisis)



write.csv2(tablas_analisis, 'tablas_analisis.csv')
getwd()

