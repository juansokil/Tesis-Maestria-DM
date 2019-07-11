
library(readr)
library(dplyr)
library(tidyr)
library(HH)
library(ggplot2)
library(dunn.test)

indices2 <- read_delim("https://raw.githubusercontent.com/juansokil/Scripts-Tesis/master/resultados/total_indices.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
indices2$especializacion_pct = indices2$especializacion*100

####Selecciono las variables que me sirven####
indice_gdi <- indices2 %>% select (country, ISO2, ISO3, continente, subcontinente, especializacion, Global_Index)

###Transformo la base
indice_gathered_1 <- indice_gdi %>%
  gather(key = Global_index, value = measurement, -c(country, ISO2, ISO3, continente, subcontinente, especializacion) )

indice_gathered_1_head <- indice_gathered_1 %>% arrange(desc(especializacion))
indice_gathered_1_head <- head(indice_gathered_1_head,92)


####TEST DE HIPOTESIS####
indice_gathered_1_head %>%
  group_by(continente) %>%
  summarize(mean(especializacion), n())

###EXCLUYO OCEANIA PORQUE TIENE MENOS DE 3 CASOS####
indice_gathered_1_head_gathered <- 
  indice_gathered_1_head %>%
  filter(continente %in%  c('Africa', 'Americas', 'Asia', 'Europe')) 

indice_gathered_1_head_gathered %>%
  summarise(statistic = shapiro.test(especializacion)$statistic,
            p.value = shapiro.test(especializacion)$p.value)

#################SUPUESTOS######################

#####Normalidad#####
indice_gathered_1_head_gathered %>%
  group_by(continente) %>%
  summarise(statistic = shapiro.test(especializacion)$statistic,
            p.value = shapiro.test(especializacion)$p.value)

ggplot(indice_gathered_1_head_gathered, aes(x=especializacion)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  facet_wrap(~continente) +
  geom_density(alpha=.2, fill="#FF6666") 


#####Homocedasticidad#####
bartlett.test(especializacion ~ continente, data = indice_gathered_1_head_gathered)
fligner.test(especializacion ~ continente, data = indice_gathered_1_head_gathered)

# Homogeneity of Variance Plot
hov(especializacion ~ continente, data = indice_gathered_1_head_gathered)
hovPlot(especializacion ~ continente, data = indice_gathered_1_head_gathered)

###Una vez realizadas las pruebas, se ve que test se puede aplicar###


###ANOVA en el caso que supere ambas pruebas
model1<- aov(indice_gathered_1_head_gathered$especializacion~indice_gathered_1_head_gathered$continente)
summary(model1)
plot(model1)

###Post hoc que nos permite ver la diferencia entre que grupos# - SIRVE PARA ANOVA##

tuk<-TukeyHSD(model1, conf.level = 0.99)
psig=as.numeric(apply(tuk$`indice_gathered_1_head_gathered$subcontinente`[,2:3],1,prod)>=0)+1
op=par(mar=c(4.2,9,3.8,2))
plot(tuk,col=psig,yaxt="n")
for (j in 1:length(psig)){
  axis(2,at=j,labels=rownames(tuk$`indice_gathered_1_head_gathered$subcontinente`)[length(psig)-j+1],
       las=1,cex.axis=.55,col.axis=psig[length(psig)-j+1])
}
par(op)




###Sino supera la homocedasticidad y la normalidad se pueden aplicar cualquiera de estas dos pruebas###
oneway.test(indice_gathered_1_head_gathered$especializacion~indice_gathered_1_head_gathered$continente)
kruskal.test(indice_gathered_1_head_gathered$especializacion~indice_gathered_1_head_gathered$continente)


###Post hoc que nos permite ver la diferencia entre que grupos# - SIRVE PARA KRUSKAL TEST##
dt <- dunn.test(indice_gathered_1_head_gathered$especializacion, indice_gathered_1_head_gathered$continente)
dunnTest <- as.data.frame(dt)







##############CORRELACIONES#######################
####Selecciono las variables que me sirven####
indices_gdp <- indices2 %>% arrange(desc(gdp_capita))
indices_gdp <- head(indices_gdp,90)

indices_gdp %>% 
  select (country, ISO2, ISO3, continente, subcontinente, especializacion, gdp_capita) %>% 
filter(continente %in% c('Americas','Asia','Europe','Africa')) %>% 
  group_by(continente) %>% 
  summarise(COR=cor(especializacion, gdp_capita), P_VALUE=cor.test(especializacion, gdp_capita)$p.value, count=n()) 

indices_gdp %>% 
  select (country, ISO2, ISO3, continente, subcontinente, especializacion, gdp_capita) %>% 
  filter(subcontinente %in% c('Eastern Africa','Eastern Europe','South America','Channel Islands',
                              'Southern Europe','Western Europe','Western Asia','South-eastern Asia'
                              ,'Southern Asia','Northern Africa')) %>%  
  group_by(subcontinente) %>% 
  summarise(COR=cor(especializacion, gdp_capita), P_VALUE=cor.test(especializacion, gdp_capita)$p.value, count=n()) 







#############################REGRESIONES#########################

totales <- read_delim("C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/graficos/totales.csv", ";", escape_double = FALSE, trim_ws = TRUE)
lm.genero <- lm(`Estudios de G<e9>nero` ~ year, data=totales)
sm <- summary(lm.genero)
anova(lm.genero)
# Function for Root Mean Squared Error
RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(sm$residuals)
# Function for Mean Absolute Error
mae <- function(error) { mean(abs(error)) }
mae(sm$residuals)


