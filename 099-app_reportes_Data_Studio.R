




### LIBRERIAS###
#install.packages("shiny")
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(here)
library(maptools)
library(maps)
library(readr)
library(tidyr)
library(plotly)
library(directlabels)
library(ggrepel)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(rlist)
library(RJSONIO)
library(dplyr)
library(tibble)
######LIBRERIAS#############



##################################################################
##MODULO CONTEXTO#####
indicador01_name="POBLA"
indicador02_name="PBIPPC"


##MODULO GASTO#####
indicador03_name="GAS_IMD_PPC"
indicador04_name="GASTOxPBI"
indicador05_name="GASIDSFPER"
indicador06_name="GASIDSEPER"








##################################################################

#LEVANTA PAISES y ASIGNA COLORES
listado <- RJSONIO::fromJSON("http://dev.ricyt.org/api/comparative/AR,BO,BR,CL,CO,CR,CU,EC,SV,ES,GT,HN,JM,MX,NI,PA,PY,PE,PT,PR,DO,TT,UY,VE,AL/2015/POBLA")
pais <- c()
i=1
for (i in 1:25){
  pais[i] <- listado$metadata$countries[[i]]$name_es
}
pais <- as.data.frame(pais)


prefijo <- c()
i=1
for (i in 1:25){
  prefijo[i] <- listado$metadata$countries[[i]]$country_id
}
prefijo <- as.data.frame(prefijo)
listado <- cbind(pais,prefijo)


min=2000
max=2017

pais1 <- data.frame(year = c(min:max), "country" = as.character(c("Argentina")), "prefijo" = as.character(c("AR")))
pais2 <- data.frame(year = c(min:max), "country"= as.character(c("Bolivia")), "prefijo" = as.character(c("BO")))
pais3 <- data.frame(year = c(min:max), "country" = as.character(c("Brasil")), "prefijo" = as.character(c("BR")))
pais4 <- data.frame(year = c(min:max), "country"= as.character(c("Chile")), "prefijo" = as.character(c("CL")))
pais5 <- data.frame(year = c(min:max), "country"= as.character(c("Colombia")), "prefijo" = as.character(c("CO")))
pais6 <- data.frame(year = c(min:max), "country"= as.character(c("Costa Rica")), "prefijo" = as.character(c("CR")))
pais7 <- data.frame(year = c(min:max), "country"= as.character(c("Cuba")), "prefijo" = as.character(c("CU")))
pais8 <- data.frame(year = c(min:max), "country"= as.character(c("Ecuador")), "prefijo" = as.character(c("EC")))
pais9 <- data.frame(year = c(min:max), "country"= as.character(c("El Salvador")), "prefijo" = as.character(c("SV")))
pais10 <- data.frame(year = c(min:max), "country"= as.character(c("España")), "prefijo" = as.character(c("ES")))
pais11 <- data.frame(year = c(min:max), "country"= as.character(c("Guatemala")), "prefijo" = as.character(c("GT")))
pais12 <- data.frame(year = c(min:max), "country"= as.character(c("Honduras")), "prefijo" = as.character(c("HO")))
pais13 <- data.frame(year = c(min:max), "country"= as.character(c("Jamaica")), "prefijo" = as.character(c("JM")))
pais14 <- data.frame(year = c(min:max), "country"= as.character(c("México")), "prefijo" = as.character(c("MX")))
pais15 <- data.frame(year = c(min:max), "country"= as.character(c("Nicaragua")), "prefijo" = as.character(c("NI")))
pais16 <- data.frame(year = c(min:max), "country"= as.character(c("Panamá")), "prefijo" = as.character(c("PA")))
pais17 <- data.frame(year = c(min:max), "country"= as.character(c("Paraguay")), "prefijo" = as.character(c("PY")))
pais18 <- data.frame(year = c(min:max), "country"= as.character(c("Perú")), "prefijo" = as.character(c("PE")))
pais19 <- data.frame(year = c(min:max), "country"= as.character(c("Portugal")), "prefijo" = as.character(c("PT")))
pais20 <- data.frame(year = c(min:max), "country"= as.character(c("Puerto Rico")), "prefijo" = as.character(c("PR")))
pais21 <- data.frame(year = c(min:max), "country"= as.character(c("Dominicana")), "prefijo" = as.character(c("DO")))
pais22 <- data.frame(year = c(min:max), "country"= as.character(c("Trinidad y Tobago")), "prefijo" = as.character(c("TT")))
pais23 <- data.frame(year = c(min:max), "country"= as.character(c("Uruguay")), "prefijo" = as.character(c("UY")))
pais24 <- data.frame(year = c(min:max), "country"= as.character(c("Venezuela")), "prefijo" = as.character(c("VE")))
pais25 <- data.frame(year = c(min:max), "country"= as.character(c("América Latina y el Caribe")), "prefijo" = as.character(c("AL")))

colores <- as.data.frame(rainbow(25))
estructura <- rbind(pais1, pais2, pais3, pais4, pais5, pais6, pais7, pais8, pais9, pais10, pais11, pais12, pais13, pais14, pais15, pais16, pais17, pais18, pais19, pais20, pais21, pais22, pais23, pais24, pais25)
country <- unique(estructura$country)
prefijo <- unique(estructura$prefijo)
pais_color <- cbind(country,colores)

rm(list = c('pais1','pais2','pais3','pais4','pais5','pais6','pais7','pais8','pais9','pais10','pais11','pais12','pais13','pais14','pais15','pais16','pais17','pais18','pais19','pais20','pais21','pais22','pais23','pais24','pais25','listado','colores','pais'))

string01 <- paste("http://dev.ricyt.org/api/comparative/AR,BO,BR,CO,CR,CU,CL,EC,SV,GT,HN,JM,MX,NI,PA,PY,PE,PT,TT,UY,VE,AL/1990/",indicador01_name,sep="")
indicador01 <- RJSONIO::fromJSON(string02)
string02 <- paste("http://dev.ricyt.org/api/comparative/AR,BO,BR,CO,CR,CU,CL,EC,SV,GT,HN,JM,MX,NI,PA,PY,PE,PT,TT,UY,VE,AL/1990/",indicador02_name,sep="")
indicador02 <- RJSONIO::fromJSON(string02)
string03 <- paste("http://dev.ricyt.org/api/comparative/AR,BO,BR,CO,CR,CU,CL,EC,SV,GT,HN,JM,MX,NI,PA,PY,PE,PT,TT,UY,VE,AL/1990/",indicador03_name,sep="")
indicador03 <- RJSONIO::fromJSON(string03)
string04 <- paste("http://dev.ricyt.org/api/comparative/AR,BO,BR,CO,CR,CU,CL,EC,SV,GT,HN,JM,MX,NI,PA,PY,PE,PT,TT,UY,VE,AL/1990/",indicador03_name,sep="")
indicador04 <- RJSONIO::fromJSON(string04)
string05 <- paste("http://dev.ricyt.org/api/comparative/AR,BO,BR,CO,CR,CU,CL,EC,SV,GT,HN,JM,MX,NI,PA,PY,PE,PT,TT,UY,VE,AL/1990/",indicador03_name,sep="")
indicador05 <- RJSONIO::fromJSON(string05)
string06 <- paste("http://dev.ricyt.org/api/comparative/AR,BO,BR,CO,CR,CU,CL,EC,SV,GT,HN,JM,MX,NI,PA,PY,PE,PT,TT,UY,VE,AL/1990/",indicador03_name,sep="")
indicador06 <- RJSONIO::fromJSON(string06)





#####INDICADOR 01######################
indicador01_completo = data.frame()
for (i in 1:(length(indicador01$metadata$countries))){
  if (ncol(cbind(indicador01$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador01$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador01$indicators[[1]]$countries[[i]]$name_es))==3) 
  {
    indicador01_completo <- rbind(indicador01_completo,assign(paste('pais',i, sep = ""), as.data.frame(cbind(indicador01$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador01$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador01$indicators[[1]]$countries[[i]]$name_es))))
  }
}
indicador01
indicador01$indicators[[1]]$countries[[i]]$rows[[1]]$values
indicador01_completo <-  rownames_to_column(as.data.frame(indicador01_completo) , var = "rowname")
indicador01_completo$rowname <- substr(indicador01_completo$rowname, 1, 4)
indicador01_completo$rowname <- as.numeric(as.character(indicador01_completo$rowname))
indicador01_completo$V1 <- as.numeric(as.character(indicador01_completo$V1))
colnames(indicador01_completo)<- c("year",indicador01_name,"medida","country")





#####INDICADOR 02######################

indicador02_completo = data.frame()
for (i in 1:(length(indicador02$metadata$countries))){
  if (ncol(cbind(indicador02$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador02$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador02$indicators[[1]]$countries[[i]]$name_es))==3) 
  {
    indicador02_completo <- rbind(indicador02_completo,assign(paste('pais',i, sep = ""), as.data.frame(cbind(indicador02$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador02$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador02$indicators[[1]]$countries[[i]]$name_es))))
  }
}

indicador02_completo <-  rownames_to_column(as.data.frame(indicador02_completo) , var = "rowname")
indicador02_completo$rowname <- substr(indicador02_completo$rowname, 1, 4)
indicador02_completo$rowname <- as.numeric(as.character(indicador02_completo$rowname))
indicador02_completo$V1 <- as.numeric(as.character(indicador02_completo$V1))
colnames(indicador02_completo)<- c("year",indicador02_name,"medida","country")

#####INDICADOR 03######################
indicador03_completo = data.frame()
for (i in 1:(length(indicador03$metadata$countries))){
  if (ncol(cbind(indicador03$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador03$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador03$indicators[[1]]$countries[[i]]$name_es))==3) 
  {
    indicador03_completo <- rbind(indicador03_completo,assign(paste('pais',i, sep = ""), as.data.frame(cbind(indicador03$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador03$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador03$indicators[[1]]$countries[[i]]$name_es))))
  }
}

indicador03_completo <-  rownames_to_column(as.data.frame(indicador03_completo) , var = "rowname")
indicador03_completo$rowname <- substr(indicador03_completo$rowname, 1, 4)

indicador03_completo$rowname <- as.numeric(as.character(indicador03_completo$rowname))
indicador03_completo$V1 <- as.numeric(as.character(indicador03_completo$V1))
colnames(indicador03_completo)<- c("year",indicador03_name,"medida","country")



#####INDICADOR 04######################
indicador04_completo = data.frame()
for (i in 1:(length(indicador04$metadata$countries))){
  if (ncol(cbind(indicador04$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador04$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador04$indicators[[1]]$countries[[i]]$name_es))==3) 
  {
    indicador04_completo <- rbind(indicador04_completo,assign(paste('pais',i, sep = ""), as.data.frame(cbind(indicador04$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador04$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador04$indicators[[1]]$countries[[i]]$name_es))))
  }
}

indicador04_completo <-  rownames_to_column(as.data.frame(indicador04_completo) , var = "rowname")
indicador04_completo$rowname <- substr(indicador04_completo$rowname, 1, 4)

indicador04_completo$rowname <- as.numeric(as.character(indicador04_completo$rowname))
indicador04_completo$V1 <- as.numeric(as.character(indicador04_completo$V1))
colnames(indicador04_completo)<- c("year",indicador04_name,"medida","country")


#####INDICADOR 05######################
indicador05_completo = data.frame()
for (i in 1:(length(indicador05$metadata$countries))){
  if (ncol(cbind(indicador05$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador05$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador05$indicators[[1]]$countries[[i]]$name_es))==3) 
  {
    indicador05_completo <- rbind(indicador05_completo,assign(paste('pais',i, sep = ""), as.data.frame(cbind(indicador05$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador05$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador05$indicators[[1]]$countries[[i]]$name_es))))
  }
}

indicador05_completo <-  rownames_to_column(as.data.frame(indicador05_completo) , var = "rowname")
indicador05_completo$rowname <- substr(indicador05_completo$rowname, 1, 4)

indicador05_completo$rowname <- as.numeric(as.character(indicador05_completo$rowname))
indicador05_completo$V1 <- as.numeric(as.character(indicador05_completo$V1))
colnames(indicador05_completo)<- c("year",indicador05_name,"medida","country")


#####INDICADOR 06######################
indicador06_completo = data.frame()
for (i in 1:(length(indicador06$metadata$countries))){
  if (ncol(cbind(indicador06$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador06$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador06$indicators[[1]]$countries[[i]]$name_es))==3) 
  {
    indicador06_completo <- rbind(indicador06_completo,assign(paste('pais',i, sep = ""), as.data.frame(cbind(indicador06$indicators[[1]]$countries[[i]]$rows[[1]]$values, indicador06$indicators[[1]]$countries[[i]]$rows[[1]]$name_es, indicador06$indicators[[1]]$countries[[i]]$name_es))))
  }
}

indicador06_completo <-  rownames_to_column(as.data.frame(indicador06_completo) , var = "rowname")
indicador06_completo$rowname <- substr(indicador06_completo$rowname, 1, 4)

indicador06_completo$rowname <- as.numeric(as.character(indicador06_completo$rowname))
indicador06_completo$V1 <- as.numeric(as.character(indicador06_completo$V1))
colnames(indicador06_completo)<- c("year",indicador06_name,"medida","country")





















###############JUNTA DATOS######################

estructura2 <- left_join(estructura, pais_color, by = "country")
df1 <- left_join(estructura2, indicador01_completo, by = c("year", "country"))
df1 <- left_join(df1, indicador02_completo, by = c("year", "country"))
df1 <- left_join(df1, indicador03_completo, by = c("year", "country"))
df1 <- left_join(df1, indicador04_completo, by = c("year", "country"))
df1 <- left_join(df1, indicador05_completo, by = c("year", "country"))
df1 <- left_join(df1, indicador06_completo, by = c("year", "country"))

#Filtro las columnas y dejo solo los que tienen datos completos

df1 <- setnames(df1, "rainbow(25)", "colores")
View(df1)


###########DICCIONARIO DE DATOS####

indicador01$indicators[[1]]$id[[1]]
indicador01$indicators[[1]]$name_es[[1]]
indicador02$indicators[[1]]$id[[1]]
indicador02$indicators[[1]]$name_es[[1]]
indicador03$indicators[[1]]$id[[1]]
indicador03$indicators[[1]]$name_es[[1]]
indicador04$indicators[[1]]$id[[1]]
indicador04$indicators[[1]]$name_es[[1]]
indicador05$indicators[[1]]$id[[1]]
indicador05$indicators[[1]]$name_es[[1]]
indicador06$indicators[[1]]$id[[1]]
indicador06$indicators[[1]]$name_es[[1]]

diccionario_var <- c(indicador01$indicators[[1]]$id[[1]],indicador02$indicators[[1]]$id[[1]],indicador03$indicators[[1]]$id[[1]])
diccionario_label <- c(indicador01$indicators[[1]]$name_es[[1]],indicador02$indicators[[1]]$name_es[[1]],indicador03$indicators[[1]]$name_es[[1]])
























#setwd("C:/Users/observatorio/Documents/Scripts-Tesis2")

####Conector Shiny###

rsconnect::setAccountInfo(name='juanpablosokil', 
                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')


####### BASES ######


###ESTA ES LA BASE RESUMIDA
base <- read_delim("./base_app/base_resumida.csv", ",", col_types = cols(X1 = col_skip()))

base <- base %>%
  rename(year = Year) %>%
  rename(term = topico) %>%
  rename(prob = mean)


#topicos <- unique(base$term)
#topicos <- as.data.frame(cbind(topicos, colores))
#setnames(topicos, "rainbow(100)", "colores")
#base <- left_join(base, topicos, by = c("term"="topicos"))



#########################################


####RELEVANCE#####
#write.csv(base_topic, 'datos_tsne_COMPLETO.csv')
###LEVANTAR DATOS TSNE COMPLETO CON CSV Y EJECUTAR TODO LO QUE ESTA ACA
#################################BASE TSNE - POR ANIOS ###############################
base_topic <- read_delim('./base_app/datos_tsne_COMPLETO.txt',"\t")
base_topic2 <- base_topic %>%
  select (Category,  Term, year, Relevance)
base_topic2 <- base_topic2 %>% 
  rename(word = Term)
base_topic2 <- base_topic2 %>% 
  rename(term = Category)
base_topic2 <- base_topic2 %>% 
  filter (term != 'Default')

base_topic2 <- base_topic2 %>%
  group_by(term, year) %>%
  top_n(15, Relevance) %>%
  ungroup() %>%
  arrange(term, year, desc(-Relevance))

base_topic2$importancia <- (base_topic2$Relevance)+3

##################################################################################
########################BASE TSNE - TOTAL ########################################

base_topic_total <- read_delim('./base_app/datos_tsne.txt', delim ='\t')
base_topic_total$Relevance <- base_topic_total$loglift*0.4+base_topic_total$logprob*0.6

base_topic_total2 <- base_topic_total %>%
  select (Category,  Term, Relevance)
base_topic_total2 <- base_topic_total2 %>% 
  rename(word = Term)
base_topic_total2 <- base_topic_total2 %>% 
  rename(term = Category)
base_topic_total2 <- base_topic_total2 %>% 
  filter (term != 'Default')

base_topic_total2 <- base_topic_total2 %>%
  group_by(term) %>%
  top_n(10, Relevance) %>%
  ungroup() %>%
  arrange(term, desc(-Relevance))

base_topic_total2$importancia <- (base_topic_total2$Relevance)+3

######################################################################

world <- map_data("world") 

######################################################################

global_periodos_disponibles <- c('T1'='topico01','T2'='topico02','T3'='topico03','T4'='topico04','T5'='topico05','T6'='topico06','T7'='topico07','T8'='topico08','T9'='topico09','T10'='topico10','T11'='topico11','T12'='topico12','T13'='topico13','T14'='topico14','T15'='topico15','T16'='topico16','T17'='topico17','T18'='topico18','T19'='topico19','T20'='topico20','T21'='topico21','T22'='topico22','T23'='topico23','T24'='topico24','T25'='topico25','T26'='topico26','T27'='topico27','T28'='topico28','T29'='topico29','T30'='topico30','T31'='topico31','T32'='topico32','T33'='topico33','T34'='topico34',
                                 'T35'='topico35','T36'='topico36','T37'='topico37','T38'='topico38','T39'='topico39','T40'='topico40','T41'='topico41','T42'='topico42','T43'='topico43','T44'='topico44','T45'='topico45','T46'='topico46','T47'='topico47','T48'='topico48','T49'='topico49','T50'='topico50','T51'='topico51','T52'='topico52','T53'='topico53','T54'='topico54','T55'='topico55','T56'='topico56','T57'='topico57','T58'='topico58','T59'='topico59','T60'='topico60','T61'='topico61','T62'='topico62','T63'='topico63','T64'='topico64','T65'='topico65','T66'='topico66','T67'='topico67','T68'='topico68',
                                 'T69'='topico69','T70'='topico70','T71'='topico71','T72'='topico72','T73'='topico73','T74'='topico74','T75'='topico75','T76'='topico76','T77'='topico77','T78'='topico78','T79'='topico79','T80'='topico80','T81'='topico81','T82'='topico82','T83'='topico83','T84'='topico84','T85'='topico85','T86'='topico86','T87'='topico87','T88'='topico88','T89'='topico89','T90'='topico90','T91'='topico91','T92'='topico92','T93'='topico93','T94'='topico94','T95'='topico95','T96'='topico96','T97'='topico97','T98'='topico98','T99'='topico99','T100'='topico100')

###############APLICACION SHINY##################

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Explorador sobre produccion cientifica sobre Genero"),
  sidebarLayout(sidebarPanel(
    pickerInput(inputId = "topico", label = "Seleccione los topicos",  choices = global_periodos_disponibles, options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE),
    sliderInput("year", "Evolucion anual de las palabras:", min=2003, max=2017, value=2003, animate =TRUE)),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Treemap", plotOutput(outputId = "treemap", height= "800px")),
                  tabPanel("Evolucion Topico",
                           fluidRow(plotOutput(outputId = "distPlot",height = "400px"))
                  ),
                  tabPanel("Evolucion Terminos", plotOutput(outputId = "distPlot2", height= "800px")),
                  tabPanel("Analisis Regional", plotOutput(outputId = "distPlot4", width  = "600px",height = "600px"))
      ))
  ))




server <- function(input, output, session) {
  base2 <- reactive({
    a <- base %>% filter(term %in% input$topico)
    a <- data.frame(a)
    a$year <- as.integer(a$year)
    return(a)
    
  })
  
  output$distPlot <- renderPlot({
    ggplot(base2(), aes(year, prob, color=term, label = term)) + 
      geom_point(size=5, alpha=0.3) +
      geom_line(size=2, alpha=0.2) +
      geom_text_repel(data = subset(base2(), year == max(year)),aes(color = term),size = 6, nudge_x = 3, segment.color = NA, show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 0.15)) +
      scale_x_continuous(breaks = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
      geom_smooth(size=2, alpha=0.7, se=FALSE) +
      theme(legend.title=element_blank(), axis.title.y = element_blank(), legend.position="bottom") 
  })
  

  
  base3 <- reactive({
    a <- base_topic2 %>% filter(term %in% input$topico)
    a <- data.frame(a)
    return(a)
    
  })
  
  
  #
  output$distPlot2 <- renderPlot({
    ggplot(base3(), aes(year, importancia, color=word, label = word)) + 
      geom_line(aes(group = word), size=2, alpha=0.3) +
      geom_point(size=4, alpha=0.2) +
      geom_smooth(size=2, alpha=0.7, se=FALSE, na.rm = TRUE) +
      geom_text_repel(data = subset(base3(), year == max(year)),aes(color = word),size = 4, nudge_x = 3, segment.color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
      theme(axis.title.y = element_blank(),legend.position="none") +
      theme(legend.title=element_blank(), axis.title.y = element_blank(), legend.position="bottom") 
  })
  
  
  
  output$distPlot4 <- renderPlot({
    ggplot(data = world) +
      geom_polygon(aes(x = long, y = lat,  group = group))  + 
      theme_bw() 
  })
  
  
  
  output$treemap <- renderPlot({
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Espere por favor',
                 detail = 'La carga Inicial de datos puede tardar mas de 1 minuto')
    
    for (i in 1:1000) {
      progress$set(value = 100)
      Sys.sleep(0.5)
    }
    
    
    treemap(base_topic_total2, #Your data frame object
            index=c("term","word"),  #A list of your categorical variables
            vSize = "importancia",  #This is your quantitative variable
            type="index", #Type sets the organization and color scheme of your treemap
            palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
            title="Topicos", #Customize your title
            fontsize.title = 10 #Change the font size of the title
    )
  })
  
  
  
  
  

   
}

# Run the application 
shinyApp(ui = ui, server = server)#


