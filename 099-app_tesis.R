
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
#install.packages("directlabels")
library(directlabels)
library(ggrepel)
#setwd("C:/Users/observatorio/Documents/Scripts-Tesis2")

####Conector Shiny###

rsconnect::setAccountInfo(name='juanpablosokil', 
                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')


####### BASES ######

############################## NUEVO## TOMO LA BASE COMPLETA DE TOPICOS Y LA RESUMO####
####ESTA BASE TAMBIEN DEBERIA RESUMIRLA PARA HACE POR PAIS / ETC########PERO ES MUY PESADA##

#base_topicos <- read_delim("./base_app/base_topicos.csv", "\t", escape_double = FALSE, col_types = cols(X1 = col_skip()), trim_ws = TRUE)
#base_topicos <- subset(base_topicos, Year <=2017)
#base_topicos[base_topicos == 0] <- NA
###Elimino las columnas que sobran###
#base_topicos = subset(base_topicos, select = -c(doc, topicodom, ut, Pais_x, Pais_y, Resumen, Prefijo, Continente, Subcontinente) )
#base_year <- base_topicos %>% gather(topico, valor, -Year) %>% group_by (Year, topico) %>%  summarise_all(funs(mean= mean(., na.rm=TRUE)))
#base = base_year
#
#write.csv(base, './base_app/base_resumida.csv', row.names = FALSE)


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
  top_n(15, Relevance) %>%
  ungroup() %>%
  arrange(term, desc(-Relevance))

base_topic_total2$importancia <- (base_topic_total2$Relevance)+3

######################################################################

world <- map_data("world") 

######################################################################


primeros=c('T1'='topico01','T2'='topico02','T3'='topico03','T4'='topico04','T5'='topico05','T6'='topico06','T7'='topico07','T8'='topico08','T9'='topico09','T10'='topico10','T11'='topico11','T12'='topico12','T13'='topico13','T14'='topico14','T15'='topico15','T16'='topico16','T17'='topico17','T18'='topico18','T19'='topico19','T20'='topico20','T21'='topico21','T22'='topico22','T23'='topico23','T24'='topico24','T25'='topico25','T26'='topico26','T27'='topico27','T28'='topico28','T29'='topico29','T30'='topico30','T31'='topico31','T32'='topico32','T33'='topico33','T34'='topico34')
segundos=c('T35'='topico35','T36'='topico36','T37'='topico37','T38'='topico38','T39'='topico39','T40'='topico40','T41'='topico41','T42'='topico42','T43'='topico43','T44'='topico44','T45'='topico45','T46'='topico46','T47'='topico47','T48'='topico48','T49'='topico49','T50'='topico50','T51'='topico51','T52'='topico52','T53'='topico53','T54'='topico54','T55'='topico55','T56'='topico56','T57'='topico57','T58'='topico58','T59'='topico59','T60'='topico60','T61'='topico61','T62'='topico62','T63'='topico63','T64'='topico64','T65'='topico65','T66'='topico66','T67'='topico67','T68'='topico68')
terceros=c('T69'='topico69','T70'='topico70','T71'='topico71','T72'='topico72','T73'='topico73','T74'='topico74','T75'='topico75','T76'='topico76','T77'='topico77','T78'='topico78','T79'='topico79','T80'='topico80','T81'='topico81','T82'='topico82','T83'='topico83','T84'='topico84','T85'='topico85','T86'='topico86','T87'='topico87','T88'='topico88','T89'='topico89','T90'='topico90','T91'='topico91','T92'='topico92','T93'='topico93','T94'='topico94','T95'='topico95','T96'='topico96','T97'='topico97','T98'='topico98','T99'='topico99','T100'='topico100')




###############APLICACION SHINY##################


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Explorador de Topicos - Estudios sobre Genero"),
  sidebarLayout(sidebarPanel(fluidRow(
    column(4,checkboxGroupInput("topico1", "", choices=primeros)),
    column(4,checkboxGroupInput("topico2", "", choices=segundos)),
    column(4,checkboxGroupInput("topico3", "", choices=terceros))), width = 3),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Caracterizacion Topico",
                           fluidRow(plotOutput(outputId = "distPlot",height = "400px"))
                           #,fluidRow(sliderInput("year", "Evolucion anual de las palabras:", min=2003, max=2017, value=2003, animate =TRUE))
                           ,fluidRow(plotOutput(outputId = "distPlot3", height = "500px"))
                  ),
                  tabPanel("Evolucion Terminos", plotOutput(outputId = "distPlot2", height= "800px")),
                  tabPanel("Analisis Regional", plotOutput(outputId = "distPlot4", width  = "600px",height = "600px"))
      ))
  ))




server <- function(input, output, session) {
  base2 <- reactive({
    a <- base %>% filter(term %in% input$topico1 | term %in% input$topico2 | term %in% input$topico3)
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
  
  
  
  base4 <- reactive({
    a <- base_topic_total2 %>% filter((term %in% input$topico1 | term %in% input$topico2 | term %in% input$topico3)) %>% arrange(desc(importancia)) 
    a <- data.frame(a)
    return(a)
  })
  
  
  
  
  output$distPlot3 <- renderPlot({
    ggplot(base4(), aes(x=word, y=importancia, color=term, fill=term)) + 
      geom_bar(stat='identity') +
      scale_y_continuous(limits = c(0, 3)) +
      coord_flip()  +
      theme(axis.title.y = element_blank(),legend.position="none") 
  })
  
  
  
  base3 <- reactive({
    a <- base_topic2 %>% filter(term %in% input$topico1 | term %in% input$topico2 | term %in% input$topico3)  %>% arrange(desc(importancia)) 
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
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)#


