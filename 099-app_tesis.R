
### LIBRERIAS###
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(here)

#setwd("C:/Users/observatorio/Documents/Scripts-Tesis2")

####Conector Shiny###

rsconnect::setAccountInfo(name='juanpablosokil', 
                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')


####### BASES ######
#base <- fread('./base_app/base.txt', sep='\t', encoding='Latin-1')
base <- fread(here('/base_app/base.txt'), sep='\t', encoding='Latin-1')
base$topico <- str_sub(base$term, -1)

tsne <- fread(here('/base_app/datos_tsne_DTM1.csv'), sep='\t')
tsne2 <- fread(here('/base_app/datos_tsne_DTM2.csv'), sep='\t')
tsne3 <- fread(here('/base_app/datos_tsne_DTM3.csv'), sep='\t')
tsne4 <- fread(here('/base_app/datos_tsne_DTM4.csv'), sep='\t')
tsne5 <- fread(here('/base_app/datos_tsne_DTM5.csv'), sep='\t')
tsne6 <- fread(here('/base_app/datos_tsne_DTM6.csv'), sep='\t')


tsne$year <- 2003
tsne2$year <- 2004
tsne3$year <- 2005
tsne4$year <- 2006
tsne5$year <- 2007
tsne6$year <- 2008

base_topic <- rbind(tsne, tsne2, tsne3, tsne4, tsne5, tsne6)



####RELEVANCE#####
base_topic$prob <- base_topic$loglift*0.4+base_topic$logprob*0.6
base_topic2 <- base_topic %>%
  select (Category,  Term, year, prob)

base_topic2 <- base_topic2 %>% 
  rename(word = Term)
base_topic2 <- base_topic2 %>% 
  rename(term = Category)
base_topic2 <- base_topic2 %>% 
  filter (term != 'Default')




# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Aplicacion ejemplo"),
  sidebarLayout(sidebarPanel(checkboxGroupInput("topico", "topico:", choices=unique(base$term)),
                             sliderInput("year", "year:", min=2003, max=2008, value=2003, animate =TRUE)),
  

mainPanel(
tabsetPanel(type = "tabs",
            tabPanel("Plot", plotOutput(outputId = "distPlot", width  = "500px",height = "500px")),
            tabPanel("Evolucion palabras", plotOutput(outputId = "distPlot2", width  = "500px",height = "500px")),
            tabPanel("Palabras Temporal", plotOutput(outputId = "distPlot3", width  = "700px",height = "900px")),
            tabPanel("Analisis Regional")
)
)))

server <- function(input, output, session) {
  base2 <- reactive({
    a <- base %>% filter(term %in% input$topico)
    a <- data.frame(a)
    a$year <- as.integer(a$year)
    return(a)
    
  })
  
  output$distPlot <- renderPlot({
    ggplot(base2(), aes(year, prob, color=term)) + 
      geom_point(size=5) +
      geom_line(size=2, linetype = "dashed", alpha=0.5) +
      scale_y_continuous(limits = c(0, 2)) +
      theme(axis.title.y = element_blank(), legend.position="none") 
  })
  
  
  base3 <- reactive({
    a <- base_topic2 %>% filter(term %in% input$topico)
    a <- data.frame(a)
    return(a)
    
  })
  
  
  
  output$distPlot2 <- renderPlot({
    ggplot(base3(), aes(year, prob, color=term)) + 
      geom_line(aes(group = word)) +
      scale_y_continuous(limits = c(-2, 2)) +
      theme(axis.title.y = element_blank(),legend.position="none") 
  })
  
  
  base4 <- reactive({
    a <- base_topic2 %>% filter(year== input$year & term==input$topico)
    #group_by(word) %>%
    #top_n(10, prob) %>%
    #ungroup() %>%
    #arrange(word, -prob)
    a <- data.frame(a)
    return(a)
    
  })
  
  
  output$distPlot3 <- renderPlot({
    ggplot(base4(), aes(x=word, y=prob, color=term, fill=term)) + 
      geom_bar(stat='identity') +
      coord_flip()  +
      theme(axis.title.y = element_blank(),legend.position="none") 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)







