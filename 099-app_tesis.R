### LIBRERIAS###
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)


####### BASES ######

#base <- fread('C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/resultados/base.txt', sep='\t', encoding='Latin-1')
base <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/base.txt', sep='\t', encoding='Latin-1')
base$topico <- str_sub(base$term, -1)





#tsne <- fread('C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/resultados/datos_tsne_DTM1.csv', sep='\t')
#tsne2 <- fread('C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/resultados/datos_tsne_DTM2.csv', sep='\t')
#tsne3 <- fread('C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/scripts/scripts/resultados/datos_tsne_DTM3.csv', sep='\t')

tsne <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/datos_tsne_DTM1.csv', sep='\t')
tsne2 <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/datos_tsne_DTM2.csv', sep='\t')
tsne3 <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/datos_tsne_DTM3.csv', sep='\t')
tsne4 <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/datos_tsne_DTM4.csv', sep='\t')
tsne5 <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/datos_tsne_DTM5.csv', sep='\t')
tsne6 <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/datos_tsne_DTM6.csv', sep='\t')

tsne$year <- 2003
tsne2$year <- 2004
tsne3$year <- 2005
tsne4$year <- 2006
tsne5$year <- 2007
tsne6$year <- 2008


base_topic <- rbind(tsne, tsne2, tsne3, tsne4, tsne5, tsne6)

##RELEVANCE
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
  #sidebarLayout(sidebarPanel(sliderInput("bins","Year:",min = 2000,max = 2007, value=2000)),
  sidebarLayout(sidebarPanel(checkboxGroupInput("topico", "topico:", choices=unique(base$term)),
                             sliderInput("year", "year:", min=2003, max=2008, value=2003, animate =TRUE)
  ),
  # Show a plot of the generated distribution
  mainPanel( fluidRow(
    column(2,
           plotOutput(outputId = "distPlot", width  = "300px",height = "200px"),  
           plotOutput(outputId = "distPlot2", width  = "300px",height = "200px")),
    fluidRow(plotOutput(outputId = "distPlot3", width  = "500px",height = "200px"))
  ))))

# Define server logic required to draw a histogram
#server <- function(input, output) {
server <- function(input, output, session) {
  base2 <- reactive({
    #a <- subset(base, term == "top1")
    # You can also use the pipe (%>%) operator
    a <- base %>% filter(term %in% input$topico)
    #  a <- subset(base, term == input$topico)
    a <- data.frame(a)
    a$year <- as.integer(a$year)
    return(a)
    
  })
  
  output$distPlot <- renderPlot({
    ggplot(base2(), aes(year, prob, color=term)) + 
      geom_point(size=2) +
      geom_line(size=3) +
      scale_y_continuous(limits = c(0, 2)) +
      theme(axis.title.y = element_blank(), legend.position="none") 
  })
  
  
  base3 <- reactive({
    #a <- subset(base, term == "top1")
    # You can also use the pipe (%>%) operator
    a <- base_topic2 %>% filter(term %in% input$topico)
    #  a <- subset(base, term == input$topico)
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
      coord_flip() 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)




























































































































### LIBRERIAS###
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)


####




# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Aplicacion ejemplo"),
   #sidebarLayout(sidebarPanel(sliderInput("bins","Year:",min = 2000,max = 2007, value=2000)),
   sidebarLayout(sidebarPanel(checkboxGroupInput("topico", "topico:", choices=unique(base$term))),
      # Show a plot of the generated distribution
      mainPanel(plotOutput("distPlot"))
   )
)



# Define server logic required to draw a histogram
#server <- function(input, output) {
server <- function(input, output, session) {
  
  
base2 <- reactive({
    #a <- subset(base, term == "top1")
  # You can also use the pipe (%>%) operator
  a <- base %>% filter(term %in% input$topico)
  #  a <- subset(base, term == input$topico)
    a <- data.frame(a)
    return(a)
  
   })


   output$distPlot <- renderPlot({
     ggplot(base2(), aes(year, prob, color=term)) + 
       geom_line() +
       scale_y_continuous(limits = c(0, 2)) + 
       scale_x_continuous(limits = c(2000, 2007))
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

