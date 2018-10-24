### LIBRERIAS###
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)


####
base <- fread('C:/Users/observatorio/Desktop/App_Tesis/App-Tesis/base/base.txt', sep='\t', encoding='Latin-1')


base$topico <- str_sub(base$term, -1)



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

