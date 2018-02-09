#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# import and transpose dataset
library(readr)
bp10 <- read_csv("BP Apprehensions 2010.csv")[,2:13]
bp10 <- t(bp10)
sectors <- read_csv("BP Apprehensions 2010.csv")[,1]
sectors <- t(sectors)
colnames(bp10) <- sectors

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("BP Apprehensions 2010 Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("sector",
                     "Sector:",
                     choices= colnames(bp10),
                     hr()
         )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # selectedData <- reactive({
  #   bp10[,bp10$Sector==input$sector]
  # })
   output$distPlot <- renderPlot({

     # draw the bar plot with the specified sector
     barplot(bp10[,input$sector]*1, 
             main=input$sector,
             ylab="Number of People",
             xlab="Month")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

