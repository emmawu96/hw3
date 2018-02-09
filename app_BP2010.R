# app_BP2010.R
# Description: To illustrate monthly 2017 BP Apprehensions data 
# by sector
# Author: Yue Wu, emmawu@bu.edu
# 02/09/2018
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# import without sector names
library(readr)
bp10 <- read_csv("BP Apprehensions 2010.csv")[,2:13]
# transpose dataset to make sectors columns
bp10 <- t(bp10)
# import sector names and make it colnames
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

# Define server logic required to draw a barplot
server <- function(input, output) {
  
   output$distPlot <- renderPlot({

     # draw the 2010 bar plot with the specified sector
     barplot(bp10[,input$sector]*1, 
             main=input$sector,
             ylab="Number of People",
             xlab="Month")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

