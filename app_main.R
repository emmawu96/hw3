# app_main.R
# Description: To illustrate monthly BP Apprehensions data
# by year and sector of user's choice
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
library(readr)
library(shiny)

# import 2017 data without sector names
library(readr)
bp17 <- read_csv("PB Apprehensions 2017.csv")[1:10,2:13]
# transpose dataset to make sectors columns
bp17 <- t(bp17)
# import sector names and make it colnames
sectors <- read_csv("PB Apprehensions 2017.csv")[,1]
sectors <- t(sectors)
colnames(bp17) <- sectors

# import 2010 data without sector names

bp10 <- read_csv("BP Apprehensions 2010.csv")[1:10,2:13]
# transpose dataset to make sectors columns
bp10 <- t(bp10)
# import sector names and make it colnames
colnames(bp10) <- sectors

as.data.frame(bp10)
bp10[,1:9]
as.data.frame(bp17)
bp17[,1:9]


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PB Apprehensions"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("dataset", "Choose a year:",
                  choices = c("2010", "2017")),
      # Input: Select a sector ----
      selectInput("sector",
                  "Sector:",
                  choices= colnames(bp17),
                  hr()),
      
      # Include clarifying text ----
      helpText("Note: Click update button after choosing"),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution ----
      h4("Bar Plot"),
      plotOutput("distPlot")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "2010" = bp10,
           "2017" = bp17)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset[,input$sector])
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$distPlot <- renderPlot({
    
    # draw the chosen year bar plot with the specified sector
    dataset <- datasetInput()
    barplot(dataset[,input$sector], 
            main="Apprehensions by Month",
            las=2,
            ylab="Number of People")
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
