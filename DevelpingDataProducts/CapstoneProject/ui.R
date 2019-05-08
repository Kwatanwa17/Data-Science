#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Load packeges
require(quanteda)
require(readtext)
require(data.table)
require(dplyr)
require(stringr)
require(shiny)

# 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict next word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
