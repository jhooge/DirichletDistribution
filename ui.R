#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dirichlet Visualization"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("a1",
                   "Alpha 1",
                   min = 0.1,
                   max = 10,
                   step = 0.1,
                   value = 1),
       sliderInput("a2",
                   "Alpha 2",
                   min = 0.1,
                   max = 10,
                   step = 0.1,
                   value = 1),
       sliderInput("a3",
                   "Alpha 3",
                   min = 0.1,
                   max = 10,
                   step = 0.1,
                   value = 1),
       sliderInput("n",
                   "Random Draws",
                   min = 1,
                   max = 25,
                   value = 5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h2("Dirichlet Distribution (k=3)"),
       plotOutput("diriContour"),
       h2("Random Draws"),
       plotOutput("diriVectors")
    )
  )
))
