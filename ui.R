
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simulation of Disease Spread in a Population"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("iteration",
                  "Number of iterations:", min = 1, max = 20, value = 1, animate = TRUE),
      br(),
      wellPanel(
        helpText("Probabilities per iteration:"),
        sliderInput("pUP",
                    "Probability of getting sicker:", min = 1, max = 70, value = 50),
        br(),
        sliderInput("pDN",
                    "Probability of recovery:", min = 10, max = 50, value = 10))),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Outbreak", plotOutput("outbreakPlot", width = "600px", height = "550px")),
        tabPanel("Line Plot", plotOutput("linePlot", width = "800px", height = "600px"))
        )
      )
    )
  ))
  