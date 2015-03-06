
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
                  "Number of iterations:", min = 1, max = 20, value = 1),
      br(),
      wellPanel(
        helpText("Probabilities per iteration:"),
      sliderInput("pUP",
                  "Probability of getting sicker:", min = 1, max = 80, value = 50),
      br(),
      sliderInput("pDN",
                  "Probability of recovery:", min = 5, max = 30, value = 10))),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
