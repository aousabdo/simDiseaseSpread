
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  dataTable <- reactive({
    N <- 20 # maximum number of iterations to go through
    
    pUP <- input$pUP/100.
    pDN <- input$pDN/100.
    
    # initial distribution of health statuses
    M <- 100 # number of simulated personas
    dist <- c(rep(1:2, M*0.3) , rep(3:4, M*0.15),rep(5:6, M*0.05))
    
    # start building the data.table
    population <- data.table(x = rep(1:10, each = 10), y = 1:10, HS.1 = sample(dist))
    
    # we'll be adding a factor varialbe to show the levels of health status
    population[, level.1 := as.factor(sapply(HS.1, bucket))]
    
    for(i in 2:N){
      one <- paste("HS",i, sep=".")
      two <- paste("HS",i-1, sep=".")
      three <- paste("level", i, sep=".")
      population[, as.character(one) := sapply(eval(parse(text=two)), change, p_up = pUP, p_dn = pDN)]
      population[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket))]
    }
    return(population)
  })
  output$distPlot <- renderPlot({
    simPop <- dataTable()
    makePlot(DT = simPop, level = input$iteration)
  })

})
