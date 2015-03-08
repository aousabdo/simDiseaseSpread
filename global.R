library(data.table)
library(Hmisc)
library(ggplot2)
library(reshape2)

# function to factorize health statuses
bucket <- function(x){
  if(x<3) HS.level <- "Healthy"
  else if(x <5) HS.level <- "Symptomatic"
  else HS.level <- "Infictious"
  return(HS.level)
}

# function to change health status accroding to coin tossing
change <- function(HS, p_up, p_dn){
  if(HS < 6){
    if(rbinom(1, 1, p_up)){HS = HS + 1}
    if(rbinom(1, 1, p_dn)){HS = 1}
  }
  return(HS)
}

simPopulation <- function(iter, Npop, pUP, pDN){
  # initial distribution of health statuses
  Npop <- 100 # number of simulated personas
  dist <- c(rep(1:2, Npop*0.3) , rep(3:4, Npop*0.15),rep(5:6, Npop*0.05))
  
  # start building the data.table
  population <- data.table(x = rep(1:10, each = 10), y = 1:10, HS.1 = sample(dist))
  
  # we'll be adding a factor varialbe to show the levels of health status
  population[, level.1 := as.factor(sapply(HS.1, bucket))]
  
  for(i in 2:iter){
    one <- paste("HS",i, sep=".")
    two <- paste("HS",i-1, sep=".")
    three <- paste("level", i, sep=".")
    population[, as.character(one) := sapply(eval(parse(text=two)), change, p_up = pUP, p_dn = pDN)]
    population[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket))]
  }
  return(population)
}

Bo <- function(DT){
  
  DT.tmp <- copy(DT)
  cols <- c('x', 'y', paste('level', 1:((ncol(DT.tmp)-2)/2) , sep = "."))
  DT.tmp[, (cols) := NULL]
  
  vec <- vector()
  tmp <- data.table()
  for (i in 1:6) vec[i] <- DT.tmp[, sum(HS.1==i)]
  tmp <- rbindlist(list(tmp, as.list(vec)))
  
  for(i in 2:ncol(DT.tmp)){
    for(j in 1:6) vec[j] <- DT.tmp[, sum(eval(parse(text = paste0('HS.', i))) == j )]
    tmp <- rbindlist(list(tmp, as.list(vec)))    
  }
  
  setnames(tmp, paste0('HS.', 1:ncol(tmp)))
  tmp[, iter := 1:nrow(tmp)]
  return(tmp)
}

makePlot <- function(DT, level = 1){
  population <- copy(DT)
  Level <- paste('level', level, sep=".")
  p <- ggplot(population, aes(x = x, y = y))
  p <- p + geom_point(aes_string(col = Level),
                      position=position_jitter(width=0.2, height=0.2), 
                      size = 12) 
  p <- p + theme(axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 legend.position="none",
                 panel.grid.major=element_blank(),
                 panel.background = element_rect(fill = "#F0F0F0", colour = "grey50", size = 2),
                 panel.grid.minor=element_blank()
  )
  p <- p + scale_color_manual(values=c("#30AC30", "#FF3030", "#FFCC00"))
  print(p)
}

linePlot <- function(DT){
  trend <- melt(Bo(DT), id = 'iter')
  pline <- ggplot(trend, aes(x = iter, y = value, col = variable)) + geom_line() + theme_bw()
  pline <- pline + theme(legend.position = "bottom")
  print(pline)
}