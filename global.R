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
    one <- paste0("HS.",i)
    two <- paste0("HS.",i-1)
    three <- paste0("level.", i)
    four <- paste("change", i, i-1, sep = "_")
    population[, as.character(one) := sapply(eval(parse(text=two)), change, p_up = pUP, p_dn = pDN)]
    population[, as.character(four) := as.factor(sapply(diag(outer(eval(parse(text = one)), eval(parse(text= two)), "-")), factorize))]
    # population[, as.character(four) := sapply(eval(parse(text = four)), factorize)]
    population[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket))]
  }
  return(population)
}

factorize <- function(x){
  if(x < 0) value <- "Recovery"
  else if(x == 0) value <- "Steady"
  else value <- "Sicker"
  return(value)
}

Bo <- function(DT){
  
  DT.tmp <- copy(DT)
  
  # delete unwanted columns
  cols <- c(1, 2, DT[, grep("level", colnames(DT))], DT[, grep("change", colnames(DT))])
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
  set.seed(123)
  population <<- copy(DT)
  
  Level <- paste('level', level, sep=".")
  Change <- paste('change', level , level-1 , sep="_")
  
  p <- ggplot(population, aes(x = x, y = y))
  p <- p + geom_point(aes_string(fill = Level), shape = 21, size = 12, col = "white") 
  if(level >= 2){
    p <- p + geom_point(shape = 21, aes_string(col = Change), size = 14)
    p <- p + scale_color_manual(values=c("black", "white", "white"), breaks = c("Recovery", "Sicker", "Steady"))
  }
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
  p <- p + scale_fill_manual(values=c("#30AC30", "#FF3030", "#FFCC00"))
  print(p)
}

linePlot <- function(DT){
  trend <- melt(Bo(DT), id = 'iter')
  pline <- ggplot(trend, aes(x = iter, y = value, col = variable)) + geom_point() + geom_line() + theme_bw()
  pline <- pline + theme(legend.position = "bottom") + xlim(1,30) + xlab("Iteration") + ylab("Count")
  pline <- pline + geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, formula = 'y ~ ns(x, 2)', 
                               aes(fill = variable), alpha = 0.115, lty = 2) + facet_wrap(~ variable)
  pline <- pline + commonTheme
  print(pline)
}

# -------------------------------------------------------------------------------------#
# Add percentages at each iteration. 
# make plot of those
# show rate of change plot
# -------------------------------------------------------------------------------------#

trendPlot <- function(DT){ 
  pop.tmp <- DT[, lapply(.SD, summaryFun), .SDcols = DT[ , grep("level", colnames(DT)) ]]
  pop.tmp[, reference := c("Healthy", "Symptomatic", "Infictious")]
  setkey(pop.tmp, reference)
  
  pop.tmp.long <- melt(pop.tmp, id.vars = "reference")
  pop.tmp.long[, time := rep(seq(1, ncol(pop.tmp)-1), each = 3)]
  
  p2 <- ggplot(pop.tmp.long, aes(x = time, y = value, col = reference)) + geom_line(size = 1.25) + geom_point(size = 4) + theme_bw()
  p2 <- p2 + theme(legend.position = "bottom") + ylab("Percentage of Populatoin\n") + xlab("\nTime (Hours)")
  p2 <- p2 + ggtitle("Trend of Disease Outbreak Over Time\n")
  p2 <- p2 + commonTheme
  p2 <- p2 +  scale_color_manual(name  = "", breaks = c("Healthy", "Infictious", "Symptomatic"),
                                 labels =  c("Healthy  ", "Symptomatic  ", "Infictious  "),
                                 values = c("Healthy" = "#30AC30", "Symptomatic" = "#FF3030", "Infictious" = "#FFCC00"))
  print(p2)
}
###### THIS IS VERY IMPORTANT

summaryFun <- function(x){
  tmp <- summary(x)
  if(is.na(tmp["Healthy"])){tmp["Healthy"] <- 0}
  if(is.na(tmp["Symptomatic"])){tmp["Symptomatic"] <- 0}
  if(is.na(tmp["Infictious"])){tmp["Infictious"] <- 0}
  return(tmp)
}

# -------------------------------------------------------------------------------------#
# common theme for the ggplots
commonTheme <- theme(axis.text.x = element_text(angle=0, hjust=1, size = 14),
                     axis.title.x = element_text(face="bold", colour="black", size=16),
                     axis.text.y = element_text(angle=0, hjust=1, size = 14),
                     axis.title.y = element_text(face="bold", colour="black", size=16),
                     plot.title = element_text(size = 20), 
                     legend.title = element_text(colour="black", size=16, face="bold"),
                     legend.text = element_text(colour="black", size = 16, face = "bold"))
# -------------------------------------------------------------------------------------#