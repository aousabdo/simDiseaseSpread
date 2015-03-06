library(data.table)
library(Hmisc)
library(ggplot2)

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

makePlot <- function(DT, level = 1){
  set.seed(123)
  population <- copy(DT)
  Level <- paste('level', level, sep=".")
  p <- ggplot(population, aes(x = x, y = y))
  p <- p + geom_point(aes_string(col = Level),
                      position=position_jitter(width=0.2, height=0.2), size = 12) 
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