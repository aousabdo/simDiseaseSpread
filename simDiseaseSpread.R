library(data.table)
library(Hmisc)
library(ggplot2)

bucket <- function(x){
  if(x<3) HS.level <- "Healthy"
  else if(x <5) HS.level <- "Symptomatic"
  else HS.level <- "Infictious"
  return(HS.level)
}

N <- 100
t0 <- Sys.time()
change <- function(HS, p_up, p_dn){
  if(HS < 6){
    if(rbinom(1, 1, p_up)){HS = HS + 1}
    if(rbinom(1, 1, p_dn)){HS = 1}
  }
  return(HS)
}
dist <- c(rep(1:2, N*0.3) , rep(3:4, N*0.15),rep(5:6, N*0.05))
population <- data.table(x = rep(1:10, each = 10), y = 1:10, HS.1 = sample(dist))

population[, level.1 := as.factor(sapply(HS.1, bucket))]

for(i in 2:10){
  one <- paste("HS",i, sep=".")
  two <- paste("HS",i-1, sep=".")
  three <- paste("level", i, sep=".")
  population[, as.character(one) := sapply(eval(parse(text=two)), change, p_up = 0.6, p_dn = 0.1)]
  population[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket))]
}

set.seed(123)
t1 <- Sys.time()
p <- ggplot(population, aes(x = x, y = y, col = level.10))
p <- p + geom_point(position=position_jitter(width=0.2, height=0.2), size = 12) + theme_bw()
p <- p + theme(legend.position = "none",
               axis.text.x = element_text(colour = "white"),
               axis.title.x = element_text(colour="white"),
               axis.text.y = element_text(colour = "white"),
               axis.title.y = element_text(colour="white"))
p <- p + scale_color_manual(values=c("#30AC30", "#FF3030", "#FFCC00"))

t2 <- Sys.time()
print(p)
t3 <- Sys.time()
print(difftime(t1, t0))
print(difftime(t2, t1))
print(difftime(t3, t2))
# dt <- data.table(x = rep(1:10, each = 10), y = 1:10)
# dt[, plot(x,y)]



theme_nothing <- function(base_size = 12, base_family = "Helvetica")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      rect             = element_blank(),
      line             = element_blank(),
      text             = element_blank(),
      axis.ticks.margin = unit(0, "lines")
    )
}