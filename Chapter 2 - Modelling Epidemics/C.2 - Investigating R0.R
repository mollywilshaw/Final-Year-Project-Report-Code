# Install and load required packages

# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('grid')
library(ggplot2)
library(tidyverse)
library(grid)

# An abridged SIR function (implementing SIR functions in R is discussed further in code file C.4)
sir <- function(N, beta, gamma, t.end, dt){
  St <- N   
  It <- 10
  Susceptible <- c(N) 
  Infected <- c(10)
  t <- seq(0, t.end, dt)  
  
  for (i in 2:length(t)){
    
    St.1 <- St - beta*St*It*dt/N
    It.1 <- It + beta*St*It*dt/N - gamma*It*dt
    Susceptible <- append(Susceptible, St.1) 
    Infected <- append(Infected, It.1)
    
    St <- St.1
    It <- It.1
  }
  
  return(data.frame(t, Infected)) 
}

# Here beta = 0.12 and gamma = 0.1, so R0 = 1.2 > 1 
gt1 <- sir(15000,0.12,1/10,365,1)|>
  mutate(R0='R0 > 1')
# Here beta = 0.1 and gamma = 0.125, so R0 = 0.8 < 1
lt1 <- sir(15000,0.1,1/8,365,1)|>
  mutate(R0='R0 < 1')

# We combine the generated data from these two models into one data set, then make a faceted plot:
plotdata <- rbind(gt1, lt1)
p <- ggplot(plotdata,aes(x=t,y=Infected))+geom_line()+xlab('Time t')+ylab('Number of infectives I(t)')+facet_wrap(~R0,ncol=2)

# The following code changes the colours of the title strip backgrounds for the two plots
# This code is adapted from the following code request: https://github.com/tidyverse/ggplot2/issues/2096

g <- ggplot_gtable(ggplot_build(p))
striprt <- which( grepl('strip-1', g$layout$name) | grepl('strip-t', g$layout$name) )
fills <- c("chartreuse","red")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

