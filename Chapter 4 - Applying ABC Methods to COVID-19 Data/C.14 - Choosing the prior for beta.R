# Install and load required packages:

# install.packages('tidyverse')
# install.packages('grid')
# install.packages('ggplot2')
library(tidyverse)
library(grid)
library(ggplot2)

# Load combined data set:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true')
data$date <- as.Date(data$date)

# Create separate data sets for each of the 3 considered areas:
# South West
swdata <- data|>
  filter(area_name=='Cornwall'|area_name=='Devon'|area_name=='Dorset'|area_name=='Somerset'|area_name=='Gloucestershire'|area_name=='Wiltshire'|area_name=='West of England')|>
  group_by(date)|>
  summarise(area='South West',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases))

# North East
nedata <- data|>
  filter(area_name=='County Durham'|area_name=='Tyne and Wear'|area_name=='Tees Valley'|area_name=='Northumberland')|>
  group_by(date)|>
  summarise(area='North East',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases))

# Merseyside
msdata <- data|>
  filter(area_name=='Merseyside')|>
  group_by(date)|>
  summarise(area='Merseyside',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases))

# Define SIRS function as in Figure 4.5:
sirs <- function(beta, gamma, xi, d.start, d.end, I0, R0, data){
  N <- data$population[1]
  
  St <- N-R0-I0  
  It <- I0
  Rt <- R0 
  
  Susceptible <- c(St) 
  Infected <- c(It)
  Recovered <- c(Rt)
  
  date <- seq(as.Date(d.start,'%d/%m/%Y')-1,
              as.Date(d.end,'%d/%m/%Y'), by='days')  
  
  for (i in 2:length(date)){
    St.1 <- St + (xi*Rt - beta*St*It/N)
    It.1 <- It + (beta*St*It/N - gamma*It)
    Rt.1 <- Rt + (gamma*It - xi*Rt)
    
    Susceptible <- append(Susceptible, St.1)
    Infected <- append(Infected, It.1)
    Recovered <- append(Recovered,Rt.1)
    
    St <- St.1
    It <- It.1
    Rt <- Rt.1
  }
  
  new_cases <- c()
  for(j in 2:length(Infected)){
    new_cases[j-1] <- Infected[j] - Infected[j-1]
  }
  
  date <- date[-1]
  Susceptible <- Susceptible[-1]
  Infected <- Infected[-1]
  Recovered <- Recovered[-1]
  
  return(data.frame(date, Susceptible, Infected, Recovered, new_cases)) 
}

# Here we use an adapted ABC rejection sampler function, which will return all of the proposed parameters and an additional variable 'post', which indicates whether or not those parameter values were accepted into the posterior:
abc.prior <- function(N, epsilon, prior.beta, prior.I0, prior.R0, dates, inpt.data){ 
  
  d.start <- as.Date(dates[1],'%d/%m/%Y')
  d.end <- as.Date(dates[2],'%d/%m/%Y')
  
  prior <- data.frame(beta=runif(N,prior.beta[1],prior.beta[2]),
                      I0=runif(N,prior.I0[1],prior.I0[2]),
                      R0=runif(N,prior.R0[1],prior.R0[2]),
                      post=rep(NA,N))
  
  for(i in 1:N){
    beta <- prior[i,1] 
    I0 <- prior[i,2]
    R0 <- prior[i,3]
    
    data <- sirs(beta, 1/10, 1/182.5, d.start, d.end, I0, R0, inpt.data)
    diff <- abs(as.numeric(data$new_cases) - filter(inpt.data,between(inpt.data$date,d.start,d.end))$new_cases)
    
    j <- 0 
    
    for(x in diff){
      if(x < epsilon){
        j <- j + 1
      } 
    }
    # If all points are within the tolerance, we set the corresponding value of variable 'post' to be 1, otherwise we set post=0 
    ifelse(j == length(diff), prior$post[i] <- 1, prior$post[i] <- 0)
  }
  return(prior)
}

set.seed(1) # Run to reproduce report plot

# We run this algorithm for each area, using more iterations than we will in the rest of the report:
abc.sw <- abc.prior(50000, 1000, c(0,1), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), swdata)|>
  mutate(area='South West')
abc.ne <- abc.prior(50000, 1000, c(0,1), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), nedata)|>
  mutate(area='North East')
abc.ms <- abc.prior(50000, 1000, c(0,1), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), msdata)|>
  mutate(area='Merseyside')

abc <- rbind(abc.sw, abc.ne, abc.ms)

# The following plot shows that all of our accepted beta values lie within (0.1, 0.2):
p <- ggplot(abc, aes(x=beta, color=factor(post), fill=factor(post))) + geom_histogram(binwidth=0.01) + xlab('Attempted Beta Value') + ylab('Frequency') + scale_fill_discrete(name='', breaks=c('0','1'), labels=c('Not accepted','Accepted'), type = c('darkgrey','red')) + scale_color_discrete(type=c('darkgrey','black')) + guides(color='none') + facet_wrap(~factor(area, levels=c('South West','North East','Merseyside')), nrow=1, ncol=3)
p <- p + geom_vline(xintercept=0.1, linetype='dotted', color='black')
p <- p + geom_vline(xintercept=0.2, linetype='dotted', color='black')
p <- p + annotate("text", x = 0.08, y = -10, size=2, label = "0.1", color = "black")
p <- p + annotate("text", x = 0.18, y = -10, size=2, label = "0.2", color = "black")
p <- p + theme(legend.position = 'bottom')

# The following code changes the colours of the title strip backgrounds for the plots
# This code is adapted from the following code request: https://github.com/tidyverse/ggplot2/issues/2096
g <- ggplot_gtable(ggplot_build(p))
striprt <- which( grepl('strip-1', g$layout$name) | grepl('strip-t', g$layout$name) )
fills <- c("deepskyblue","yellow","red")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)