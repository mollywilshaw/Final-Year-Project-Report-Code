# Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('grid')
library(ggplot2)
library(tidyverse)
library(grid)

# Load combined data set:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true')
data$date <- as.Date(data$date)

# Define SIRS function:
sirs <- function(beta, gamma, xi, d.start, d.end, I0, R0, data){
  N <- data$population[1]
  
  St <- N-R0-I0  
  It <- I0
  Rt <- R0 
  
  Susceptible <- c(St) 
  Infected <- c(It)
  Recovered <- c(Rt)
  
  date <- seq(as.Date(d.start,'%d/%m/%Y')-1, as.Date(d.end,'%d/%m/%Y'), by='days')  
  
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

# Define ABC function:
abc <- function(N, epsilon, prior.beta, prior.I0, prior.R0, dates, inpt.data){ 
  
  d.start <- as.Date(dates[1],'%d/%m/%Y')
  d.end <- as.Date(dates[2],'%d/%m/%Y')
  
  prior <- data.frame(beta = runif(N, prior.beta[1], prior.beta[2]),
                      I0 = runif(N, prior.I0[1], prior.I0[2]),
                      R0 = runif(N, prior.R0[1], prior.R0[2]))

  post <- data.frame('beta'=character(), 'I0'=character(), 'R0'=character(), 'diff'=character())
  
  for(i in 1:N){
    beta <- prior[i,1] 
    I0 <- prior[i,2]
    R0 <- prior[i,3]
    
    data <- sirs(beta, 1/10, 1/152, d.start, d.end, I0, R0, inpt.data)
    
    diff <- abs(as.numeric(data$new_cases) - filter(inpt.data, between(inpt.data$date, d.start, d.end))$new_cases)
    
    j <- 0 
    
    for(x in diff){
      if(x < epsilon){
        j <- j + 1
      } 
    }

    if(j == length(diff)){
      post <- rbind(post, data.frame(beta=beta, I0=I0, R0=R0, diff=mean(diff)))
    }
  }
  return(post)
}

# Define function to average case data (from Code C.16):
average.cases <- function(data){
  alldates <- seq(as.Date(data$date[3]), as.Date(data$date[length(data$date)-2]), by='days')
  cases.avg <- data.frame(date=alldates, avg_cases=rep(NA, length(alldates)))
  for(i in 1:length(alldates)){
    cases.avg$avg_cases[i] <- as.numeric(colMeans(data[between(data$date, cases.avg$date[i]-2, cases.avg$date[i]+2), 4])) 
  }
  data.av <- data[c(3:(dim(data)[1]-2)), ]
  data.av$new_cases <- cases.avg$avg_cases
  return(data.av)
}

# Combine the population, new cases and cumulative case variables to create a separate data set for the whole of England:
england.data <- data[,c(1:8)]|>
  group_by(date)|>
  summarise(area='England', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))
england.data <- unique(england.data) # Make sure that we don't have repeated rows
england.data$date <- as.Date(england.data$date) # Make sure that R recognises the date variable as a date

# Filter this data set over the two national lockdown periods:
england.l1 <- filter(england.data, between(date, as.Date('23/3/2020','%d/%m/%Y'), as.Date('4/7/2020','%d/%m/%Y')))|>
  mutate(lockdown='Lockdown 1')
england.l2 <- filter(england.data, between(date, as.Date('5/11/2020','%d/%m/%Y'), as.Date('1/12/2020','%d/%m/%Y')))|>
  mutate(lockdown='Lockdown 2')
england.lockdowns <- rbind(england.l1, england.l2)

# Run ABC algorithm for both lockdown periods:
set.seed(1) # Run to reproduce report results
abc.england.l1 <- abc(10000, 4250, c(0.1,0.2), c(450000,650000), c(1100000,1200000), c('2/4/2020','4/7/2020'), average.cases(england.data))
abc.england.l2 <- abc(10000, 14000, c(0.1,0.2), c(0,2000000), c(1000000,4000000), c('5/11/2020','1/12/2020'), average.cases(england.data))

# Find the summary statistics for each period:
find.summary.stats <- function(abc.output){
  mean <- mean(abc.output$beta)
  median <- median(abc.output$beta)
  min.diff <- abc.output[which.min(abc.output$diff), 1]
  return(c(min.diff=min.diff, mean=mean, median=median))
}

find.summary.stats(abc.england.l1)
find.summary.stats(abc.england.l2)

# Plot marginal posterior histograms for beta:
par(mfrow=c(1,2))
hist(abc.england.l1$beta, main='First National Lockdown', xlab='Beta', breaks=seq(0.104, 0.118, by=0.001), ylim=c(0,120))
hist(abc.england.l2$beta, main='Second National Lockdown', xlab='Beta', breaks=seq(0.11, 0.16, by=0.0035), ylim=c(0,250))

# Plot simulated v.s. observed data:

# Here we specify the start date based on the lockdown - we also include the period with no restrictions, which we will use later:
plotdata <- function(abc.output, lockdown, data){
  d.start <- ifelse(lockdown==1, '2/4/2020', ifelse(lockdown==2,'5/11/2020', '17/5/2021'))
  d.end <- ifelse(lockdown==1, '4/7/2020', ifelse(lockdown==2, '1/12/2020', '19/7/2021'))
  
  lockdown.name <- ifelse(lockdown==1, 'Lockdown 1', ifelse(lockdown==2, 'Lockdown 2', 'None')) 
  
  data.mean <- sirs(mean(abc.output$beta), 1/10, 1/152, d.start, d.end, mean(abc.output$I0), mean(abc.output$R0), data)|>
    mutate(cat='sim.mean', lockdown=lockdown.name)
  data.median <- sirs(median(abc.output$beta), 1/10, 1/152, d.start, d.end, median(abc.output$I0), median(abc.output$R0), data)|>
    mutate(cat='sim.median', lockdown=lockdown.name)
  data.mindiff <- sirs(abc.output[which.min(abc.output$diff),1], 1/10, 1/152, d.start, d.end, abc.output[which.min(abc.output$diff),2], abc.output[which.min(abc.output$diff),3], data)|>
    mutate(cat='sim.min', lockdown=lockdown.name)
  return(rbind(data.mean, data.median, data.mindiff)[,c(1,5:7)])
}

plot.l1 <- plotdata(abc.england.l1, 1, england.data)
plot.l2 <- plotdata(abc.england.l2, 2, england.data)
plot.data <- rbind(plot.l1, plot.l2, mutate(england.lockdowns[,c(1,4,6)], cat='obs'))

ggplot(plot.data, aes(x=date, y=new_cases, color=cat, linetype=cat)) + geom_line(lwd=1) + facet_wrap(~lockdown, ncol=2, scales = 'free') + scale_color_discrete(name='Data type',breaks=c('obs','sim.min','sim.mean','sim.median'), labels=c('Observed','Simulated (minimising difference)','Simulated (mean)','Simulated (median)'), type=c('black','steelblue3','chartreuse3','red')) + scale_linetype_manual(name='Data type', breaks=c('obs','sim.min','sim.mean','sim.median'), labels=c('Observed','Simulated (minimising difference)','Simulated (mean)','Simulated (median)'), values=c('solid','longdash','longdash','longdash')) + xlab('Date') + ylab('New Daily Cases') + theme(legend.position = 'bottom', legend.key.size = unit(25,'points'), legend.text=element_text(hjust = 0.5))

# We now run the ABC algorithm for the period with no restrictions:
set.seed(1) # Run to reproduce report results
abc.no.ld <- abc(10000, 16000, c(0.1,0.2), c(50000,150000), c(0,100000), c('17/5/2021','19/7/2021'), average.cases(england.data))

# Plot the marginal posterior histogram for beta:
par(mfrow=c(1,1))
hist(abc.no.ld$beta, main=NULL, xlab='Beta', breaks=seq(0.135, 0.155, by=0.001), ylim=c(0,80))

# Find the summary statistics:
find.summary.stats(abc.no.ld)

# Plot the observed v.s. simulated data:
simdata.no.ld <- plotdata(abc.no.ld, 0, england.data)[,c(1,2,3)]
england.no.ld <- filter(england.data, between(date, as.Date('17/5/2021','%d/%m/%Y'), as.Date('19/7/2021','%d/%m/%Y')))[c(1,4)]|>
  mutate(cat='obs')

plotdata.no.ld <- rbind(simdata.no.ld, england.no.ld)

ggplot(plotdata.no.ld, aes(x=date, y=new_cases, color=cat, linetype=cat)) + geom_line(lwd=1) + scale_color_discrete(name='Data type', breaks=c('obs','sim.min','sim.mean','sim.median'), labels=c('Observed','Simulated (minimising difference)','Simulated (mean)','Simulated (median)'), type=c('black','steelblue3','chartreuse3','red')) + scale_linetype_manual(name='Data type', breaks=c('obs','sim.min','sim.mean','sim.median'), labels=c('Observed','Simulated (minimising difference)','Simulated (mean)','Simulated (median)'), values=c('solid','longdash','longdash','longdash')) + xlab('Date') + ylab('New Daily Cases')
