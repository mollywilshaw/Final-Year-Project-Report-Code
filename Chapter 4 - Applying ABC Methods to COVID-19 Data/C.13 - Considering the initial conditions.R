# Install and load required packages:

# install.packages('tidyverse')
# install.packages('ggplot2')
# install.packages('grid')
# install.packages('janitor')
library(tidyverse)
library(ggplot2)
library(grid)
library(janitor)

# Here we define a slightly different SIRS function to the one in Figure 4.5 of the report:
sirs <- function(beta,gamma,xi,d.start,d.end,data){
  
  N <- data$population[1]
  
  start.row <- which(data$date==as.Date(d.start,'%d/%m/%Y'))
  
  # Here we calculate the infective and immunity periods from parameters gamma and xi, respectively:
  inf.period <- ceiling(1/gamma)
  imm.period <- ceiling(1/xi)
  
  # We then use these time periods to work backwards to calculate the initial conditions from the data itself, rather than inputting our own:
  It <- data$cumulative_cases[start.row-1] - data$cumulative_cases[start.row-inf.period-1]
  Rt <- data$cumulative_cases[start.row-inf.period-1] - data$cumulative_cases[start.row-imm.period-1]
  St <- N - Rt - It
  
  # We then proceed with the SIRS solution method as usual:
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
    Recovered <- append(Recovered, Rt.1)
    
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

# We also define the rejection sampler function for this case (i.e. only beta is randomly proposed at each iteration):
abc <- function(N, epsilon, prior.beta, dates, inpt.data){ 
  d.start <- as.Date(dates[1],'%d/%m/%Y')
  d.end <- as.Date(dates[2],'%d/%m/%Y')
  
  post <- data.frame('beta'=character(), 'diff'=character())
  
  for(i in 1:N){
    beta <- runif(1,prior.int[1],prior.int[2])
    
    data <- sirs(beta, 1/10, 1/152, d.start, d.end, inpt.data)
    
    diff <- abs(as.numeric(data$new_cases) - filter(inpt.data, between(inpt.data$date, d.start, d.end))$new_cases)
    
    j <- 0 
    
    for(x in diff){
      if(x < epsilon){
        j <- j + 1
      } 
    }
    if(j == length(diff)){
      post <- rbind(post, data.frame(beta=beta, diff=mean(diff)))
    }
  }
  return(post)
}

# We now load the combined data set:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true')
data$date <- as.Date(data$date) 

# We create separate data sets for each of the areas we are considering:
swdata <- data|>
  filter(area_name=='Cornwall'|area_name=='Devon'|area_name=='Dorset'|area_name=='Somerset'|area_name=='Gloucestershire'|area_name=='Wiltshire'|area_name=='West of England')|>
  group_by(date)|>
  summarise(area='South West', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))

nedata <- data|>
  filter(area_name=='County Durham'|area_name=='Tyne and Wear'|area_name=='Tees Valley'|area_name=='Northumberland')|>
  group_by(date)|>
  summarise(area='North East', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))

msdata <- data|>
  filter(area_name=='Merseyside')|>
  group_by(date)|>
  summarise(area='Merseyside', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))

# We now run the rejection sampler over each of these areas, then generate SIRS data based on the beta value which minimises the average distance between the observed and simulated data: 
# South West
abc.sw <- abc(10000, 1000, c(0,1), c('14/10/2020','5/11/2020'), swdata)
best.sw <- abc.sw[abc.sw$diff == min(abc.sw$diff), 1]
simdata.sw <- sirs(best.sw, 1/10, 1/152, '14/10/2020', '5/11/2020', swdata)[ ,c(1,5)]|>
  mutate(cat='simulated', area='South West')
swdata.1 <- filter(swdata, between(swdata$date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))[ ,c(1,2,4)]|>
  mutate(cat='observed') # We also filter the observed data between the dates we are considering, ready for plotting

# North East
abc.ne <- abc(10000, 1000, c(0,1), c('14/10/2020','5/11/2020'), nedata)
best.ne <- abc.ne[abc.ne$diff == min(abc.ne$diff), 1]
simdata.ne <- sirs(best.ne, 1/10, 1/152, '14/10/2020', '5/11/2020', nedata)[ ,c(1,5)]|>
  mutate(cat='simulated', area='North East')
nedata.1 <- filter(nedata, between(nedata$date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))[ ,c(1,2,4)]|>
  mutate(cat='observed')

# Merseyside
abc.ms <- abc(10000, 1000, c(0,1), c('14/10/2020','5/11/2020'), msdata)
best.ms <- abc.ms[abc.ms$diff == min(abc.ms$diff), 1]
simdata.ms <- sirs(best.ms, 1/10, 1/152, '14/10/2020', '5/11/2020', msdata)[ ,c(1,5)]|>
  mutate(cat='simulated', area='Merseyside')
msdata.1 <- filter(msdata, between(msdata$date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))[ ,c(1,2,4)]|>
  mutate(cat='observed')

# We combine the simulated and observed data sets for each area into one data set:
plotdata <- rbind(simdata.sw,simdata.ne,simdata.ms,swdata.1,nedata.1,msdata.1)

# We then use this data set to create a plot of the observed v.s. simulated data, faceted by the area:
p <- ggplot(plotdata, aes(x=date, y=new_cases, colour=cat, linetype=cat)) + geom_line() + xlab('Date') + ylab('New Daily Cases') + scale_linetype_manual(name='Data type', labels=c('Observed','Simulated'), values=c('solid','longdash')) + scale_color_discrete(name='Data type', type=c('black','red'), labels=c('Observed','Simulated')) + facet_wrap(~factor(area, levels = c('South West','North East','Merseyside')), ncol=3)

# The following code changes the colours of the title strip backgrounds for the plots
# This code is adapted from the following code request: https://github.com/tidyverse/ggplot2/issues/2096
g <- ggplot_gtable(ggplot_build(p))
striprt <- which( grepl('strip-1', g$layout$name) | grepl('strip-t', g$layout$name) )
fills <- c('deepskyblue','yellow','red')
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

# We plot the observed v.s. simulated data for Merseyside over a longer time period:
ms.sim <- sirs(best.ms, 1/10, 1/152, '14/10/2020', '1/1/2021', msdata)[ ,c(1,5)]|>
  mutate(cat='simulated')
msdata.2 <- filter(msdata, between(msdata$date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('1/1/2021','%d/%m/%Y')))[ ,c(1,4)]|>
  mutate(cat='observed')
ggplot(rbind(ms.sim,msdata.2), aes(x=date, y=new_cases, colour=cat, linetype=cat)) + geom_line() + xlab('Date') + ylab('New Daily Cases') + scale_linetype_manual(name='Data type', labels=c('Observed','Simulated'), values=c('solid','longdash')) + scale_color_discrete(name='Data type', type=c('black','red'), labels=c('Observed','Simulated'))


# We decide to treat the initial conditions as uncertain, and so need to find prior bounds for them.

# We will start the model on 14/10/2020 for each area, although we actually look at the data from the day before due to the 'day zero' counting convention used for the infective period:
sw.start <- swdata$cumulative_cases[which(swdata$date == as.Date('13/10/2020','%d/%m/%Y'))]
ne.start <- nedata$cumulative_cases[which(nedata$date == as.Date('13/10/2020','%d/%m/%Y'))]
ms.start <- msdata$cumulative_cases[which(msdata$date == as.Date('13/10/2020','%d/%m/%Y'))]

# People who first became infected 10 days before this (3/10/2020) are the active cases on 14/10/2020 - we use the cumulative case numbers to calculate this number:
sw.inf <- sw.start - swdata$cumulative_cases[which(swdata$date==as.Date('3/10/2020','%d/%m/%Y'))]
ne.inf <- ne.start - nedata$cumulative_cases[which(nedata$date==as.Date('3/10/2020','%d/%m/%Y'))]
ms.inf <- ms.start - msdata$cumulative_cases[which(msdata$date==as.Date('3/10/2020','%d/%m/%Y'))]
sw.inf
ne.inf
ms.inf
# To allow the same prior for each area, we set the lower bound of the I0 prior to be 6000. 

# We now attempt to estimate the upper bound for I0 using data on the proportion of positive results in samples of test swabs:

# Load and clean data:
testresults <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/raw/main/Data%20Sets/20230127covid19infectionsurveydatasetsengland%20%5B9%5D.csv')[c(5,18:19),]|>
  row_to_names(1)|>
  pivot_longer(2:21, names_to = 'area', values_to = 'test_numbers')
testresults$area <- gsub('Nu', '/Nu', testresults$area)
testresults$area <- gsub('To', '/To', testresults$area)
testresults <- separate(testresults, 2, sep='/', into=c('Area','2'))
testresults$Area <- gsub('England ', 'England', testresults$Area)
testresults <- pivot_wider(testresults, names_from = 3, values_from = 4)|>
  filter(Area=='South West\n'|Area=='North East\n'|Area=='North West\n')
testresults$`Total number of tests in sample ` <- as.numeric(gsub(',', '', testresults$`Total number of tests in sample `))
testresults <- testresults|>  
  group_by(Area)|>
  summarise(n_pos = sum(as.numeric(`Number of tests positive for COVID-19`)), n_tests = sum(`Total number of tests in sample `))

# Add population data:
populations <- data.frame('Area' = c('South West\n', 'North East\n', 'North West\n'), population = c(swdata$population[1], nedata$population[1], msdata$population[1]))

# Add variables for the proportions of positive tests and estimated number of cases:
testresults <- full_join(testresults, populations)|>
  mutate(pos_prop = n_pos/n_tests)|>
  mutate(estimated_pos = population*pos_prop)|>
  summarise(Area,population,pos_prop,estimated_pos)
testresults$Area[2] <- 'Merseyside'
View(testresults)
# Highest here is over 30000 - since this is still only from a sample, we still have some uncertainty. We express this by extending the upper bound to 50000.

# R0 = number of individuals who have had COVID, are no longer active cases and had it less than 5 months ago (i.e. those who had COVID between 13/5/2020 and 3/10/2020)
sw.rec <- swdata$cumulative_cases[which(swdata$date==as.Date('3/10/2020','%d/%m/%Y'))] - swdata$cumulative_cases[which(swdata$date==as.Date('13/5/2020','%d/%m/%Y'))]
ne.rec <- nedata$cumulative_cases[which(nedata$date==as.Date('3/10/2020','%d/%m/%Y'))] - nedata$cumulative_cases[which(nedata$date==as.Date('13/5/2020','%d/%m/%Y'))]
ms.rec <- msdata$cumulative_cases[which(msdata$date==as.Date('3/10/2020','%d/%m/%Y'))] - msdata$cumulative_cases[which(msdata$date==as.Date('13/5/2020','%d/%m/%Y'))]
sw.rec
ne.rec
ms.rec
# Set lower bound for R0 to be 9000

# We can't gain any more information about R0 from the infection survey, so we set the upper bound for R0 arbitrarily to be 50000 also. 