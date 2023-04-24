# Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
library(ggplot2)
library(tidyverse)

# Load combined data set:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true')

# We first run the combined ABC algorithm for the whole of England, in order to obtain appropriate initial conditions to use in our predictions:

# Define function to average out case data:
average.cases <- function(data){
  alldates <- seq(as.Date(data$date[3]), as.Date(data$date[length(data$date)-2]), by='days')
  cases.avg <- data.frame(date=alldates, avg_cases=rep(NA, length(alldates)))
  for(i in 1:length(alldates)){
    cases.avg$avg_cases[i] <- as.numeric(colMeans(data[between(data$date, cases.avg$date[i]-2, cases.avg$date[i]+2),4])) 
  }
  data.av <- data[c(3:(dim(data)[1]-2)),]
  data.av$new_cases <- cases.avg$avg_cases
  return(data.av)
}

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
    diff <- abs(as.numeric(data$new_cases) - filter(inpt.data, between(date, d.start, d.end))$new_cases)
    
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

# Adapt the combined ABC function from Code C.18 to apply to the whole of England:
abc.combined.eng <- function(inpt.data, N, epsilon, prior.beta, prior.I0, prior.R0){
  # Now we are considering the entire country - we need to find dates on which any council in England changed tier:
  # Pivot the combined data set to be wide, such that the first column contains the date, and each other column corresponds to the tier classification of an area in England on that date:
  data <- pivot_wider(inpt.data[,c(2,5,9)],2,names_from='la_name',values_from='tier',values_fn = list)|>
    arrange(date)
  data <- as.data.frame(data)
  
  changerows <- c()
  
  # Iterate through each date:
  for(i in 1:(dim(data)[1]-1)){
    counter <- 0 # Initialise the counter
    # Iterate through each area (i.e. each column of our wide data set - excluding the first, as this is the date variable):
    for(j in 2:dim(data)[2]){
      # If the tier classification on date i is the same as the date classification on date i+1, we add one to the counter
      if(data[[i,j]] == data[[i+1,j]]){
        counter <- counter+1
      }
    }
    # If the counter isn't equal to the number of columns we have iterated through in the previous step, then there must be some councils which moved tier - in this case, we append the index i to variable changerows
    if(counter!=(dim(data)[2]-1)){
      changerows <- append(changerows,i)
    }
  }
  
  # We now use the variable changerows to create a list of all dates on which any council area in England moved tier:
  # We again need to remove 30/3/2021 from this list and replace with 6/2/2021, as this is the end of the time period we are considering:
  dates <- append(as.Date(data[changerows[-length(changerows)]+1,1]), as.Date('6/2/2021','%d/%m/%Y'))
  
  plotdata <- data.frame()
  
  abc.output <- list()
  
  # We now combine the data set so that all population and case numbers are with respect to the entire country:
  summary.data <- inpt.data[,c(4:6,8,9)]|>
    group_by(date)|>
    summarise(area='England', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))
  summary.data <- unique(summary.data)
  summary.data$date <- as.Date(summary.data$date)
  
  # We now run the combined ABC algorithm similarly to Code C.18:
  for(i in 1:(length(dates)-1)){
    date1 <- dates[i]+1
    date2 <- dates[i+1]
    abc.out <- abc(N, epsilon, prior.beta, prior.I0, prior.R0, c(date1,date2), average.cases(summary.data))
    simdata <- sirs(abc.out[which.min(abc.out$diff),1], 1/10, 1/152, date1, date2, abc.out[which.min(abc.out$diff),2], abc.out[which.min(abc.out$diff),3], summary.data)|>
      mutate(cat='sim', area=summary.data$area[1])
    obsdata <- filter(summary.data, between(date, as.Date(date1,'%d/%m/%Y'), as.Date(date2,'%d/%m/%Y')))|>
      mutate(cat='obs', area=summary.data$area[1])
    sep.plotdata <- rbind(simdata[,c(1,5:7)], obsdata[,c(1,2,4,6)])
    plotdata <- rbind(plotdata, sep.plotdata)
    abc.output <- append(abc.output, list(abc.out))
    names(abc.output)[i] <- paste('abc.out.', i, sep='')
  }
  abc.output <- append(abc.output, list(plotdata=plotdata))
  return(abc.output)
}

# Run this algorithm:
set.seed(1) # Run to reproduce report results
eng.out <- abc.combined.eng(data,1000,10000,c(0.1,0.2),c(50000,5000000),c(50000,5000000))

# Extract the initial conditions which minimise the average distance between observed and simulated data:
I0 <- eng.out[[1]][which.min(eng.out[[1]]$diff),2]
R0 <- eng.out[[1]][which.min(eng.out[[1]]$diff),3]

# Combine and filter the observed data ready for plotting:
england.data <- data[,c(1:8)]|>
  group_by(date)|>
  summarise(area='England', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))
england.data <- unique(england.data)
england.data$date <- as.Date(england.data$date)

eng.1 <- filter(england.data, between(date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('6/2/2021','%d/%m/%Y')))[,c(1,4)]|>
  mutate(cat='obs', tier='obs')

# List the beta estimates obtained from our posterior samples for constant tiers:
betas <- c(0.1449410, 0.1359998, 0.1354498, 0.1290876, 0.1105676)
# c(No restrictions, Tier 1, Tier 2, Tier 3, Tier 4/National lockdown)
simdata <- data.frame()

# Generate SIRS data for the entire considered period based on each of these betas:
for(i in 1:length(betas)){
  sirsdata <- sirs(betas[i], 1/10, 1/152, '14/10/2020', '6/2/2021', I0, R0, england.data)[,c(1,3,5)]|>
    mutate(cat='sim', tier=as.character(i-1))
  simdata <- rbind(simdata, sirsdata)
}

plotdata <- rbind(simdata[,c(1,3:5)], eng.1)

# Plot observed v.s. simulated data:
ggplot(plotdata, aes(x=date, y=new_cases, color=tier, linetype=cat)) + geom_line(lwd=1) + scale_color_discrete(name='Tier', breaks=c('0','1','2','3','4'), type=c('chartreuse3','deepskyblue','yellow','red','darkred','azure4'), labels=c('No restrictions','Tier 1','Tier 2','Tier 3','Tier 4 (Lockdown)')) + scale_linetype_manual(name='Data type', breaks=c('obs','sim'), values=c('solid','longdash'), labels=c('Observed','Simulated')) + xlab('Date') + ylab('New Daily Cases') + theme(legend.key.size = unit(30,'points'))

# Plot simulated data for the size of the infected compartment, I(t):
ggplot(simdata, aes(x=date, y=Infected, colour=tier)) + geom_line(lwd=1, lty='longdash') + scale_color_discrete(name='Tier', labels=c('No restrictions','Tier 1','Tier 2','Tier 3','Tier 4 (Lockdown)'), type=c('chartreuse3','deepskyblue','yellow','red','darkred')) + xlab('Date') + ylab('Size of infected compartment')

# Create a variable for the number of hospitalised cases:
simdata <- simdata|>
  mutate(hospitalised=0.0686*Infected)

# Plot simulated hospitalisations:
ggplot(simdata, aes(x=date, y=hospitalised, colour=tier)) + geom_line(lwd=1, lty='longdash') + scale_color_discrete(name='Tier', labels=c('No restrictions','Tier 1','Tier 2','Tier 3','Tier 4 (Lockdown)'), type=c('chartreuse3','deepskyblue','yellow','red','darkred')) + xlab('Date') + ylab('Number of hospitalised individuals')