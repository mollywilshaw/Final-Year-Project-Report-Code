# Install and load required packages:

# install.packages('tidyverse')
# install.packages('ggplot2')
library(tidyverse)
library(ggplot2)

# Load combined data set:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true')

# We now consider London - create a separate data set with only this area:
ldndata <- data|>
  filter(area_name=='Inner London'|area_name=='Outer London')|>
  group_by(date)|>
  summarise(area='London',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases),tier=tier)
ldndata <- unique(ldndata)
ldndata$date <- as.Date(ldndata$date)

# Define the SIRS function:
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

# Define the ABC function:
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

# Define the function to average out the case data:
average.cases <- function(data){
  alldates <- seq(as.Date(data$date[3]), as.Date(data$date[length(data$date)-2]), by='days')
  cases.avg <- data.frame(date=alldates, avg_cases=rep(NA, length(alldates)))
  for(i in 1:length(alldates)){
    cases.avg$avg_cases[i] <- as.numeric(colMeans(data[between(data$date, cases.avg$date[i]-2, cases.avg$date[i]+2),4])) 
  }
  data.av <- data[c(3:(dim(data)[1]-2)), ]
  data.av$new_cases <- cases.avg$avg_cases
  return(data.av)
}

# We now adapt the combined ABC function from Code C.18 to carry through the initial conditions:
abc.combined <- function(inpt.data, N, epsilon, prior.beta, prior.I0, prior.R0){
  # Create a list of dates on which the tier changed:
  dates <- inpt.data[which(c(FALSE, tail(inpt.data$tier,-1) != head(inpt.data$tier,-1))),1]$date
  dates <- dates[1:(length(dates)-1)]
  dates <- append(dates, as.Date('2021-02-06'))
  
  # Create data frame and list to store outputs:
  plotdata <- data.frame()
  abc.output <- list()
  
  # Iterate through the dates:
  for(i in 1:(length(dates) - 1)){
    
    date1 <- dates[i]
    date2 <- dates[i+1]
    
    # If we are on the first iteration, we have not yet simulated any data, so have nowhere to inherit the initial conditions from.
    # We therefore estimate initial conditions by specifying prior intervals in the function abc(), as usual:
    if(i==1){
      abc.out <- abc(N, epsilon, prior.beta, prior.I0, prior.R0, c(date1, date2), average.cases(inpt.data))
      simdata <- sirs(abc.out[which.min(abc.out$diff),1], 1/10, 1/152, date1, date2, 
                      abc.out[which.min(abc.out$diff),2], abc.out[which.min(abc.out$diff),3], inpt.data)|>
        mutate(cat = 'sim', area = inpt.data$area[1])
    }
    # For all other iterations, we use the final states of the simulated data from the previous period as the initial conditions:
    if(i!=1){
      # We achieve this by using the abc() function still, but setting the prior intervals for I0 and R0 to be one number - the final state of the simulated data. 
      abc.out <- abc(N, epsilon, prior.beta, c(simdata[dim(simdata)[1],3], simdata[dim(simdata)[1],3]), 
                     c(simdata[dim(simdata)[1],4], simdata[dim(simdata)[1],4]), c(date1, date2), average.cases(inpt.data))
      simdata <- sirs(abc.out[which.min(abc.out$diff),1], 1/10, 1/152, date1, date2, 
                      simdata[dim(simdata)[1],3], simdata[dim(simdata)[1],4], inpt.data)|>
        mutate(cat = 'sim', area = inpt.data$area[1])
    }
    # We then output the same objects as the function in Code C.18:
    obsdata <- filter(inpt.data, between(date, as.Date(date1,'%d/%m/%Y'), as.Date(date2,'%d/%m/%Y')))|>
      mutate(cat = 'obs', area = inpt.data$area[1])
    
    sep.plotdata <- rbind(simdata[,c(1,5:7)], obsdata[,c(1,2,4,7)])
    plotdata <- rbind(plotdata, sep.plotdata)
    
    abc.output <- append(abc.output, list(abc.out))
    names(abc.output)[i] <- paste('abc.out.', i, sep='')
  }
  abc.output <- append(abc.output, list(plotdata = plotdata))
  return(abc.output)
}

# Run for London:
set.seed(1) # Run to reproduce report results
ldn.out <- abc.combined(ldndata, 1000, 5000, c(0.1,0.2), c(5000,600000), c(5000,600000))

# Plot marginal posterior histograms for betas:
par(mfrow=c(2,3))
plothists <- function(abc.out,inpt.data){
  
  dates <- inpt.data[which(c(FALSE, tail(inpt.data$tier,-1) != head(inpt.data$tier,-1))),1]$date
  dates <- dates[1:(length(dates)-1)]
  dates <- append(dates, as.Date('2021-02-06'))
  
  for(i in 1:(length(dates)-1)){
    tier <- inpt.data[inpt.data$date==as.Date(dates[i]),6]
    dates.title <- paste(format.Date(dates[i],'%d/%m/%Y'),format.Date(dates[i+1],'%d/%m/%Y'),sep=' - ')
    tier.title <- ifelse(tier=='National Lockdown','(National Lockdown)',paste('(Tier ',tier,')',sep=''))
    hist(abc.out[[i]]$beta, main=paste(dates.title,tier.title,sep=' '),xlab='Beta')
  }
}

plothists(ldn.out, ldndata)

# Find summary statistics:
find.summary.stats <- function(abc.out, inpt.data){
  inpt.data$date <- as.Date(inpt.data$date)
  
  dates <- inpt.data[which(c(FALSE, tail(inpt.data$tier,-1) != head(inpt.data$tier,-1))),1]$date
  dates <- dates[1:(length(dates)-1)]
  dates <- append(dates, as.Date('2021-02-06'))
  
  best <- data.frame()
  
  for(i in 1:(length(abc.out)-1)){
    tier <- inpt.data[inpt.data$date==as.Date(dates[i]),6]
    data <- abc.out[[i]]
    best <- rbind(best,cbind(data.frame(minimal_diff=data[which.min(data$diff),1],mean=mean(data$beta),median=median(data$beta),date=dates[i]),tier))
  }
  return(best)
}

View(find.summary.stats(ldn.out,ldndata))

# Plot observed v.s. simulated data:
ggplot(ldn.out$plotdata, aes(x=date, y=new_cases, color=cat, linetype=cat)) + geom_line(lwd=1) + xlab('Date') + ylab('New Daily Cases') + scale_linetype_manual(name='Data type', labels=c('Observed','Simulated'), values=c('solid','longdash')) + scale_color_discrete(name='Data type', type=c('azure4','red'), labels=c('Observed','Simulated'))

# We explore what happens if we plot observed data v.s. data simulated using the estimates we obtained for constant tiers:
# Create a list of all dates when tier changed:
dates <- ldndata[which(c(FALSE, tail(ldndata$tier,-1) != head(ldndata$tier,-1))),1]$date
dates <- dates[1:(length(dates)-1)]
dates <- append(dates, as.Date('2021-02-06'))
sirs.out <- data.frame()
parms.out <- data.frame()
betas <- c(0.1359998, 0.1354498, 0.1290876, 0.1269176 )
# c(Tier 1, Tier 2, Tier 3, Tier 4/National lockdown)

# Iterate through list of dates
for(i in 1:(length(dates)-1)){
  # Filter data between the period considered in each iteration:
  in.period <- filter(ldndata, between(date, as.Date(dates[i]), as.Date(dates[i+1])))
  # Record the tier that London was in during this time period
  tier <- in.period$tier[1]
  # Assign the beta value that we estimated for areas constantly in this corresponding tier:
  beta <- ifelse(tier=='1',betas[1],
                 ifelse(tier=='2',betas[2],
                        ifelse(tier=='3',betas[3],
                               ifelse(tier=='4'|tier=='National Lockdown',betas[4],NA))))
  if(i==1){
    # For the first iteration, we use the initial conditions from our posterior sample which minimised the average distance:
    sirs.data <- sirs(beta, 1/10, 1/152, format(as.Date(dates[i]), "%d/%m/%Y"), format(as.Date(dates[i+1]), "%d/%m/%Y"), ldn.out[[1]][which.min(ldn.out[[1]]$diff),2], ldn.out[[1]][which.min(ldn.out[[1]]$diff),3], ldndata)
    parms.out <- rbind(parms.out, data.frame(beta=beta, I0=ldn.out[[1]][which.min(ldn.out[[1]]$diff),2], R0=ldn.out[[1]][which.min(ldn.out[[1]]$diff),3], tier=tier, section=i))
  }
  if(i!=1){
    # For all other iterations, we use the final states of the previous time period as the initial conditions:
    sirs.data <- sirs(beta, 1/10, 1/152, format(as.Date(dates[i]), "%d/%m/%Y"), format(as.Date(dates[i+1]), "%d/%m/%Y"), sirs.out[dim(sirs.out)[1],3], sirs.out[dim(sirs.out)[1],4], ldndata)
    parms.out <- rbind(parms.out, data.frame(beta=beta, I0=sirs.out[dim(sirs.out)[1],3], R0=sirs.out[dim(sirs.out)[1],4], tier=tier, section=i))
  }
  sirs.out <- rbind(sirs.out, sirs.data)
}

# Filter observed data between considered dates ready for plotting:
ldn.1 <- filter(ldndata,between(date,as.Date('14/10/2020','%d/%m/%Y'),as.Date('6/2/2021','%d/%m/%Y')))[,c(1,4)]|>
  mutate(cat='obs')

plotdata <- rbind(ldn.1, mutate(sirs.out[,c(1,5)], cat='sim'))
ggplot(plotdata, aes(x=date, y=new_cases, color=cat, linetype=cat)) + geom_line(lwd=0.75) + scale_color_discrete(name='Data type', labels=c('Observed','Simulated'), type=c('azure4','red')) + scale_linetype_manual(name='Data type', labels=c('Observed','Simulated'), values=c('solid','longdash')) + xlab('Date') + ylab('New Daily Cases')
