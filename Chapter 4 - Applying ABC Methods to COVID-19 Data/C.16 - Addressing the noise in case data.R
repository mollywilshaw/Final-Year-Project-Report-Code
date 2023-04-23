# Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('grid')
# install.packages('janitor')
library(ggplot2)
library(tidyverse)
library(grid)
library(janitor)

# Load combined data set:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true')

# Create new data sets for each of the 3 areas considered:

# South West - Tier 1
swdata <- data|>
  filter(area_name=='Cornwall'|area_name=='Devon'|area_name=='Dorset'|area_name=='Somerset'|area_name=='Gloucestershire'|area_name=='Wiltshire'|area_name=='West of England')|>
  group_by(date)|>
  summarise(area='South West', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))
swdata$date <- as.Date(swdata$date)

# North East - Tier 2
nedata <- data|>
  filter(area_name=='County Durham'|area_name=='Tyne and Wear'|area_name=='Tees Valley'|area_name=='Northumberland')|>
  group_by(date)|>
  summarise(area='North East', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))
nedata$date <- as.Date(nedata$date)

# Merseyside - Tier 3
msdata <- data|>
  filter(area_name=='Merseyside')|>
  group_by(date)|>
  summarise(area='Merseyside', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))
msdata$date <- as.Date(msdata$date)

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

# We now define a function which will average out the case data for each day based on the two days either side:
average.cases <- function(data){
  # Create a list of all the dates we will output averaged data for
  # Note that we have to exclude the two dates at either end of our data set, as there aren't two lots of data either side for these values
  alldates <- seq(as.Date(data$date[3]), as.Date(data$date[length(data$date)-2]), by='days')
  # Create a data set with this list of dates and a variable avg_cases in which we will store the average case numbers
  cases.avg <- data.frame(date=alldates, avg_cases=rep(NA,length(alldates)))
  
  # Iterate over the list of dates to compute the averaged case number for each:
  for(i in 1:length(alldates)){
    cases.avg$avg_cases[i] <- as.numeric(colMeans(data[between(data$date, cases.avg$date[i]-2, cases.avg$date[i]+2), 4])) 
  }
  # Subset the original data set such that the rows correspond to the dates we have averaged data for:
  data.av <- data[c(3:(dim(data)[1]-2)), ]
  # Replace the variable 'new_cases' in this data set with the averaged cases:
  data.av$new_cases <- cases.avg$avg_cases
  return(data.av)
}

# Use this function for each of our 3 areas:
sw.avg <- average.cases(swdata)
ne.avg <- average.cases(nedata)
ms.avg <- average.cases(msdata)

# We can create a plot to show the averaged v.s. true data:

# Combine the 3 averaged data sets, and filter between the dates we are considering: 
avg.data <- rbind(sw.avg, ne.avg, ms.avg)|>
  mutate(cat='average')|>
  filter(between(date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))

# Do the same for the true data:
true.data <- rbind(swdata, nedata, msdata)|>
  mutate(cat='real')|>
  filter(between(date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))

# Combine the true and averaged data into one data set:
plotdata <- rbind(true.data, avg.data)

# Plot:
p <- ggplot(plotdata, aes(x=date, y=new_cases, color=cat)) + geom_line(lwd=1.5) + facet_wrap(~factor(area, levels=c('South West', 'North East','Merseyside')), ncol=3, scales='free_y') + scale_color_discrete(breaks=c('real','average'), name='Data type', labels=c('Original','Averaged'), type=c('steelblue3','darkgrey')) + aes(group=rev(cat)) + xlab('Date') + ylab('New Daily Cases') + theme(legend.position = 'bottom')

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

# Using the averaged data, we run the ABC algorithm:
set.seed(1)
abc.sw.avg <- abc(10000, 550, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), sw.avg)
abc.ne.avg <- abc(10000, 650, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), ne.avg)
abc.ms.avg <- abc(10000, 550, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), ms.avg)

# For comparison, we also run the ABC algorithm for the true data, as in Code C.15:
abc.sw <- abc(10000, 850, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), swdata)
abc.ne <- abc(10000, 800, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), nedata)
abc.ms <- abc(10000, 750, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), msdata)

# Plot marginal posterior histograms for beta (for the averaged data):
par(mfrow=c(1,3))
hist(abc.sw.avg$beta, col='deepskyblue', xlab='Beta', main='South West (Tier 1)', breaks=seq(0.11, 0.18, by=0.0025), ylim=c(0,200))
hist(abc.ne.avg$beta, col='yellow', xlab='Beta', main='North East (Tier 2)', breaks=seq(0.11, 0.18, by=0.0025), ylim=c(0,200))
hist(abc.ms.avg$beta, col='red', xlab='Beta', main='Merseyside (Tier 3)', breaks=seq(0.11, 0.18, by=0.0025), ylim=c(0,200))

# Find the summary statistics:
find.summary.stats <- function(abc.output){
  mean <- mean(abc.output$beta)
  median <- median(abc.output$beta)
  min.diff <- abc.output[which.min(abc.output$diff), 1]
  return(c(min.diff=min.diff, mean=mean, median=median))
}

find.summary.stats(abc.sw.avg)
find.summary.stats(abc.ne.avg)
find.summary.stats(abc.ms.avg)

# Plot observed v.s. simulated data for both averaged and true data:
plotdata <- function(abc.output, area, data, avg=FALSE){
  # We add ifelse statements to change the variable 'cat' depending on whether the data is averaged or not.
  data.mean <- sirs(mean(abc.output$beta), 1/10, 1/152, '14/10/2020', '5/11/2020', mean(abc.output$I0), mean(abc.output$R0), data)|>
    mutate(cat=ifelse(avg==FALSE, 'sim.mean','sim.mean.avg'), area=area)
  data.median <- sirs(median(abc.output$beta), 1/10, 1/152, '14/10/2020', '5/11/2020', median(abc.output$I0), median(abc.output$R0), data)|>
    mutate(cat=ifelse(avg==FALSE, 'sim.median', 'sim.median.avg'), area=area)
  data.mindiff <- sirs(abc.output[which.min(abc.output$diff),1], 1/10, 1/152, '14/10/2020', '5/11/2020', abc.output[which.min(abc.output$diff),2], abc.output[which.min(abc.output$diff),3], data)|>
    mutate(cat=ifelse(avg==FALSE, 'sim.min', 'sim.min.avg'), area=area)
  return(rbind(data.mean, data.median, data.mindiff)[c(1,5:7)])
}

plotdata.sw <- plotdata(abc.sw, 'South West', swdata)
plotdata.sw.avg <- plotdata(abc.sw.avg, 'South West', sw.avg, avg=T)
plotdata.ne <- plotdata(abc.ne, 'North East', nedata)
plotdata.ne.avg <- plotdata(abc.ne.avg, 'North East', ne.avg, avg=T)
plotdata.ms <- plotdata(abc.ms, 'Merseyside', msdata)
plotdata.ms.avg <- plotdata(abc.ms.avg, 'Merseyside', ms.avg, avg=T)

plotdata <- rbind(plotdata.sw, plotdata.ne, plotdata.ms, plotdata.sw.avg, plotdata.ne.avg, plotdata.ms.avg, true.data[c(1,2,4,6)], avg.data[c(1,2,4,6)])

q <- ggplot(plotdata, aes(x=date, y=new_cases, color=cat, linetype=cat)) + geom_line(lwd=1) + facet_wrap(~factor(area, levels=c('South West','North East','Merseyside')) ,ncol=3, scales='free_y') + scale_color_manual(breaks=c('real','average','sim.min','sim.min.avg','sim.mean','sim.mean.avg','sim.median','sim.median.avg'), name='Data type', labels=c('Original','Averaged','Simulated (minimising difference)','Simulated (minimising difference, averaged)','Simulated (mean)','Simulated (mean, averaged)','Simulated (median)','Simulated (median, averaged)'), values=c('average'='black','real'='darkgrey','sim.mean'='lightblue2','sim.mean.avg'='steelblue3','sim.median'='darkolivegreen2','sim.median.avg'='chartreuse3','sim.min'='lightsalmon','sim.min.avg'='red')) + scale_linetype_manual(breaks=c('real','average','sim.min','sim.min.avg','sim.mean','sim.mean.avg','sim.median','sim.median.avg'), name='Data type', labels=c('Original','Averaged','Simulated (minimising difference)','Simulated (minimising difference, averaged)','Simulated (mean)','Simulated (mean, averaged)','Simulated (median)','Simulated (median, averaged)'), values=c('real'='solid','average'='solid','sim.mean'='longdash','sim.mean.avg'='longdash','sim.median'='longdash','sim.median.avg'='longdash','sim.min'='longdash','sim.min.avg'='longdash')) + theme(legend.text=element_text(hjust = 0.5), legend.position = 'bottom', legend.key.size=unit(30,"points")) + xlab('Date') + ylab('New Daily Cases')

g <- ggplot_gtable(ggplot_build(q))
striprt <- which( grepl('strip-1', g$layout$name) | grepl('strip-t', g$layout$name) )
fills <- c('deepskyblue','yellow','red')
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)


# We now overlay the marginal posterior histograms for the true and averaged data:
# The following function allows us to make the histograms transparent so we can overlay them:

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk, specifically https://www.dataanalytics.org.uk/make-transparent-colors-in-r/#:~:text=Make%20transparent%20colors%20in%20R&text=The%20rgb()%20command%20is,255%20being%20%E2%80%9Csolid%E2%80%9D).

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

par(mfrow=c(1,3))
hist(abc.sw.avg$beta, col='deepskyblue', main='South West (Tier 1)', xlab='Beta', breaks=seq(0.11,0.18,by=0.0025), ylim=c(0,200))
hist(abc.sw$beta, col=t_col('deepskyblue4'), breaks=seq(0.11,0.18,by=0.0025), add=TRUE)
legend('topright', c('Original','Averaged'), fill=c(t_col('deepskyblue4'),'deepskyblue'), title='Data type')

hist(abc.ne.avg$beta, col='yellow', main='North East (Tier 2)', xlab='Beta', breaks=seq(0.11,0.18,by=0.0025), ylim=c(0,200))
hist(abc.ne$beta, col=t_col('darkgoldenrod'), breaks=seq(0.11,0.18,by=0.0025), add=TRUE)
legend('topright', c('Original','Averaged'), fill=c(t_col('darkgoldenrod'),'yellow'), title='Data type')

hist(abc.ms.avg$beta, col='red', main='Merseyside (Tier 3)', xlab='Beta', breaks=seq(0.11,0.18,by=0.0025), ylim=c(0,200))
hist(abc.ms$beta, col=t_col('red4'), breaks=seq(0.11,0.18,by=0.0025), add=TRUE)
legend('topright', c('Original','Averaged'), fill=c(t_col('red4'),'red'), title='Data type')