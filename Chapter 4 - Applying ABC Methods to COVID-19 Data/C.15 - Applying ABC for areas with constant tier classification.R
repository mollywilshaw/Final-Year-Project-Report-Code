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
data$date <- as.Date(data$date) # Make sure R sees 'date' variable as date rather than text

# Create new data sets for each of the 3 areas considered:

# South West - Tier 1
swdata <- data|>
  filter(area_name=='Cornwall'|area_name=='Devon'|area_name=='Dorset'|area_name=='Somerset'|area_name=='Gloucestershire'|area_name=='Wiltshire'|area_name=='West of England')|>
  group_by(date)|>
  summarise(area='South West', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))

# North East - Tier 2
nedata <- data|>
  filter(area_name=='County Durham'|area_name=='Tyne and Wear'|area_name=='Tees Valley'|area_name=='Northumberland')|>
  group_by(date)|>
  summarise(area='North East', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))

# Merseyside - Tier 3
msdata <- data|>
  filter(area_name=='Merseyside')|>
  group_by(date)|>
  summarise(area='Merseyside', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases))

# Define SIRS function as in Figure 4.5:
sirs <- function(beta, gamma, xi, d.start, d.end, I0, R0, data){

  N <- data$population[1] # Extract population size N from the inputted data set

  # Initialise the state of the model:
  St <- N-R0-I0 # Initial condition for S is calculated using I0 and R0 
  It <- I0
  Rt <- R0 
  
  Susceptible <- c(St) 
  Infected <- c(It)
  Recovered <- c(Rt)
  
  # Time iteration vector is now a list of dates:
  date <- seq(as.Date(d.start,'%d/%m/%Y')-1, as.Date(d.end,'%d/%m/%Y'), by='days')  
  
  # Compute the iterative SIRS solutions as before:
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
  
  # To compare directly with the real COVID-19 data, which is given in terms of new daily cases, we calculate the daily change in the size of the infective compartment:
  new_cases <- c()
  for(j in 2:length(Infected)){
    new_cases[j-1] <- Infected[j] - Infected[j-1]
  }
  
  # Remove the 1st date - this was only here to calculate the new cases on our desired start date:
  date <- date[-1]
  Susceptible <- Susceptible[-1]
  Infected <- Infected[-1]
  Recovered <- Recovered[-1]
  
  return(data.frame(date, Susceptible, Infected, Recovered, new_cases)) 
}

# Define ABC function as in Figure 4.9:
abc <- function(N, epsilon, prior.beta, prior.I0, prior.R0, dates, inpt.data){ 
  # Arguments are:
  # - Number of iterations, N
  # - Tolerance, epsilon
  # - c(lower bound/upper bound) of the uniform priors for beta/I0/R0, prior.beta/prior.I0/prior.R0
  # - c(start date, end date), dates
  # - input data, inpt.data
  
  # Extract start/end date from 'dates' argument:
  d.start <- as.Date(dates[1],'%d/%m/%Y')
  d.end <- as.Date(dates[2],'%d/%m/%Y')
  
  # Create data frame of randomly generated prior values for beta, I0 and R0:
  prior <- data.frame(beta = runif(N, prior.beta[1], prior.beta[2]),
                      I0 = runif(N, prior.I0[1], prior.I0[2]),
                      R0 = runif(N, prior.R0[1], prior.R0[2]))
  # Create an empty data frame to hold the posterior values, as well as the average distance between observed/simulated data points, 'diff' 
  post <- data.frame('beta'=character(), 'I0'=character(), 'R0'=character(), 'diff'=character())
  
  # Filter the observed data set between the specified dates, ready for the comparison step:
  obscases <- filter(inpt.data, between(inpt.data$date, d.start, d.end))$new_cases
  
  # Loop over N iterations:
  for(i in 1:N){
    # Select the ith prior values as proposed parameters:
    beta <- prior[i,1] 
    I0 <- prior[i,2]
    R0 <- prior[i,3]
    
    # Run the SIRS model with these parameters:
    data <- sirs(beta, 1/10, 1/152, d.start, d.end, I0, R0, inpt.data)
    
    # Calculate the absolute difference between the simulated and observed data:
    diff <- abs(as.numeric(data$new_cases) - obscases)
    
    j <- 0 # Set counter
    
    # Count the number of simulated points that are within the chosen tolerance of the 'observed' data: 
    for(x in diff){
      if(x < epsilon){
        j <- j + 1
      } 
    }
    # If all points are within the tolerance, we accept these parameters into the posterior distribution: 
    if(j == length(diff)){
      post <- rbind(post, data.frame(beta=beta, I0=I0, R0=R0, diff=mean(diff)))
    }
  }
  return(post)
}

# Run the rejection sampler algorithm for the 3 considered areas:
set.seed(1) # Run to reproduce report results
abc.sw <- abc(10000, 850, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), swdata)
abc.ne <- abc(10000, 800, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), nedata)
abc.ms <- abc(10000, 750, c(0.1,0.2), c(6000,50000), c(9000,50000), c('14/10/2020','5/11/2020'), msdata)

# Plot histograms of the marginal posterior distributions for beta:
par(mfrow=c(1,3))
h <- hist(abc.sw$beta, col='deepskyblue', main='South West (Tier 1)', xlab='Beta', breaks=20, xlim=c(0.11,0.18), ylim=c(0,150))
hist(abc.ne$beta, col='yellow', main='North East (Tier 2)', xlab='Beta', breaks=20, xlim=c(0.12,0.17), ylim=c(0,150))
hist(abc.ms$beta, col='red', main='Merseyside (Tier 3)', xlab='Beta', breaks=20, xlim=c(0.11,0.16), ylim=c(0,150))

# Histograms for I0 and R0
par(mfrow=c(1,3))
hist(abc.sw$I0, col='deepskyblue', main='South West (Tier 1)', xlab='I0', xlim=c(0,50000), ylim=c(0,200))
hist(abc.ne$I0, col='yellow', main='North East (Tier 2)', xlab='I0', xlim=c(0,50000), ylim=c(0,200))
hist(abc.ms$I0, col='red', main='Merseyside (Tier 3)', xlab='I0', xlim=c(0,50000), ylim=c(0,200))

hist(abc.sw$R0, col='deepskyblue', main='South West (Tier 1)', xlab='R0', xlim=c(0,50000), ylim=c(0,200))
hist(abc.ne$R0, col='yellow', main='North East (Tier 2)', xlab='R0', xlim=c(0,50000), ylim=c(0,200))
hist(abc.ms$R0, col='red', main='Merseyside (Tier 3)', xlab='R0', xlim=c(0,50000), ylim=c(0,200))

# We can combine the marginal posterior histograms for beta as follows:

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
## END

par(mfrow=c(1,1))
hist(abc.ms$beta, col=t_col('red'), main=NULL, xlab='Beta', breaks=20, xlim=c(0.11,0.18))
hist(abc.sw$beta, col=t_col('deepskyblue'), add=T, breaks=20, xlim=c(0.11,0.18))
hist(abc.ne$beta, col=t_col('yellow'), add=T, breaks=20, xlim=c(0.11,0.18))
legend('topright', c('South West (Tier 1)','North East (Tier 2)', 'Merseyside (Tier 3)'), fill=c(t_col('deepskyblue'), t_col('yellow'), t_col('red')), bty='n')

# We can also compute summary statistics from the obtained posterior samples:
find.summary.stats <- function(abc.output){
  mean <- mean(abc.output$beta)
  median <- median(abc.output$beta)
  min.diff <- abc.output[which.min(abc.output$diff), 1]
  return(c(min.diff=min.diff, mean=mean, median=median))
}

find.summary.stats(abc.sw)
find.summary.stats(abc.ne)
find.summary.stats(abc.ms)

# We now create a plot to show how well our obtained posterior samples estimate the true data:

# Filter the 3 area data sets between the dates we are considering:
swdata.1 <- filter(swdata, between(date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))|>
  mutate(cat='real', area='South West')
nedata.1 <- filter(nedata, between(date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))|>
  mutate(cat='real', area='North East')
msdata.1 <- filter(msdata, between(date, as.Date('14/10/2020','%d/%m/%Y'), as.Date('5/11/2020','%d/%m/%Y')))|>
  mutate(cat='real', area='Merseyside')

# Join for all areas to create one 'observed' data set:
obsdata <- rbind(swdata.1, nedata.1, msdata.1)

# Create a plot of the true case data:
p <- ggplot(obsdata, aes(x=date, y=new_cases, color=cat, linetype=cat)) + geom_line(lwd=1) + xlab('Date') + ylab('New Daily Cases')+ facet_wrap(~factor(area, levels=c('South West','North East','Merseyside')), nrow=1, ncol=3, scales='free_y') + theme(legend.position='bottom')

# We now take a random sample of size 50 from our posterior samples for each area:
abc.sw.sample <- abc.sw[sample(seq(1:dim(abc.sw)[1]),50), ]
abc.ne.sample <- abc.ne[sample(seq(1:dim(abc.ne)[1]),50), ]
abc.ms.sample <- abc.ms[sample(seq(1:dim(abc.ms)[1]),50), ]

# For each of these samples, we simulate SIRS data and add the corresponding curve to the plot:
for(i in 1:dim(abc.sw.sample)[1]){
  simdata.new <- sirs(abc.sw.sample[i,1], 1/10, 1/152, '14/10/2020', '5/11/2020', abc.sw.sample[i,2], abc.sw.sample[i,3], swdata)|>
    mutate(type=paste('test', i, sep = '.'), cat='sim', area='South West')
  p <- p + geom_line(aes(x=date, y=new_cases), data=simdata.new, size=0.2)
}

for(i in 1:dim(abc.ne.sample)[1]){
  simdata.new <- sirs(abc.ne.sample[i,1], 1/10, 1/152, '14/10/2020', '5/11/2020', abc.ne.sample[i,2], abc.ne.sample[i,3], nedata)|>
    mutate(type=paste('test', i, sep = '.'), cat='sim', area='North East')
  p <- p + geom_line(aes(x=date, y=new_cases), data=simdata.new, size=0.2)
}

for(i in 1:dim(abc.ms.sample)[1]){
  simdata.new <- sirs(abc.ms.sample[i,1], 1/10, 1/152, '14/10/2020', '5/11/2020', abc.ms.sample[i,2], abc.ms.sample[i,3], msdata)|>
    mutate(type=paste('test', i, sep = '.'), cat='sim', area='Merseyside')
  p <- p + geom_line(aes(x=date, y=new_cases), data=simdata.new, size=0.2)
}

# We now generate SIRS data based on the summary statistics for each area:

plotdata <- function(abc.output, area, data){
  data.mean <- sirs(mean(abc.output$beta), 1/10, 1/152, '14/10/2020', '5/11/2020', mean(abc.output$I0), mean(abc.output$R0), data)|>
    mutate(cat='sim.mean', area=area)
  data.median <- sirs(median(abc.output$beta), 1/10, 1/152, '14/10/2020', '5/11/2020', median(abc.output$I0), median(abc.output$R0), data)|>
    mutate(cat='sim.median', area=area)
  data.mindiff <- sirs(abc.output[which.min(abc.output$diff),1], 1/10, 1/152, '14/10/2020', '5/11/2020', abc.output[which.min(abc.output$diff),2], abc.output[which.min(abc.output$diff),3], data)|>
    mutate(cat='sim.mindiff', area=area)
  return(rbind(data.mean, data.median, data.mindiff)[c(1,5:7)])
}

summary.stat.data <- rbind(plotdata(abc.sw, 'South West', swdata), plotdata(abc.ne, 'North East', nedata), plotdata(abc.ms, 'Merseyside', msdata))

# We now add lines corresponding to this data to the plot:
p <- p + geom_line(aes(x=date, y=new_cases), data=summary.stat.data, lwd=1) + scale_color_discrete(name='Data type', type=c('black','darkgrey','steelblue3','chartreuse3','red'), labels=c('Observed','Simulated','Simulated (mean)','Simulated (median)','Simulated (minimising difference)')) + scale_linetype_manual(name='Data type', labels=c('Observed','Simulated','Simulated (mean)','Simulated (median)','Simulated (minimising difference)'), values=c('solid','longdash','longdash','longdash','longdash')) + theme(legend.key.size = unit(30,'points'))

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

# We plot specifically for the North East, as motivation for the next subsection: 

ne.plotdata <- rbind(nedata.1[,c(1,4,6)], plotdata(abc.ne, 'North East', nedata)[,1:3])
ggplot(ne.plotdata, aes(x=date, y=new_cases, color=cat, linetype=cat)) + geom_line(lwd=1) + scale_color_discrete(name='Data type', labels=c('Observed','Simulated (mean)','Simulated (median)', 'Simulated (minimising difference)'), type=c('black','steelblue3','chartreuse3','red')) + scale_linetype_manual(name='Data type', labels=c('Observed','Simulated (mean)','Simulated (median)','Simulated (minimising difference)'), values=c('solid','longdash','longdash','longdash')) + xlab('Date') + ylab('New Daily Cases') + theme(legend.position = 'bottom')
