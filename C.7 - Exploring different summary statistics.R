# Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
library(ggplot2)
library(tidyverse)

# We use the SIR function from Code C.4:
sir <- function(N, beta, gamma, t.end, dt){
  
  St <- N  
  It <- 1
  Rt <- 0
  
  Susceptible <- c(N) 
  Infected <- c(1)
  Recovered <- c(0)
  
  t <- seq(0, t.end, dt)  
  
  for (i in 2:length(t)){
    St.1 <- St - beta*St*It*dt/N
    It.1 <- It + beta*St*It*dt/N - gamma*It*dt
    Rt.1 <- Rt + gamma*It*dt
    
    Susceptible <- append(Susceptible, St.1) 
    Infected <- append(Infected, It.1)
    Recovered <- append(Recovered, Rt.1)
    
    St <- St.1
    It <- It.1
    Rt <- Rt.1
  }
  return(data.frame(t, Susceptible, Infected, Recovered)) 
}

# As in Code C.6, we generate a synthetic data set:

obsdata <- sir(500000, 0.25, 0.1, 250, 0.5)

set.seed(1) # Run to reproduce report plot
for(i in 1:nrow(obsdata)){
  obsdata[i,3] <- obsdata[i,3] + rnorm(1, 10000, 3000)
}

# We now adapt the rejection sampler function - it will now also return the average difference between the observed data and the data simulated from an accepted beta value.

abc.diff <- function(N.iter, epsilon, prior.int, obsdata, pars){ 
  # Arguments as in abc() from Code C.6

  # We will now store the posterior values in a data frame:  
  post <- data.frame() 
  
  for(i in 1:N.iter){
    beta <- runif(1, prior.int[1], prior.int[2]) 
    
    data <- sir(pars[1], beta, pars[2], pars[3], pars[4])
    diff <- abs(data[,3] - obsdata[,3])
    
    j <- 0 
    
    for(x in diff){
      if(x < epsilon){
        j <- j + 1
      } 
    }

    if(j == length(diff)){
      # We now append both the accepted beta value and the average difference to 'post'
      post <- rbind(post, data.frame(beta = beta, diff = mean(diff)))
    }
  }
  return(post)
}

# Run for the synthetic dataset:
set.seed(1)
post <- abc.diff(5000, 30000, c(0.1,0.4), obsdata, c(500000,0.1,250,0.5))

# Find summary statistics from the posterior:
mean <- mean(post$beta) 
med <- median(post$beta)
closest <- post[which.min(post$diff),1]

# To find the mode, look at the histogram:
h <- hist(post$beta)
h$breaks
mode <- 0.248

# Generate SIR model data based on these beta estimates:
simuldata.mean <- sir(500000, mean, 0.1, 250, 0.5)|>
  mutate(cat='mean')
simuldata.med <- sir(500000, med, 0.1, 250, 0.5)|>
  mutate(cat='median')
simuldata.mode <- sir(500000, mode, 0.1, 250, 0.5)|>
  mutate(cat='mode')
simuldata.closest <- sir(500000, closest, 0.1, 250, 0.5)|>
  mutate(cat='closest')

# Combine all into one data frame:
simuldata <- rbind(simuldata.mean, simuldata.med, simuldata.mode, simuldata.closest)

obsdata <- obsdata|>
  mutate(cat="obs")

# Plot the observed v.s. simulated data, with the simulated data based on the different summary statistics above:
ggplot(rbind(simuldata, obsdata), aes(x=t, y=Infected, color=cat)) + geom_line(lwd=1) + scale_color_discrete(name='Data type', breaks=c('obs','mean','median','mode','closest'), labels=c('Observed','Simulated (mean)','Simulated (median)','Simulated (mode)','Simulated (minimal distance)'), type=c('cornflowerblue','red','goldenrod1','springgreen2','darkgrey')) + xlab('Time t') + ylab('Number of infected individuals')

