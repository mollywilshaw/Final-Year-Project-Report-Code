#Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('gridExtra')
library(ggplot2)
library(tidyverse)
library(gridExtra)

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

# First we create a synthetic data set of 'observed' infective values using a specific parameter, chosen to be beta = 0.25:

obsdata <- sir(500000, 0.25, 0.1, 250, 0.5)

# We then add some noise to simulate real infection data:

set.seed(1) # Run to reproduce report plot
for(i in 1:nrow(obsdata)){
  obsdata[i,3] <- obsdata[i,3] + rnorm(1, 10000, 3000)
}

min(obsdata[,3]) # We perform a 'sanity check' here - the number of people in a compartment cannot be negative, so if this value < 0, we should run lines 41-50 again until a value >= 0 is returned.

# The algorithm to obtain a sample from an approximate posterior distribution for beta via a rejection sampler proceeds as follows:

abc <- function(N.iter, epsilon, prior.int, obsdata, pars){ 
  # N = number of prior values we try, epsilon = tolerance, prior.int is the interval beta could lie in based on our prior beliefs, obsdata = observed data set we wish to compare to
  # pars = c(N, gamma, t.end, dt, t.start) = vector of values specifying other arguments of SIR function which we want to control
  
  post <- c() # Create vector to store posterior values 
  
  for(i in 1:N.iter){
    # Generate a random value from our uniform prior for beta:
    beta <- runif(1, prior.int[1], prior.int[2]) 
    
    # Simulate data based on this beta:
    data <- sir(pars[1], beta, pars[2], pars[3], pars[4])
    # Calculate the absolute difference between the simulated data and our observed data:
    diff <- abs(data[,3] - obsdata[,3])
    
    j <- 0 # Set counter
    
    # Count the number of simulated points that are within the chosen tolerance of the observed data: 
    for(x in diff){
      if(x < epsilon){
        j <- j + 1
      } 
    }
    # If all points are within the tolerance, we accept this beta as a posterior value:
    if(j == length(diff)){
      post <- append(post, beta)
    }
  }
  return(post)
}

# Run rejection sampler algorithm for our synthetic data:
set.seed(1)
post <- abc(5000, 30000, c(0.1,0.4), obsdata, c(500000,0.1,250,0.5))

# We can see how closely our ABC algorithm has approximated our 'true' beta:
beta_abc <- mean(post) # Use the mean of the posterior values
simuldata <- sir(500000, beta_abc, 0.1, 250, 0.5) # Create simulated data using the posterior mean

# Here we combine the simulated and observed data ready to plot on one graph:
simuldata <- simuldata|>
  mutate(cat="sim")

obsdata <- obsdata|>
  mutate(cat="obs")

# Plot the data v.s. the modelled data based on the posterior mean:
p <- ggplot(rbind(obsdata, simuldata), aes(x=t, y=Infected, color=cat)) + geom_line() + xlab('Time') + ylab('Number of infected individuals')+scale_color_manual(values=c("black","red"), name="Dataset", labels=c("Observed*","Simulated")) + theme(legend.position = 'bottom')

# Plot a posterior histogram for beta:
q <- ggplot(as.data.frame(post), aes(x=post)) + geom_histogram(breaks=c(0.247,0.248,0.249,0.250,0.251,0.252,0.253,0.254), col='black', fill='grey') + xlab('Beta') + ylab('Count')

# View both plots:
grid.arrange(p, q, ncol=2)

# We now explore the effect of tolerance epsilon on the posterior sample:

# Run the rejection sampler with the same arguments as before, but with tolerance epsilon = 21000:
post.2 <- abc(5000, 21000, c(0.1,0.4), obsdata, c(500000,0.1,250,0.5))
# This value gives no output.

set.seed(1) # Run to reproduce report plot
# Run the rejection sampler with the same arguments as before, but with tolerance epsilon = 5000000:
post.3 <- abc(5000, 5000000, c(0.1,0.4), obsdata, c(500000,0.1,250,0.5))

# Create similar plots as before: 
beta_abc.3 <- mean(post.3) 
simuldata.3 <- sir(500000, beta_abc.3, 0.1, 250, 0.5) 

simuldata.3 <- simuldata.3|>
  mutate(cat="sim")

p <- ggplot(rbind(obsdata, simuldata.3), aes(x=t, y=Infected, color=cat)) + geom_line() + xlab('Time') + ylab('Number of infected individuals') + scale_color_manual(values=c("black","red"),name="Dataset",labels=c("Observed*","Simulated")) + theme(legend.position = 'bottom')

q <- ggplot(as.data.frame(post.3), aes(x=post.3)) + geom_histogram(binwidth=0.01, col='black', fill='grey') + xlab('Beta') + ylab('Count')

grid.arrange(p, q, ncol=2)



