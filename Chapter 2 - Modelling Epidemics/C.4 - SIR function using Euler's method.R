# Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('gridExtra')
library(ggplot2)
library(tidyverse)
library(gridExtra)

# We can create a vector of solutions to the SIR equations at time t using the following iterative function:

sir <- function(N, beta, gamma, t.end, dt){
  # N = population size, beta = rate of infection, gamma = rate of recovery
  # t.end = end of time period, dt = time change increment
  
  St <- N # Initialise the numbers within the compartments   
  It <- 1
  Rt <- 0
  
  Susceptible <- c(N) # Create the vectors to store the results
  Infected <- c(1)
  Recovered <- c(0)
  
  t <- seq(0, t.end, dt) # Create the vector of time increments 
  
  for (i in 2:length(t)){
    # Use iterative solutions in Equation 2.7 to get the number within each compartment at time t:
    St.1 <- St - beta*St*It*dt/N
    It.1 <- It + beta*St*It*dt/N - gamma*It*dt
    Rt.1 <- Rt + gamma*It*dt
    
    Susceptible <- append(Susceptible, St.1) # Add results to the vector
    Infected <- append(Infected, It.1)
    Recovered <- append(Recovered, Rt.1)
    
    St <- St.1 # Overwrite the current states ready for the next iteration
    It <- It.1
    Rt <- Rt.1
  }
  
  # Create a data frame of t values and the corresponding sizes of the compartments:
  return(data.frame(t, Susceptible, Infected, Recovered)) 
}

# Plot the model:
plotdata <- sir(67100000, 0.275, 1/10, 365, 1)
plotdata <- pivot_longer(plotdata, 2:4, names_to = 'Compartment', values_to = 'size')

p <- ggplot(plotdata, aes(x=t, y=size, col=Compartment)) + geom_line(lwd=1.25) + xlab('Time t') + ylab('Size of compartment at time t') + scale_color_discrete(breaks=c('Susceptible','Infected','Recovered'), type=c('red','springgreen2','cornflowerblue'))
q <- ggplot(filter(plotdata, Compartment=='Infected'), aes(x=t, y=size)) + geom_line(lwd=1.25, colour='red') + xlab('Time t') + ylab('Size of compartment at time t')
grid.arrange(p, q, ncol=2)
