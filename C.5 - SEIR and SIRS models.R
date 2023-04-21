# Install and load required packages:

# install.packages('tidyverse')
# install.packages('ggplot2')
# install.packages('gridExtra')
library(tidyverse)
library(ggplot2)
library(gridExtra)

# We first define the SEIR function:
seir <- function(N, beta, gamma, epsilon, t.end, dt){
  # epsilon = rate of movement from exposed to infected compartment
  # Other parameters are as in the SIR function from Code C.4
  
  St <- N    
  It <- 1
  Et <- 0 # We now initialise the exposed compartment, as well as S, I and R
  Rt <- 0
  
  Susceptible <- c(N)
  Infected <- c(1)
  Exposed <- c(0) # We also include a new vector 'Exposed' to store the E(t) values
  Recovered <- c(0)
  
  t <- seq(0, t.end, dt)  
  
  for (i in 2:length(t)){
    # Use solutions from Equation 2.11:
    St.1 <- St - beta*St*It*dt/N
    Et.1 <- Et + (beta*St*It/N - epsilon*Et)*dt
    It.1 <- It + (epsilon*Et - gamma*It)*dt
    Rt.1 <- Rt + gamma*It*dt
    
    Susceptible <- append(Susceptible, St.1)
    Exposed <- append(Exposed, Et.1)
    Infected <- append(Infected, It.1)
    Recovered <- append(Recovered, Rt.1)
    
    St <- St.1
    Et <- Et.1
    It <- It.1
    Rt <- Rt.1
  }
  
  return(data.frame(t, Susceptible, Exposed, Infected, Recovered)) 
}

# Generate and plot SEIR data:
data.seir <- pivot_longer(seir(67100000, 0.275, 1/10, 1/5, 365, 1), cols=2:5, names_to='cat', values_to = 'n')
p <- ggplot(data.seir, aes(x=t, y=n, color=cat)) + geom_line(lwd=1.1) + xlab('Time t') + ylab('Size of compartment at time t') + labs(color='Compartment') + scale_color_discrete(breaks=c('Susceptible','Exposed','Infected','Recovered'), type=c('goldenrod1','red','springgreen2','cornflowerblue')) + ggtitle('SEIR Model')

# We now define the SIRS function:
sirs <- function(N, beta, gamma, xi, t.end, dt){
  # xi = rate of loss of immunity
  # Other parameters are as in the SIR function from Code C.4
  
  St <- N    
  It <- 1
  Rt <- 0 
  
  Susceptible <- c(N)
  Infected <- c(1)
  Recovered <- c(0)
  
  t <- seq(0, t.end, dt)  
  
  for (i in 2:length(t)){
    # Use solutions from Equation 2.13:
    St.1 <- St + (xi*Rt - beta*St*It/N)*dt
    It.1 <- It + (beta*St*It/N - gamma*It)*dt
    Rt.1 <- Rt + (gamma*It - xi*Rt)*dt
    
    Susceptible <- append(Susceptible, St.1)
    Infected <- append(Infected, It.1)
    Recovered <- append(Recovered, Rt.1)
    
    St <- St.1
    It <- It.1
    Rt <- Rt.1
  }
  
  return(data.frame(t, Susceptible, Infected, Recovered)) 
}

# Generate and plot SIRS data:
data.sirs <- pivot_longer(sirs(67100000, 0.275, 1/10, 1/152, 365, 1), cols=c(2,3,4), names_to='cat', values_to = 'n')
q <- ggplot(data.sirs, aes(x=t, y=n, color=cat)) + geom_line(lwd=1.1) + xlab('Time t') + ylab('Number of individuals in compartment') + labs(color='Compartment') + scale_color_discrete(breaks=c('Susceptible','Infected','Recovered'), type=c('red','springgreen2','cornflowerblue')) + ggtitle('SIRS Model')

# View both plots together:
grid.arrange(p, q, ncol=2)

# We now compare the SIR, SEIR and SIRS models:

# From Code C.4, the SIR function is:
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

# Create a data frame with generated data from all 3 models combined:
plotdata <- rbind(mutate(sir(67100000, 0.275, 1/10, 365, 1)[,c(1,3)], cat='SIR'),
                  mutate(seir(67100000, 0.275, 1/10, 1/5, 365, 1)[,c(1,4)], cat='SEIR'),
                  mutate(sirs(67100000, 0.275, 1/10, 1/152, 365, 1)[,c(1,3)], cat='SIRS'))

# Plot I(t) for each model to compare:
ggplot(plotdata, aes(x=t, y=Infected, colour=cat)) + geom_line(lwd=1.1) + scale_color_discrete(name='Model', breaks=c('SIR','SEIR','SIRS'), type=c('red','black','cornflowerblue')) + xlab('Time t') + ylab('Size of infected compartment I(t)') + theme(legend.position = 'bottom')

# We now explore the effect of different epsilon/xi values on the shape of the infective curve for the SEIR/SIRS models (respectively):

epsilons <- c(1/3, 1/5, 1/10, 1/14)
xis <- c(1/30, 1/91, 1/152, 1/243)

# Initialise data frames to store the generated data values:
seirdata <- data.frame()
sirsdata <- data.frame()

# Iterate through the lists of values:
for(i in 1:4){
  epsilon <- epsilons[i]
  xi <- xis[i]
  # Generate data for each based on the current iteration, and append to the relevant data frame:
  seirdata <- rbind(seirdata, mutate(seir(67100000, 0.275, 1/10, epsilon, 365, 1)[,c(1,4)], epsilon=epsilon))
  sirsdata <- rbind(sirsdata, mutate(sirs(67100000, 0.275, 1/10, xi, 365, 1)[,c(1,3)], xi=xi))
}

# Plot the generated data for each model:
s <- ggplot(seirdata, aes(x=t, y=Infected, colour=factor(epsilon))) + geom_line(lwd=1.1) + xlab('Time t') + ylab('Size of infected compartment I(t)') + ggtitle('SEIR Model') + scale_color_discrete(name='Exposure period', breaks=epsilons, labels=c('3 days','5 days','10 days','14 days'), type=c('cornflowerblue','springgreen2','goldenrod1','red'))
t <- ggplot(sirsdata, aes(x=t, y=Infected, colour=factor(xi))) + geom_line(lwd=1.1) + xlab('Time t') + ylab('Size of infected compartment I(t)') + ggtitle('SIRS Model') + scale_color_discrete(name='Immunity period', breaks=xis, labels=c('1 month','3 months','5 months','8 months'), type=c('cornflowerblue','springgreen2','goldenrod1','red'))
grid.arrange(s, t, ncol=2)


