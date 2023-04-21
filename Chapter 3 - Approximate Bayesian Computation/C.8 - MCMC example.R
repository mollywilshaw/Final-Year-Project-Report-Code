# As in Code C.6, we generate a synthetic data set by using the SIR function, then adding randomly generated noise:
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

set.seed(1) # Run to reproduce report plot
obsdata <- sir(500000, 0.25, 0.1, 250, 0.5)
for(i in 1:nrow(obsdata)){
  obsdata[i,3] <- obsdata[i,3] + rnorm(1, 10000, 3000)
}

# We now define the MCMC function:
mcmc <- function(N.iter, epsilon, beta.0, obsdata, pars){
  
  post <- data.frame()
  # Rather than specifying a prior interval for beta, we now initialise beta at beta.0:
  beta <- beta.0
  
  for(i in 1:N.iter){
    # Generate a value from the proposal distribution:
    beta.star <- rbeta(1, 0.5, 10*beta)
    # Generate SIR data based on this proposed beta value:
    data <- sir(pars[1], beta.star, pars[2], pars[3], pars[4])
    
    # Calculate the difference between the observed and simulated data as usual:
    diff <- abs(obsdata[,3] - data[,3])
    
    j <- 0
    
    for(x in diff){
      if(x < epsilon){
        j <- j + 1
      }
    }
    if(j == length(diff)){
      # We now add the acceptance probability step described in the report:
      u <- runif(1, 0, 1) 
      if(log(u) <= log(dbeta(beta, 0.5, 10*beta.star)) 
                   - log(dbeta(beta.star, 0.5, 10*beta))){
        post <- rbind(post, data.frame(beta = beta,
                                       diff = mean(diff)))
        beta <- beta.star
      }
    }
  }
  return(post)
}

# Run MCMC for our synthetic data set, initialising at beta.0 = 0.25: 
set.seed(1) # Run to reproduce report plot
mcmc.out <- mcmc(20000, 30000, 0.25, obsdata, c(500000,0.1,250,0.5))

# Plot the posterior histogram and trace plot:
p <- hist(mcmc.out$beta)
par(mfrow=c(1,2))
plot(p,col='grey',main=NULL,xlab='Beta')
plot(ts(mcmc.out$beta),xlab='Iteration',ylab='Beta')

# We now investigate the burn-in period:

# Initialise at beta.0 = 0.2 - further from our true value of 0.25
set.seed(1) # Run to reproduce report plot
mcmc.out.burn.in <- mcmc(20000, 30000, 0.2, obsdata, c(500000,0.1,250,0.5))

# Plot posterior histogram and trace plot:
p <- hist(mcmc.out.burn.in$beta)
par(mfrow=c(1,2))
plot(p,col='grey',main=NULL,xlab='Beta')
plot(ts(mcmc.out.burn.in$beta),xlab='Iteration',ylab='Beta')

# We can remove the burn-in period and plot again:
p <- hist(mcmc.out.burn.in$beta[4:length(mcmc.out.burn.in$beta)])
par(mfrow=c(1,2))
plot(p,col='grey',main=NULL,xlab='Beta')
plot(ts(mcmc.out.burn.in$beta[4:length(mcmc.out.burn.in$beta)]),xlab='Iteration',ylab='Beta')
