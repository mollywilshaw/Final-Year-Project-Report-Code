# Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
library(ggplot2)
library(tidyverse)

# Create a vector of x values:
x <- seq(0, 1, by=0.0001)

# Compute the true value of the function at each x:
y <- exp(x)

# Plot true function:
plot(x,y,type='l')

# Specify some step sizes h to try:
h.list <- c(0.1,0.2,0.3)

# Run for each h in our list:
for(h in h.list){
  # Set initial conditions:
  x <- 0
  y <- 1
  
  # Create vectors to store the iterations of x and y:
  x.vec <- c(x)
  y.vec <- c(y)
  
  # Run 10 iterations:
  for(i in 0:10){
    # Use the iterative formula from Euler's method:
    y <- y + h*y
    x <- x+h
    # Append the new values to the x and y vectors:
    x.vec <- append(x.vec,x)
    y.vec <- append(y.vec,y)
  }
  # Plot the approximation of the function gained from this h value:
  lines(x.vec, y.vec, col=(h*10)+1, lty=2)
}

legend(x='topleft',legend=c('True function', 'h=0.1','h=0.2','h=0.3'),col=c(1,2,3,4),lty=c(1,2,2,2))

