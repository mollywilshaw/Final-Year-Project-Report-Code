# Install and load required packages:

# install.packages('tidyverse')
# install.packages('ggplot2')
# install.packages('cowplot')
# install.packages('grid')
# install.packages('gtable')
library(tidyverse)
library(ggplot2)
library(cowplot)
library(grid)
library(gtable)

# Load combined data set:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true')

# Create separate data sets for the considered areas:
cwdata <- data|>
  filter(area_name=='Cornwall')|>
  group_by(date)|>
  summarise(area='Cornwall',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases),tier=tier)
cwdata <- unique(cwdata)
cwdata$date <- as.Date(cwdata$date)

nsdata <- data|>
  filter(area_name=='Suffolk'|area_name=='Norfolk')|>
  group_by(date)|>
  summarise(area='Norfolk & Suffolk',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases),tier=tier)
nsdata <- unique(nsdata)
nsdata$date <- as.Date(nsdata$date)

iwdata <- data|>
  filter(area_name=='Isle of Wight')|>
  group_by(date)|>
  summarise(area='Isle of Wight',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases),tier=tier)
iwdata <- unique(iwdata)
iwdata$date <- as.Date(iwdata$date)

nedata <- data|>
  filter(area_name=='County Durham'|area_name=='Tyne and Wear'|area_name=='Tees Valley'|area_name=='Northumberland')|>
  group_by(date)|>
  summarise(area='North East', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases),tier=tier)
nedata <- unique(nedata)
nedata$date <- as.Date(nedata$date)

msdata <- data|>
  filter(area_name=='Merseyside')|>
  group_by(date)|>
  summarise(area='Merseyside', population=sum(population), new_cases=sum(new_cases), cumulative_cases=sum(cumulative_cases),tier=tier)
msdata <- unique(msdata)
msdata$date <- as.Date(msdata$date)

ldndata <- data|>
  filter(area_name=='Inner London'|area_name=='Outer London')|>
  group_by(date)|>
  summarise(area='London',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases),tier=tier)
ldndata <- unique(ldndata)
ldndata$date <- as.Date(ldndata$date)

sydata <- data|>
  filter(area_name=='South Yorkshire')|>
  group_by(date)|>
  summarise(area='South Yorkshire',population=sum(population),new_cases=sum(new_cases),cumulative_cases=sum(cumulative_cases),tier=tier)
sydata <- unique(sydata)
sydata$date <- as.Date(sydata$date)

# Define SIRS function:
sirs <- function(beta, gamma, xi, d.start, d.end, I0, R0, data){
  N <- data$population[1]
  
  St <- N-R0-I0  
  It <- I0
  Rt <- R0 
  
  Susceptible <- c(St) 
  Infected <- c(It)
  Recovered <- c(Rt)
  
  date <- seq(as.Date(d.start)-1, as.Date(d.end), by='days')  
  
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

# Define ABC functon:
abc <- function(N, epsilon, prior.beta, prior.I0, prior.R0, dates, inpt.data){ 
  
  d.start <- as.Date(dates[1])
  d.end <- as.Date(dates[2])
  
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

# Define function to average out case data:
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

# We now define a function to run ABC separately over each time period that a given area was in a different tier:
abc.combined <- function(inpt.data, N, epsilon, prior.beta, prior.I0, prior.R0){
  # Create a list of all dates on which the area moved tier:
  # Note that the last of these dates will be 30/3/2021 (the date on which the tier system technically ended). However, we do not want to consider this period, so we replace this final date with 6/2/2021:
  dates <- inpt.data[which(c(FALSE, tail(inpt.data$tier,-1) != head(inpt.data$tier,-1))),1]$date
  dates <- dates[1:(length(dates)-1)]
  dates <- append(dates, as.Date('2021-02-06'))
  # Create a data frame to store the plotting data, and a list to store the ABC output data frames:
  plotdata <- data.frame()
  abc.output <- list()
  
  # Iterate through the list of dates:
  for(i in 1:(length(dates) - 1)){
    # We choose adjacent pairs of dates to be the start/end dates for each period:
    date1 <- dates[i]
    date2 <- dates[i+1]
    
    # Run the usual ABC algorithm for the dates of this iteration:
    abc.out <- abc(N, epsilon, prior.beta, prior.I0, prior.R0, c(date1, date2), average.cases(inpt.data))
    
    # For the simulated data, we choose the parameters which minimise the average distance between the observed and simulated data:
    simdata <- sirs(abc.out[which.min(abc.out$diff),1], 1/10, 1/152, date1, date2, 
                    abc.out[which.min(abc.out$diff),2], abc.out[which.min(abc.out$diff),3], inpt.data)|>
      mutate(cat = 'sim', area = inpt.data$area[1])
    obsdata <- filter(inpt.data, between(date, as.Date(date1,'%d/%m/%Y'), as.Date(date2,'%d/%m/%Y')))|>
      mutate(cat = 'obs', area = inpt.data$area[1])
    
    # We combine the observed and simulated data into one data set, which we add to 'plotdata':
    sep.plotdata <- rbind(simdata[,c(1,5:7)], obsdata[,c(1,2,4,7)])
    plotdata <- rbind(plotdata, sep.plotdata)
    
    # We append the ABC output for this time period to the list of outputs:
    abc.output <- append(abc.output, list(abc.out))
    # The name corresponds to the iteration:
    names(abc.output)[i] <- paste('abc.out.', i, sep='')
  }
  # We add the plotdata to the output list:
  abc.output <- append(abc.output, list(plotdata = plotdata))
  return(abc.output)
}

# Run this algorithm for our considered areas:
set.seed(1) # Run to reproduce report results:
cw.out <- abc.combined(cwdata, 1000, 400, c(0.1,0.2), c(0,75000), c(0,75000))
# cw <- ggplot(cw.out$plotdata,aes(x=date,y=new_cases,color=cat))+geom_line()+ggtitle('Cornwall')
# cw
ns.out <- abc.combined(nsdata, 1000, 650, c(0.1,0.2), c(0,75000), c(0,75000))
# ns <- ggplot(ns.out$plotdata,aes(x=date,y=new_cases,color=cat))+geom_line()+ggtitle('Norfolk & Suffolk')
# ns

iw.out <- abc.combined(iwdata, 1000, 200, c(0.1,0.2), c(0,20000), c(0,20000))
# iw <- ggplot(iw.out$plotdata,aes(x=date,y=new_cases,color=cat))+geom_line()+ggtitle('Isle of Wight')
# iw

ne.out <- abc.combined(nedata, 1000, 2000, c(0.1,0.2), c(0,75000), c(0,75000))
# ne <- ggplot(ne.out$plotdata,aes(x=date,y=new_cases,color=cat))+geom_line()+ggtitle('North East')
# ne

ms.out <- abc.combined(msdata, 1000, 1250, c(0.1,0.2), c(0,100000), c(0,100000))
# ms <- ggplot(ms.out$plotdata,aes(x=date,y=new_cases,color=cat))+geom_line()+ggtitle('Merseyside')
# ms

ldn.out <- abc.combined(ldndata, 1000, 5000, c(0.1,0.2), c(5000,600000), c(5000,600000))
# ldn <- ggplot(ldn.out$plotdata,aes(x=date,y=new_cases,color=cat))+geom_line()+ggtitle('London')
# ldn

sy.out <- abc.combined(sydata, 1000, 1000, c(0.1,0.2), c(0,200000), c(0,200000))
# sy <- ggplot(sy.out$plotdata,aes(x=date,y=new_cases,color=cat))+geom_line()+ggtitle('South Yorkshire')
# sy

plotdata <- rbind(cw.out$plotdata,ns.out$plotdata,iw.out$plotdata,ne.out$plotdata,ms.out$plotdata,ldn.out$plotdata,sy.out$plotdata)

# Plot observed v.s. simulated data for all considered areas:
p <- ggplot(plotdata,aes(x=date,y=new_cases,color=cat,linetype=cat))+geom_line(lwd=1)+facet_wrap(~factor(area,levels=c('Cornwall','Norfolk & Suffolk','Isle of Wight','North East','Merseyside','London','South Yorkshire')),ncol=3,nrow=4,scales='free_y')+ xlab('Date') + ylab('New Daily Cases') + scale_linetype_manual(name='Data type', labels=c('Observed','Simulated'), values=c('solid','longdash')) + scale_color_discrete(name='Data type', type=c('azure4','red'), labels=c('Observed','Simulated'))

# The following code changes the colours of the title strip backgrounds for the plots
# This code is adapted from the following code request: https://github.com/tidyverse/ggplot2/issues/2096

g <- ggplot_gtable(ggplot_build(p))
striprt <- which( grepl('strip-1', g$layout$name) | grepl('strip-t', g$layout$name) )
striprt <- striprt[striprt!=48&striprt!=49]
fills <- c("red","chartreuse","darkorange","pink","deepskyblue","yellow","purple")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)


# The following function is from https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}

grid.draw(shift_legend(g))

# We now find summary statistics of our posterior samples, and also return the dates and tiers to which these summary statistics correspond:
find.summary.stats <- function(abc.out, inpt.data){
  inpt.data$date <- as.Date(inpt.data$date)
  
  # Create lists of dates when the tiers moved
  dates <- inpt.data[which(c(FALSE, tail(inpt.data$tier,-1) != head(inpt.data$tier,-1))),1]$date
  dates <- dates[1:(length(dates)-1)]
  dates <- append(dates, as.Date('2021-02-06'))
  
  # Create data frame to store summary statistics:
  best <- data.frame()
  
  for(i in 1:(length(abc.out)-1)){
    tier <- inpt.data[inpt.data$date==as.Date(dates[i]),6]
    data <- abc.out[[i]]
    best <- rbind(best, cbind(data.frame(minimal_diff=data[which.min(data$diff),1], mean=mean(data$beta), median=median(data$beta), date=dates[i]), tier))
  }
  return(best)
}

View(find.summary.stats(ne.out, nedata))
View(find.summary.stats(cw.out, cwdata))

# We also plot the marginal posterior histograms for beta over each time period for a given area:
par(mfrow=c(2,2))
plothists <- function(abc.out,inpt.data){
  
  dates <- inpt.data[which(c(FALSE, tail(inpt.data$tier,-1) != head(inpt.data$tier,-1))),1]$date
  dates <- dates[1:(length(dates)-1)]
  dates <- append(dates, as.Date('2021-02-06'))
  
  for(i in 1:(length(dates)-1)){
    tier <- inpt.data[inpt.data$date==as.Date(dates[i]),6]
    dates.title <- paste(format.Date(dates[i],'%d/%m/%Y'),format.Date(dates[i+1],'%d/%m/%Y'),sep=' - ')
    tier.title <- ifelse(tier=='National Lockdown','National Lockdown',paste('(Tier ',tier,')',sep=''))
    hist(abc.out[[i]]$beta, main=paste(dates.title,tier.title,sep=' '),xlab='Beta')
  }
}

plothists(ne.out,nedata)
