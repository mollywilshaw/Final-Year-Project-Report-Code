# Install and load required packages:

# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('rgeos')
# install.packages('rgdal')
# install.packages('maptools')
library(ggplot2)
library(tidyverse)
library(rgeos)
library(rgdal)
library(maptools)

# Download the Local Authority names and codes:
data <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/raw/main/Data%20Sets/Combined%20Data.csv')[,c(1,2)]
data <- unique(data)
# Download data on the changes in tiers (during the first system):
changes <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/raw/main/Data%20Sets/1st%20system%20tier%20changes%20%5B2%5D%5B3%5D%5B4%5D.csv')

# Create a list of all the Local Authorities:
la <- changes$la_name

# Create vectors to store the sorted areas:
tier1 <- c() 
tier2 <- c() 
tier3 <- c() 
change12 <- c() 
change13 <- c() 
change23 <- c() 
change123 <- c() 

# Iterate through the list of areas:
for(x in la){ 
  # Filter the tier change data so that only the current area is considered:
  perarea <- filter(changes, changes$la_name==x) 
  # Store in 't' the distinct tiers which the area was in:
  t <- unique(as.numeric(perarea[1,2:8])) 
  
  if(length(t)==1){ # Area was only in one tier
    # Determine which of the 3 tiers the area was in:
    if(t==1){
      tier1 <- append(tier1, x)
    }
    else if(t==2){
      tier2 <- append(tier2, x)
    }
    else if(t==3){
      tier3 <- append(tier3, x)
    } 
  }
  
  if(length(t)==2) { # Area moved between 2 tiers
    # Determine which 2 tiers the area moved between:
    if((t[1]==1 & t[2]==2) | (t[1]==2 & t[2]==1)){
      change12 <- append(change12, x)
    }
    else if((t[1]==1 & t[2]==3) | (t[1]==3 & t[2]==1)){
      change13 <- append(change13, x)
    }
    else if((t[1]==2 & t[2]==3) | (t[1]==3 & t[2]==2)){
      change23 <- append(change23, x)
    }
  }
  
  if(length(t)==3){ # Area moved between all 3 tiers
    change123 <- append(change123, x)
  }
}
# Note that change13 and change123 are empty.

# Create a variable 'status' which classifies areas based on their behaviour over the entire (first) tier system:
data <- data|>
  mutate(status=ifelse(la_name %in% tier1,'tier 1',
                       ifelse(la_name %in% tier2,'tier 2',
                              ifelse(la_name %in% tier3,'tier 3',
                                     ifelse(la_name %in% change12,'changed 1 to 2',
                                            ifelse(la_name %in% change23,'changed 2 to 3',NA))))))

# Load map boundary data:
bdys <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/raw/main/Data%20Sets/LAD_DEC_2020_UK_BUC%20%5B1%5D.csv')

# Join map data and tier change data:
datawmap <- left_join(data, bdys, by=c('la_code' = 'id'))
datawmap <- arrange(datawmap)

# Plot map of England, with fill colours based on variable 'status':
ggplot(datawmap, aes(x=long, y=lat, group=group, fill=factor(status))) + geom_polygon(colour='black') + coord_equal() + theme_void() + scale_fill_discrete(name="Status over first system", breaks=c("tier 1", "tier 2", "tier 3", "changed 1 to 2", "changed 2 to 3"), labels=c("Tier 1", "Tier 2", "Tier 3","Changed between Tier 1 and Tier 2","Changed between Tier 2 and Tier 3"), type=c("chartreuse","darkorange","deepskyblue","yellow","red"))
