# Install and load required packages:

# install.packages('tidyverse')
# install.packages('rgeos')
# install.packages('rgdal')
# install.packages('maptools')
# install.packages('ggplot2')
# install.packages('gridExtra')
# install.packages('cowplot')
library(tidyverse)
library(rgeos)
library(rgdal)
library(maptools)
library(ggplot2)
library(gridExtra)
library(cowplot)

# Load map boundary data:
bdys <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/raw/main/Data%20Sets/LAD_DEC_2020_UK_BUC%20%5B1%5D.csv')

# Load combined data set:
data <- read.csv("https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true")

# Filter relevant variables for 1st tier system:
data.1 <- unique(data[,c(1:3,10)])
data.1$first_system[data.1$first_system=='different within councils'] <- 'not considered'

# Create lists of areas within each category (shown in report in Table A.3)
unique(data.1[data.1$first_system=='tier 1',3])
unique(data.1[data.1$first_system=='tier 2',3])
unique(data.1[data.1$first_system=='tier 3',3])
unique(data.1[data.1$first_system=='changed 1 to 2',3])
unique(data.1[data.1$first_system=='changed 2 to 3',3])
unique(data.1[data.1$first_system=='not considered',3])

# Create a data set specifying the areas we have chosen to consider: 
data.1b <- data.1|>
  mutate(first_system=ifelse(area_name=='Cornwall'|
                           area_name=='Devon'|
                           area_name=='Dorset'|
                           area_name=='Somerset'|
                           area_name=='Gloucestershire'|
                           area_name=='West of England'|
                           area_name=='Wiltshire'|
                           area_name=='County Durham'|
                           area_name=='Tyne and Wear'|
                           area_name=='Tees Valley'|
                           area_name=='Northumberland'|
                           area_name=='Merseyside',first_system,'not considered'))

# Join the two data sets with the map boundary data:
datawmap.1 <- left_join(data.1, bdys, by=c("la_code" ="id"))
datawmap.1 <- arrange(datawmap.1)

datawmap.1b <- left_join(data.1b, bdys, by=c("la_code" ="id"))
datawmap.1b <- arrange(datawmap.1b)

# Plot the maps of all areas and the areas we consider further:
p <- ggplot(data=datawmap.1, aes(x=long, y=lat, group=group, fill=factor(first_system))) + geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Status over first system", breaks=c("tier 1", "tier 2", "tier 3", "changed 1 to 2", "changed 2 to 3", "not considered"), labels=c("Tier 1", "Tier 2", "Tier 3","Changed between Tiers 1 and 2","Changed between Tiers 2 and 3","Not considered"), type=c("chartreuse","darkorange","darkgrey","deepskyblue","yellow","red"))
l <- get_legend(p)
p <- p+theme(legend.position = 'none')

q <- ggplot(data=datawmap.1b, aes(x=long, y=lat, group=group, fill=factor(first_system))) + geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Status over first system", breaks=c("tier 1", "tier 2", "tier 3", "not considered"), labels=c("Tier 1", "Tier 2", "Tier 3","Not considered"),type=c("darkgrey","deepskyblue","yellow","red")) + theme(legend.position = 'none')

grid.arrange(p, l, q, ncol=3)

# Repeat for the second tier system:

data.2 <- unique(data[,c(1:3,11)])
data.2$second_system[data.2$second_system=='different within councils'] <- 'not considered'

# This information is shown in Table A.4 in the report.
unique(data.2[data.2$second_system=='tier 3',3])
unique(data.2[data.2$second_system=='changed between tiers 2 and 3',3])
unique(data.2[data.2$second_system=='changed between tiers 2 and 4',3])
unique(data.2[data.2$second_system=='changed between tiers 3 and 4',3])
unique(data.2[data.2$second_system=='changed between tiers 1, 2 and 3',3])
unique(data.2[data.2$second_system=='changed between tiers 1, 3 and 4',3])
unique(data.2[data.2$second_system=='changed between tiers 2, 3 and 4',3])
unique(data.2[data.2$second_system=='not considered',3])

# We now choose areas to consider further over both tier systems:

# Create a list of areas where councils moved tiers together over both tier systems:
areas <- unique(data[which(data$first_system!='different within councils' & data$second_system!='different within councils'),3])

# Filter the combined data set to only these areas, and select only the relevant columns:
data.potential.areas <- unique(filter(data, area_name%in%areas)[ ,c(2:4,10,11)])|>
  group_by(area_name)|>
  summarise(area_name, population=sum(population), first_system, second_system)
data.potential.areas <- unique(data.potential.areas)

filter(data.potential.areas, area_name=='Cornwall' | area_name=='Herefordshire') # Cornwall has higher population

# Check that whole of North East/Norfolk and Suffolk (i.e. the areas we have combined) did move tiers at the same time:
data.ne <- unique(filter(data,area_name=='County Durham'|area_name=='Tees Valley'|area_name=='Northumberland'|area_name=='Tyne and Wear')[,c(3,5,9)])
data.ne$date <- as.Date(data.ne$date)
data.ne <- unique(filter(data.ne,between(date,as.Date('2/12/2020','%d/%m/%Y'),as.Date('6/2/2021','%d/%m/%Y'))))
data.ne <- pivot_wider(data.ne,names_from = date,values_from = tier)

data.ns <- unique(filter(data,area_name=='Suffolk'|area_name=='Norfolk'))[,c(3,5,9)]
data.ns$date <- as.Date(data.ns$date)
data.ns <- unique(filter(data.ns,between(date,as.Date('14/10/2020','%d/%m/%Y'),as.Date('6/2/2021','%d/%m/%Y'))))
data.ns <- pivot_wider(data.ns,names_from = date,values_from = tier)

# We now create a data set for the areas we have decided to consider further:
data.2b <- data.2|>
  mutate(second_system=ifelse(area_name=='Cornwall'|
                                area_name=='Norfolk'|
                                area_name=='Suffolk'|
                                area_name=='Isle of Wight'|
                                area_name=='County Durham'|
                                area_name=='Tyne and Wear'|
                                area_name=='Tees Valley'|
                                area_name=='Northumberland'|
                                area_name=='Merseyside'|
                                area_name=='Inner London'|
                                area_name=='Outer London'|
                                area_name=='South Yorkshire', second_system ,'not considered'))

datawmap.2 <- left_join(data.2,bdys,by=c("la_code" ="id"))
datawmap.2 <- arrange(datawmap.2)

datawmap.2b <- left_join(data.2b,bdys,by=c("la_code" ="id"))
datawmap.2b <- arrange(datawmap.2b)


p <- ggplot(data=datawmap.2, aes(x = long, y = lat, group=group, fill=factor(second_system))) + geom_polygon() + coord_equal() + theme_void()+scale_fill_discrete(name="Status over second system",breaks=c("tier 3", "changed between tiers 2 and 3", "changed between tiers 2 and 4", "changed between tiers 3 and 4", "changed between tiers 1, 2 and 3","changed between tiers 1, 3 and 4","changed between tiers 2, 3 and 4", "not considered"),labels=c("Tier 3", "Changed between Tiers 2 and 3", "Changed between Tiers 2 and 4", "Changed between Tiers 3 and 4", "Changed between Tiers 1, 2 and 3","Changed between Tiers 1, 3 and 4","Changed between Tiers 2, 3 and 4", "Not considered"),type=c("deepskyblue","purple","darkorange","yellow","pink","chartreuse","darkgrey","red"))
l <- get_legend(p)
p <- p+theme(legend.position = 'none')

q <- ggplot(data=datawmap.2b, aes(x = long, y = lat, group=group, fill=factor(second_system))) + geom_polygon() + coord_equal() + theme_void()+scale_fill_discrete(name="Status over second system",breaks=c("tier 3", "changed between tiers 2 and 3", "changed between tiers 2 and 4", "changed between tiers 3 and 4", "changed between tiers 1, 2 and 3","changed between tiers 1, 3 and 4","changed between tiers 2, 3 and 4", "not considered"),labels=c("Tier 3", "Changed between Tiers 2 and 3", "Changed between Tiers 2 and 4", "Changed between Tiers 3 and 4", "Changed between Tiers 1, 2 and 3","Changed between Tiers 1, 3 and 4","Changed between Tiers 2, 3 and 4", "Not considered"),type=c("deepskyblue","purple","darkorange","yellow","pink","chartreuse","darkgrey","red"))+theme(legend.position = "none")
q
grid.arrange(p,l,q,ncol=3)
