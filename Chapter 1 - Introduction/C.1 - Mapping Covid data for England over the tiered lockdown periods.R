# Install and load required packages:

# install.packages('shiny')
# install.packages('leaflet')
# install.packages('tidyverse')
# install.packages('rgeos')
# install.packages('rgdal')
# install.packages('maptools')
# install.packages('ggplot2')

library(shiny)
library(leaflet)
library(tidyverse)
library(rgeos)
library(rgdal)
library(maptools)
library(ggplot2)

# Load map boundary data set:
bdys <- read.csv('https://github.com/mollywilshaw/Final-Year-Project-Report-Code/raw/main/Data%20Sets/LAD_DEC_2020_UK_BUC%20%5B1%5D.csv')

# Load combined data set:
data <- read.csv("https://github.com/mollywilshaw/Final-Year-Project-Report-Code/blob/main/Data%20Sets/Combined%20Data.csv?raw=true")

# Filter combined data set between the dates of the tier system:
data$date <- as.Date(data$date)
data <- filter(data,between(date,as.Date("14/10/2020","%d/%m/%Y"),as.Date("6/1/2021","%d/%m/%Y")))

# Subset parts of data set we will use:
casedata <- data[,c(1:3,5,6)] 
casepropdata <- data[,c(1:3,5,7)]
tierdata <- data[,c(1:3,5,9)]

# Create discrete variables for new case numbers and new cases as a proportion of the population:
casedata <- mutate(casedata,'New cases'=cut(casedata$new_cases,breaks=c(0,100,200,300,400,500,Inf),right=FALSE))
casepropdata <- mutate(casepropdata,'New cases (as a proportion of the population)'=cut(casepropdata$new_cases_proportion,breaks=c(0,5,10,20,30,40),right=FALSE))

# Join data sets with map boundary data:
casedatawmap <- arrange(left_join(casedata,bdys,by=c("la_code"="id")))
casepropdatawmap <- arrange(left_join(casepropdata,bdys,by=c("la_code"="id")))
tierdatawmap <- arrange(left_join(tierdata,bdys,by=c("la_code"="id")))

# Create interactive dashboard:
ui <- fluidPage(
  
  titlePanel("Mapping Covid-19 Data for England over the Tiered Lockdown Periods"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("date","Date",
                  min = as.Date("14/10/2020","%d/%m/%Y"),
                  max = as.Date("6/1/2021","%d/%m/%Y"),
                  value = as.Date("14/10/2020","%d/%m/%Y"),
                  step = 1,
                  timeFormat = "%d/%m/%y"),
      selectInput("council","Select a specific area",append(c("All"),unique(data$la_name)),multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tiers",plotOutput("maptiers")),
        tabPanel("Cases",selectInput("scale1","Continuous or discrete scale?",c('Continuous','Discrete')),plotOutput("mapcases")),
        tabPanel("Cases (as a proportion of the population)",selectInput("scale2","Continuous or discrete scale?",c('Continuous','Discrete')),plotOutput("mapcasesprop"))
      )
    )
  )
)

server <- function(input, output) {
  output$maptiers <- renderPlot({
    if(between(input$date,as.Date('14/10/2020','%d/%m/%Y'),as.Date('4/11/2020','%d/%m/%Y'))==TRUE|between(input$date,as.Date('2/12/2020','%d/%m/%Y'),as.Date('17/12/2020','%d/%m/%Y'))==TRUE){
      p <- ggplot(data = filter(tierdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = factor(tier))) + 
        geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Tier",labels=c("Tier 1","Tier 2","Tier 3"),type=c("chartreuse3","goldenrod1","brown1"))
      p <- p+geom_polygon(data=unique(filter(tierdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=factor(tier))) 
    }
    else if(between(input$date,as.Date('5/11/2020','%d/%m/%Y'),as.Date('1/12/2020','%d/%m/%Y'))==TRUE){
      p <- ggplot(data = filter(tierdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = factor(tier))) + 
        geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Tier",labels='National lockdown',type='darkred')
      p <- p+geom_polygon(data=unique(filter(tierdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=factor(tier))) 
    }
    else if(between(input$date,as.Date('18/12/2020','%d/%m/%Y'),as.Date('25/12/2020','%d/%m/%Y'))==TRUE){
      p <- ggplot(data = filter(tierdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = factor(tier))) + 
        geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Tier",labels=c("Tier 1","Tier 2","Tier 3","Tier 4"),type=c("chartreuse3","goldenrod1","brown1","darkred"))
      p <- p+geom_polygon(data=unique(filter(tierdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=factor(tier))) 
    }
    else if(between(input$date,as.Date('26/12/2020','%d/%m/%Y'),as.Date('30/12/2020','%d/%m/%Y'))==TRUE){
      p <- ggplot(data = filter(tierdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = factor(tier))) + 
        geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Tier",labels=c("Tier 2","Tier 3","Tier 4"),type=c("goldenrod1","brown1","darkred"))
      p <- p+geom_polygon(data=unique(filter(tierdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=factor(tier))) 
    }
    else if(between(input$date,as.Date('31/12/2020','%d/%m/%Y'),as.Date('5/1/2021','%d/%m/%Y'))==TRUE){
      p <- ggplot(data = filter(tierdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = factor(tier))) + 
        geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Tier",labels=c("Tier 3","Tier 4"),type=c("brown1","darkred"))
      p <- p+geom_polygon(data=unique(filter(tierdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=factor(tier))) 
    }
    else if(input$date==as.Date('6/1/2021','%d/%m/%Y')){
      p <- ggplot(data = filter(tierdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = factor(tier))) + 
        geom_polygon() + coord_equal() + theme_void() + scale_fill_discrete(name="Tier",labels="Tier 4",type="darkred")
      p <- p+geom_polygon(data=unique(filter(tierdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=factor(tier))) 
    }
    if(!("All"%in%input$council)){p <- p+geom_polygon(data=unique(filter(tierdatawmap,date==input$date,la_name%in%input$council)),aes(x=long,y=lat,group=group),colour='black')}
    p
  })
  
  output$mapcases <- renderPlot({
    if(input$scale1=='Continuous'){
      q <- ggplot(data = filter(casedatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = new_cases)) + 
        geom_polygon() + coord_equal() + theme_void()+scale_fill_gradientn(name='New cases',colours = c("chartreuse3","goldenrod1","darkorange","brown1","darkred"))
      q <- q+geom_polygon(data=unique(filter(casedatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=new_cases))
    }
    if(input$scale1=='Discrete'){
      q <- ggplot(data = filter(casedatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = `New cases`)) + 
        geom_polygon() + coord_equal() + theme_void()+scale_fill_manual(values = c("[0,100)" = "chartreuse3","[100,200)"="goldenrod1","[200,300)"="darkorange","[300,400)"="brown1","[400,500)"="brown3","[500,Inf)"="darkred"))
      q <- q+geom_polygon(data=unique(filter(casedatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=`New cases`))
    }
    if(!("All"%in%input$council)){q <- q+geom_polygon(data=unique(filter(casedatawmap,date==input$date,la_name%in%input$council)),aes(x=long,y=lat,group=group),colour='black')}
    q
  })
  
  output$mapcasesprop <- renderPlot({
    if(input$scale2=='Continuous'){
      s <- ggplot(data = filter(casepropdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = new_cases_proportion)) + 
        geom_polygon() + coord_equal() + theme_void() +scale_fill_gradientn(name='New cases (as a proportion of the population)',colours = c("chartreuse3","goldenrod1","darkorange","brown1","darkred"))
      s <- s+geom_polygon(data=unique(filter(casepropdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=new_cases_proportion))
    }
    if(input$scale2=='Discrete'){
      s <- ggplot(data = filter(casepropdatawmap,date==input$date), aes(x = long, y = lat, group=group, fill = `New cases (as a proportion of the population)`)) + 
        geom_polygon() + coord_equal() + theme_void()+scale_fill_manual(values = c("[0,5)" = "chartreuse3","[5,10)"="goldenrod1","[10,20)"="darkorange","[20,30)"="brown1","[30,40)"="darkred"))
      s <- s+geom_polygon(data=unique(filter(casepropdatawmap,date==input$date,la_name=='High Peak')),aes(x=long,y=lat,group=group,fill=`New cases (as a proportion of the population)`))
      }
    if(!("All"%in%input$council)){s <- s+geom_polygon(data=unique(filter(casepropdatawmap,date==input$date,la_name%in%input$council)),aes(x=long,y=lat,group=group),colour='black')}
    s
  })
}

# Launch interactive dashboard:
shinyApp(ui = ui, server = server)
