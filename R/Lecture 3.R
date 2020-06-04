## -------------------------------------------- ##
## EDSD 2020: Population projections
## Lecture 3
## Date: 03/06/2020
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages & data
library(tidyverse)
library(viridis)
load("EDSD.lecture2.Rdata")

## explore the data
head(dta.swe)

## dimension of the problem
m <- nrow(dta.swe)

## get ingredients of Leslie matrix
NFx <- dta.swe$NFx
bFx <- dta.swe$bFx
sFx <- dta.swe$sFx

## let's remove the NA from bFx
bFx[is.na(bFx)] <- 0

## let's take only the non-NA element from sFx 
sFx <- sFx[!is.na(sFx)]

## create our Leslie matrix
L <- matrix(0,nrow=m,ncol=m)
L[1,] <- bFx   ## assign to the first row of L the bFx vector
diag(L[-1,]) <- sFx  ## asssign the sFx vector to the sub-diagonal of L
L[m,m] <- sFx[m-1]  ## adjust the last element of L

## project the population using matrix multiplication
NFx5.matrix <- c(L %*% NFx)
NFx5.manual <- dta.swe$NFx5
all.equal(NFx5.manual,NFx5.matrix)

## first function for population projection
## here for a closed population, only females
pop.proj <- function(x,AgeGroup,Nx,sFx,bFx,n){
  ## dimension
  m <- length(x)
  ## create Leslie matrix
  L <- matrix(0,m,m)
  L[1,] <- bFx   
  diag(L[-1,]) <- sFx  
  L[m,m] <- sFx[m-1]  
  ## create a matrix containing my projections
  N <- matrix(NA,nrow=m,ncol = (n+1))
  N[,1] <- Nx
  for (i in 1:n){
    N[,i+1] <- L %*% N[,i]
  }
  out <- cbind(data.frame(x=x,AgeGroup=AgeGroup),N)
  return(out)
}

## let's project 20 periods ahead
n <- 20
my.proj <- pop.proj(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                    Nx=NFx,sFx=sFx,bFx=bFx,n=n)
all.equal(NFx,my.proj$`1`)
all.equal(NFx5.matrix,my.proj$`2`)

## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",
               values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))

## plotting
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_fill_manual(name = 'Year', 
                    values=c("#E69F00", "#56B4E9","#1C7C54"))


## create a multipage pdf of your pyramids
## create a multipage pdf of your pyramids
plots <- list()
my.cols <- cividis(n+1)
my.years <- unique(dta.swe.l$Year)
i <- 10
for (i in 1:(n+1)){
  gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
    geom_bar(data = subset(dta.swe.l, period == i),
             stat = "identity",color = "black") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(paste("Swedish female population, year",my.years[i])) +
    scale_fill_manual(values=my.cols[i])
  plots[[i]] <- gg
}
## saving plots in a single file
pdf("myAnimFig.pdf")
invisible(lapply(plots, print))
dev.off()


## create .gif using gganimate
library(gganimate)
gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option="cividis")
gg + transition_states(YearF) +
  ggtitle('Swedish female population, year {closest_state}')

anim_save("myFirstGif.gif")

## create .gif from multi-page .pdf
## convert the previously created .pdf into a .gif using ImageMagick.
## this is done internally by the terminal
## the "-delay" sets the time between the frames, i.e. the speed of the animation.
system("convert -delay 40 myAnimFig.pdf mySecondGif.gif")


## SHINY APP ## SHINY APP
## SHINY APP ## SHINY APP
library(shiny)

## general parameters
n1 <- max(dta.swe.l$period)
my.cols <- cividis(n1)
my.years <- unique(dta.swe.l$Year)

## build your user interfact
ui <- fluidPage(
  ## title of your shiny
  titlePanel('My first shiny app'),
  ## display a slider that returns input$year to pass to the server function
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(my.years), min = min(my.years), max = max(my.years)),
  ## display a plot returned from the server
  plotOutput("plot_pyr1")
)

## build your server
server <- function(input, output){
  ## create an output that renders a plot
  output$plot_pyr1 <- renderPlot({   ## name of output object needs to be the same as defined in the UI
    ## any ggplot or plot,
    ## here subsetting the year of the given input$year
    ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
      geom_bar(data = subset(dta.swe.l, Year == input$year),
               stat = "identity",color = "black") +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste("Swedish female population, year",input$year)) +
      scale_fill_manual(values=my.cols[which(input$year==my.years)])
  })
}

## run the shiny app, which puts together the ui and server
shinyApp(ui = ui, server = server)


## SECOND SHINY ## SECOND SHINY 
## with INTERACTIVITY
library(plotly)

## build your user interfact
ui <- fluidPage(
  ## title of your shiny
  titlePanel('My first interactive shiny app'),
  ## display a slider that returns input$year to pass to the server function
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(my.years), min = min(my.years), max = max(my.years)),
  ## display a plot returned from the server
  plotlyOutput("plot_pyr1")
)

## build your server
server <- function(input, output){
  ## create an output that renders a plot
  output$plot_pyr1 <- renderPlotly({   ## name of output object needs to be the same as defined in the UI
    ## any ggplot or plot,
    ## here subsetting the year of the given input$year
    ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
      geom_bar(data = subset(dta.swe.l, Year == input$year),
               stat = "identity",color = "black") +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste("Swedish female population, year",input$year)) +
      scale_fill_manual(values=my.cols[which(input$year==my.years)])
  })
}

## run the shiny app, which puts together the ui and server
shinyApp(ui = ui, server = server)

## ADDING MALES ## ADDING MALES 
## ADDING MALES ## ADDING MALES 

## ingredients for males
sMx <- dta.swe$sMx
bMx <- dta.swe$bMx 
NMx <- dta.swe$NMx

## similar adjustments to bMx and sMx
bMx[is.na(bMx)] <- 0
sMx <- sMx[!is.na(sMx)]

## female Leslie matrix (derived before)
LF <- L

## male Leslie matrix
BM <- LM <- matrix(0,m,m)
BM[1,] <- bMx
diag(LM[-1,]) <- sMx
LM[m,m] <- sMx[m-1]
## putting male and females together
ZEROS <- diag(0,m)
Lup <- cbind(LF,ZEROS)
Ldown <- cbind(BM,LM)
L <- rbind(Lup,Ldown)
## matrix projection and comparison
Nx <- c(NFx,NMx)
Nx5.matrix <- c(L%*%Nx)
NFx5.manual <- dta.swe$NFx5
NMx5.manual <- dta.swe$NMx5
all.equal(NFx5.manual,Nx5.matrix[1:m])
all.equal(NMx5.manual,Nx5.matrix[1:m+m])

## function to project several periods
## here for a closed population, females and males
pop.proj.v2 <- function(x,AgeGroup,NFx,sFx,bFx,NMx,sMx,bMx,n){
  ## number of age groups
  m <- length(x); m2 <- m*2
  ## female Leslie matrix
  LF <- matrix(0,m,m)
  LF[1,] <- bFx
  diag(LF[-1,]) <- sFx
  LF[m,m] <- sFx[m-1]
  ## male Leslie matrix
  BM <- LM <- matrix(0,m,m)
  BM[1,] <- bMx
  diag(LM[-1,]) <- sMx
  LM[m,m] <- sMx[m-1]
  ## putting them together
  ZEROS <- diag(0,m)
  Lup <- cbind(LF,ZEROS)
  Ldown <- cbind(BM,LM)
  L <- rbind(Lup,Ldown)
  ## create population matrix
  N <- matrix(0,m2,n+1)
  N[,1] <- c(NFx,NMx)
  for (i in 1:n){
    N[,i+1] <- L%*%N[,i]
  }
  out <- cbind(data.frame(x=rep(x,2),AgeGroup=rep(AgeGroup,2),
                          sex=rep(c("Females","Males"),each=m)),N)
  return(out)
}

my.proj <- pop.proj.v2(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,NFx=NFx,sFx=sFx,bFx=bFx,
                       NMx=NMx,sMx=sMx,bMx=bMx,n=20)

## some checks
all.equal(NFx,my.proj$`1`[1:m])
all.equal(NMx,my.proj$`1`[1:m+m])
all.equal(NFx5.matrix,my.proj$`2`[1:m])
all.equal(NMx5.manual,my.proj$`2`[1:m+m])

## long data for plotting
dta.swe.l <- my.proj %>%
  pivot_longer(-c(x,AgeGroup,sex),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))
## plotting
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21) & sex == "Males"),
           stat = "identity",position = "dodge",color = "black",mapping = aes(y = -population)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21) & sex == "Females"),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish population") +
  scale_y_continuous(limits=c(-3.5e5,3.5e5),
                     breaks = seq(-4e5,4e5,1e5),
                     labels = abs(seq(-4e5,4e5,1e5))) +
  scale_fill_brewer(name="Year",palette = 'Blues', direction = -1) +
  geom_text(data = subset(dta.swe.l, period %in% c(1)),
            aes(y = max(population)/1.25, x = 17, label='Females'),size=7) +
  geom_text(data = subset(dta.swe.l, period %in% c(1)),
            aes(y = -max(population)/1.25, x = 17, label='Males'),size=7)

## saving the data for tomorrow's lecture
save.image("EDSD.lecture3.Rdata")

## END