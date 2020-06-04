## -------------------------------------------- ##
## EDSD 2020: Population projections
## Lecture 4
## Date: 04/06/2020
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##

## EXERCISE 1: RC migration schedule
## cleaning the workspace
rm(list=ls(all=TRUE))

## load the migest package
library(migest)

## check the fundamental RC parameters
rc9.fund

## five-year age groups (works well also with one-year)
x <- seq(0,85,5)
mx <- rc9(x, param = rc9.fund)
plot(x, mx, type="o",pch=16)

## assume a total of 100000 net migration counts
I <- 1e5
Ix <- I*mx
sum(Ix)
plot(x, Ix, type="o",pch=16,
     xlab = "Age group",ylab= "Net migrant counts",
     main="RC migration schedule for 100000 net migrants")


## EXERCISE 2: 
## Projections with open population

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(migest)
library(viridis)

## loading the data
load("EDSD.lecture3.Rdata")

## specify age groups and derive the number of net migrants
## using the RC scheduel
x <- dta.swe$Age
I <- 25000
mx <- rc9(x, param = rc9.fund)
Ix <- I*mx

## third function for population projections
## here for a open population, only females
pop.proj.v3 <- function(x,AgeGroup,Nx,sFx,bFx,Ix,n){
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
    N[,i+1] <- L %*% (N[,i] + Ix/2) + Ix/2
  }
  out <- cbind(data.frame(x=x,AgeGroup=AgeGroup),N)
  return(out)
}

## actual projection
n <- 20
my.proj.base <- pop.proj(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                         Nx=NFx,sFx=sFx,bFx=bFx,n=n)
my.proj.migr <- pop.proj.v3(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                            Nx=NFx,sFx=sFx,bFx=bFx,Ix=Ix,n=n)

## long data
dta.swe.l <- my.proj.base %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",
               values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         type = "no migration")
dta.swe.l.mig <- my.proj.migr %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",
               values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         type = "with migration")
## bind the two long dataset together
## with the bind_rows function from tidyverse
dta.swe.all <- dta.swe.l %>% 
  bind_rows(dta.swe.l.mig)

## plotting with pyramid to compare
## closed and open population
ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
  geom_bar(data = subset(dta.swe.all, period == 21),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle(paste("Swedish female population, year",
      subset(dta.swe.l,period==21)$Year)) +
  scale_fill_manual(name = 'Year', 
        values=c("#E69F00", "#56B4E9","#1C7C54"))


## exercise with TIME_VARYING assumptions on TFR

## age-specific fertility rates provided
Fx <- dta.swe$Fx
tfr.start <- 5*sum(Fx)  ## starting TFR
tfr.target <- 0.75
TFR <- seq(tfr.start,tfr.target,length.out = n)

## crate a matrix of fertility rates in different periods
FX <- matrix(Fx,nrow = m, ncol = n)
for (i in 1:n){
  FX[,i] <- FX[,i] * TFR[i] / tfr.start
}
5*colSums(FX)

## plotting fertility rates
matplot(x,FX,t="l",lty=1,col=viridis(n))
lines(x,FX[,1],lwd=2,col=1)
lines(x,FX[,n],lwd=2,col=2)

## new function for projecting the population
## with time-varying fertility rates

## first function for population projection
## here for a closed population, only females
pop.proj.TFR <- function(x,AgeGroup,Nx,sFx,FX,
                         srb=1.05,l0=1e5,
                         L0F,n){
  ## dimension
  m <- length(x)
  ## factors for bFx calculations
  fact.srb <- 1/(1+srb)
  fact.sur <- L0F/(2*l0)
  ## create Leslie matrix
  L <- matrix(0,m,m)
  diag(L[-1,]) <- sFx  
  L[m,m] <- sFx[m-1]  
  ## create a matrix containing my projections
  N <- matrix(NA,nrow=m,ncol = (n+1))
  N[,1] <- Nx
  for (i in 1:n){
    ## update the bFx and Leslie matrix in each projection period
    bFx <- fact.srb*fact.sur*(FX[-m,i]+sFx*FX[-1,i])
    L[1,] <- c(bFx,0)
    N[,i+1] <- L %*% N[,i]
  }
  out <- cbind(data.frame(x=x,AgeGroup=AgeGroup),N)
  return(out)
}

## actual projection
n <- 20
my.proj.TFR <- pop.proj.TFR(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                            Nx=NFx,sFx=sFx,FX=FX,
                            L0F=dta.swe$LFx[1],n=n)

## long data
dta.swe.l <- my.proj.base %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",
               values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         type = "no assumptions")
dta.swe.l.TFR <- my.proj.TFR %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",
               values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         type = "TFR assumption")
## bind the two long dataset together
## with the bind_rows function from tidyverse
dta.swe.all <- dta.swe.l %>% 
  bind_rows(dta.swe.l.TFR)

## SHINY APP for dynamic visualization of your results
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(dta.swe.l$Year), min = min(dta.swe.l$Year), max = max(dta.swe.l$Year)),
  column(12, plotOutput("plot_pyr1"))
)
server <- function(input, output){
  output$plot_pyr1 <- renderPlot({
    ## plotting pyramid
    ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
      geom_bar(data = subset(dta.swe.all, Year == input$year),
               stat = "identity",position = "dodge",color = "black") +
      coord_flip() +
      theme_bw() +
      ggtitle(paste("Swedish female population, year",subset(dta.swe.all, Year == input$year)$Year)) +
      scale_fill_manual(name = 'Projection', values=c("#E69F00", "#56B4E9","#1C7C54")) +
      scale_y_continuous(limits = c(0, 350000), breaks = seq(0, 350000, 100000))
  })
}
shinyApp(ui = ui, server = server)


## EXERCISE on stable population and 
## decomposition of the Leslie matrix

## eigendecomposition of the Leslie matrix
ev.decomp <- eigen(L)

## extract largest eigenvalue
## this will give you the long-term CGR of the stable population
ev.val <- abs(ev.decomp$values)[1]
cgr_leslie <- log(ev.val)

## extract corresponding eigenvectors
ev.vec <- ev.decomp$vectors[,1]
ev.vecF <- ev.vec[1:m]
ev.vecM <- ev.vec[1:m + m]

## rescale them to sum to 1
## this will give you the long-term distribution of the stable population
ev.vecF <- ev.vecF/sum(ev.vecF)
ev.vecM <- ev.vecM/sum(ev.vecM)

## longer time horizon for projections
n <- 50
my.proj <- pop.proj.v2(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,NFx=NFx,sFx=sFx,bFx=bFx,
                       NMx=NMx,sMx=sMx,bMx=bMx,n=n)
## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(x,AgeGroup,sex),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))

## compute total population by sex in each year
tot.pop.by.sex <- dta.swe.l %>%
  group_by(sex,YearF) %>%
  summarise(TotPop=sum(population))

## compute distribution of the population in each age group
dta.swe.l <- dta.swe.l %>%
  left_join(tot.pop.by.sex) %>%
  mutate(Pch=population/TotPop)

## extract distribution in last year
x <- dta.swe$Age
xG <- dta.swe$AgeGroup
yF <- dta.swe.l$Pch[dta.swe.l$Year == max(dta.swe.l$Year) & dta.swe.l$sex == "Females"]
yM <- dta.swe.l$Pch[dta.swe.l$Year == max(dta.swe.l$Year) & dta.swe.l$sex == "Males"]

## compare Male distribution of projection vs Leslie
plot(x,yM,t="n",axes = F,xlab = "age group",ylab = "",
     ylim = range(yM,yF),main="Males",cex.main=1.5)
axis(1)
axis(2);grid();box()
mtext("Pop distribution",side=2,cex=1.5,line=2.4,las=3)
points(x,yM,lwd=2)
points(x,ev.vecM,col=4,pch=4,lwd=2)
legend("bottomleft",c("From projections","From Leslie"),pch=c(1,4),col=c(1,4),
       lwd=2,cex=1.25,inset = 0.01,lty=NA,bg="white")

## compute overall total population
tot.pop <- tot.pop.by.sex %>%
  ungroup() %>%
  group_by(YearF) %>%
  summarise(TotPop = sum(TotPop)) %>%
  pull(TotPop)

## compute growth rate
CGR <- rep(NA,n)
for (i in 1:n){
  CGR[i] <- log(tot.pop[i+1]/tot.pop[i])
}

## plotting crude growth rate
plot(1:n,CGR,col=4,t="l",lwd=2)
abline(h=cgr_leslie,col=2,lty=2,lwd=2)
legend("topright",c("From projections","From Leslie"),pch=NA,col=c(4,2),
       lwd=2,cex=1.25,lty=c(1,2),bg="white")
