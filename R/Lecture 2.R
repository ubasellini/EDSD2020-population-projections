## -------------------------------------------- ##
## EDSD 2020: Population projections
## Lecture 2
## Date: 02/06/2020
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages & data
library(tidyverse)
load("dta.swe.1993.Rdata")

## explore the data
head(dta.swe)

## we have a two-sex population (F stands for females,
## M for males) divided in 5y age groups 
dta.swe$AgeGroup

## the population at time t is denoted NFx and NMx; 
## person-years lived are denoted by LFx and LMx
dta.swe$NFx 
dta.swe$LFx

## project population forward by one period (5 years, as the age groups)
## project population forward by one period (5 years, as the age groups)
dta.swe <- as_tibble(dta.swe) %>% 
  mutate(sFx = lead(LFx)/LFx,    
         NFx5 = lag(NFx * sFx))

## NOTES: the lead function takes the value in the next row
## while the lag function takes the value in the previous row
head(dta.swe[,c(1:4,8,9)])

## practical example of lead function
dta.swe$sFx[1]
dta.swe$LFx[2] / dta.swe$LFx[1] 

## practical example of lag function
dta.swe$NFx5[2]
dta.swe$NFx[1] * dta.swe$sFx[1] 

## look at the last age group - WE NEED TO ADJUST THIS
dta.swe$sFx[dta.swe$Age==80]
dta.swe$NFx5[nrow(dta.swe)]

## adjusting the last age group
## adjusting the last age group
dta.swe <- dta.swe %>% 
  mutate(sFx = ifelse(test = Age == 80,
                      yes  = lead(LFx) / (LFx + lead(LFx)),
                      no   = sFx),
         NFx5 = ifelse(test = Age == 85,
                       yes  = (NFx + lag(NFx))*lag(sFx),
                       no   = NFx5))

## NOTE: the ifelse function performs a test on some variables of your dataset
## if the test is satisfied or not (here age == 80), you can specify what to do
tail(dta.swe[,c(1:4,8,9)],n = 3)
  
## let us adjust the first age group 
srb <- 1.05
fact.srb <- 1/(1+srb)
l0 <- 1e5
LF0 <- dta.swe$LFx[dta.swe$Age==0]  

dta.swe <- dta.swe %>% 
  mutate(bFx = fact.srb * LF0 / (2*l0) * (Fx + sFx * lead(Fx)),
         Bx = Fx * 5 * (NFx + NFx5) / 2,
         NFx5 = ifelse(test = Age == 0,
                       yes  = fact.srb * LF0 / (5 * l0) * sum(Bx,na.rm = T),
                       no   = NFx5))

dta.swe$NFx5[1] ## [1] 293573.8

## compare with other formula
sum(dta.swe$bFx * dta.swe$NFx,na.rm=T) ## [1] 293573.8

## we get the same, the formulas work :)

## long data
dta.swe.l <- dta.swe %>%
  select(AgeGroup,NFx,NFx5) %>%    ## select variable of interes
  rename('1993'=NFx,'1998'=NFx5) %>%  ## rename the populations
  pivot_longer(-AgeGroup,names_to = "year",values_to = "population")

## pivot_longer: first argument is what I do not want to pivot
## names_to creates a column containg the names of the previous columns
## value_to creates a new colum containing the actual numbers in the previous columns

## plot a pyramid
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=year)) +
  geom_bar(stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))

## MALES ## MALES ## MALES ##
## MALES ## MALES ## MALES ##

## male-specific factors
fact.srb.M <- srb/(1+srb)  ## sex-ratio at birth ajustment for males
LM0 <- dta.swe$LMx[dta.swe$Age==0]

## project the male population
dta.swe <- dta.swe %>%
  mutate(sMx=lead(LMx)/LMx,
         NMx5=lag(NMx*sMx),
         sMx=ifelse(test = Age==80,
                    yes = lead(LMx)/(LMx + lead(LMx)),
                    no = sMx),
         NMx5=ifelse(test = Age==85,
                     yes = (NMx+lag(NMx))*lag(sMx),
                     no = NMx5),
         bMx=fact.srb.M * LM0 / (2*l0) * (Fx + sFx*lead(Fx)),
         NMx5=ifelse(test = Age==0,
                     yes = sum(bMx*NFx,na.rm = T),
                     no = NMx5))
head(dta.swe[,c(1:3,6,9,13)],n=3)

## create a data.frame for both sexes
dta.swe.both <- data.frame(AgeGroup = rep(dta.swe$AgeGroup,2),
                           Baseline = c(dta.swe$NFx,dta.swe$NMx),
                           Projected = c(dta.swe$NFx5,dta.swe$NMx5),
                           Sex = rep(c("Female","Male"),each=nrow(dta.swe)))
## long data
dta.swe.l <- dta.swe.both %>%
  pivot_longer(-c(AgeGroup,Sex),names_to = "type",values_to = "population")

## first example of both-sex pyramid
## (we will see more examples tomorrow)
## in 1993
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=Sex)) +
  geom_bar(data = subset(dta.swe.l, type == "Baseline" & Sex == "Male"),
           stat = "identity",position = "dodge",color = "black",mapping = aes(y = -population)) +
  geom_bar(data = subset(dta.swe.l, type == "Baseline" & Sex == "Female"),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish population, year 1993") 

## in 1998
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=Sex)) +
  geom_bar(data = subset(dta.swe.l, type == "Projected" & Sex == "Male"),
           stat = "identity",position = "dodge",color = "black",mapping = aes(y = -population)) +
  geom_bar(data = subset(dta.swe.l, type == "Projected" & Sex == "Female"),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish population, year 1998") 

## saving the data for tomorrow's lecture
save.image("EDSD.lecture2.Rdata")


## END