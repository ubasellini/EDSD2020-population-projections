## -------------------------------------------- ##
## EDSD 2020: Population projections
## Lecture 1
## Date: 01/06/2020
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##

## EXERCISE 1
rm(list=ls(all=TRUE))
N0_a <- 100
N0_b <- 90
r_a <- 0.05
r_b <- 0.10
t <- seq(0,10,0.01)
PopProj <- function(N0,r,t){
  NT <- N0*(1+r)^t
  return(NT)
}
Na <- PopProj(N0=N0_a,r=r_a,t=t)
Nb <- PopProj(N0=N0_b,r=r_b,t=t)
t_hat <- t[which(Nb>Na)][1]

## plotting
plot(t,Na,t="l",lwd=2,ylim=range(Na,Nb),
     xlab="time (years)",ylab="population size")
lines(t,Nb,col=2,lwd=2)
abline(v=t_hat,lty=2)
legend("top",c("pop A","pop B"),col=1:2,lwd=2)

## EXERCISE 2
rm(list=ls(all=TRUE))
N0_a <- 100
N0_b <- 90
b_a <- 0.05
d_a <- 0.07
m_a <- 0.01
b_b <- 0.10
d_b <- 0.05
m_b <- 0.03
t <- seq(0,10,0.01)
PopProj <- function(N0,b,d,m,t){
  r <- b - d + m
  NT <- N0*(1+r)^t
  return(NT)
}
Na <- PopProj(N0=N0_a,b=b_a,d=d_a,m=m_a,t=t)
Nb <- PopProj(N0=N0_b,b=b_b,d=d_b,m=m_b,t=t)
t_hat <- t[which(Nb>Na)][1]

## plotting
plot(t,Na,t="l",lwd=2,ylim=range(Na,Nb),
     xlab="time (years)",ylab="population size")
lines(t,Nb,col=2,lwd=2)
abline(v=t_hat,lty=2)
legend("top",c("pop A","pop B"),col=1:2,lwd=2)


## EXERCISE 3 & 4
rm(list=ls(all=TRUE))
N0 <- 100
t <- 1:21  ## one year more than the projection
b <- 0.03
d <- 0.02
e <- 0.005
I <- 0.75
PopProj <- function(N0,b,d,e,I,t){
  r <- b - d - e
  NT <- rep(NA,t)
  NT[1] <- N0
  for (i in 2:t){
    NT[i] <- NT[i-1]*(1+r) + I
  }
  return(NT)
}
NT <- PopProj(N0,b,d,e,I,max(t))
NT[max(t)]

## making time-specific migration assumptions
e <- c(rep(0.005,11),rep(0.01,10))
I <- c(rep(0.75,11),seq(0.75,0,length.out = 10))
PopProj <- function(N0,b,d,e,I,t){
  NT <- rep(NA,t)
  NT[1] <- N0
  for (i in 2:t){
    r <- b - d - e[i]
    NT[i] <- NT[i-1]*(1+r) + I[i]
  }
  return(NT)
}
NT_scenario <- PopProj(N0,b,d,e,I,max(t))

## plotting
plot(t,NT,t="n",xlab="time (years)",ylab="population size",axes="F")
axis(1,at=c(1,6,11,16,21),labels = c(0,5,10,15,20))
axis(2,cex.axis=1.25);grid();box()
lines(t,NT,lwd=2)
lines(t,NT_scenario,col=2,lwd=2,lty=2)
abline(v=11,lty=2)
legend("topleft",c("no change","migration scenario"),col=1:2,lwd=2,lty=c(1,2))
