## Hinweise zur Aufgabe reg-snowdens.tex

#source("ftp://stat.ethz.ch/NDK/Source-WBL-R/R/regr.R")
library(regr)
options(digits=3)
## ----------------------------------------------------------------
## --- read data and combine the two data sets
c.env$project <- "Snow density in Davos"
c.env$step <- "Data"
t.d1 <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/snowdens1.dat",header=T, sep=";")
t.d2 <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/snowdens2.dat",header=T, sep=",")
str(t.d1)
  names(t.d1) <- c("Date","AirTemp","RelHumid","SurfTemp","WindVeloc","Density","Elev")
  str(t.d1)
str(t.d2)
  names(t.d2) <- c("Date","Density","AirTemp","SurfTemp","RelHumid","WindVeloc")
  str(t.d2) 

##############################

table(t.d1$Elev)
## data set 2 comes from lower elevation.
## dirty trick: invent small difference in elevation, then combine
t.d2$Elev <- 1550
t.d <- rbind(t.d1,t.d2) ## identifies column names automatically!!
## elevation as factor with 3 levels
t.d$Loc <- factor(t.d$Elev)
levels(t.d$Loc) <- c("D2","D1","WJ")
u.showd(t.d) ## shows the first three and a choice of the remaining observations
  table(t.d$Loc)
  dim(t.d2)
## ----------------------------------------------------------------
## Date
t.d$Datestring <- t.d$Date
t.dt <- as.Date(as.character(t.d$Date),format="%d.%m.%Y %H:%M")
  plot(as.numeric(t.dt[1:nrow(t.d1)]),type="l") ## counts days from 1.1.1970
  table(t.dt)
  length(table(t.dt))
  table(table(t.dt))
t.d$Date <- t.dt
t.d$Daterank <- rank(t.dt)
  table(t.dt, t.d$Loc)
## save data
d.snowdens <- t.d
save(d.snowdens, file="d.snowdens") ## save in working directory (try: getwd())
## ----------------------------------------------------------------
## first aid, screening
summary(t.d)
hist(t.d$WindVeloc,nclass=20)
## pairs
plot( ~ rank(Date) + AirTemp + SurfTemp + asin(sqrt(RelHumid/100)) +
     log10(WindVeloc+0.5) + log10(Density),
     data=t.d)
stamp()  ## prints date, time, project name and step onto the plot
pairs(t.d[,c(10,2:6)], pch=as.numeric(t.d$Loc), col=as.numeric(t.d$Loc)+1)
plmatrix(t.d[,c(10,2:6)], pch=as.numeric(t.d$Loc), col=as.numeric(t.d$Loc)+1)
plmatrix( ~ rank(Date) + AirTemp + SurfTemp + asin(sqrt(RelHumid/100)) +
     log10(WindVeloc+0.5) + log10(Density),
     data=t.d, pch=as.numeric(t.d$Loc), col=as.numeric(t.d$Loc)+1)

## --------------------------------------------------------------
## Full model with first aid
c.env$step <- "Model"
u.mf(3,2) ## = par(mfrow=c(3,2)) with minor changes
plot( log10(Density) ~
     Daterank + AirTemp + SurfTemp + asin(sqrt(RelHumid/100)) +
     log10(WindVeloc+0.5) + Loc,
     data=t.d, pch=as.numeric(t.d$Loc), col=as.numeric(t.d$Loc))

t.r <- regr( log10(Density) ~
             AirTemp + SurfTemp + asin(sqrt(RelHumid/100)) +
             log10(WindVeloc+0.5) + Loc,
             data=t.d)
plot(t.r)

t.r <- regr( log10(Density) ~
             AirTemp + SurfTemp + asin(sqrt(RelHumid/100)) +
             pmin(3,WindVeloc) + pmax(0,WindVeloc-3) +
             pmax(0,WindVeloc-4) + Loc,
             data=t.d)
summary(t.r)
plot(t.r, smooth.sim=0)

t.d$TempDiff <- t.d$AirTemp-t.d$SurfTemp
t.r <- regr( log10(Density) ~
             AirTemp + TempDiff + asin(sqrt(RelHumid/100)) +
             pmin(3,WindVeloc) + pmax(0,WindVeloc-3) +
             pmax(0,WindVeloc-4) + Loc,
             data=t.d)
plresx(t.r, smooth.sim=0)

t.r <- regr( log10(Density) ~
             pmax(-11,AirTemp) + pmax(0,WindVeloc-3) +
             pmax(0,WindVeloc-4) + Loc,
             data=t.d)
u.mf(2,2)
plresx(t.r, smooth.sim=0, vars=~.+TempDiff)

t.r <- regr( log10(Density) ~
             pmax(-11,AirTemp) + TempDiff + pmax(0,WindVeloc-3) +
             pmax(0,WindVeloc-4) + Loc,
             data=t.d)
plot(t.r, smooth.sim=0)
t.r <- regr( log10(Density) ~
             pmax(-11,AirTemp) + TempDiff + pmax(0,WindVeloc-3) +
             pmax(0,WindVeloc-4) + Loc,
             data=t.d, subset=-129) ## subset=-129: ?
plot(t.r, smooth.sim=0)


## Plots ohne "Ausreisser"
t.re <- (t.r$stres>2) + 2*(t.r$stres< -2)
na.omit(t.d$Date[t.re==1])
na.omit(t.d$Date[t.re==2])
plmatrix(t.d[,c(2:6,10:11)], pch=as.numeric(t.d$Loc))#, col=c(3,4,2)[1+t.re])
## -----------------------------------------
## individual locations
t.d <- d.snowdens[d.snowdens$Loc=="D2",]
t.d$TempDiff <- t.d$AirTemp-t.d$SurfTemp
t.r <- regr( log10(Density) ~
             AirTemp + TempDiff + asin(sqrt(RelHumid/100)) +
             log10(WindVeloc+0.5),
             data=t.d)
plot(t.r)
t.r <- regr( log10(Density) ~
             AirTemp + asin(sqrt(RelHumid/100)) +
             pmin(1,pmax(0,WindVeloc-3)) + pmin(1,pmax(0,WindVeloc-4)),
             data=t.d)
plot(t.r)
t.r <- regr( log10(Density) ~
             AirTemp + pmin(0,AirTemp+12) + SurfTemp +
             asin(sqrt(RelHumid/100)) + pmin(1,pmax(0,WindVeloc-3)),
             data=t.d)
plot(t.r)
## -----------------------------------------------
t.d <- d.snowdens[d.snowdens$Loc=="D1",]
t.d$TempDiff <- t.d$AirTemp-t.d$SurfTemp
t.r <- regr( log10(Density) ~
             AirTemp + TempDiff + asin(sqrt(RelHumid/100)) +
             log10(WindVeloc+0.5),
             data=t.d)
plot(t.r)
t.r <- regr( log10(Density) ~
             AirTemp + pmin(0,AirTemp+1.5) + TempDiff + pmax(90,RelHumid) +
             pmin(1,pmax(0,WindVeloc-2)),
             data=t.d, method="rlm")
plot(t.r)
## -----------------------------------------------
t.d <- d.snowdens[d.snowdens$Loc=="WJ",]
t.d$TempDiff <- t.d$AirTemp-t.d$SurfTemp
t.r <- regr( log10(Density) ~
             AirTemp + SurfTemp + asin(sqrt(RelHumid/100)) +
             log10(WindVeloc+0.5),
             data=t.d)
plot(t.r)
t.r <- regr( log10(Density) ~
             AirTemp + SurfTemp + pmin(0,WindVeloc-2),
             data=t.d)
plot(t.r)

plot(t.r)
t.r <- regr( log10(Density) ~
             AirTemp + pmin(0,AirTemp+1.5) + pmin(0,SurfTemp+1.5) +
             exp(100-RelHumid) + pmin(2,WindVeloc) + pmin(2,pmax(0,WindVeloc-2)),
             data=t.d)
plot(t.r)
step(t.r)
## ---------------------------------------------
t.d <- d.snowdens
t.d$TempDiff <- t.d$AirTemp-t.d$SurfTemp
t.r <- regr( log10(Density) ~
             AirTemp + pmin(0,AirTemp+1.5) + pmin(0,AirTemp+12) + TempDiff + 
             RelHumid + pmax(90,RelHumid) + pmin(0,WindVeloc-3) +
             pmin(1,pmax(0,WindVeloc-3)) + Loc,
             data=t.d)
plot(t.r,smooth.sim=0)
t.rs <- step(t.r,trace=F)
t.rs$anova
t.rs

plot(t.rs,lab=t.d$Loc,cex=1.2)
formula(t.rs)
t.r <- t.rs
## leverage !
t.i <- t.r$h>0.18|is.na(t.r$h)
t.rr <- update(t.r,subset=!t.i)
plot(t.rr,smooth.sim=0)
t.rs <- step(t.rr)
t.rs$anova
plot(t.rs,smooth.sim=0)
t.re <- (t.rs$stres< -2)+2*(t.rs$stres>2)
table(t.re)
pairs(t.d[,2:6], pch=as.numeric(t.d$Loc), col=c(56,2,4)[t.re+1])

## do not log target variable
t.r <- regr( Density ~ AirTemp + TempDiff + asin(sqrt(RelHumid/100))+
             WindVeloc + Loc,
             data=t.d)
plot(t.r,smooth.sim=0)
t.r <- regr( Density ~
             AirTemp + pmin(0,AirTemp+1.5) + pmin(0,AirTemp+12) + TempDiff + 
             RelHumid + pmax(90,RelHumid) + pmin(3,WindVeloc) +
             pmin(1,pmax(0,WindVeloc-3)) + Loc,
             data=t.d)
plot(t.r,smooth.sim=0)

## ------------------------------------------------

## Lasso
r.l <- lasso(log10(Density) ~
             AirTemp + pmin(0,AirTemp+1.5) + pmin(0,AirTemp+12) + TempDiff + 
             RelHumid + pmax(90,RelHumid) + pmin(0,WindVeloc-3) +
             pmin(1,pmax(0,WindVeloc-3)) + Loc,
             data=t.d)

par(mfrow=c(1,1))
plot(r.l)

cv.lasso(r.l)
#oder
plot(r.l,type="criteria", cv=TRUE)
