## clear R
rm(list = ls())


## also available on openedx

## load some libraries
library(tidyverse)
library(ggfortify)
library(simex)

## Now read in the data, using the read_csv() function. We give it the URL of the published version of the google sheet data.
dd_all <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv")

## Must be very careful to get the next line right!!! Really important!!!
names(dd_all) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
               "Verbal_memory_score", "Number_memory_score",
               "Visual_memory_score",
               "Weight_kgs", "Handed", "Nonpref_Reaction_time_ave",  
               "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
               "Pref_Reaction_time_4", "Pref_Reaction_time_5",
               "Pref_Reaction_time_ave", "Random_number")

dd_filtered <- dd_all %>%
  filter(Pref_Reaction_time_ave > 50,
         Pref_Reaction_time_ave < 500)


ggplot(dd_filtered, aes(x=Pref_Reaction_time_ave, y=Nonpref_Reaction_time_ave)) +
  geom_point()

## plot the mean of the preferred hand reaction time on the x-axis in black
## and one of the individual measures of that also on x-axis.
## And plot against nonpreferred-hand reaction time
ggplot(dd_filtered,
       aes(x=Pref_Reaction_time_ave,
           y=Nonpref_Reaction_time_ave)) +
  geom_point(col="blue") +
  #geom_smooth(method="lm",col="blue") +
  geom_point(mapping=aes(x=Pref_Reaction_time_1),colour=2) #+
  #geom_smooth(mapping=aes(x=Pref_Reaction_time_1),method="lm",col=2)
## the slope with more x-error is shallower... the points
## are spread out more in the x direction

## here is the model and estimated slope of the relationship
## between the means
lm_aves <- lm(Nonpref_Reaction_time_ave ~ Pref_Reaction_time_ave, dd_filtered, x=TRUE)
autoplot(lm_aves)
summary(lm_aves)

# Estimate error variance using the 5 repeated measurements of reaction time
temp <- select(dd_filtered,
              Pref_Reaction_time_1,
              Pref_Reaction_time_2,
              Pref_Reaction_time_3,
              Pref_Reaction_time_4,
              Pref_Reaction_time_5)
error_var <- sum((temp - rowMeans(temp))^2) / (nrow(temp)*4) / 5

set.seed(123)
r.simex <- simex(lm_aves,
                 SIMEXvariable="Pref_Reaction_time_ave",
                 measurement.error=sqrt(error_var),
                 lambda=seq(0.1,2,0.1),
                 B=50)
summary(r.simex)

plot(r.simex)


ggplot(dd_filtered,aes(x=Pref_Reaction_time_ave,y=Nonpref_Reaction_time_ave)) +
  geom_point()  +
  xlim(0,700) +
  ylim(0,700) +
  geom_abline( slope=r.lm$coefficients[2], intercept=r.lm$coefficients[1]) +
  geom_abline( slope=r.simex$coefficients[2], intercept=r.simex$coefficients[1],colour="green",size=1.5)









