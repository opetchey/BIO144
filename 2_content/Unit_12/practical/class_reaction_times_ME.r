## First section is the same as the example solution (2.6.2021)

## clear R
rm(list = ls())


## also available on openedx

## load some libraries
library(tidyverse)
library(ggfortify)
library(simex)

## Now read in the data, using the read_csv() function. We give it the URL of the published version of the google sheet data.
## 2024 link
class_RTs <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT0S581py-IDp4bIQnqFFTttQ1JChFHMMjgkYEbSv88nD1yqV1ocNY1eqrLifEPOHxkCZ4q73XeJcTX/pub?gid=1441390006&single=true&output=csv")

## Must be very careful to get the next line right!!! Really important!!!
names(class_RTs) <- c("Timestamp", "ID", "Gender", "Weight",
                      "Handedness", "Pref_Reaction_time_1",
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3", 
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reaction_time",
                      "Nonpref_Reaction_time_ave",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score", "Random_number")

dd_filtered <- class_RTs %>%
  filter(Pref_Reaction_time > 50,
         Pref_Reaction_time < 500) %>%
  filter(Nonpref_Reaction_time_ave > 50,
         Nonpref_Reaction_time_ave < 500)


ggplot(dd_filtered, aes(x=Pref_Reaction_time, y=Nonpref_Reaction_time_ave)) +
  geom_point()

## plot the mean of the preferred hand reaction time on the x-axis in black
## and one of the individual measures of that also on x-axis.
## And plot against nonpreferred-hand reaction time
ggplot(dd_filtered,
       aes(x=Pref_Reaction_time,
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


# ## get the mean preferred reaction times from the five individual measures
# temp <-  select(dd,
#                                   Pref_Reaction_time_1,
#                                   Pref_Reaction_time_2,
#                                   Pref_Reaction_time_3,
#                                   Pref_Reaction_time_4,
#                                   Pref_Reaction_time_5)  %>%
#   rowMeans
# 
# ## add this variable to the dataset
# dd <- mutate(dd, Pref_Reactiontime = temp)




########
Here is an older version




library(tidyverse)
library(ggfortify)
library(simex)

## Now read in the data, using the read_csv() function. We give it the URL of the published version of the google sheet data.
## 2024 link
the_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT0S581py-IDp4bIQnqFFTttQ1JChFHMMjgkYEbSv88nD1yqV1ocNY1eqrLifEPOHxkCZ4q73XeJcTX/pub?gid=1441390006&single=true&output=csv"
class_RTs_all <- read_csv(the_URL)

## Must be very careful to get the next line right!!! Really important!!!
names(class_RTs_all) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reactiontime",  
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reactiontime", "Random_number")

dd <- class_RTs_all %>%
  filter(Pref_Reaction_time_ave > 50,
         Pref_Reaction_time_ave < 500)

dd <- mutate(class_RTs,Nonpref_Reactiontime=Nonpref_Reactiontime)

Mean_pref_reactiontime <-  select(dd, Pref_Reaction_time_1, Pref_Reaction_time_2, Pref_Reaction_time_3, Pref_Reaction_time_4, Pref_Reaction_time_5)  %>% rowMeans

dd <- mutate(dd, Pref_Reactiontime = Mean_pref_reactiontime)
#dd <- filter(dd,Nonpref_Reactiontime > Pref_Reactiontime-200 & Nonpref_Reactiontime <1000)

ggplot(dd,aes(x=Pref_Reactiontime,y=Nonpref_Reactiontime)) + geom_point()
#ggplot(dd,aes(x=log10(Pref_Reactiontime),y=log10(Nonpref_Reactiontime))) + geom_point()

#dd <- filter(dd,Nonpref_Reactiontime<1000 & Pref_Reactiontime<1000)

#write.table(dd2,file="/home/steffi/Teaching/Bio144/practical/practical11/reaction_times.csv",sep=",",quote=FALSE,row.names=F)

ggplot(dd,aes(x=Pref_Reactiontime,y=Nonpref_Reactiontime)) +
  geom_point(col="blue") +
  #geom_smooth(method="lm",col="blue") +
  geom_point(mapping=aes(x=Pref_Reaction_time_1),colour=2) 
  #geom_smooth(mapping=aes(x=Pref_Reaction_time_1),method="lm",col=2)#+ xlim(0,700) + ylim(0,700)  



r.lm <- lm(Nonpref_Reactiontime ~ Pref_Reactiontime, dd,x=TRUE)
autoplot(r.lm)
summary(r.lm)

# Estimate error variance using the 5 repeated measurements of reaction time
dd2 <- select(dd,Pref_Reaction_time_1,
              Pref_Reaction_time_2,
              Pref_Reaction_time_3,
              Pref_Reaction_time_4,
              Pref_Reaction_time_5)
error_var <- sum((dd2 - rowMeans(dd2))^2) / (nrow(dd2)*4) /5


r.lm <- lm(Nonpref_Reactiontime ~ Pref_Reactiontime, dd,x=TRUE)
autoplot(r.lm)
summary(r.lm)

set.seed(123)
r.simex <- simex(r.lm,SIMEXvariable="Pref_Reactiontime",measurement.error=sqrt(error_var),lambda=seq(0.1,2,0.1),B=50)
summary(r.simex)

plot(r.simex)


ggplot(dd,aes(x=Pref_Reactiontime,y=Nonpref_Reactiontime)) + geom_point()  + xlim(0,700) + ylim(0,700) +
  geom_abline( slope=r.lm$coefficients[2], intercept=r.lm$coefficients[1]) +
  geom_abline( slope=r.simex$coefficients[2], intercept=r.simex$coefficients[1],colour="green",size=1.5)

