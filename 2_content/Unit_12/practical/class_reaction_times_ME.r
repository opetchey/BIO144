library(tidyverse)
library(ggfortify)
library(simex)

## Now read in the data, using the read_csv() function. We give it the URL of the published version of the google sheet data.
the_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv"
class_RTs <- read_csv(the_URL)

## Must be very careful to get the next line right!!! Really important!!!
names(class_RTs) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reactiontime",  
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reactiontime", "Random_number")

dd <- mutate(class_RTs,Nonpref_Reactiontime=Nonpref_Reactiontime)

Mean_pref_reactiontime <-  select(dd, Pref_Reaction_time_1, Pref_Reaction_time_2, Pref_Reaction_time_3, Pref_Reaction_time_4, Pref_Reaction_time_5)  %>% rowMeans

dd <- mutate(dd, Pref_Reactiontime = Mean_pref_reactiontime)
#dd <- filter(dd,Nonpref_Reactiontime > Pref_Reactiontime-200 & Nonpref_Reactiontime <1000)

ggplot(dd,aes(x=Pref_Reactiontime,y=Nonpref_Reactiontime)) + geom_point()
#ggplot(dd,aes(x=log10(Pref_Reactiontime),y=log10(Nonpref_Reactiontime))) + geom_point()

#dd <- filter(dd,Nonpref_Reactiontime<1000 & Pref_Reactiontime<1000)

#write.table(dd2,file="/home/steffi/Teaching/Bio144/practical/practical11/reaction_times.csv",sep=",",quote=FALSE,row.names=F)

ggplot(dd,aes(x=Pref_Reactiontime,y=Nonpref_Reactiontime)) +
  geom_point() +
  geom_smooth(method="lm",col=1) +
  geom_point(mapping=aes(x=Pref_Reaction_time_1),colour=2) +
  geom_smooth(mapping=aes(x=Pref_Reaction_time_1),method="lm",col=2)#+ xlim(0,700) + ylim(0,700)  



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

