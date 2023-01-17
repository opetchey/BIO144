library(tidyverse)
library(ggfortify)

## Now read in the data, using the read_csv() function. We give it the URL of the published version of the google sheet data.
the_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv"
class_RTs <- read_csv(the_URL)

## Must be very careful to get the next line right!!! Really important!!!
names(class_RTs) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reactiontime",  "Pref_Reaction_time_2",
                      "Pref_Reaction_time_3",  "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reactiontime")
## now we use some R magic to sort out the data into the long format we need it in
## to be able to use ggplot and do a linear model
class_RTs <- select(class_RTs, ID, Gender, Pref_Reactiontime, Nonpref_Reactiontime)
dd <- tidyr::gather(class_RTs, key=key, value=Reaction_time, 3:4)
dd <- tidyr::separate(dd, col=key, sep="_", into=c("Hand", "Junk"))
dd <- select(dd, -Junk)


ggplot(dd, aes(x=Gender, y=Reaction_time, col=Hand)) +
  geom_point(position = position_dodge(width=0.1))

ggplot(dd, aes(x=Reaction_time, col=Hand)) +
  geom_density() + facet_wrap(~Gender)

m1 <- lm(Reaction_time ~ Gender * Hand, data=dd)
autoplot(m1)

anova(m1)
summary(m1)
