#BIO 144, week 11BC, 01.06.18
#Anna-Lea Ribi

#Class reaction time and random effects

#Workflow:
##1. Label your skript (title, name, date)
##2. Come up with your question and write down the null-hypothesis.
##3. Import your data
##4. Check wether your data has been imported correctly.
##5. Plot the data.
##6. Plot the data for a statistical model.
##7. Check the assumptions of that model par(mfrow), plot(m1).
##8. Look at the summary.
##9. Make a nice graph for communication.
##10. Write down method and result section.


#clear R's mind
rm(list = ls())
#open needed libraries

library(tidyverse)
library(boot)
library(dplyr)
library(ggfortify)
library(lme4)


## read in the data
class_RTs <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv")


#Week 9 Tidying data:
#Problem: For one individual you have: Reaction time for preferred hand you've got 5 measurements!
#Solution: Gather all five measurements for one individual into one column.

## Must be very careful to get the next line right!!! Really important!!!
names(class_RTs) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reactiontime",  
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reactiontime", "Random_number")

#Removing columns
ddnew <- select(class_RTs, -Timestamp,
                -Verbal_memory_score,
                -Number_memory_score,
                -Visual_memory_score,
                -Weight_kgs,
                -Handed)

gathered_dd <- gather(class_RTs,
                      key = 'measurement',
                      value = 'react_time',
                      c('Pref_Reactiontime',  #key stands for the columns that are now put into one, later you can group them and use a legend
                        'Pref_Reaction_time_2',
                        'Pref_Reaction_time_3',
                        'Pref_Reaction_time_4',
                        'Pref_Reaction_time_5')) #value are the values of the columns that are no longer divided into several columns.
#View(gathered_dd)


#####variation within individuals vs. variation among indiciduals:

#within individuals (boxes): There's some variation, 
#among individuals (along the x-axis): There's apart from some outliers not much variation.
## ie. there is as much variation within individuals as among, by the looks of it.

ggplot(gathered_dd, aes(x = ID, y = react_time))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(aes(colour = measurement))+
  theme_bw()

####Reaction time as a function of ID (individuals), gender, measurements


####Table
##y = react_time   (continuous variable, that cannot be negative -> bounded)
##fixed effect : measurement
##random effect: ID
##R-model_formula: react_time~
##R-function:
##effect size: different slopes between gender
##CI of effect size:
##Number of observations: 177
##df : 



####Calculate average of reaction time for each individual : 

pref_average <- gathered_dd %>%
  group_by(ID, Gender) %>%
  ##mutate(mean.rt = mean(react_time)) %>% ## Owen: you wrote this, but it should be done with summarise() not mutate, like this:
  summarise(mean.rt = mean(react_time)) %>%
  arrange(mean.rt)
  

#Using an average for individual (reducing variation)

## Owen: changed to linear models, and fixed them

m1 <- lm(mean.rt ~ Gender, data = pref_average)
autoplot(m1)
anova(m1)
## no evidence of sex difference (in 2020)


# Ignoring multiple observations (ignoring random effects)
m2 <- lm(react_time ~  Gender, data = gathered_dd)
autoplot(m2)
anova(m2)
## very strong evidence of sex difference (in 2020)


# Taking variation into account within individuals:
# Measurment can change according to gender and according to individual
m3 <- lmer(react_time ~ Gender + (1|ID),
           data = gathered_dd, REML = FALSE)
summary(m3)
confint(m3)
## difference between sexes CI overlaps zero, so no evidence of difference
