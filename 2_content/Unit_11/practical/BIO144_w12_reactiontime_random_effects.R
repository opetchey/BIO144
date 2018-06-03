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



library(readr)
dd <- read_csv("~/Downloads/My reaction time (Responses) - Form responses 1.csv")


#Week 9 Tidying data:
#Problem: For one individual you have: Reaction time for preferred hand you've got 5 measurements!
#Solution: Gather all five measurements for one individual into one column.

names(dd) <- c('Timestamp', 'ID', 'gender', 'pref_1', 'verbal','number', 'visual', 'weight', 'handed', 
               'non_pref', 'pref_2', 'pref_3', 'pref_4', 'pref_5', 'pref_average')

#Removing columns
ddnew <- select(dd, -Timestamp, -verbal, -number, -visual, -weight, -handed
                ) ## Owen: I removed "-pref_average and non_pref" so that its kept in the dataset
#View(ddnew)
gathered_dd <- gather(ddnew, key = 'measurement', value = 'react_time', c('pref_1',  #key stands for the columns that are now put into one, later you can group them and use a legend
                                                                          'pref_2', 'pref_3', 'pref_4', 'pref_5')) #value are the values of the columns that are no longer divided into several columns.
#View(gathered_dd)


## Owen: fix reaction times that are in wrong units:
gathered_dd <- mutate(gathered_dd, react_time=ifelse(react_time<1, react_time*1000, react_time))

#####variation within individuals vs. variation among indiciduals:

#within individuals (boxes):There's some variation, 
#among individuals (along the x-axis): There's apart from some outliers not much variation. Could be neglected.

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
  group_by(ID, gender) %>%
  ##mutate(mean.rt = mean(react_time)) %>% ## Owen: you wrote this, but it should be done with summarise() not mutate, like this:
  summarise(mean.rt = mean(react_time)) %>%
  arrange(mean.rt)
  

#Using an average for individual (reducing variation)

## Owen: changed to linear models, and fixed them

m1 <- lm(mean.rt ~ gender, data = pref_average)
anova(m1)

# Ignoring multiple observations (ignoring random effects)

m2 <- lm(react_time ~  gender, data = gathered_dd)
summary(m2)

#Taking variation into account within individuals: Measurment can change according to gender and according to individual
m3 <- lmer(react_time ~ gender + (1|ID), data = gathered_dd, REML = FALSE)
summary(m3)

# Note sure what is the aim here, so commented out some code.
# 
# ############
# names(dd) <- c('Timestamp', 'ID', 'gender', 'pref_1', 'verbal',
#                'number', 'visual', 'weight' , 'handed', 'non_pref', 'pref_2', 'pref_3',
#                'pref_4', 'pref_5', 'pref_average')
# 
# mean_pref <- select(dd, pref_1, pref_2, pref_3, pref_4, pref_5) %>%
#   rowMeans()
# 
# dd <- mutate(dd, mean_pref=pref_average)
# ddnew <- select(dd, -Timestamp, -verbal, -number, -visual, -weight, -handed)
# View(ddnew)
# gathered_dd <- gather(ddnew, key = 'measurement', value = 'react_time', c('pref_1',  #key stands for the columns that are now put into one, later you can group them and use a legend
#                                                                           'pref_2', 'pref_3', 'pref_4', 'pref_5')) #value are the values of the columns that are no longer divided into several columns.



##non-preferred-hand-reaction time against mean of preferred-hand-reaction time:

#sum(table(gathered_dd$mean_pref))
#855 different means, 1 measurement (e.g. pref_1) should have 177 datapoints

## Owen: variable is pref_average, not mean_pref
ggplot(ddnew, aes( x = pref_average, y = non_pref))+
  geom_point(size = 1) +
  geom_smooth(method="lm") +
  geom_point(aes(x=pref_1), col="red") +
  geom_smooth(mapping=aes(x=pref_1), method="lm", col="red")
  ## and now add the red points


#Fit a naive regression:

m1 <- lm(non_pref ~ pref_1, data = ddnew, x = TRUE)
summary(m1)

##Error-modelling
library(simex)

#Estimating error variance

dd2 <- select(dd, pref_1)

error_var <- sum((dd2-mean(dd2$pref_1))^2)/(177*4)/5  #(level values-level means)^2/ (observation* levels-1)/ levels

#Fit a naive regression:

set.seed(123)

r.simex <- simex(m1, SIMEXvariable = "pref_1", measurement.error = sqrt(error_var),
                 lambda = seq(0.1,2,0.1), B = 50, fitting.method = 'quadratic')

plot(r.simex)
