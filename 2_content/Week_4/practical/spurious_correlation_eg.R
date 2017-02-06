## Spurious correlation

## Here we look at a dataset about marriage and divorce across the USA.
## Each row of the dataset is a state, and there are 50.
## There are 13 variables, but we'll look at only three in this example.
## Marriage rate: the state-wide number of marriages per 1000 people.
## Divorce rate: the state-wide number of divorces per 1000 people.
## Median age at marriage: the state wide median age when people get married.

## Before we start the analysis, what do you expect to see?
## Greater divorce rate when there is a greater marriage rate? I.e. marriage causes divorce?!
## And what relationship between divorce rate and median age when married.
## I.e. does marrying young result in greater risk of divorce?


library(rethinking)
library(tidyverse)
library(ggfortify)
library(cowplot)

dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/datasets/WaffleDivorce_rethinking.csv")

## Rates are in number of individuals per 1000 individuals


## The relationship between marriage rate and divorce rate
## It seems positive. This seems odd!
ggplot(dd, aes(Marriage, Divorce)) + geom_point() +
  ylab("Divorce rate (per 1000)") +
  xlab("Marriage rate (per 1000)")


## Here the relationship between marriage age and divorce rate
## Seems like a negative relationship.
ggplot(dd, aes(MedianAgeMarriage, Divorce)) + geom_point() +
  ylab("Divorce rate (per 1000)") +
  xlab("Median age at marriage")


## After I know and account for the marriage rate, what additional value does median age of marriage give?
only_marriage_rate <- lm(Divorce ~ Marriage, dd)
both <-  lm(Divorce ~ Marriage + MedianAgeMarriage, dd)
summary(only_marriage_rate)$r.squared
summary(both)$r.squared
## After accounting for age, marriage age has lots of value (increase in explanatory power of about 12%)
## We can use an F-test to do a hypothesis test on this
anova(both, only_marriage_rate)


## After I know and account for the median age of marriage rate, what additional value does marriage rate give?
only_marriage_age <- lm(Divorce ~ MedianAgeMarriage, dd)
summary(only_marriage_age)$r.squared
## After accounting for age, marriage rate has next to no value (less than 1% increase in explanatory power)
## We can use an F-test to do a hypothesis test on this
anova(both, only_marriage_age)

## We can also look at the summary table of the model with both explanatory variables.
summary(both)


## Correlation between the explanatory variables?
ggplot(dd, aes(MedianAgeMarriage, Marriage)) + geom_point() +
  ylab("Marriage rate (per 1000)") +
  xlab("Median age at marriage")



## What did we not do?
## Model diagnostics!!!
## I left this like this, as I really did forget to do them.
## We will have to continually guard against this.
autoplot(both)



## Here is the observed versus predicted plot
ggplot(dd, aes(Divorce, predict(both))) + geom_point() + geom_abline(intercept=0, slope=1)




## Get marriage rate residuals and plot against divorce, and same for
## marriage age residuals
Marriage_rate_residuals <- residuals(lm(Marriage ~ MedianAgeMarriage, dd))
p1 <- ggplot(dd, aes(Marriage_rate_residuals, Divorce)) + geom_point()
Marriage_age_residuals <- residuals(lm(MedianAgeMarriage ~ Marriage, dd))
p2 <- ggplot(dd, aes(Marriage_age_residuals, Divorce)) + geom_point()
plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)


## counterfactual plots
new_data <- expand.grid(Marriage=mean(dd$Marriage),
                        MedianAgeMarriage=dd$MedianAgeMarriage)
new_data <- mutate(new_data, predicted=predict(both, newdata = new_data))
p1 <- ggplot(new_data, aes(dd$MedianAgeMarriage, predicted)) + geom_point()
p1


new_data <- expand.grid(Marriage=dd$Marriage,
                          MedianAgeMarriage=mean(dd$MedianAgeMarriage))
new_data <- mutate(new_data, predicted=predict(both, newdata = new_data))
p2 <- ggplot(new_data, aes(dd$Marriage, predicted)) + geom_point()
p2

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)



## corrected response variable plots
new_data <- expand.grid(Marriage=mean(dd$Marriage),
                        MedianAgeMarriage=dd$MedianAgeMarriage)
new_data <- mutate(new_data, predicted=predict(both, newdata = new_data),
                   corrected=dd$Divorce-predicted)
p1 <- ggplot(new_data, aes(dd$Marriage, corrected)) + geom_point()
p1


new_data <- expand.grid(Marriage=dd$Marriage,
                        MedianAgeMarriage=mean(dd$MedianAgeMarriage))
new_data <- mutate(new_data, predicted=predict(both, newdata = new_data),
                   corrected=dd$Divorce-predicted)
p2 <- ggplot(new_data, aes(dd$MedianAgeMarriage, corrected)) + geom_point()
p2

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)

