# clear R
rm(list=ls())

## load some libraries
library(tidyverse)
library(ggfortify)

# load the data
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/presabs_both_IndYears_allvars_final.csv")

## have a look at the relationship between presence/absence and
## one of the variables, e.g. Elevation, for which we expect
## a relationship
ggplot(dd, aes(x=Elevation, y=pres)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))
## yes, there is greater presence at higher elevation

## and do the same for aspect
ggplot(dd, aes(x=Aspect, y=pres)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))
## perhaps a weak relationship with aspect
  
## we can check if this relationship is supported with a binomial model:
m1 <- glm(pres ~ Elevation + Aspect, data=dd, family=binomial)
## the autoplot is not so informative
autoplot(m1)
anova(m1, test = "LRT")
## with a likelihood ratio test, we find very strong evidence
## for both variables

## we can also make of one relationship
## while we hold the other explanatory variable (as we did before in the course)
new <- expand.grid(Elevation = seq(min(dd$Elevation),
                                    max(dd$Elevation),
                                    length = 500),
                   Aspect = mean(dd$Aspect))

new2 <- predict (m1, newdata = new, se.fit = TRUE, type = 'response')
together <- cbind (new, new2)

ggplot (dd, aes (x = Elevation, y = pres)) +
  geom_point (position=position_jitter(height=0.05, width=0), alpha = 0.5) +
  geom_smooth (data = together,
               mapping = aes (x = Elevation, y = fit),
               stat = "identity")
