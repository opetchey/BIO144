## clear R
rm(list=ls())

## load required libraries
library(tidyverse)
library(ggfortify)

## read in the data
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/intertidalalgae.csv")

## take a look at it
dd

## check if we have any NAs
na.omit(dd)
## same number of rows as before (64) so no NAs

## how many replicates of each combination
table(dd$height, dd$herbivores)
## 16... and all the same, which means the design is *balanced*

## check the distribution of the response variable
ggplot(dd, aes(x=Area_cm2)) +
  geom_histogram()
## ooooo... nasty!
## and for each treatment combination
ggplot(dd, aes(x=Area_cm2)) +
  geom_histogram() +
  facet_wrap(height ~ herbivores)
## not so many data points.

## square root transform the area_cm2 response variable
dd <- mutate(dd, sqrt_Area = sqrt(Area_cm2))

## look again at the histogram
ggplot(dd, aes(x=sqrt_Area)) + geom_histogram()
## yeah, not much better
ggplot(dd, aes(x=sqrt_Area)) +
  geom_histogram() +
  facet_grid(height ~ herbivores)
## Still crap!!!

## Hopefully ANOVA is robust enough!!!
## Could use a randomisation test, to avoid assumptions.
## Or non-parameteric.

## make a graph that should tell us the answer
ggplot(dd, aes(x=height, y=sqrt_Area, col=herbivores)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.2))

## Degrees of freedom
## four means, so 64 -4

## Calculate the mean and standard deviation of each treatment combination
meanz <- dd %>% group_by(herbivores, height) %>%
  summarise(mean=mean(sqrt_Area),
            sd=sd(sqrt_Area))
meanz

## lets put the mean of each treatment combination onto the graph
ggplot(dd, aes(x=herbivores, y=sqrt_Area, col=height)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.2,
                                           dodge.width = 0.5)) +
  geom_point(data=meanz, mapping=aes(y=mean), size=10, shape=1,
             position = position_dodge(width=0.5))
## definitely looks like there's an interaction

## fit full model (i.e. with interaction)
m1 <- lm(sqrt_Area ~ herbivores * height, dd)
## model diagnostics
autoplot(m1)
## surprisingly good!
anova(m1)
## yes, there is quite strong evidence of an interaction, as we expected.

## here we get the confidence intervals (default is 95% CI)
confint(m1)

## and use the summary table to give us the r-squared
summary(m1)

## at mid heights, there is no effect of herbivores
## herbivores reduce algal cover only at low heights.
## and a nice graph to show this
ggplot(dd, aes(x=height, y=sqrt_Area, col=herbivores)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.2,
                                           dodge.width = 0.5)) +
  geom_point(data=meanz, mapping=aes(y=mean), size=10, shape=1,
             position = position_dodge(width=0.5)) +
  theme_bw() +
  ylab(expression("Algal cover - square root cm"^"2")) +
  xlab("Height on the shoreline")

    

