## Africa is special exercise
## Owen L Petchey
## February 2017

## Inspriation from the excellent Statistical Rethinking, by Richard McElreath.

## Please not that the script is not in exactly the same order as the exercise on openedx.

## Clear R
rm(list=ls())

## load libraries
library(tidyverse)
library(ggfortify)

## Load the data
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/rugged.csv")

## We are interested in whether the relationship between ruggedness of the landscape is related
## to GDP, and if this relationship differs between African and non-African countries.
## I.e. is Africa special in this regard?

## we should check for NAs... lets do it now
skimr::skim(dd)
## the rgdppc_2000 variable has 64 missing values
## the rugged variable has no missing values
## the cont_africa variable has no missing values
## or use the method on openedx: sum(is.na(the_variable_name)

## Check the distribution of the GDP variable, our response variable
ggplot(dd, aes(x=rgdppc_2000)) + geom_histogram()
## We're getting a warning that 64 rows were removed,
## that will be the 64 NAs in the rgdppc_2000 variable

## very skewed distribution, try log10 transformation
dd <- mutate(dd, log10_gdp=log10(rgdppc_2000))
ggplot(dd, aes(x=log10_gdp)) + geom_histogram(bins=13)
## much better

## And check the distribution of the ruggedness variable, rugged
ggplot(dd, aes(x=rugged)) + geom_histogram(bins=15)
## Also very skewed. We don't have to have a normally distributed explanatory
## variable though, so we'll continue without log transformation, but
## keep this feature of the data in mind.

## The Africa / not Africa variable
table(dd$cont_africa)
## ***note that this includes the NAs***

## rather unequal numbers, unsurprisingly 177, and 57
## lets rename the variable levels, as this will make things easier later
## i.e. tables and graphs will have "not Africa" and "Africa" rather than 0 and 1
dd <- mutate(dd, cont_africa1=ifelse(cont_africa==1, "Africa", "not Africa"))
table(dd$cont_africa1)


## Lets deal with the NAs
ddr <- select(dd, rugged, cont_africa1, log10_gdp) %>%
  na.omit()
table(ddr$cont_africa1)
## 8 Africa countries with NA, 56 non-Africa.

## which country has the lowest ruggedness
filter(dd, rugged == min(rugged)) %>%
  pull(country)

## where does Switzerland come, in the league table of most rugged countries?
dplyr::arrange(dd, desc(rugged)) %>%
  mutate(rank = 1:n()) %>%
  filter(country =="Switzerland") %>%
  pull(rank)
## 10th

## Plot the graph that will tell us the answer!...
ggplot(ddr, aes(x=rugged, y=log10_gdp, colour=cont_africa1)) +
  geom_point()
## not totally clear eh!
## try with different facets for africa and not africa
ggplot(ddr, aes(x=rugged, y=log10_gdp)) +
  geom_point() +
  facet_wrap(~cont_africa1)
## From this, I would say there's not much evidence of a relationship
## in either case!

## Well, anyway, lets confirm our eyes are not fooling us.

## Degrees of freedom 170 (not 234) - 4 = 166
m1 <- lm(log10_gdp ~ rugged * cont_africa1, ddr)
autoplot(m1)
## qq plot not great
ggplot(mapping=aes(x=residuals(m1))) + geom_histogram(bins=15)
## bimodal residuals... not good. Something in the data we're not explaining
## perhaps another grouping of countries we're not including
## for teaching purposes only, lets stick with the ANCOVA
summary(m1)
## significant interaction, so Africa is special
## effect size is negative, so lower slope for not Africa,
## and actual slope for not Africa is negative 0.08275 - 0.17085 = -0.088
## whereas that for Africa is 0.083


## Make a nice graph
ggplot(ddr, aes(x=rugged, y=log10_gdp)) +
  geom_point() +
  facet_wrap(~cont_africa1) +
  geom_smooth(method="lm") +
  xlab("Terrain Ruggedness Index") +
  ylab("Log10 Real Gross Domestic Product per capita,\nfrom year 2000")


## There is a danger here that ggplot has done two separate regression
## If it has, the slopes will be the same as for ANCOVA, but the errors
## will not be... to get the ANCOVA errors, we have to...

## use expand.grid and predict
new_data1 <- expand.grid(rugged=seq(min(ddr$rugged), max(ddr$rugged), length=100),
                        cont_africa1=unique(ddr$cont_africa1))
p1 <- predict(m1, newdata = new_data1, interval="confidence")
n1 <- cbind(new_data1, p1)

## and plot the predictions and confidence intervals
ggplot(ddr, aes(x=rugged, y=log10_gdp)) +
  geom_point() +
  facet_wrap(~cont_africa1) +
  geom_smooth(data=n1, aes(y=fit, ymin=lwr, ymax=upr), stat="identity") +
  xlab("Terrain Ruggedness Index") +
  ylab("Log10 Real Gross Domestic Product per capita,\nfrom year2000")

## Wow, these look really similar to geom_smooth("lm") version.
## There is a tiny difference in the size of the error bars,
## one gets bigger, the other gets smaller!

## separate linear regressions...
m2 <- lm(log10_gdp ~ rugged, filter(ddr, cont_africa1=="Africa"))
m3 <- lm(log10_gdp ~ rugged, filter(ddr, cont_africa1=="not Africa"))
coef(m2)
coef(m3)
new_data2 <- expand.grid(rugged=seq(min(ddr$rugged), max(ddr$rugged), length=100))
p2 <- predict(m2, newdata = new_data2, interval="confidence")
n2 <- cbind(new_data2, p2, cont_africa1="Africa")
p3 <- predict(m3, newdata = new_data2, interval="confidence")
n3 <- cbind(new_data2, p3, cont_africa1="not Africa")
n4 <- rbind(n2, n3)
ggplot(ddr, aes(x=rugged, y=log10_gdp)) +
  geom_point() +
  facet_wrap(~cont_africa1) +
  geom_smooth(data=n4, aes(y=fit, ymin=lwr, ymax=upr), stat="identity") +
  xlab("Terrain Ruggedness Index") +
  ylab("Log10 Real Gross Domestic Product per capita,\nfrom year2000")



## What if we did the linear model on the data with the NAs
m2 <- lm(log10_gdp ~ rugged * cont_africa1, dd)
## no warning!!!
summary(m2)
## Correct number of degrees of freedom, so lm has removed the NAs
## Better to do this ourselves, rather than let lm do it with no notification.
