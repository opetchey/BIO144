setwd("~/Teaching/Bio144/data_examples/anova/Blutzucker/")

d.blz <- read.table("blutzucker.dat",header=T)


plot(BLUTZUCK ~ DIAET,d.blz,xaxt="n")
axis(1,1:4)

d.blz$DIAET <- as.factor(d.blz$DIAET)

r.blz <- aov(BLUTZUCK ~ DIAET, data = d.blz)
summary(r.blz)
