library(lattice)

setwd("/home/steffi/Teaching/Bio144/data_examples/ancova/cholesterol/")

d.chol <- read.table ("cholesterol.txt",header=T)


r.aov0 <- aov(cholesterol ~ state,data=d.chol)

r.aov <- aov(cholesterol ~ age*state,data=d.chol)
