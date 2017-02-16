rm(list=ls())

dd <- read_csv("~/Desktop/parkinsons_updrs_data.csv")

ggplot(dd, aes(x=total_UPDRS)) + geom_density(bw=5)
ggplot(dd, aes(x=total_UPDRS)) + geom_density(bw=2)
ggplot(dd, aes(x=total_UPDRS)) + geom_density(bw=0.5)
ggplot(dd, aes(x=total_UPDRS)) + geom_density(bw=0.05)
