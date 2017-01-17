# Example taken from Whitlock and Schluter "The analysis of biological data", Ex 17 10. (p.501)
# Example from Lappin and Husak (2005)
# Male lizards use their jaws as weapons during territorial defence. Question: does bite force predict terrotory size in the species?

path <- "~/Teaching/Bio144/data_examples/lizards/"
d.lizard <- read.table(paste(path,"lizard.txt",sep=""),header=T)

plot(d.lizard)

r.lizard <- lm(area ~ force, data=d.lizard)

# schaue summary an
summary(r.lizard)


# Coeffs und p-Wert:
summary(r.lizard)$coef

# Interpretation des force-Koeffizienten und des p-Werts?

# Zeichne Resultat:
plot(d.lizard)
abline(r.lizard,lwd=2)

# R^2?

# residuen:
r.res <- resid(r.lizard)

hist(r.res)
qqnorm(r.res)
qqline(r.res)


# Tukey-Anscombe plot:
r.fit <- fitted(r.lizard)

plot(r.fit,r.res)
abline(h=0,lty=2)
