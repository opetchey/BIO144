politeness_data <- read_csv("~/Desktop/politeness_data.csv")

politeness.model = lmer(frequency ~ gender + (1|subject), data=politeness_data)

summary(politeness.model)
