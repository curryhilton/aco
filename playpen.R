y6 <- subset(agg_2016, Yearly.Patient.Spend < 25000)
y7 <- subset(agg_2017, Yearly.Patient.Spend < 25000)

l <- list(y6$Yearly.Patient.Spend, y7$Yearly.Patient.Spend)
names(l) <- c("2016", "2017")

boxplot(l, horizontal = T, col = c("light blue", "grey"))