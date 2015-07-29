## pls ensure that working directory contains the unzipped file for the assignment
NEI <- readRDS("SummarySCC_PM25.rds")  ## reads the data into R
names(NEI) <- tolower(names(NEI))  ## changes the colnames to lower letters
library(dplyr)  ## for data cleaning
## next i group the emissions according to year and add them up for each year
NEI2 <- NEI %>%
  group_by(year) %>%
  summarize(total.emissions = sum(emissions))
NEI2$year <- as.character(NEI2$year)  ## base plot won't give the desired plot with years as class factor
png("plot1.png", height = 400, bg = "transparent")
## next i plot the graph
with(NEI2, plot(year, log(total.emissions), type = "o", xlab = "year", pch = 16, col = "steelblue", 
             lwd = 1.5, ylab = "log of total emissions", main = "Trend of PM2.5 emission", tcl = -0.15,
             col.lab = "steelblue4", col.axis = "steelblue", col.main = "steelblue4"))
## then add annotations (a linear regression line and legend)
    box(col = "steelblue")
    abline(lm(log(NEI2$total.emissions)~as.numeric(NEI2$year)), lty = "dashed", col = "red")
    legend("topright", legend = c("lm", "lineplot"), lty = c("dashed", "solid"), col = c("red", "steelblue"), bty = "n", text.col = "steelblue")
dev.off()