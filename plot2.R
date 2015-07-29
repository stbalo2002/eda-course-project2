## pls ensure that working directory contains the unzipped file for the assignment
NEI <- readRDS("SummarySCC_PM25.rds")  ## reads the data into R
names(NEI) <- tolower(names(NEI))  ## changes colnames to lower letters
library(dplyr)
## next i select the rows with the desired fips, then group them into years and addup the emissions for each year
NEIbalt <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarize(emissions = sum(emissions))
png("plot2.png", height = 400, bg = "transparent")
## next i plot the data using base plot
with(NEIbalt, plot(year, emissions, type = "o", xlab = "year", ylab = "emissions",
                main = "Trend of PM2.5 emission in Maryland, Baltimore City", tcl = -0.15, col.main = "slategray3",
                col = "slategray", pch = 20, lwd = 1.8, col.axis = "slategray", col.lab = "slategray4"))
## then add annotations (linear regression and legend)
box(col = "slategray")     
abline(lm(NEIbalt$emissions~NEIbalt$year), lty = "dashed", col = "red")
    legend("topright", legend = c("lm", "lineplot"), lty = c("dashed", "solid"), col = c("red", "slategray"), bty = "n", text.col = "slategray")
dev.off()