## pls ensure that working directory contains the unzipped file for the assignment
NEI <- readRDS("SummarySCC_PM25.rds") ## reads the data into R
names(NEI) <- tolower(names(NEI)) ## changes the colnames to lower letters for easy handling
library(dplyr)
## next i selected the observations with the required fips, group them according 
## to year and addup the emissions for each year
NEIbalt2 <- NEI %>%
  filter(fips == "24510") %>%
  group_by(type, year) %>%
  summarize(emissions = sum(emissions))
## i convert type into factors to allow for easy plotting
NEIbalt2 <- transform(NEIbalt2, type = factor(type))
library(ggplot2)
png("plot3.png")
## next i plot the graph using ggplot()
g <- ggplot(data = NEIbalt2, aes(year, emissions)) + geom_smooth(method = "lm", se = F, lty = "dashed", col = "red") + geom_point() + geom_line(col = "purple") + facet_wrap(~ type, ncol = 2)
g + theme_bw() + labs(title = "Trend of PM2.5 emission from Baltimore city for each of the sources", subtitle = "red line indicates linear regression")
dev.off()
