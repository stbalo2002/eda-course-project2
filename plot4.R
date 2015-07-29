## pls ensure that working directory contains the unzipped file for the assignment
## read the NEI and SCC dataframe into R using readRDS()
NEI <- readRDS("SummarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
## change the colnames for the datasets into lower letters
names(SCC) <- tolower(names(SCC))
names(NEI) <- tolower(names(NEI))
library(dplyr)
## to free memory, i select only the columns i am interested in for the 2 datasets
scc.sel <- select(SCC, scc, ei.sector, data.category)
nei.sel <- select(NEI, scc, emissions, year)
## next i merge the selected columns together
data <- merge(nei.sel, scc.sel)
## then i find all the observations with coal combustion and assign them to nei.sub
data.sub <- grepl("Coal", data$ei.sector, ignore.case = T)
data.sub <- subset(data, subset = data.sub)
## group data.sub according to year and add-up the emissions for each year
data.grp <- data.sub %>% group_by(year) %>% summarise(emissions = sum(emissions))
library(ggplot2)
png("plot4.png", height = 360, bg = "transparent")
## plot the data using ggplot()
ggplot(data.grp, aes(year, log(emissions))) + geom_smooth(method = "lm", se = F, lty = "dashed", col = "red") +
    geom_line(color = "blue", linesize = 2) + geom_point() + geom_segment(aes(x = 2003.5, y = 13.05, xend = 2004, yend = 13.08), arrow = arrow(length = unit(0.3, "cm"))) +
    annotate("text", x = 2003, y = 13.03, label = "linear model") + ggtitle("Trend of PM2.5 emission from coal-related combustion") + ylab("log of emissions") + theme_bw()
dev.off()
