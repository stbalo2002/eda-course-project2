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
nei.sel <- select(NEI, scc, emissions, year, fips)
## next i merge the selected columns together
data <- merge(nei.sel, scc.sel)
## then i find all the observations with motor vehicle source and assign them to nei.sub
data.sub <- grepl("Vehicles", data$ei.sector, ignore.case = T)
data.sub <- subset(data, subset = data.sub)
## group data.sub according to year and addup the emissions for each year
data.grp <- data.sub %>% filter(fips == "24510") %>% group_by(year) %>% summarise(emissions = sum(emissions))
library(ggplot2)
png("plot5.png", height = 400, bg = "transparent")
## plot the data using base plot and add annotations
with(data.grp, plot(year, emissions, type = "o", pch = 20, col = "steelblue3", lwd = 1.5,
                  col.axis = "steelblue", col.lab = "steelblue4", tcl = -0.15, col.main = "steelblue4",
                   main = "Trend of PM2.5 emissions from motor vehicle-related sources"))
      box(col = "steelblue")
      abline(lm(data.grp$emissions ~ data.grp$year), col = "red", lwd = 1.5, lty = "dashed")
      legend("topright", bty = "n", lty = c("solid", "dashed"), col = c("steelblue", "red"), legend = c("plotline", "lm"), text.col = "steelblue")
dev.off()