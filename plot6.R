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
## first i convert fips to numeric to allow me filter the required fips
## and summarise my sum of emissions according to year
data.sub <- transform(data.sub, fips = as.numeric(fips))
data2 <- data.sub %>% 
            filter(fips == c(24510, 06037)) %>% 
            group_by(fips, year) %>% 
            summarise(emissions = sum(emissions))
## then i change filtered fips to factors to allow for easy plotting
data2 <- transform(data2, fips = factor(fips))
library(ggplot2)
png("plot6.png",height = 400)
## plot the graph using ggplot2 qplot()
qplot(year, emissions, data = data2, col = fips, geom = c("point", "line"), 
      main = "Comparison of emissions from Motor Vehicles between \n Baltimore City(24510) and Los Angeles County(6037)") +
  geom_smooth(method = "lm", se = F, lty = "dashed")
dev.off()