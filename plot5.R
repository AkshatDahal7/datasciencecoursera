## plot5.R
## 
## 5. How have emissions from motor vehicle sources changed from 1999–2008 in
## Baltimore City?

# Load libraries
library(dplyr)
library(ggplot2)

# init variables about subdirectory, zipfilename and zipfilepath
subdirectory              <- "./data/"
zipFileName               <- "NEI_data.zip"
zipFilePath               <- paste0(subdirectory, zipFileName)
fileURL                   <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

# If subdirectory "data" doesn't exists, then create this.
if (!file.exists("data")) {
  dir.create("data")
}

# If zip file isn't stored in subdirectory data, it will be downloaded.
if (!file.exists(zipFilePath)){
  download.file(fileURL, zipFilePath, method="curl")
}  

# Upzip downloaded zip file
if (file.exists(zipFilePath)) { 
  unzip(zipFilePath, exdir = subdirectory) 
}

SCCFile <- "./data/Source_Classification_Code.rds"
summarySCC_PM25File <- "./data/summarySCC_PM25.rds"

# Read files
NEI <- readRDS(summarySCC_PM25File)
SCC <- readRDS(SCCFile)

# Fetch data of BaltimoreCity and ON-ROAD
BaltimoreCityMV <- subset(NEI, fips == "24510" & type=="ON-ROAD")

# Calculate the sum of emission by year
BaltimoreMVPM25ByYear <- BaltimoreCityMV %>% select (year, Emissions) %>% 
                                             group_by(year) %>% 
                                             summarise_each(funs(sum))

# Plot the result
qplot(year, Emissions, data=BaltimoreMVPM25ByYear, geom="line") +
  ggtitle(expression("Baltimore City" ~ PM[2.5] ~ "Motor Vehicle Emissions by Year")) +
  xlab("Year") + 
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))

# Copy Plot in png-file
dev.copy(device = png, filename = 'plot5.png', width = 500, height = 400)
dev.off ()


