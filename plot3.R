## plot3.R
## 
## 3. Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoi
## nt, onroad, nonroad) variable, which of these four sources have seen 
## decreases in emissions from 1999–2008 for Baltimore City? Which have seen 
## increases in emissions from 1999–2008? Use the ggplot2 plotting system to 
## make a plot answer this question.


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

# Fetch data of BaltimoreCity
BaltimoreCity <- subset(NEI, fips == "24510")

# Calculate the sum of emission by Typetfor and year BaltimoreCity
typePMPM25ofBMar <- BaltimoreCity %>% select (year, type, Emissions) %>% 
                                group_by(year, type) %>% 
                                summarise_each(funs(sum))

# Plot the result
qplot(year, Emissions, data = typePMPM25ofBMar, color = type, geom = "line") +
  ggtitle(expression("Baltimore City" ~ PM[2.5] ~ "Emissions by Source Type and Year")) +
  xlab("Year") +
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))

# Copy Plot in png-file
dev.copy(device = png, filename = 'plot3.png', width = 500, height = 400)
dev.off ()



