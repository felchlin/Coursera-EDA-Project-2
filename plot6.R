###############################################################################
#
# Project 2, Plot 6: Exploratory Data Analysis on Coursera
# 
# The objective of this R script is to answer the following question:
# 
# "Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California (fips
# == "06037"). Which city has seen greater changes over time in motor vehicle
# emissions?"
#
# Requirement(s):
#
# Use any plotting system in R.
#
# The output of this script is a PNG plot answering the above question and
# satisfying the above requirement(s).
#

library(ggplot2)

setwd("C:/Users/jfelchli/Documents/Coursera/Data Science/Exploratory Data Analysis/Project 2")

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge NEI and SCC in to a single data frame
NEI <- merge(NEI, SCC, by="SCC")

# fips (a code) for Baltimore City, Maryland and Los Angeles County, California
fips.baltimore.city <- "24510"
fips.la.county <- "06037"

# Subset by Baltimore City or LA County
NEI <- subset(NEI, (fips==fips.baltimore.city | fips==fips.la.county) & !is.na(Emissions))

# Sum the PM2.5 emissions by year and fuel
eby <- aggregate(NEI$Emissions, list(year=NEI$year, fuel=NEI$SCC.Level.Two, fips=NEI$fips), sum)

# Subset data to keep only motor vehicle-related rows
eby <- eby[grep(" Vehicle", as.character(eby$fuel)), ]

# Replace fips with metros
name.la.county <- "LA County"
name.baltimore.city <- "Baltimore City"
eby$Metro <- as.character(eby$fips)
eby$Metro <- gsub(fips.la.county, name.la.county, eby$Metro)
eby$Metro <- gsub(fips.baltimore.city, name.baltimore.city, eby$Metro)
eby$Metro <- as.factor(eby$Metro)

# Replace spaces with newlines in fuel names for a neat appearance in the plot
eby$fuel <- as.character(eby$fuel)
eby$fuel <- gsub("[ ]+", "\n", eby$fuel)
eby$fuel <- as.factor(eby$fuel)

# Set up for plotting to PNG graphics device
png("plot6.png")

# Create a vector of colors to share for plot points and legend
num.colors <- length(eby$year)
colors <- rainbow(num.colors)

# Use expressions for subscripting in axes and main title
ylab <- expression(PM[2.5] * " Emissions [tons] (Note: scales independent)")
title <- expression(PM[2.5] * " Motor Vehicle Emissions by Metro")

# Generate the plot
g <- ggplot(eby, aes(year, x))
g <- g + geom_point(aes(color=Metro))
g <- g + geom_line(aes(color=Metro))
g <- g + facet_grid(fuel ~ ., scales="free_y")
g <- g + xlab("Year")
g <- g + ylab(ylab)
g <- g + labs(title=title)

print(g)

# Turn off graphics devicse
dev.off()





