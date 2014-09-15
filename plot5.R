###############################################################################
#
# Project 2, Plot 5: Exploratory Data Analysis on Coursera
# 
# The objective of this R script is to answer the following question:
# 
# "How have emissions from motor vehicle sources changed from 1999-2008 in 
#  Baltimore City?"
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

# fips (a code) for Baltimore City, Maryland
fips.baltimore.city <- "24510"

# Subset by Baltimore City
NEI <- subset(NEI, fips==fips.baltimore.city & !is.na(Emissions))

# Sum the PM2.5 emissions by year and fuel
eby <- aggregate(NEI$Emissions, list(year=NEI$year, fuel=NEI$SCC.Level.Two), sum)

# Subset data to keep only motor vehicle-related rows
eby <- eby[grep(" Vehicle", as.character(eby$fuel)), ]

# Replace spaces with newlines in fuel names for a neat appearance in the plot
eby$fuel <- as.character(eby$fuel)
eby$fuel <- gsub("[ ]+", "\n", eby$fuel)
eby$fuel <- as.factor(eby$fuel)

# Set up for plotting to PNG graphics device
png("plot5.png")

# Create a vector of colors to share for plot points and legend
num.colors <- length(eby$year)
colors <- rainbow(num.colors)

# Use expressions for subscripting in axes and main title
ylab <- expression(PM[2.5] * " Emissions [tons] (Note: scales independent)")
title <- expression("Baltimore City " * PM[2.5] * " Motor Vehicle Emissions")

# Generate the plot
g <- ggplot(eby, aes(year, x))
g <- g + geom_point(aes(color=fuel))
g <- g + geom_line(aes(color=fuel))
g <- g + facet_grid(fuel ~ ., scales="free_y")
g <- g + xlab("Year")
g <- g + ylab(ylab)
g <- g + labs(title=title)

print(g)

# Turn off graphics devicse
dev.off()





