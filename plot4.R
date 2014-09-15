###############################################################################
#
# Project 2, Plot 4: Exploratory Data Analysis on Coursera
# 
# The objective of this R script is to answer the following question:
# 
# "Across the United States, how have emissions from coal combustion-related
# sources changed from 1999-2008?"
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

# Sum the PM2.5 emissions by year and fuel
eby <- aggregate(NEI$Emissions, list(year=NEI$year, fuel=NEI$SCC.Level.Three), sum)

# Subset data to keep only coal-related rows
eby <- eby[grep(" Coal", as.character(eby$fuel)), ]

# Set up for plotting to PNG graphics device
png("plot4.png")

# Create a vector of colors to share for plot points and legend
num.colors <- length(eby$year)
colors <- rainbow(num.colors)

# Use expressions for subscripting in axes and main title
ylab <- expression(PM[2.5] * " Emissions [tons] (Note: scales independent)")
title <- expression(PM[2.5] * " Emissions from coal combustion (USA)")

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





