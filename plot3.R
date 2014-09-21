###############################################################################
#
# Project 2, Plot 3: Exploratory Data Analysis on Coursera
# 
# The objective of this R script is to answer the following question:
# 
# "Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999-2008 for Baltimore City? Which have seen increases in
# emissions from 1999-2008?"
#
# Requirement(s):
#
# Use the ggplot2 plotting system to make a plot answer this question.
#
# The output of this script is a PNG plot answering the above question and
# satisfying the above requirement(s).
#

library(ggplot2)

setwd("C:/Users/jfelchli/Documents/Coursera/Data Science/Exploratory Data Analysis/Coursera-EDA-Project-2")

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# fips (a code) for Baltimore City, Maryland
fips.baltimore.city <- "24510"

# Subset by Baltimore City
NEI <- subset(NEI, fips==fips.baltimore.city & !is.na(Emissions))

# Sum the PM2.5 emissions by year and type
eby <- aggregate(NEI$Emissions, list(year=NEI$year, Source=NEI$type), sum)
eby$Source <- as.factor(eby$Source)

# Replace spaces with newlines in fuel names for a neat appearance in the plot
eby$Source <- as.character(eby$Source)
eby$Source <- gsub("[ ]+", "\n", eby$Source)
eby$Source <- as.factor(eby$Source)

# Set up for plotting to PNG graphics device
png("plot3.png")

# Create a vector of colors to share for plot points and legend
num.colors <- length(eby$year)
colors <- rainbow(num.colors)

# Use expressions for subscripting in axes and main title
ylab <- "Emissions [tons] (Note: scales independent)"
title <- expression(PM[2.5] * " Emissions Per Source in Baltimore City, Maryland")

# Generate the plot
g <- ggplot(eby, aes(year, x))
g <- g + geom_point(aes(color=Source))
g <- g + geom_line(aes(color=Source))
g <- g + facet_grid(Source ~ ., scales="free_y")
g <- g + theme(strip.text.y = element_text(angle=0),
               legend.position="none")
g <- g + xlab("Year")
g <- g + ylab(ylab)
g <- g + labs(title=title)
print(g)

# Turn off graphics devicse
dev.off()





