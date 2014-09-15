###############################################################################
#
# Project 2, Plot 1: Exploratory Data Analysis on Coursera
# 
# The objective of this R script is to answer the following question:
# 
# "Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008?"
#
# Requirement(s):
#
# Using the base plotting system, make a plot showing the total PM2.5 emission
# from all sources for each of the years 1999, 2002, 2005, and 2008.
#
# The output of this script is a PNG plot answering the above question and
# satisfying the above requirement(s).
#

setwd("C:/Users/jfelchli/Documents/Coursera/Data Science/Exploratory Data Analysis/Project 2")


# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Sum the PM25 emissions by year
eby <- aggregate(NEI$Emissions, list(year=NEI$year), sum)


# Set up for plotting to PNG graphics device
png("plot1.png")

# Create a vector of colors to share for plot points and legend
num.colors <- length(eby$year)
colors <- rainbow(num.colors)


# Axis limits adjustments, if any (defaults look OK)
xlim.min <- min(eby$year) - 0
xlim.max <- max(eby$year) + 0
ylim.min <- floor(1.0*min(eby$x))
ylim.max <- ceiling(1.0*max(eby$x))

# Use expressions for subscripting in axes and main title
ylab <- expression(PM[2.5] * " Emissions [tons]")
title <- expression(PM[2.5] * " by Year (USA)")

# Symbol is a filled circle
symbol <- 19

# Generate the plot
plot(eby$year, eby$x, pch=symbol, cex=1.5, col=colors, xlim=c(xlim.min, xlim.max), ylim=c(ylim.min, ylim.max), xlab="Year", ylab=ylab, main=title)

# Connect points with lines
lines(eby$year, eby$x, type="l")

# Add a legend
legend("topright", as.character(eby$year), pch=symbol, col=colors, cex=1, pt.cex=1)

# Turn off graphics devicse
dev.off()





