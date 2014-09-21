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
library(gridExtra)

setwd("C:/Users/jfelchli/Documents/Coursera/Data Science/Exploratory Data Analysis/Coursera-EDA-Project-2")

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset data to keep only motor vehicle-related rows
SCC <- SCC[grep(" Vehicle", as.character(SCC$SCC.Level.Two)), ]

# Merge NEI and SCC in to a single data frame
NEI <- merge(NEI, SCC, by="SCC")

# fips (a code) for Baltimore City, Maryland
fips.baltimore.city <- "24510"

# Subset by Baltimore City
NEI <- subset(NEI, fips==fips.baltimore.city & !is.na(Emissions))

# Sum the PM2.5 emissions by year and vehicle
eby <- aggregate(NEI$Emissions, list(year=NEI$year, Vehicle=NEI$SCC.Level.Two), sum)

# Sum the PM2.5 emissions by year
eby.total <- aggregate(NEI$Emissions, list(year=NEI$year), sum)

# Replace spaces with newlines in vehicle names for a neat appearance in the plot
eby$Vehicle <- as.character(eby$Vehicle)
eby$Vehicle <- gsub("[ ]+", "\n", eby$Vehicle)
eby$Vehicle <- as.factor(eby$Vehicle)


# Set up for plotting to PNG graphics device
png.width <- 480*2
png.height <- ceiling((8.5/11)*png.width) # proportion of a standard sheet of copy paper
png("plot5.png", width=png.width, height=png.height)

# X axis label
x.axis.str <- "Year"

# Generate the plot broken down by Vehicle by year
ylab <- "Emissions [tons] (Note: scales independent)"
title <- "By Vehicle Type"

g <- ggplot(eby, aes(year, x)) +
    geom_point(aes(color=Vehicle)) +
    geom_line(aes(color=Vehicle)) +
    facet_grid(Vehicle ~ ., scales="free_y") +
    theme(strip.text.y = element_text(angle=0),
          legend.position="none") +
    xlab(x.axis.str) +
    ylab(ylab) +
    labs(title=title)


# Generate the plot of total emissions by year
ylab.total <- "Emissions [tons]"
title.total <- expression("Total For All Vehicle Types")

g.total <- ggplot(eby.total, aes(year, x)) +
    geom_point() +
    geom_line() +
    theme(legend.position="none") +
    xlab(x.axis.str) +
    ylab(ylab.total) +
    labs(title=title.total)


# Plot Baltimore City emissions faceted by Vehicle type in left pane, then plot 
# Baltimore City emissions for all vehicles in right pane
title.text.expr <- expression("Baltimore City " * PM[2.5] * " Motor Vehicle Emissions")
grid.arrange(g,
             g.total,
             main=textGrob(title.text.expr,
                           gp=gpar(cex=1.8)
             ),
             ncol = 2
)


# Turn off graphics devicse
dev.off()
