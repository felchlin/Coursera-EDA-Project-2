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
library(gridExtra)


setwd("C:/Users/jfelchli/Documents/Coursera/Data Science/Exploratory Data Analysis/Coursera-EDA-Project-2")

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Merge NEI and SCC in to a single data frame
NEI.merged <- merge(NEI, SCC, by="SCC")

# fips (a code) for Baltimore City, Maryland and Los Angeles County, California
fips.baltimore.city <- "24510"
fips.la.county <- "06037"

# Subset by Baltimore City or LA County vehicle data
NEI.selected.metros <- subset(NEI.merged,
                              (fips==fips.baltimore.city | fips==fips.la.county) &
                                !is.na(Emissions)
                       )

# Subset to keep only motor vehicle-related emissions data
NEI.vehicle.selected.metros <- subset(NEI.selected.metros,
                                      grepl(" Vehicle", as.character(SCC.Level.Two))
                                      )

# Sum the PM2.5 emissions by year and fips code.
# "eby" is an acronym for "Emissions By Year".
eby <- with(NEI.vehicle.selected.metros,
            aggregate(Emissions,
                      list(year=year,
                           fips=fips),
                      sum
                      )
            )

# For readability, rename the "x" column in eby to "pm25"
eby$pm25 <- eby$x
eby$x <- NULL

# Replace fips with metro names, a factor variable
name.la.county <- "Los Angeles County"
name.baltimore.city <- "Baltimore City"
eby$Metro <- as.character(eby$fips)
eby$Metro <- gsub(fips.la.county, name.la.county, eby$Metro)
eby$Metro <- gsub(fips.baltimore.city, name.baltimore.city, eby$Metro)
eby$Metro <- as.factor(eby$Metro)
eby$fips <- NULL


# Get the first year data was recorded
first.year <- eby$year[which.min(eby$year)] # works because eby$year are integers

# Function to normalize a metro's PM25 emissions by year
normalize.metro.pm25 <- function(df, metro.name, first.year) {
    # Subset the original data frame by metro.name
    logi <- df$Metro==metro.name
    metro.df <- df[logi,]

    # order the metro subset data frame by year
    metro.df <- metro.df[order(metro.df$year),]
    metro.normalized.df <- metro.df

    # The pm15 emissions for the first year in this metro
    first.year.df <- df[logi & df$year==first.year,]

    # Metro's pm25 values normalized by the pm25 values in the first year
    metro.normalized.df$pm25 <- metro.df$pm25/first.year.df$pm25
    
    # Return the entire normalized data frame
    return(metro.normalized.df)
}

# Baltimore subset normalization
baltimore.normalized.df <- normalize.metro.pm25(eby, name.baltimore.city, first.year)

# LA subset normalization
la.normalized.df <- normalize.metro.pm25(eby, name.la.county, first.year)

# Combine the normalized LA and Baltimore data frames
eby.normalized <- rbind(la.normalized.df, baltimore.normalized.df)

# Turn into a percent
eby.normalized$pm25 <- 100*(eby.normalized$pm25 - 1)

# Title and y-axis label for Relative plot
title.normalized <- "Relative"
ylab.normalized <- paste("Emissions [% change from", first.year, "levels]")

# Title and y-axis label for Absolute plot
title <- "Absolute"
ylab <- "Emissions [tons]"

# Set up for plotting to PNG graphics device
png.width <- 480*2
png.height <- ceiling((8.5/11)*png.width) # proportion of a standard sheet of copy paper
png("plot6.png", width=png.width, height=png.height)

# String to share between plots for x-axis label
xlab.str <- "Year"

# Generate the plot of normalized values
g.normalized <- ggplot(eby.normalized, aes(year, pm25)) +
    geom_point(aes(color=Metro)) +
    geom_line(aes(color=Metro)) +
    theme(legend.position=c(.5, .5)) +
    xlab(xlab.str) +
    ylab(ylab.normalized) +
    labs(title=title.normalized)


# Generate the plot of absolute values
g <- ggplot(eby, aes(year, pm25)) +
    geom_point(aes(color=Metro)) +
    geom_line(aes(color=Metro)) +
    theme(legend.position=c(.5, .5)) +
    xlab(xlab.str) +
    ylab(ylab) +
    labs(title=title)


# Plot Absolute emissions from Baltimore vs. LA in left-hand pane, then plot
# the Relative emissions from Baltimore vs. LA in righ-hand pane.
title.text.expr <- expression(PM[2.5] * " Motor Vehicle Emissions by Metro Area")
grid.arrange(g,
             g.normalized,
             main=textGrob(title.text.expr,
                           gp=gpar(cex=1.8)
                           ),
             ncol = 2
             )


# Turn off graphics devicse
dev.off()
