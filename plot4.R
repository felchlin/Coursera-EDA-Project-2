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
library(gridExtra)

# Load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Limit sources to coal combustion-related sources. Why did I choose the following method
# to filter for coal-combustion? After a review of the SCC data set, SCC.Level.Three with
# the string " Coal" in it seems to cover all Coal sources, and SCC.Level.One with the
# string " Combustion" in seems to cover all coal sources using combustion.
SCC <- SCC[grep(" Coal", as.character(SCC$SCC.Level.Three)), ]
SCC <- SCC[grep(" Combustion", as.character(SCC$SCC.Level.One)), ]

# Merge NEI and SCC in to a single data frame
NEI <- merge(NEI, SCC, by="SCC")

# Sum the PM2.5 emissions by year and fuel
eby <- aggregate(NEI$Emissions, list(year=NEI$year, fuel=NEI$SCC.Level.Three), sum)

# Replace spaces with newlines in fuel names for a neat appearance in the plot
eby$fuel <- as.character(eby$fuel)
eby$fuel <- gsub("[ ]+", "\n", eby$fuel)
eby$fuel <- gsub("/", "/\n", eby$fuel)
eby$fuel <- as.factor(eby$fuel)

# Sum the PM2.5 emissions by year
eby.total <- aggregate(NEI$Emissions, list(year=NEI$year), sum)

# Set up for plotting to PNG graphics device
png.width <- 480*2
png.height <- ceiling((8.5/11)*png.width) # proportion of a standard sheet of copy paper
png("plot4.png", width=png.width, height=png.height)

# X axis label
x.axis.str <- "Year"

# Generate the plot for emissions faceted by coal type
ylab <- "Emissions [tons] (Note: scales independent)"
title <- "By Coal Type"

g <- ggplot(eby, aes(year, x)) +
    geom_point(aes(color=fuel)) +
    geom_line(aes(color=fuel)) +
    facet_grid(fuel ~ ., scales="free_y") +
    theme(strip.text.y = element_text(angle=0),
          legend.position="none") +
    xlab(x.axis.str) +
    ylab(ylab) +
    labs(title=title)

# Generate the plot for total emissions due to coal combistion
ylab.total <- "Emissions [tons]"
title.total <- expression("Total For All Coal Types")

g.total <- ggplot(eby.total, aes(year, x)) +
    geom_point() +
    geom_line() +
    theme(legend.position="none") +
    xlab(x.axis.str) +
    ylab(ylab.total) +
    labs(title=title.total)


# Plot coal combustion by type in the left pane and coal combustion total  in the right
# pane
title.text.expr <- expression(PM[2.5] * " Emissions From Coal Combustion in USA")
grid.arrange(g,
             g.total,
             main=textGrob(title.text.expr,
                           gp=gpar(cex=1.8)
             ),
             ncol = 2
)


# Turn off graphics devicse
dev.off()
