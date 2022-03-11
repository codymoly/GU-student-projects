###### set working directory
getwd()
setwd("~/Documents/marine_science")

###### load libraries
library(tidyverse)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)

# read data
UIB_data <- read_delim("BIO458_review_details.csv", delim = ",")

# plot locations and years
locations <- ggplot(UIB_data, aes(x=Loc_code, fill=Loc_code)) +
  geom_bar(stat="count", position=position_dodge(), show.legend = FALSE) +
  labs(title = "a) Number of investigations per location",
       x = "Location codes",
       y="Counts") +
  theme(text = element_text(size=14),
        axis.text=element_text(size=18),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

years <- ggplot(UIB_data, aes(x=as.factor(Year), fill=Year)) +
  geom_bar(stat="count", position=position_dodge(), show.legend = FALSE) +
  labs(title = "b) Number of investigations per year",
       x = "Years",
       y="Number of investigations") +
  theme(text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text=element_text(size=18),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

loc_year <- ggarrange(locations, years, ncol=2, nrow=1)
ggarrange(locations, years, ncol=2, nrow=1)
