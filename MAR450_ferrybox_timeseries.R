# MAR450: FerryBox time series
# load required libraries
library(tidyverse) # version 1.3.1
library(gridExtra) # version 2.3
library(ggplot2) # version 3.3.5
library(corrplot) # version 0.92
library(ggpubr) # version 0.4.0

# set working directory
getwd()
setwd("~/Downloads")

# read data
raw_data <- readr::read_delim("ferrybox_licore_rad7_edited.csv", delim = ";")

# plot time series
temp_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Temp_SBE45)) +
  geom_line() +
  xlab("") +
  ylab("Temperature (°C)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sal_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Salinity_SBE45)) +
  geom_line() +
  xlab("") +
  ylab("Salinity (PSU)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

oxy_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Oxygen)) +
  geom_line() +
  xlab("Time (UTC+2)") +
  ylab("Oxygen (mg/l)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

turbo_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Turbidity)) +
  geom_line() +
  xlab("") +
  ylab("Turbidity (NTU)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ph_plot <- ggplot(data=raw_data, aes(x=Datetime, y=pH)) +
  geom_line() +
  xlab("Time (UTC+2)") +
  ylab("pH") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

press_plot <- ggplot(data=raw_data, aes(x=Datetime, y=pressure)) +
  geom_line() +
  xlab("") +
  ylab("Pressure (kPa)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

grid.arrange(temp_plot, sal_plot, turbo_plot, 
             press_plot, oxy_plot, ph_plot,
             nrow=3)