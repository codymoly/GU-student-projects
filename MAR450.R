# MAR450
# load required libraries
library(tidyverse) # version 1.3.1
library(gridExtra) # version 2.3
library(ggplot2) # version 3.3.5
library(corrplot) # version 0.92

# set working directory
getwd()
setwd("~/Downloads")

# read data
raw_data <- readr::read_delim("ferrybox_licore_rad7_edited.csv", delim = ";")

# # make txt for ODV
# ## select relevant columns
# odv_data <- raw_data %>% select(2, 3, 4, 5, 6, 7, 9, 
#                                 10, 13, 14, 15, 16, 17, 18, 19, 
#                                 20, 21, 22, 23, 24, 27, 
#                                 31, 35, 51, 58)
# 
# ## create unit row
# unit_row <- (c("year", "month", 
#                "day", "hour", "minute", 
#                "second", "degrees_north", "degrees_east",
#                "degrees_celsius", "NA", "PSU",
#                "NA", "degrees_celsius", "mg/l",
#                "NA", "degrees_celsius", "NA",
#                "fluorescence", "FTU", "kPa",
#                "fluorescence", "ppm", "ppm",
#                "Bq/m3", "Bq/m3"))
# 
# ## combine unit row and df
# odv_data_selected <- rbind(unit_row, odv_data)
# 
# ## save file as txt
# write.table(odv_data_selected, "odv_data.txt", sep = "\t", dec = ".",
#             row.names = FALSE, col.names = TRUE)

# plot time series of ferrybox (temperature, salinity, oxygen, chlorophyll)
temp_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Temp_SBE45)) +
  geom_line() +
  xlab("Time (UTC+2)") +
  ylab("Temperature (°C)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sal_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Salinity_SBE45)) +
  geom_line() +
  xlab("Time (UTC+2)") +
  ylab("Salinity (PSU)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

oxy_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Oxygen)) +
  geom_line() +
  xlab("Time (UTC+2)") +
  ylab("Oxygen (mg/l)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

chloro_plot <- ggplot(data=raw_data, aes(x=Datetime, y=Chlorophyll)) +
  geom_line() +
  xlab("Time (UTC+2)") +
  ylab("Chlorophyll (µg/l)") +
  facet_grid(cols = vars(as.Date(raw_data$Datetime)), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

grid.arrange(temp_plot, sal_plot, oxy_plot, chloro_plot,
             nrow=2)

# sort data by day
april_25_raw <- raw_data %>%
  filter(Day == 25)
april_26_raw <- raw_data %>%
  filter(Day == 26)
april_27_raw <- raw_data %>%
  filter(Day == 27)
april_28_raw <- raw_data %>%
  filter(Day == 28)

# filter dataset; drop rows with radon </= 0
raw_data_filtered <- raw_data %>% filter(Radon_Bq_m3 > 0)

# sort filterd data by day
april_25 <- raw_data_filtered %>%
  filter(Day == 25)
april_26 <- raw_data_filtered %>%
  filter(Day == 26)
april_27 <- raw_data_filtered %>%
  filter(Day == 27)
april_28 <- raw_data_filtered %>%
  filter(Day == 28)
# 
# # plot radon and carbon dioxide per day and thus per transect
# par(mfrow = c(2,2))
# par(mar=c(5, 5, 3, 5))
# plot(april_25$Datetime, april_25$co2_MEAN,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "deepskyblue2",
#      xlab = "Latitude", ylab = "CO2 (ppm)")
# par(new = TRUE)
# plot(april_25$Datetime, april_25$Radon_Bq_m3,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "orange1",
#      axes = FALSE, xlab = "", ylab = "")
# axis(side=4, at = pretty(range(april_25$Radon_Bq_m3)))
# mtext("Radon (Bq/m^3)", side = 4, line = 3)
# 
# plot(april_26$Datetime, april_26$co2_MEAN,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "deepskyblue2",
#      xlab = "Latitude", ylab = "CO2 (ppm)")
# par(new = TRUE)
# plot(april_26$Datetime, april_26$Radon_Bq_m3,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "orange1",
#      axes = FALSE, xlab = "", ylab = "")
# axis(side=4, at = pretty(range(april_26$Radon_Bq_m3)))
# mtext("Radon (Bq/m^3)", side = 4, line = 3)
# 
# plot(april_27$Datetime, april_27$co2_MEAN,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "deepskyblue2",
#      xlab = "Datetime", ylab = "CO2 (ppm)")
# par(new = TRUE)
# plot(april_27$Datetime, april_27$Radon_Bq_m3,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "orange1",
#      axes = FALSE, xlab = "", ylab = "")
# axis(side=4, at = pretty(range(april_27$Radon_Bq_m3)))
# mtext("Radon (Bq/m^3)", side = 4, line = 3)
# 
# plot(april_28$Datetime, april_28$co2_MEAN,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "deepskyblue2",
#      xlab = "Latitude", ylab = "CO2 (ppm)")
# par(new = TRUE)
# plot(april_28$Datetime, april_28$Radon_Bq_m3,
#      type = "o", pch = 16, bty = "l", lwd = 2, col = "orange1",
#      axes = FALSE, xlab = "", ylab = "")
# axis(side=4, at = pretty(range(april_28$Radon_Bq_m3)))
# mtext("Radon (Bq/m^3)", side = 4, line = 3)
# 
# legend("bottom", c("CO2", "Radon"),
#        col = c("deepskyblue2", "orange1"), lty = c(1, 2))

# plot radon over co2
co2_25 <- ggplot(data = april_25, aes(x = co2_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("CO2 (ppm)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/25 - Offshore")

co2_26 <- ggplot(data = april_26, aes(x = co2_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("CO2 (ppm)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/26 - Skälderviken")

co2_27 <- ggplot(data = april_27, aes(x = co2_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("CO2 (ppm)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/27 - Laholm Bay")

co2_28 <- ggplot(data = april_28, aes(x = co2_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("CO2 (ppm)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/28 - Falkenberg")

grid.arrange(co2_25, co2_26, co2_27, co2_28, nrow = 2)

# plot radon over methane
ch4_25 <- ggplot(data = april_25, aes(x = ch4_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "red") +
  xlab("CH4 (ppb)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/25 - Offshore")

ch4_26 <- ggplot(data = april_26, aes(x = ch4_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "red") +
  xlab("CH4 (ppb)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/26 - Skälderviken")

ch4_27 <- ggplot(data = april_27, aes(x = ch4_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "red") +
  xlab("CH4 (ppb)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/27 - Laholm Bay")

ch4_28 <- ggplot(data = april_28, aes(x = ch4_MEAN, y = Radon_Bq_m3)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "red") +
  xlab("CH4 (ppb)") +
  ylab("Radon (Bq/m³)") +
  ggtitle("2022/04/28 - Falkenberg") 

grid.arrange(ch4_25, ch4_26, ch4_27, ch4_28, nrow = 2)

# chloro and co2
chloro_25 <- ggplot(data = april_25_raw, aes(x = co2_MEAN, y = Chlorophyll)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "chartreuse3") +
  xlab("CO2 (ppm)") +
  ylab("Chlorophyll (µg/l)") +
  ggtitle("2022/04/25 - Offshore")

chloro_26 <- ggplot(data = april_26_raw, aes(x = co2_MEAN, y = Chlorophyll)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "chartreuse3") +
  xlab("CO2 (ppm)") +
  ylab("Chlorophyll (µg/l)") +
  ggtitle("2022/04/26 - Skälderviken")

chloro_27 <- ggplot(data = april_27_raw, aes(x = co2_MEAN, y = Chlorophyll)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "chartreuse3") +
  xlab("CO2 (ppm)") +
  ylab("Chlorophyll (µg/l)") +
  ggtitle("2022/04/27 - Laholm Bay")

chloro_28 <- ggplot(data = april_28_raw, aes(x = co2_MEAN, y = Chlorophyll)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "chartreuse3") +
  xlab("CO2 (ppm)") +
  ylab("Chlorophyll (µg/l)") +
  ggtitle("2022/04/28 - Falkenberg")

grid.arrange(chloro_25, chloro_26, chloro_27, chloro_28, nrow = 2)

# # ferrybox raw_data exploration
# max(raw_data$Temp_SBE45) # 9.929302
# min(raw_data$Temp_SBE45) # 7.981338
# 
# max(raw_data$Salinity_SBE45) # 19.16396
# min(raw_data$Salinity_SBE45) # 9.000374
# 
# max(raw_data$Oxygen) # 10.29782
# min(raw_data$Oxygen) # 9.479888
# 
# max(raw_data$Chlorophyll) # 1.552735
# min(raw_data$Chlorophyll) # 0.277183

# corrplot
## all
corr_data <- raw_data %>% select(13, 15, 18, 22, 31, 35, 51)
corr_data_filtered <- corr_data %>% filter(Radon_Bq_m3 > 0)
names(corr_data_filtered) <- c("Temp", "Sal", "Oxy", "Chl", "CH4", "CO2", "Rad")
C = cor(corr_data_filtered)
corrplot <- corrplot(C, method = "circle", addCoef.col = 'black', 
         col = COL2('PiYG'), tl.col = 'black', tl.srt = 45)

ggplot(data = raw_data_filtered, aes(x = co2_MEAN, y = Chlorophyll)) +
  geom_point() + 
  geom_smooth(method=lm, colour = "turquoise4") +
  xlab("CO2 (ppm)") +
  ylab("Chlorophyll (µg/l)") +
  ggtitle("Chlorophyll-a over CO2")

