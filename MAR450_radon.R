# MAR450
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

# filter dataset; drop rows with radon </= 0
raw_data_filtered <- raw_data %>% filter(Radon_Bq_m3 > 0)

# remove outliers
## subset data
selected <- raw_data_filtered %>% select(4, 13, 15, 18, 22, 31, 35, 51, 58)
selected_raw <- raw_data %>% select(4, 13, 15, 18, 22, 31, 35, 51, 58)
## write function to remove outliers
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

## apply remove_outliers() on both datasets
no_outliers <- remove_outliers(selected, c('Temp_SBE45', 
                                           'Salinity_SBE45', 
                                           'Oxygen',
                                           'Chlorophyll',
                                           'ch4_MEAN',
                                           'co2_MEAN',
                                           'Radon_Bq_m3',
                                           'Calculated_radon_Bq_m3'))

no_outliers_raw <- remove_outliers(selected_raw, c('Temp_SBE45', 
                                                   'Salinity_SBE45', 
                                                   'Oxygen',
                                                   'Chlorophyll',
                                                   'ch4_MEAN',
                                                   'co2_MEAN',
                                                   'Radon_Bq_m3',
                                                   'Calculated_radon_Bq_m3'))

####################################

# # 0. corrplot
# ## all
# corr_data_filtered <- raw_data_filtered %>% select(13, 15, 18, 22, 31, 35, 51)
# names(corr_data_filtered) <- c("Temp", "Sal", "Oxy", "ChlA", "CH4", "CO2", "Rn")
# C = cor(corr_data_filtered, method = "spearman")
# corrplot <- corrplot.mixed(C, order = "AOE", addCoef.col = 'black', 
#          col = COL2('RdBu'), tl.col = 'black', tl.srt = 45, insig = 'p-value')
# testRes = cor.mtest(C, conf.level = 0.95)
# corrplot(C, p.mat = testRes$p, insig = 'p-value')

####################################

# 1. plot spearman correlation with outliers
## use radon-trimmed data for radon plots and complete data for the rest
all1 <- ggscatter(raw_data_filtered, x = "Calculated_radon_Bq_m3", y = "co2_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "palevioletred1", fill = "gray55"),
                      title = "a) CO2 over radon",
                      xlab = "Radon (Bq/m³)",
                      ylab = "CO2 (ppm)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

all2 <- ggscatter(raw_data_filtered, x = "Calculated_radon_Bq_m3", y = "ch4_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "cornflowerblue", fill = "gray55"),
                      title = "b) CH4 over radon",
                      xlab = "Radon (Bq/m³)",
                      ylab = "CH4 (ppb)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

all3 <- ggscatter(raw_data, x = "co2_MEAN", y = "ch4_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "chartreuse3", fill = "gray55"),
                      title = "c) CH4 over CO2",
                      xlab = "CO2 (ppm)",
                      ylab = "CH4 (ppb)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 425)

all4 <- ggscatter(raw_data, x = "Salinity_SBE45", y = "co2_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "darkgoldenrod1", fill = "gray55"),
                      title = "d) CO2 over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "CO2 (ppm)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

all5 <- ggscatter(raw_data, x = "Salinity_SBE45", y = "ch4_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "indianred1", fill = "gray55"),
                      title = "e) CH4 over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "CH4 (ppb)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

all6 <- ggscatter(raw_data, x = "co2_MEAN", y = "Chlorophyll",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "turquoise2", fill = "gray55"),
                      title = "f) Chlorophyll-a over CO2",
                      xlab = "CO2 (ppm)",
                      ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 425)

all7 <- ggscatter(raw_data, x = "ch4_MEAN", y = "Chlorophyll",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "yellow1", fill = "gray55"),
                      title = "g) Chlorophyll-a over CH4",
                      xlab = "CH4 (ppb)",
                      ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho")

all8 <- ggscatter(raw_data, x = "Salinity_SBE45", y = "Chlorophyll",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      add.params = list(color = "darkorchid1", fill = "gray55"),
                      title = "h) Chlorophyll-a over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

grid.arrange(all1, all2, all3, 
             all4, all5, all6,
             all7, all8,
             nrow = 4)

####################################

# 2. plot spearman correlation WITHOUT outliers
all1_out <- ggscatter(no_outliers, x = "Calculated_radon_Bq_m3", y = "co2_MEAN",
                  cor.method = "spearman",
                  conf.int.level = 0.95,
                  add = "reg.line", conf.int = TRUE,
                  #add.params = list(color = "palevioletred1", fill = "gray55"),
                  title = "a) CO2 over radon",
                  xlab = "Radon (Bq/m³)",
                  ylab = "CO2 (ppm)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 5)

all2_out <- ggscatter(no_outliers, x = "Calculated_radon_Bq_m3", y = "ch4_MEAN",
                  cor.method = "spearman",
                  conf.int.level = 0.95,
                  add = "reg.line", conf.int = TRUE,
                  #add.params = list(color = "cornflowerblue", fill = "gray55"),
                  title = "b) CH4 over radon",
                  xlab = "Radon (Bq/m³)",
                  ylab = "CH4 (ppb)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 5)

all3_out <- ggscatter(no_outliers_raw, x = "co2_MEAN", y = "ch4_MEAN",
                  cor.method = "spearman",
                  conf.int.level = 0.95,
                  add = "reg.line", conf.int = TRUE,
                  #add.params = list(color = "chartreuse3", fill = "gray55"),
                  title = "c) CH4 over CO2",
                  xlab = "CO2 (ppm)",
                  ylab = "CH4 (ppb)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 435)

all4_out <- ggscatter(no_outliers_raw, x = "Salinity_SBE45", y = "co2_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      #add.params = list(color = "darkgoldenrod1", fill = "gray55"),
                      title = "d) CO2 over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "CO2 (ppm)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

all5_out <- ggscatter(no_outliers_raw, x = "Salinity_SBE45", y = "ch4_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      #add.params = list(color = "indianred1", fill = "gray55"),
                      title = "e) CH4 over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "CH4 (ppb)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

all6_out <- ggscatter(no_outliers_raw, x = "co2_MEAN", y = "Chlorophyll",
                  cor.method = "spearman",
                  conf.int.level = 0.95,
                  add = "reg.line", conf.int = TRUE,
                  #add.params = list(color = "turquoise2", fill = "gray55"),
                  title = "f) Chlorophyll-a over CO2",
                  xlab = "CO2 (ppm)",
                  ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 435)

all7_out <- ggscatter(no_outliers_raw, x = "ch4_MEAN", y = "Chlorophyll",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      add = "reg.line", conf.int = TRUE,
                      #add.params = list(color = "yellow1", fill = "gray55"),
                      title = "g) Chlorophyll-a over CH4",
                      xlab = "CH4 (ppb)",
                      ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho")

all8_out <- ggscatter(no_outliers_raw, x = "Salinity_SBE45", y = "Chlorophyll",
                  cor.method = "spearman",
                  conf.int.level = 0.95,
                  add = "reg.line", conf.int = TRUE,
                  #add.params = list(color = "darkorchid1", fill = "gray55"),
                  title = "h) Chlorophyll-a over salinity",
                  xlab = "Salinity (PSU)",
                  ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 15)

grid.arrange(all1_out, all2_out, all3_out, 
             all4_out, all5_out, all6_out,
             all7_out, all8_out,
             nrow = 4)

####################################

# 3. plot correlations grouped by sampling day
## write "Day" as categorial grouping variable
no_outliers$Day <- as.factor(no_outliers$Day)
no_outliers_raw$Day <- as.factor(no_outliers_raw$Day)

all1_day <- ggscatter(no_outliers, x = "Calculated_radon_Bq_m3", y = "co2_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "a) CO2 over radon",
                      xlab = "Radon (Bq/m³)",
                      ylab = "CO2 (ppm)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho", label.x = 5) +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

all2_day <- ggscatter(no_outliers, x = "Calculated_radon_Bq_m3", y = "ch4_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "b) CH4 over radon",
                      xlab = "Radon (Bq/m³)",
                      ylab = "CH4 (ppb)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho", label.x = 5) +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

all3_day <- ggscatter(no_outliers_raw, x = "co2_MEAN", y = "ch4_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "c) CH4 over CO2",
                      xlab = "CO2 (ppm)",
                      ylab = "CH4 (ppb)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho", label.x = 435) +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

all4_day <- ggscatter(no_outliers_raw, x = "Salinity_SBE45", y = "co2_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "d) CO2 over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "CO2 (ppm)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho", label.x = 15) +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

all5_day <- ggscatter(no_outliers_raw, x = "Salinity_SBE45", y = "ch4_MEAN",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "e) CH4 over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "CH4 (ppb)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho", label.x = 15) +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

all6_day <- ggscatter(no_outliers_raw, x = "co2_MEAN", y = "Chlorophyll",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "f) Chlorophyll-a over CO2",
                      xlab = "CO2 (ppm)",
                      ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho", label.x = 435) +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

all7_day <- ggscatter(no_outliers_raw, x = "ch4_MEAN", y = "Chlorophyll",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "g) Chlorophyll-a over CH4",
                      xlab = "CH4 (ppb)",
                      ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho") +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

all8_day <- ggscatter(no_outliers_raw, x = "Salinity_SBE45", y = "Chlorophyll",
                      cor.method = "spearman",
                      conf.int.level = 0.95,
                      color = "Day",
                      add = "reg.line", conf.int = FALSE,
                      title = "h) Chlorophyll-a over salinity",
                      xlab = "Salinity (PSU)",
                      ylab = "Chlorophyll-a (µg/L)") + 
  stat_cor(aes(color = Day), method = "spearman", cor.coef.name = "rho", label.x = 15) +
  scale_color_discrete(name = '', 
                       labels = c("2022/04/25", 
                                  "2022/04/26", 
                                  "2022/04/27", 
                                  "2022/04/28"))

grid.arrange(all1_day, all2_day, all3_day, 
             all4_day, all5_day, all6_day,
             all7_day, all8_day,
             nrow = 4)

####################################

# 4. plot pearson correlation with outliers, but log-transformed
## log-transformation
# selected_log <- selected
# selected_raw_log <- selected_raw
# selected_log[, 2:8] <- log(selected_log[, 2:8])
# selected_raw_log[, 2:7] <- log(selected_raw_log[, 2:7]) # ignore radon data
# 
# # plot log transformed data
# l1 <- ggscatter(selected_log, x = "Radon_Bq_m3", y = "co2_MEAN",
#                     cor.method = "pearson",
#                     conf.int.level = 0.95,
#                     add = "reg.line", conf.int = TRUE,
#                     add.params = list(color = "indianred1", fill = "gray55"),
#                     title = "a) CO2 over radon",
#                     xlab = "log Rn",
#                     ylab = "log CO2",
#                     xlim = c(0,5)) + 
#   stat_cor(method = "pearson", cor.coef.name = "r", label.x = 3)
# 
# l2 <- ggscatter(selected_log, x = "Radon_Bq_m3", y = "ch4_MEAN",
#                     cor.method = "pearson",
#                     conf.int.level = 0.95,
#                     add = "reg.line", conf.int = TRUE,
#                     add.params = list(color = "cornflowerblue", fill = "gray55"),
#                     title = "b) CH4 over radon",
#                     xlab = "log Rn",
#                     ylab = "log CH4") + 
#   stat_cor(method = "pearson", cor.coef.name = "r")
# 
# l3 <- ggscatter(selected_raw_log, x = "co2_MEAN", y = "ch4_MEAN",
#                     cor.method = "pearson",
#                     conf.int.level = 0.95,
#                     add = "reg.line", conf.int = TRUE,
#                     add.params = list(color = "chartreuse3", fill = "gray55"),
#                     title = "c) CH4 over CO2",
#                     xlab = "log CO2",
#                     ylab = "log CH4") + 
#   stat_cor(method = "pearson", cor.coef.name = "r", label.x = 6.05)
# 
# l4 <- ggscatter(selected_raw_log, x = "co2_MEAN", y = "Chlorophyll",
#                     cor.method = "pearson",
#                     conf.int.level = 0.95,
#                     add = "reg.line", conf.int = TRUE,
#                     add.params = list(color = "darkgoldenrod1", fill = "gray55"),
#                     title = "d) Chl-a over CO2",
#                     xlab = "log CO2",
#                     ylab = "log chl-a") + 
#   stat_cor(method = "pearson", cor.coef.name = "r", label.x = 6.05)
# 
# l5 <- ggscatter(selected_raw_log, x = "Temp_SBE45", y = "Chlorophyll",
#                     cor.method = "pearson",
#                     conf.int.level = 0.95,
#                     add = "reg.line", conf.int = TRUE,
#                     add.params = list(color = "darkorchid1", fill = "gray55"),
#                     title = "e) Chl-a over temperature",
#                     xlab = "log temperature",
#                     ylab = "log chl-a") + 
#   stat_cor(method = "pearson", cor.coef.name = "r")
# 
# l6 <- ggscatter(selected_raw_log, x = "Salinity_SBE45", y = "Chlorophyll",
#                     cor.method = "pearson",
#                     conf.int.level = 0.95,
#                     add = "reg.line", conf.int = TRUE,
#                     add.params = list(color = "turquoise2", fill = "gray55"),
#                     title = "f) Chl-a over salinity",
#                     xlab = "log salinity",
#                     ylab = "log chl-a") + 
#   stat_cor(method = "pearson", cor.coef.name = "r")
# 
# # create plot grid´
# grid.arrange(l1, l2, l3, l4, l5, l6,
#              nrow = 3)
