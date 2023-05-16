################################################################################
# BURP Reference Benchmark Calculation
# Jason Williams
# last update: May 2023
################################################################################

library(dplyr)
library(ggplot2)
library(reshape2)

# read in data------------------------------------------------------------------

# Idaho BURP data (all, reference + non-reference)
burp_data <-read.csv("./formatted_data/BURP_data.csv", header = TRUE)
str(burp_data)

# Reference percentiles, % fines------------------------------------------------

# all reference data, wet % fines < 2.5 mm
ref <-
  burp_data %>%
  filter(REFERENCE == "Reference")

ref %>%
  filter(!is.na(all_fines_2.5_mm)) %>%
  group_by(STR_ORDR) %>%
  summarise(n = n_distinct(BURPID),
            percentile_75 = quantile(all_fines_2.5_mm, probs = 0.75, na.rm = TRUE))


# Reference percentiles, FSBI---------------------------------------------------
# 25th percentile because FSBI decreases with increasing % fines

burp_data %>%
  filter(REFERENCE == "Reference") %>%
  filter(!is.na(FSBI)) %>%
  group_by(SITECLASS) %>%
  summarise(n = n_distinct(BURPID),
            percentile_25 = quantile(FSBI, probs = 0.25, na.rm = TRUE))
