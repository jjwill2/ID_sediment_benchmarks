################################################################################
# calculates % riffle and % riffle/run for each burp site
# last update: May 2023
# Jason Williams, DEQ Lewiston
################################################################################

library(dplyr)
library(reshape2)

# read in data------------------------------------------------------------------

hab <-read.csv("./raw_data/burp_habitat_type.csv", header = TRUE)
str(hab)

# format------------------------------------------------------------------------

# some data cleaning
levels(unique(as.factor(hab$RES_COMMENTS)))

hab$RES_COMMENTS[hab$RES_COMMENTS == "Glide"] <-"glide"
hab$RES_COMMENTS[hab$RES_COMMENTS == "GLIDE"] <-"glide"
hab$RES_COMMENTS[hab$RES_COMMENTS == "Pool"] <-"pool"
hab$RES_COMMENTS[hab$RES_COMMENTS == "POOL"] <-"pool"
hab$RES_COMMENTS[hab$RES_COMMENTS == "Riffle"] <-"riffle"
hab$RES_COMMENTS[hab$RES_COMMENTS == "RIFFLE"] <-"riffle"
hab$RES_COMMENTS[hab$RES_COMMENTS == "Run"] <-"run"
hab$RES_COMMENTS[hab$RES_COMMENTS == "RUN"] <-"run"

hab_formatted <-
  hab %>%
  select(MLOC_ID, RES_COMMENTS, RES_MEASURE) %>%
  filter(RES_COMMENTS != "Result did not meet data quality objectives. RES_COMMENTS") %>%
  dcast(MLOC_ID ~ RES_COMMENTS, value.var = "RES_MEASURE") %>%
  mutate(total = pool + glide + riffle + run) %>%
  mutate(riffle_pct = (riffle / total) * 100, 
         riffle_run_pct = ( (riffle + run) / total) * 100)

write.csv(hab_formatted, "./formatted_data/burp_habitat_formatted.csv")
