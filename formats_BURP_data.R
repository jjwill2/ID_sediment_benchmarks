################################################################################
# Creates BURP Dataset to use  for defining benchmarks

# Jason Williams
# May2023
################################################################################

library(dplyr)
library(ggplot2)

# read in data------------------------------------------------------------------

# BURP site info
crosswalk <-read.csv("./raw_data/AU_BURP_Crosswalk.csv", header = TRUE)
str(crosswalk)

# BURP Index Scores
burp_indices <-read.csv("./raw_data/qry_burp_indices.csv", header = TRUE,
                        na.strings = "NULL")
str(burp_indices)

# BURP FSBI data
fsbi <-read.csv("./raw_data/qry_FSBI_data.csv", header = TRUE)
str(fsbi)

# BURP % fines and other physical/habitat data
burp_fh <-read.csv("./raw_data/qry_burp_fines_habitat.csv", header = TRUE,
                   na.strings = "NULL")
str(burp_fh)

# habitat type (riffle, run, pool, glide) for sampled BURP reach
hab_type <-read.csv("./formatted_data/burp_habitat_formatted.csv", header = TRUE)
str(hab_type)

# bankfull width
bw <-read.csv("./raw_data/qry_burp_bankfull_width.csv", header = TRUE) 

# format data-------------------------------------------------------------------


fsbi_formmated <-
  fsbi %>%
  select(MLOC_ID, ACTMET_VALUE) %>%
  rename(FSBI = ACTMET_VALUE)
str(fsbi_formmated)

burp_fh_formatted <-
  burp_fh %>%
  select(BURPID, 
         Silt.Sand....2.5.mm....Wet.Fines.Wet.Total.., 
         Silt.Sand....2.5.mm....Dry.Fines.Dry.Total.., 
         Silt.Sand....2.5.mm....All.Fines.All.Total.Total..) %>%
  rename(wet_fines_2.5_mm = Silt.Sand....2.5.mm....Wet.Fines.Wet.Total..,
         dry_fines_2.5_mm = Silt.Sand....2.5.mm....Dry.Fines.Dry.Total..,
         all_fines_2.5_mm = Silt.Sand....2.5.mm....All.Fines.All.Total.Total..)
str(burp_fh_formatted)

indices_formatted <-
  burp_indices %>%
  select(-WBSEGMENT, -BURP.Stream.Name)

# calculate mean width by burpid
bw_mean <-
  bw %>%
  group_by(MLOC_ID) %>%
  summarize(bankfull_width_m = mean(RES_MEASURE, na.rm = TRUE))


# combine
formatted <-
  merge(crosswalk, fsbi_formmated, by.x = "BURPID", by.y = "MLOC_ID", all.x = TRUE) %>%
  merge(burp_fh_formatted, by = "BURPID", all.x = TRUE) %>%
  merge(indices_formatted, by = "BURPID", all.x = TRUE) %>%
  merge(hab_type, by.x = "BURPID", by.y = "MLOC_ID", all.x = TRUE) %>%
  merge(bw_mean, by.x = "BURPID", by.y = "MLOC_ID", all.x = TRUE) %>%
  select(-REACH, - OBJECTID, -OFFICE, -X)  %>%
  filter(STR_ORDR != "0L")

str(formatted)

# stream order correction
formatted$STR_ORDR[formatted$BURPID=="2001SLEWA013"]<-2
formatted$STR_ORDR[formatted$BURPID=="2001SLEWV001"]<-2
formatted$STR_ORDR[formatted$BURPID=="2015SLEWA066"]<-2
formatted$STR_ORDR[formatted$BURPID=="1998SBOIA010"]<-2

write.csv(formatted, "./formatted_data/BURP_data.csv", na = "")
