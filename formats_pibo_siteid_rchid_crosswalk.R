################################################################################
# Creates siteid / reach id crosswalk for PIBO data
# Jason Williams
# last update: May2023
################################################################################

library(tidyverse)
library(readxl)

# read in data------------------------------------------------------------------


# pibo invert data downloaded March 2023, filtered for ID
pibo_invert_cw <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Macroinverts", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, RchID, SampDate, Yr)

str(pibo_invert_cw)

# pibo habitat data downloaded March 2023, filtered for ID
pibo_habitat_cw <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Habitat", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, RchID, SampDate, Yr)

str(pibo_habitat_cw)

# pibo riparian veg data downloaded March 2023, filtered for ID
pibo_veg_cw <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Riparian_Veg", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, RchID, SampDate, Yr)

str(pibo_veg_cw)

# pibo temp data downloaded March 2023, filtered for ID; no sample date inlcuded
pibo_temp_cw <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Temperature", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, RchID, Yr)

str(pibo_temp_cw)

# pibo riparian weed downloaded March 2023, filtered for ID
pibo_weed_cw <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Riparian_Weed", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, RchID, SampDate, Yr)

str(pibo_weed_cw)

# format------------------------------------------------------------------------

# are any reach ids from temp data not already in pibo_cw
pibo_temp_formatted <-
  pibo_temp_cw %>%
  filter(!RchID %in% pibo_cw$RchID) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(SampDate = NA)

pibo_cw <-rbind(pibo_habitat_cw, pibo_invert_cw, pibo_veg_cw, pibo_weed_cw, 
                pibo_temp_formatted) %>%
  distinct(.keep_all = TRUE)

