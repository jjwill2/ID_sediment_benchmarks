################################################################################
# formats PIBO & AIM sample macro data for input to FSBI calculator
# Jason Williams, Idaho DEQ Lewiston Office
# last update: May 2023
################################################################################

library(tidyverse)
library(readxl)

# read in data------------------------------------------------------------------

# AIM macro data for use with PIBO O/E model
# From Trip Armstrong, National Aquatic Monitoring Center 3-22-2023
ID_PIBO_OTU <-read.csv("./raw_data/ID_PIBO_OTU.csv", header = TRUE)
str(ID_PIBO_OTU)

# AIM macro data for use with Westwide O/E model
# From Trip Armstrong, National Aquatic Monitoring Center 3-22-2023
ID_WW_OTU <-read.csv("./raw_data/ID_WW_OTU.csv", header = TRUE)
str(ID_PIBO_OTU)

# AIM site and indicators data
# layer 'I_indicators' in geodatabase from Logan Shank (BLM) 3-17-2023
aim_indicators <-read.csv("./raw_data/I_indicators.csv", header = TRUE)
str(aim_indicators)

# additional data from Trip Armstrong 3-24-23, for sample IDs not in 'I_indicators'
aim_part2 <-read_excel("./raw_data/NAMC Report_Proj2367_2023-03-24.xlsx",
                       sheet = "Site data", col_names = TRUE)
str(aim_part2)

# PIBO sample macro results from Trip Armstrong 5-15-2023
pibo_macro <-read.csv("./raw_data/PIBO_ID_raw.csv", header = TRUE)
str(pibo_macro)

# PIBO sampleid, siteid, rchid crosswalk from Trip Armstrong 5-15-2021
pibo_crosswalk <-read.csv("./raw_data/PIBO_ID_site_sample.csv", header = TRUE)
str(pibo_crosswalk)

# format PIBO data--------------------------------------------------------------
# want file with cols agency, project, site, sample, taxa, with one row per sample/taxa

pibo_macro_formatted <-
  merge(pibo_macro, pibo_crosswalk, by = "sampleId", all.x = TRUE) %>%
  select(siteId, RchID, scientificName) %>%
  mutate(agency = "USFS", project = "PIBO") %>%
  rename(site = siteId, sample = RchID, taxa = scientificName) %>%
  select(agency, project, site, sample, taxa)

# pibo data checks--------------------------------------------------------------

# do we have raw data for all rchids with OE data? 
# PIBO macro OE
pibo_oe_id <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Macroinverts", col_names = TRUE) %>%
  filter(State == "ID") %>%
  filter(!is.na(RIVPACS))

missing_raw_macro <-
  pibo_oe_id %>%
  filter(!RchID %in% pibo_macro_formatted$sample)

# format AIM data---------------------------------------------------------------
# want file with cols agency, project, site, sample, taxa, with one row per sample/taxa

# format site/sample crosswalk
aim_sites <-
  aim_indicators %>%
  distinct(PointID, Evaluation, NAMC_Sampl)

# format aim macro data
AIM_PIBO_macro <-
  ID_PIBO_OTU %>%
  select(sampleId, scientificName) %>%
  mutate(agency = "BLM", project = "AIM PIBO OE")

AIM_WW_macro <-
  ID_WW_OTU %>%
  select(sampleId, scientificName) %>%
  mutate(agency = "BLM", project = "AIM WW OE")

aim_macro_formatted <-
  rbind(AIM_PIBO_macro, AIM_WW_macro) %>%
  merge(aim_sites, by.x = "sampleId", by.y = "NAMC_Sampl", all.x = TRUE) %>%
  rename(site = PointID, sample = sampleId, taxa = scientificName) %>%
  select(agency, project, site, sample, taxa)


# are any sample IDs with macro data not in I_indicators?
aim_macro_formatted %>%
  filter(is.na(site)) %>%
  distinct(sample)

# add in site info from 'aim_part2'
aim_part2_formatted <-
  aim_part2 %>%
  select(`Sample ID`, `Site Name`) %>%
  rename(sample = `Sample ID`, site = `Site Name`)

aim_macro_formatted2 <-
  merge(aim_macro_formatted, aim_part2_formatted, by = "sample", all.x = TRUE) %>%
  mutate(site = ifelse(is.na(site.y), site.x, site.y)) %>%
  select(agency, project, site, sample, taxa) %>%
  
  # remove samples Trip identified as internal lab QC samples
  filter(!sample %in% c(153171, 153178, 153300, 153303, 158333))



# combine-----------------------------------------------------------------------

input_macro_data <-rbind(pibo_macro_formatted, aim_macro_formatted2)

write.csv(input_macro_data, "./formatted_data/FSBI_input_sample_macro_data.csv")

