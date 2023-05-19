################################################################################
# uses raw PIBO and AIM data files to develop list of unique taxa
# then assign taxa-level FSBI scores
# Jason Williams, Idaho DEQ
# last update: May
################################################################################

library(tidyverse)

# read in data------------------------------------------------------------------

# AIM macro data for use with PIBO O/E model
# From Trip Armstrong, National Aquatic Monitoring Center 3-22-2023
ID_PIBO_OTU <-read.csv("./raw_data/ID_PIBO_OTU.csv", header = TRUE)
str(ID_PIBO_OTU)

# AIM macro data for use with Westwide O/E model
# From Trip Armstrong, National Aquatic Monitoring Center 3-22-2023
ID_WW_OTU <-read.csv("./raw_data/ID_WW_OTU.csv", header = TRUE)
str(ID_PIBO_OTU)

# PIBO sample macro results created by processing raw data
pibo_sample_macro <-read.csv("./formatted_data/FSBI_input_sample_macro_data.csv", header = TRUE)
str(pibo_sample_macro)

# taxa FSBI scores from Relyea et al. 2012
relyea_fsbi <-read.csv("./raw_data/Relyea_taxa_FSBI.csv", header = TRUE)
str(relyea_fsbi)

# format AIM data---------------------------------------------------------------

ID_PIBO_OTU_formatted <-
  ID_PIBO_OTU %>%
  distinct(scientificName) %>%
  mutate(agency = "BLM", project = "AIM PIBO OE")

ID_WW_OTU_formatted <-
  ID_WW_OTU %>% 
  distinct(scientificName) %>%
  mutate(agency = "BLM", project = "AIM WW OE")

aim_macro <-
  rbind(ID_PIBO_OTU_formatted, ID_WW_OTU_formatted)

str(aim_macro)

# format PIBO data--------------------------------------------------------------

pibo_taxa_formatted <-
  pibo_sample_macro %>%
  distinct(taxa) %>%
  mutate(agency = "USFS", project = "PIBO") %>%
  rename(scientificName = taxa)

str(pibo_taxa_formatted)

# combine-----------------------------------------------------------------------

unique_taxa_formatted <-
  rbind(aim_macro, pibo_taxa_formatted)

str(unique_taxa_formatted)

# merge with Relyea taxa scores-------------------------------------------------
# NOTE: a manual check & taxa score assignment still needed before FSBI calculator
# merge only addresses exact matches

taxa_relyea_merged <-
  unique_taxa_formatted %>%
  merge(relyea_fsbi, by.x = "scientificName", 
        by.y = "relyea_taxa", all.x = TRUE)


write.csv(taxa_relyea_merged, "./formatted_data/taxa_relyea_merged.csv")

# manual FSBI taxon score assignments-------------------------------------------

# scientificName values that weren't assigned score in merge, but have a score in Relyea et al. 2012
# based on manual comparison of 'taxa_relyea_merged' and Relyea et al. 

manual_additions <-
  read.csv("./formatted_data/manual_fsbi_taxa_score_additions.csv") %>%
  select(-X.1) %>%
  mutate(id = paste(scientificName, agency, project)) %>%
  select(-X)
str(manual_additions)

# add manual additions----------------------------------------------------------


assigned_FSBI_taxa_scores <-
  taxa_relyea_merged %>%
  mutate(id = paste(scientificName, agency, project)) %>%
  filter(!id %in% manual_additions$id) %>%
  rbind(manual_additions) %>%
  select(-id)


str(assigned_FSBI_taxa_scores)

write.csv(assigned_FSBI_taxa_scores, "./formatted_data/FSBI_input_taxa_FSBI.csv")


# notes-------------------------------------------------------------------------
# Rhyacophilia sibirica grp-pellisa vs. multiple Rhyacophilia sibirica groups in PIBO/AIM
# are Orodobrevia nubifer & Orodobrevia nubifera the same speceis? 
# there are 2 taxa with OTUname, but no scientific name in pibo_key



