################################################################################
# formats tbl_sites.csv
# csv table with BURP, PIBO, and AIM site info
# Jason Williams
# last update: May 2023
################################################################################

library(tidyverse)
library(readxl)


# read in data------------------------------------------------------------------

# pibo sites georeferenced to 2022 ID305B layer
pibo_georef <-read.csv("./raw_data/ID_PIBO_sites_georeferenced.csv", 
                       na.strings = "")
str(pibo_georef)

# aim sites georeferenced to 2022 ID305B layer
aim_georef <-read.csv("./raw_data/AIM_data_georeferenced.csv", 
                      na.strings = "")
str(aim_georef)

# AUs with a TMDL in 2022 IR
cat4a <-read.csv("./raw_data/cat4a_2022IR.csv", header = TRUE)
str(cat4a)

# BURP site info
crosswalk <-read.csv("./raw_data/AU_BURP_Crosswalk.csv", header = TRUE)
str(crosswalk)

# blm ecoregion used for blm sed benchmarks
blm_ecoregion <-
  read.csv("./raw_data/BLM_ecoregion.csv", header = TRUE) %>%
  distinct(siteid, Ecoregion)
str(blm_ecoregion)

# pibo data downloaded march 2023, filtered for ID
pibo_invert <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Macroinverts", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, Mgmt)

str(pibo_invert)

pibo_habitat <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Habitat", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, Mgmt)

str(pibo_habitat)

pibo_veg <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Riparian_Veg", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, Mgmt)

str(pibo_veg)

pibo_temp <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Temperature", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, Mgmt)

str(pibo_temp)

pibo_weed <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Riparian_Weed", col_names = TRUE) %>%
  filter(State == "ID") %>% distinct(SiteID, Mgmt)

str(pibo_weed)


# list of aim sites aim classifies as sediment reference
# per Jenifer Cartwright, Utah State, 4/12/23

aim_sed_ref <-c(
  "BO-SS-10248",
  "CF-SS-38954",
  "CF-SS-54918",
  "CH-TR-1302",
  "CN-RV-13082",
  "CN-SS-11146",
  "CN-SS-16006",
  "CN-SS-18586",
  "CN-SS-30074",
  "CN-SS-56538",
  "CTG-SS-1007",
  "CTG-SS-1013",
  "CTO-LS-1070",
  "CTO-LS-1072",
  "CTO-SS-1058",
  "CTO-SS-1090",
  "CT-TR-1307",
  "CT-TR-1472",
  "JA-TR-3082",
  "MN-LS-1020",
  "MN-SS-1142",
  "PO-SS-1016",
  "SA-LS-14070",
  "SA-SS-10554",
  "SA-SS-12602",
  "SA-SS-14650",
  "SA-TR-1226",
  "US-SS-12190",
  "XN-LS-4003",
  "XN-LS-4014",
  "XN-LS-4058",
  "XN-SS-4145")

# AIM sites BLM classified as degraded due to fine sediment
# have more fines than predicted after accounting for model error
# per Jenifer Cartwright, Utah State, 4/25/23

aim_sed_degraded <-c("BY-TR-15985", "CTG-SS-1001", 
                     "JA-RV-41576", "JA-TR-88752",
                     "SA-LS-12318")


# format sed tmdl data----------------------------------------------------------

levels(unique(as.factor(cat4a$CAUSE_NAME)))


# AUs in cat4 a for a sediment cause, 2022 IR
sed_tmdl_aus <-
  cat4a %>%
  filter(CAUSE_NAME %in% c("SEDIMENTATION/SILTATION", 
                           "TOTAL SUSPENDED SOLIDS (TSS)")) %>%
  select(ID305B, TMDL_ID, TMDL_NAME, CAUSE_NAME, ESTABLISHMENT_DATE, WEB_LINK) %>%
  rename(au = ID305B, sed_tmdl_name = TMDL_NAME, sed_tmdl_cause = CAUSE_NAME,
         sed_tmdl_id = TMDL_ID, sed_tmdl_date = ESTABLISHMENT_DATE, 
         sed_tmdl_link = WEB_LINK)


# format pibo data--------------------------------------------------------------

pibo_siteinfo_formatted <-
  rbind(pibo_habitat, pibo_invert, pibo_temp, pibo_veg, pibo_weed) %>%
  distinct(.keep_all = TRUE)

# are there sites with multiple "Mgmt' values? 
pibo_siteinfo_formatted %>%
  group_by(SiteID) %>%
  count() %>%
  filter(n > 1) # no

pibo_georef_formatted <-
  pibo_georef %>%
  mutate(source = "USFS-PIBO") %>%
  select(source, SiteID, SiteName, Stream,  Lat, Long, COMID, AU, Order, 
         georef_ID305b_cycle, georef_notes, SITECLASS, ECO, LEVEL4_NAM, DEQ_ADMIN, 
         BASIN, HUC4CODE, HUC4NAME) %>%
  rename(siteid = SiteID, sitename = SiteName, stream = Stream, lat = Lat, 
         long = Long, comid = COMID, au = AU, order = Order,
         siteclass = SITECLASS, l4_code = ECO, l4_name = LEVEL4_NAM,
         deq_region = DEQ_ADMIN, basin = BASIN, huc4code = HUC4CODE,
         huc4name = HUC4NAME) %>%
  merge(pibo_siteinfo_formatted, by.x = "siteid", by.y = "SiteID", all.x = TRUE) %>%
  rename(source_sitetype = Mgmt) %>%
  mutate(AIM_OE_model = NA)

# format aim data---------------------------------------------------------------

aim_georef_formatted <-
  aim_georef %>% 
  mutate(source = "BLM", sitename = PointID) %>%
  select(source, PointID, sitename, StreamName,  SampledMid, SampledM_1, COMID, AU, 
         StreamOrde, georef_ID305b_cycle, georef_notes, SITECLASS, ECO, 
         LEVEL4_NAM, DEQ_ADMIN, BASIN, HUC4CODE, HUC4NAME, OE_MMI_Mod) %>%
  rename(siteid = PointID, stream = StreamName, lat = SampledMid, 
         long = SampledM_1, comid = COMID, au = AU, order = StreamOrde,
         siteclass = SITECLASS, l4_code = ECO, l4_name = LEVEL4_NAM,
         deq_region = DEQ_ADMIN, basin = BASIN, huc4code = HUC4CODE,
         huc4name = HUC4NAME, AIM_OE_model = OE_MMI_Mod) %>%
  mutate(source_sitetype = ifelse(siteid %in% aim_sed_ref, "sediment reference",
                                  ifelse(siteid %in% aim_sed_degraded, 
                                         "sediment degraded", NA))) 

str(aim_georef_formatted)

# there are some duplicate sites, remove
aim_georef_formatted_distinct <-
  aim_georef_formatted %>%
  distinct(siteid, comid, au, order, .keep_all = TRUE)

# format BURP data -------------------------------------------------------------

str(crosswalk)

burpsites_formatted <-
  crosswalk %>%
  mutate(source = "DEQ BURP", georef_ID305b_cycle = "2022",
         georef_notes = NA, sitename = BURPID, source_sitetype = REFERENCE) %>%
  select(source, BURPID, sitename, STREAM, LATITUDE, LONGITUDE, COMID, WBSEGMENT, STR_ORDR,
         georef_ID305b_cycle, georef_notes, SITECLASS, L4CODE, ECO4NAME, 
         OFFICE, BASIN, HUC4CODE, HUC4NAME, source_sitetype) %>%
  rename(siteid = BURPID, stream = STREAM, lat = LATITUDE, long = LONGITUDE, 
         au = WBSEGMENT, comid = COMID, order = STR_ORDR, 
         deq_region = OFFICE, basin = BASIN, huc4code = HUC4CODE, 
         huc4name = HUC4NAME, l4_code = L4CODE, l4_name = ECO4NAME,
         siteclass = SITECLASS) %>%
  mutate(AIM_OE_model = NA)


# there are some BURP sites with AU = 'NULL', correct these
burpsites_formatted %>% filter(au == "NULL") %>% distinct(as.factor(siteid))

burpsites_formatted$au[burpsites_formatted$siteid == "2012SCDAA053"] <-"ID17010306PN001_02"
burpsites_formatted$au[burpsites_formatted$siteid == "2012SCDAA054"] <-"ID17010306PN001_02"
burpsites_formatted$au[burpsites_formatted$siteid == "2002SCDAA003"] <-"ID17010306PN001_02"
burpsites_formatted$au[burpsites_formatted$siteid == "2002SCDAA007"] <-"ID17010306PN001_02T"
burpsites_formatted$au[burpsites_formatted$siteid == "2002SCDAA006"] <-"ID17010306PN001_02T"
burpsites_formatted$au[burpsites_formatted$siteid == "2002SCDAA004"] <-"ID17010306PN001_02T"
burpsites_formatted$au[burpsites_formatted$siteid == "2002SCDAA001"] <-"ID17010306PN001_02T"
burpsites_formatted$au[burpsites_formatted$siteid == "2002SCDAA005"] <-"ID17010306PN001_02"
burpsites_formatted$au[burpsites_formatted$siteid == "2003SCDAA001"] <-"ID17010306PN001_02T"
burpsites_formatted$au[burpsites_formatted$siteid == "2003SCDAA004"] <-"ID17010306PN001_02T"
burpsites_formatted$au[burpsites_formatted$siteid == "2003SCDAA002"] <-"ID17010306PN001_02"
burpsites_formatted$au[burpsites_formatted$siteid == "2003SCDAA005"] <-"ID17010306PN001_02"
burpsites_formatted$au[burpsites_formatted$siteid == "2003SCDAA006"] <-"ID17010306PN001_02T"
burpsites_formatted$au[burpsites_formatted$siteid == "2004SCDAA030"] <-"ID17010301PN012_02"


# stream order corrections
burpsites_formatted$order[burpsites_formatted$siteid=="2001SLEWA013"]<-2
burpsites_formatted$order[burpsites_formatted$siteid=="2001SLEWV001"]<-2
burpsites_formatted$order[burpsites_formatted$siteid=="2015SLEWA066"]<-2
burpsites_formatted$order[burpsites_formatted$siteid=="1998SBOIA010"]<-2

# l4_name corrections for 'No Data' cases

# combine & format ------------------------------------------------------------

tbl_sites <-
  rbind(pibo_georef_formatted, aim_georef_formatted_distinct, burpsites_formatted) %>%
  mutate(sed_tmdl = ifelse(au %in% sed_tmdl_aus$au, "Y", "N")) %>%
  merge(blm_ecoregion, by = "siteid", all.x = TRUE) %>%
  rename(blm_ecoregion = Ecoregion) %>%
  select(siteid, sitename,  stream, source, lat, long, comid, au, order,
         georef_ID305b_cycle, georef_notes, sed_tmdl, 
         siteclass, l4_code, l4_name, deq_region, blm_ecoregion, basin, huc4code, 
         huc4name, source_sitetype, AIM_OE_model) %>%
  arrange(source, siteid) 


str(tbl_sites)

write.csv(tbl_sites, "./formatted_data/tbl_sites.csv", na = "NA")
