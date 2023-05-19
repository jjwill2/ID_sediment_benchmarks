################################################################################
# Formats project_data.csv
# BURP, PIBO, and AIM data for benchmark analysis
# Jason Williams
# May 2023
################################################################################

library(tidyverse)
library(readxl)
library(reshape2)
library(lubridate)

#read in data-------------------------------------------------------------------

tbl_sites <-read.csv("./formatted_data/tbl_sites.csv", header = TRUE)
str(tbl_sites)

# PIBO % fines
pibo_pebble <-
  read.csv("./raw_data/PIBO_FineSediments_TransectCalc.csv", header = TRUE)
str(pibo_pebble)

# PIBO FSBI
pibo_fsbi <-
  read_excel("./raw_data/fsbi_output_template_example.xlsx", 
             sheet = "calculated_sample_fsbi", col_names = TRUE) %>%
  filter(agency == "USFS")
str(pibo_fsbi)

# PIBO macro OE
pibo_oe<-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Macroinverts", col_names = TRUE) %>%
  distinct(SiteID, RchID, RIVPACS)
str(pibo_oe)

# PIBO bankfull width
pibo_bw <-
  read_excel("./raw_data/fseprd1090246.xlsx", sheet = "Habitat", col_names = TRUE) %>%
  distinct(SiteID, RchID, Bf)
str(pibo_bw)

# pibo siteid rchid crosswalk
pibo_cw <-read.csv("./formatted_data/pibo_siteid_rchid_crosswalk.csv",
                   header = TRUE) %>% select(-X)
str(pibo_cw)


# benchmarks
framework_benchmarks <-read.csv("./formatted_data/framework_benchmarks.csv", header = TRUE)
str(framework_benchmarks)

blm_benchmarks <-read.csv("./formatted_data/blm_benchmarks.csv", header = TRUE)
str(blm_benchmarks)

# AIM % fines & OE * Bankfull Width
aim_fines <-
  read.csv("./raw_data/I_indicators.csv", header = TRUE) %>%
  mutate(no_OE = ifelse(ExpectedIn < 0.0001, "no OE", NA)) %>%
  mutate(AIM_OE = ifelse(is.na(no_OE), OE_Macroin, NA)) %>%
  select(PointID, NAMC_Sampl, FieldEvalD, Evaluation, PctFinesLe, OE_Macroin, 
         no_OE, AIM_OE, OE_MMI_Mod, BankfullWi, Project)


str(aim_fines)

# AIM FSBI
aim_fsbi <-
  read_excel("./raw_data/fsbi_output_template_example.xlsx", 
             sheet = "calculated_sample_fsbi", col_names = TRUE) %>%
  filter(agency == "BLM") %>%
  mutate(AIM_OE_model = ifelse(project == "AIM PIBO OE",
                               "ColumbiaRiverBasin_PIBO",
                               ifelse(project == "AIM WW OE",
                                      "Westwide2018_OtherEcoregions", NA)))

# BURP data
burp <-read.csv("./formatted_data/BURP_data.csv", header = TRUE)
str(burp)

# format pibo data--------------------------------------------------------------


# note - blank siteids are non-ID, site crosswlk is for Idaho only
pibo_pebble_formatted <-
  merge(pibo_pebble, pibo_cw, by.x = "Rchid", by.y = "RchID", all.x = TRUE)


pibo_project_data <-
  tbl_sites %>%
  filter(source == "USFS-PIBO") %>%
  merge(pibo_pebble_formatted, by.x = "siteid", by.y = "SiteID") %>%
  mutate(pct_fine2 = PercentFine2 * 100) %>%
  merge(pibo_fsbi, by.x = "Rchid", by.y = "sample", all.x = TRUE) %>%
  merge(pibo_oe, by.y = c("RchID", "SiteID"), by.x = c("Rchid", "siteid"), all.x = TRUE) %>%
  merge(pibo_bw, by.y = c("RchID", "SiteID"), by.x = c("Rchid", "siteid"), all.x = TRUE) %>%
  select(-PebLess2, -PebLess6, -PercentFine2, -PercentFine6, -site, 
         -n_FSBI_taxa, -X,-agency, -project, -BedPebs, -Yr, -n_taxa, 
         -AIM_OE_model, -AIM_Evaluation_ID) %>%
  rename(sampleid = Rchid, OE = RIVPACS, bankfull_width_m = Bf,
         pct_fine = pct_fine2) %>%
  mutate(OE_benchmark = 0.78, OE_model = "USFS-PIBO", 
         pct_fine_fraction = "< 2 mm", 
         bankfull_width_category = ifelse(bankfull_width_m >= 10, ">= 10 m",
                                          ifelse(bankfull_width_m < 10, "< 10 m", NA)),
         SMI2 = NA, BURP_sitescore = NA) %>%
  mutate(date_formatted = as.Date(SampDate, format = "%Y-%m-%d"))

str(pibo_project_data)

# format aim data---------------------------------------------------------------

aim_project_data <-
  tbl_sites %>%
  filter(source == "BLM") %>%
  merge(aim_fines, by.x = "AIM_Evaluation_ID", by.y = "Evaluation", all = TRUE) %>%
  merge(aim_fsbi, by.x = c("NAMC_Sampl", "AIM_OE_model"),
        by.y = c("sample", "AIM_OE_model"), all.x = TRUE) %>%
  filter(Project != "QC") %>%
  select(-X, -agency, -project, -site, -n_FSBI_taxa, -n_taxa, -no_OE, -OE_Macroin,
         -AIM_Evaluation_ID, -AIM_OE_model, -PointID, -Project) %>%
  rename(SampDate = FieldEvalD, pct_fine = PctFinesLe, sampleid = NAMC_Sampl, 
         OE = AIM_OE, bankfull_width_m = BankfullWi) %>%
  mutate(date_formatted = as.Date(SampDate, format = "%m/%d/%Y")) %>%
  mutate(OE_benchmark = ifelse(OE_MMI_Mod == "ColumbiaRiverBasin_PIBO", 0.63,
                               ifelse(OE_MMI_Mod == "Westwide2018_OtherEcoregions", 0.72, NA)),
         pct_fine_fraction = "< 2 mm",
         bankfull_width_category = ifelse(bankfull_width_m >= 10, ">= 10 m",
                                          ifelse(bankfull_width_m < 10, "< 10 m", NA)),
         SMI2 = NA, BURP_sitescore = NA) %>%
  rename(OE_model = OE_MMI_Mod)

# format BURP data--------------------------------------------------------------

burp_project_data <-
  tbl_sites %>%
  filter(source == "DEQ BURP") %>%
  merge(burp, by.x = "siteid", by.y = "BURPID", all.x = TRUE) %>%
  mutate(sampleid = siteid, SampDate = NA, OE = NA, OE_benchmark = NA,
         OE_model = NA, pct_fine_fraction = "< 2.5 mm", date_formatted = NA) %>%
  rename(pct_fine = all_fines_2.5_mm, sample_FSBI = FSBI,
         BURP_sitescore = WBAG.3.Score) %>%
  mutate(bankfull_width_category = ifelse(bankfull_width_m >= 10, ">= 10 m",
                                          ifelse(bankfull_width_m < 10, "< 10 m", NA))) %>%
  select(sampleid, siteid, sitename, stream, source, lat, long, comid, siteclass, 
         order, au, georef_ID305b_cycle, georef_notes, sed_tmdl,
         l4_code, l4_name, deq_region, blm_ecoregion, basin, huc4code, huc4name, 
         source_sitetype, sampleid, SampDate, date_formatted, 
         bankfull_width_m, bankfull_width_category, 
         pct_fine, pct_fine_fraction, sample_FSBI, SMI2, BURP_sitescore,
         OE, OE_benchmark, OE_model) %>%
  filter(!is.na(pct_fine))

str(burp_project_data)

# combiine & format-------------------------------------------------------------

project_data <-
  rbind(pibo_project_data, aim_project_data, burp_project_data) %>%
  merge(framework_benchmarks, by = c("siteclass", "order"), all.x = TRUE) %>%
  merge(blm_benchmarks, by.x = c("blm_ecoregion", "bankfull_width_category"),
        by.y = c("blm_ecoregion", "bankfull_width"), all.x = TRUE) %>%
  mutate(year = ifelse(source != "DEQ BURP", year(date_formatted),
                       ifelse(source == "DEQ BURP",
                              substr(siteid, 1, 4), NA))) %>%
  rename(sample_date = date_formatted) %>%
  select(siteid, sitename, stream, source, lat, long, comid, siteclass,
         order, au, georef_ID305b_cycle, georef_notes, sed_tmdl, 
         l4_code, l4_name, deq_region, blm_ecoregion, basin, huc4code, 
         huc4name, source_sitetype, sampleid, year, sample_date, bankfull_width_m,
         bankfull_width_category, pct_fine, pct_fine_fraction, sample_FSBI,
         SMI2, SMI2_benchmark, BURP_sitescore, OE, OE_benchmark, OE_model, 
         framework_fines_ref_benchmark, framework_FSBI_ref_benchmark,
         framework_SME75_benchmark, blm_fines_ref_benchmark) %>%
  mutate(has_both = ifelse(!is.na(pct_fine) & !is.na(sample_FSBI), "Y", "N"))

str(project_data)

write.csv(project_data, "./formatted_data/project_data.csv")

