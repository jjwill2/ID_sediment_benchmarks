################################################################################
# Calculates performance statistics for reference and sed-impaired sites
# Jason Williams
# May 2023
################################################################################

library(tidyverse)
library(readxl)
library(reshape2)

# read in data------------------------------------------------------------------

project_data <-read.csv("./formatted_data/project_data.csv", header = TRUE)
str(project_data)

framework_benchmarks <-read.csv("./formatted_data/framework_benchmarks.csv", header = TRUE)
str(framework_benchmarks)

# calculate predictions from framework and other methods------------------------

# require paired % fines and FSBI for prediction

predictions <-
  project_data %>%
  filter(has_both == "Y") %>%
  mutate(year_numeric = as.numeric(year)) %>%
  mutate(worse_than_fines_ref = ifelse(pct_fine > framework_fines_ref_benchmark, "Y", "N")) %>%
  mutate(worse_than_FSBI_ref = ifelse(sample_FSBI < framework_FSBI_ref_benchmark, "Y", "N")) %>%
  mutate(exceeds_both_ref = ifelse(worse_than_FSBI_ref == "Y" & worse_than_fines_ref == "Y", "Y", "N")) %>%
  mutate(exceeds_SME75 = ifelse(pct_fine > framework_SME75_benchmark, "Y", "N")) %>%
  mutate(framework_prediction =  ifelse(has_both == "N", "additional line of evidence needed",
                                        ifelse(exceeds_both_ref == "Y" & exceeds_SME75 == "N", "sediment effect likely",
                                               
                                               ifelse(exceeds_both_ref == "Y" & exceeds_SME75 == "Y", "sedement effect very likely",
                                                      
                                                      ifelse(exceeds_both_ref == "N" & exceeds_SME75 == "Y", "mixed evidence",
                                                             
                                                             ifelse(worse_than_fines_ref == "Y" & worse_than_FSBI_ref == "N", "mixed evidence",
                                                                    
                                                                    ifelse(worse_than_fines_ref == "N" & worse_than_FSBI_ref == "N", "no sediment effect",
                                                                           
                                                                           ifelse(worse_than_fines_ref == "N" & worse_than_FSBI_ref == "Y", "mixed evidence", NA)))))))) %>%
  mutate(OE_prediction = ifelse(OE < OE_benchmark, "not within expected condition",
                                ifelse(OE >= OE_benchmark, "within expected condition", NA))) %>%
  mutate(BLM_prediction = ifelse(pct_fine > blm_fines_ref_benchmark, "BLM fines fail", "BLM fines pass")) %>%
  mutate(SMI2_prediction = ifelse(SMI2 < SMI2_benchmark, "SMI2 fail", 
                                  ifelse(SMI2 >= SMI2_benchmark, "SMI2 pass", NA)))

write.csv(predictions, "./formatted_data/predictions.csv")

# calculate outcomes at pibo and aim reference sites----------------------------

# format for plotting
for_performance_stats <-
  predictions %>%
  filter(source_sitetype %in% c("Reference", "sediment reference")) %>%
  select(source, siteid, sampleid, sample_date, siteclass, order, blm_ecoregion,
         pct_fine, sample_FSBI, has_both, OE, OE_benchmark, SMI2, SMI2_benchmark,
         framework_prediction, OE_prediction, BLM_prediction, SMI2_prediction) %>%
  melt(id.vars = c("source", "siteid", "sampleid", "sample_date", "siteclass", "order", "blm_ecoregion",
                    "pct_fine", "sample_FSBI",
                   "has_both", "OE", "OE_benchmark", "SMI2", "SMI2_benchmark"))




# summarize predicted outcomes for reference sites-------------------------------

# summary by sample
# includes only samples with both % fines and FSBI
reference_performance_stats <-
  for_performance_stats %>%
  group_by(source, variable, value) %>%
  count()


# summarize outcomes for pibo & AIM sites with sediment TMDL--------------------


for_tmdl_validation <-
  predictions %>%
  filter(sed_tmdl == "Y") %>%
  select(source, siteid, sampleid, sample_date, siteclass, order, blm_ecoregion,
         pct_fine, sample_FSBI, has_both, OE, OE_benchmark, SMI2, SMI2_benchmark,
         framework_prediction, OE_prediction, BLM_prediction, SMI2_prediction) %>%
  melt(id.vars = c("source", "siteid", "sampleid", "sample_date", "siteclass", "order", 
                  "blm_ecoregion", "pct_fine", "sample_FSBI", "has_both", "OE", 
                  "OE_benchmark", "SMI2", "SMI2_benchmark"))


# summary by sample
tmdl_performance_stats <-
  for_tmdl_validation %>%
  group_by(source, variable, value) %>%
  count()


# plot OE vs. sed predeiction outcome
for_tmdl_validation %>%
  filter(source != "DEQ BURP") %>%
  filter(variable == "framework_prediction") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value, y = OE)) +
  geom_boxplot() +
  geom_point(shape = 1) +
  facet_wrap(~source, nrow = 2) +
  coord_flip() +
  theme_bw() +
  labs(x = "macroinvertebrate O/E") +
  theme(axis.title.y = element_blank())

# false positive rates when using OE failing-samples as 'known' impaired--------

for_tmdl_performace_stats2 <-
  predictions %>%
  filter(sed_tmdl == "Y") %>%
  mutate(OE_prediction = ifelse(OE < OE_benchmark, "not within expected condition",
                                ifelse(OE >= OE_benchmark, "within expected condition", NA))) %>%
  mutate(SMI2_prediction = ifelse(SMI2 < SMI2_benchmark, "SMI2 fail", 
                                  ifelse(SMI2 >= SMI2_benchmark, "SMI2 pass", NA)))

# OE for PIBO and AIM
tmdl_performance_stats_oe <-
  for_tmdl_performace_stats2 %>%
  filter(source != "DEQ BURP") %>%
  group_by(source, framework_prediction) %>%
  count(OE_prediction)

# SMI2 for BURP
tmdl_performance_stats_SMI2 <-
  for_tmdl_performace_stats2 %>%
  filter(source == "DEQ BURP") %>%
  group_by(framework_prediction) %>%
  count(SMI2_prediction)
