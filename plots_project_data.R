################################################################################
# plots and tablultes fomratted data
# Jason Williams
# May 2023
################################################################################

library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)

# read in data------------------------------------------------------------------

# NOTE - manually formated date in excel to be consistent
project_data <-read.csv("./formatted_data/project_data.csv", header = TRUE)
str(project_data)

framework_benchmarks <-read.csv("./formatted_data/framework_benchmarks.csv", header = TRUE)
str(framework_benchmarks)

predictions <-read.csv("./formatted_data/predictions.csv", header = TRUE)
str(predictions)

# plot OE exceedance for % fines and FSBI---------------------------------------
# USFS-PIBO and BLM data

for_external_fines_vs_fsbi <-
  project_data %>%
  filter(source != "DEQ BURP") %>%
  filter(!is.na(framework_fines_ref_benchmark)) %>%
  filter(has_both == "Y") %>%
  filter(order %in% c("1", "2", "3", "4", "5")) %>%
  select(source, siteid, source_sitetype, order, siteclass, sampleid, pct_fine,
         sample_FSBI, has_both, OE, OE_benchmark) %>%
  mutate(`achieves OE benchmark` = ifelse(OE >= OE_benchmark, "Y", 
                                          ifelse(OE < OE_benchmark, "N", NA))) %>%
  mutate(order_numeric = as.integer(order)) %>%
  mutate(`site type` = ifelse(source_sitetype %in% c("Reference", "sediment reference"), 
                              "reference", "non-reference"))


str(for_external_fines_vs_fsbi)

FSBI_ref_forplot <-
  framework_benchmarks %>%
  select(siteclass, order, framework_FSBI_ref_benchmark, framework_fines_ref_benchmark,
         framework_SME75_benchmark) %>%
  rename(order_numeric = order)


external_fines_vs_fsbi_plot <-
  for_external_fines_vs_fsbi %>%
  ggplot(aes(x = pct_fine, y = sample_FSBI, color = `site type`, 
             shape = source)) +
  geom_point() +
  facet_grid(order_numeric~siteclass) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "% fines < 2 mm", y = "FSBI") +
  geom_hline(data = FSBI_ref_forplot, aes(yintercept = framework_FSBI_ref_benchmark), 
             linetype = "dashed") +
  geom_vline(data = FSBI_ref_forplot, aes(xintercept = framework_fines_ref_benchmark), 
             linetype = "dashed")  +
  geom_vline(data = FSBI_ref_forplot, aes(xintercept = framework_SME75_benchmark), 
             linetype = "dashed", color = "#D55E00")  +
  scale_color_manual(values = c("darkgrey", "#56B4E9")) +
  scale_shape_manual(values = c(1, 17)) +
  theme(legend.title = element_blank())
external_fines_vs_fsbi_plot

ggsave("./figures/USFS_BLM_fines_vs_fsbi.png", external_fines_vs_fsbi_plot, 
       width = 8.0, height = 8.0, units = "in", dpi = 600)


# plot OE vs FSBI--------------------------------------------------------------

fsbi_vs_macro_plot <-
project_data %>%
  filter(!is.na(sample_FSBI)) %>%
  filter(!is.na(siteclass)) %>%
  select(siteid, source, siteclass, sample_FSBI, OE, SMI2) %>%
  melt(id.vars = c("siteid", "source", "sample_FSBI", "siteclass"),
       variable.name = "parameter", value.name = "value") %>%
  ggplot(aes(x = sample_FSBI, y = value, color = source, shape = source)) +
  geom_point(alpha = 0.7) +
  facet_grid(parameter~siteclass, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank()) +
  labs(x = "FSBI", y = "value") +
  scale_color_manual(values = c("black", "darkgrey", "grey"))
fsbi_vs_macro_plot

ggsave("./figures/fsbi_vs_OE_and_SMI2.png", width = 6, height = 4, 
       units = "in", dpi = 600)

# plot time series-------------------------------------------------------------

# PIBO sites with samples in > 4 years
time_series_sites <-
  predictions %>%
  filter(source != "DEQ BURP") %>%
  group_by(siteid) %>%
  summarize(min_yr = min(year, na.rm = TRUE),
            max_yr = max(year, na.rm = TRUE),
            n_sample = n_distinct(sampleid),
            n_year = n_distinct(year)) %>%
  filter(n_year > 4)

# plot 
for_plot <-
  predictions %>%
  filter(siteid %in% time_series_sites$siteid) %>%
  mutate(date_formatted = as.Date(sample_date, format = "%m/%d/%Y")) %>%
  mutate(label = paste0(siteid, ": ", stream, " (", source_sitetype, ")")) %>%
  select(label, date_formatted, year_numeric, pct_fine, sample_FSBI, framework_prediction) %>%
  melt(id.vars = c("label", "date_formatted", "year_numeric", "framework_prediction"),
       variable.name = "parameter", value.name = "value")

str(for_plot)

pibo_time_series_plot <-
for_plot %>%
  ggplot(aes(x = year_numeric, y = value)) +
  geom_point(aes(color = framework_prediction)) +
  geom_line(aes(linetype = parameter)) +
  facet_wrap(~label) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c(
    "mixed evidence" = "darkgrey",
    "no sediment effect" = "#009E73",
    "sediment effect likely" = "#E69F00",
    "sediment effect very likely" = "#D55E00"))
pibo_time_series_plot

ggsave("./figures/pibo_time_series_plot.png", pibo_time_series_plot,
       width = 8.5, height = 6, units = "in", dpi = 600)

# time series for COMIDs with data points accross multiple sources--------------

time_series_comids <-
  predictions %>%
  group_by(comid, source) %>%
  summarize(min_yr = min(year, na.rm = TRUE),
            max_yr = max(year, na.rm = TRUE),
            n_sample = n_distinct(sampleid),
            n_year = n_distinct(year)) %>%
  select(comid, source, n_sample) %>%
  dcast(comid ~ source, value.var = "n_sample")


burp_and_blm <-
  time_series_comids %>%
  filter(!is.na(BLM) & !is.na(`DEQ BURP`))

burp_and_blm_multiple <-
  burp_and_blm %>%
  filter(`DEQ BURP` >=2 & BLM >= 2)

burp_and_pibo <-
  time_series_comids %>%
  filter(!is.na(`USFS-PIBO`) & !is.na(`DEQ BURP`))

burp_and_pibo_multiple <-
  burp_and_pibo %>%
  filter(`DEQ BURP` >=2 & `USFS-PIBO` >= 2)

all_three <-
  time_series_comids %>%
  filter(!is.na(`USFS-PIBO`) & !is.na(`DEQ BURP`) & !is.na(BLM))

# plot COMIDs with data from all 3 sources
comparison_plot_all_3 <-
  predictions %>%
  filter(comid %in% all_three$comid) %>%
  ggplot(aes(x = year_numeric, y = source, color = framework_prediction)) +
    geom_point() +
    facet_wrap(~comid) +
    theme_bw() +
  scale_color_manual(values = c(
    "mixed evidence" = "darkgrey",
    "no sediment effect" = "#009E73",
    "sediment effect likely" = "#E69F00",
    "sediment effect very likely" = "#D55E00")) +
  theme(legend.title = element_blank(), axis.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle("COMIDs with data from all 3 programs")
comparison_plot_all_3

ggsave("./figures/comparison_plot_all3.png", comparison_plot_all_3,
       width = 8.0, height = 3.0, units = "in", dpi = 600)

# plot COMIDs with both DEQ & USFS, >=2 samples from each source

comparison_plot_usfs_pibo <-
  predictions %>%
  filter(comid %in% burp_and_pibo_multiple$comid) %>%
  ggplot(aes(x = year_numeric, y = source, color = framework_prediction)) +
  geom_point() +
  facet_wrap(~comid) +
  theme_bw() +
  scale_color_manual(values = c(
    "mixed evidence" = "darkgrey",
    "no sediment effect" = "#009E73",
    "sediment effect likely" = "#E69F00",
    "sediment effect very likely" = "#D55E00")) +
  theme(legend.title = element_blank(), axis.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle("COMIDS with at least 2 sample events from both BURP and PIBO")
comparison_plot_usfs_pibo

ggsave("./figures/comparison_plot_burp_pibo.png",
       comparison_plot_usfs_pibo,
       width = 8.5, height = 10.0, units = "in", dpi = 600)

# plot COMIDs with both DEQ & BLM, >=2 samples from each source

comparison_plot_blm <-
  predictions %>%
  filter(comid %in% burp_and_blm_multiple$comid) %>%
  ggplot(aes(x = year_numeric, y = source, color = framework_prediction)) +
  geom_point() +
  facet_wrap(~comid) +
  theme_bw() +
  scale_color_manual(values = c(
    "mixed evidence" = "darkgrey",
    "no sediment effect" = "#009E73",
    "sediment effect likely" = "#E69F00",
    "sediment effect very likely" = "#D55E00")) +
  theme(legend.title = element_blank(), axis.title = element_blank()) +
  theme(legend.position = "top") +
  ggtitle("COMIDS with at least 2 sample events from both BURP and BLM-AIM")
comparison_plot_blm

ggsave("./figures/comparison_plot_blm.png", comparison_plot_blm,
       width = 8.0, height = 4.0, units = "in", dpi = 600)

# plot OE vs framework prediction, using all sites with predictions-------------


for_OE_boxplot <-
  predictions %>%
  filter(source != "DEQ BURP") %>%
  select(source, siteid, sampleid, sample_date, siteclass, order, blm_ecoregion,
         pct_fine, sample_FSBI, has_both, OE, OE_benchmark, SMI2, SMI2_benchmark,
         framework_prediction, OE_prediction, BLM_prediction, SMI2_prediction, 
         source_sitetype) %>%
  mutate(site_category = ifelse(source_sitetype %in% c("Managed", "sediment degraded"), 
                                "non-reference",
                         ifelse(is.na(source_sitetype), "non-reference", source_sitetype))) %>%
  melt(id.vars = c("source", "siteid", "sampleid", "sample_date", "siteclass", "order", "blm_ecoregion",
                   "pct_fine", "sample_FSBI",
                   "has_both", "OE", "OE_benchmark", "SMI2", "SMI2_benchmark", "source_sitetype",
                   "site_category"))


# plot OE vs. sed predeiction outcome
OE_boxplot <-
  for_OE_boxplot %>%
  filter(variable == "framework_prediction") %>%
  filter(!is.na(value)) %>%
  mutate(label_order = factor(value, 
          levels = c("no sediment effect", "mixed evidence",
                          "sediment effect likely", 
                          "sediment effect very likely"))) %>%
  ggplot(aes(x = label_order, y = OE)) +
  geom_boxplot() +
  facet_wrap(~source, nrow= 2) +
  theme_bw() +
  labs(y = "macroinvertebrate O/E") +
  theme(axis.title.x = element_blank(),
         legend.title = element_blank())
OE_boxplot

ggsave("./figures/framework_classification_OE.png", OE_boxplot,
       width = 8, height = 4, units = "in", dpi = 600)

# calculate framework exceedance frequency at reference sites-------------------

for_frequency_tbl <-
  predictions %>%
  filter(siteid %in% time_series_sites$siteid) %>%
  filter(source_sitetype %in% c("Reference", "sediment reference")) %>%
  select(source, huc4code, siteid, stream, siteclass, order, framework_prediction) %>%
  melt(id.vars = c("source", "huc4code", "siteid", "stream", "siteclass", "order"),
       variable.name = "parameter", value.name = "value") %>%
  group_by(source, huc4code, siteid, stream, siteclass, order, value) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  dcast(source + huc4code + siteid + stream + siteclass + order ~ value,
        value.var = "n")

for_frequency_tbl_yrs <-
  predictions %>%
  filter(siteid %in% time_series_sites$siteid) %>%
  filter(source_sitetype %in% c("Reference", "sediment reference")) %>%
  select(siteid, year) %>%
  group_by(siteid) %>%
  summarize(n_years = n(),
            min_year = min(year),
            max_year = max(year))

frequency_table <-
  merge(for_frequency_tbl, for_frequency_tbl_yrs, by = "siteid") %>%
  select(source, huc4code, siteid, stream, siteclass, order, 
         n_years, min_year, max_year, `mixed evidence`, 
         `no sediment effect`, `sediment effect likely`) %>%
  mutate(false_positive_pct = (`sediment effect likely` / n_years) * 100)

