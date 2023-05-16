################################################################################
# Logistic regression between WetSF2.5 and FSBI
# SME50 & SME75 calculation
# Jason Williams, IDEQ Lewiston Office
# last update: May 2023
################################################################################

library(dplyr)
library(ggplot2)

# read in data------------------------------------------------------------------


project_data <-read.csv("./formatted_data/project_data.csv", header = TRUE)
str(project_data)

framework_benchmarks <-read.csv("./formatted_data/framework_benchmarks.csv", header = TRUE)
str(framework_benchmarks)


# site counts-------------------------------------------------------------------


reg_site_counts <-
  project_data %>%
  filter(source == "DEQ BURP") %>%
  filter(has_both == "Y") %>%
  filter(order %in% c(1,2,3, 4)) %>%
  select(siteid, order, siteclass, pct_fine, sample_FSBI) %>%
  group_by(siteclass, order) %>%
  summarise(site_count = n())


# plot fines 2.5 vs FSBI by siteclass and order---------------------------------


burp_fines_vs_fsbi <-
  project_data %>%
  filter(source == "DEQ BURP") %>%
  filter(has_both == "Y") %>%
  filter(order %in% c(1,2,3, 4)) %>%
  mutate(pointcolor = ifelse(source_sitetype == "Reference", "reference (1998-2007)", 
                             "non-reference (1998-2021)")) %>%
  ggplot(aes(x = pct_fine, y = sample_FSBI, 
             color = pointcolor, shape = pointcolor)) +
  geom_point() +
  facet_grid(order~siteclass) +
  theme_bw() +
  scale_color_manual(values = c("darkgrey", "#56B4E9", "#D55E00")) +
  scale_shape_manual(values = c(1, 17)) +
  labs(x = "% fines < 2.5 mm", y = "FSBI") +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_hline(data = subset(framework_benchmarks, order <5),
             aes(yintercept = framework_FSBI_ref_benchmark), 
             linetype = "dashed") +
  geom_vline(data = subset(framework_benchmarks, order <5),
             aes(xintercept = framework_fines_ref_benchmark), 
             linetype = "dashed") +
  geom_vline(data = subset(framework_benchmarks, order <5),
             aes(xintercept = framework_SME75_benchmark), 
             linetype = "dashed", color = "#D55E00")
burp_fines_vs_fsbi

ggsave("./figures/burp_fines_vs_fsbi.png", burp_fines_vs_fsbi, 
       width = 8.0, height = 8.0, units = "in", dpi = 600)


# format dataset for logistic regression----------------------------------------

for_logistic_reg <-
  project_data %>%
  filter(source == "DEQ BURP") %>%
  filter(has_both == "Y") %>%
  filter(order %in% c(1,2,3, 4))

str(for_logistic_reg)


# logistic regression model function--------------------------------------------
# to be applied by siteclass/order combo

logreg_fun <-function(str_order, site_class, FSBI_ref) {
  
  
  # filter
  for_reg <-
    for_logistic_reg %>%
    filter(order == str_order) %>%
    filter(siteclass == site_class) %>%
    select(siteid, pct_fine, sample_FSBI, framework_FSBI_ref_benchmark) %>%
    mutate(FSBI_ref_exceeded = ifelse(sample_FSBI < framework_FSBI_ref_benchmark, 1, 0))
  
  str(for_reg)
  
  # model
  logreg_model <-glm(FSBI_ref_exceeded~pct_fine, data = for_reg, 
                     family = binomial())
  summary(logreg_model)
  
  
  # chi square test
  null_deviance <-logreg_model$null.deviance
  deviance <-logreg_model$deviance
  model_Chi <-null_deviance-deviance
  chi_df <-logreg_model$df.null - logreg_model$df.residual
  chisq_prob <-1-pchisq(model_Chi, chi_df)
  
  # R2 (Homer and Lemeshow)
  r2.hl <-model_Chi/logreg_model$null.deviance 
  r2.hl
  
  # R2 (Cox and Snell)
  r2.cs <-1-exp( -(logreg_model$null.deviance = logreg_model$deviance) / length(logreg_model$fitted.values))
  r2.cs
  
  # R2 Naglerke
  r2.n <-r2.cs / (1 - ( exp (-(logreg_model$null.deviance / length(logreg_model$fitted.values)))))
  r2.n
  
  # odds ratio
  exp(logreg_model$coefficients)
  odds <-exp(confint(logreg_model))
  
  # wet fines at 75% probability of exceeding reference
  linedata <-data.frame(pct_fine= seq(from = 0, to = 100, by = 1))
  linedata$predicted = predicted_prob = predict(logreg_model, linedata, type = "response")
  
  percentile50 <-
    linedata %>%
    filter(predicted >=0.50 & predicted <=0.53)
  
  percentile <-
    linedata %>%
    filter(predicted >=0.74 & predicted <=0.76)
  
  # print outputs
  
  print("logistic regression model summary")
  print(summary(logreg_model))
  print("NOTE: use Model Chi, not function output summary null & residual deviance")
  
  print("null deviance, residual deviance, Model Chi, df")
  print(null_deviance)
  print(deviance)
  print(model_Chi)
  print(chi_df)
  
  print("pchisq value")
  print(pchisq(model_Chi, chi_df))
  
  print("Chi square prob")
  print(chisq_prob)
  
  print("Homer and Lemeshow R2")
  print(r2.hl)
  
  print("Cox Snell R2")
  print(r2.cs)
  
  print("Nagelerke R2")
  print(r2.n)
  
  print("Odds Ratio and confidence intervals")
  print(odds)
  
  print("fines at 51% probability of exceeding reference FSBI")
  print(percentile50)
  
  print("fines at 75% probability of exceeding reference FSBI")
  print(percentile)
  
  # # plot logistic binary data and predicted probability of exceedance
  
  logreg_plot <-
    ggplot() +
    geom_point(data = for_reg, aes(x = pct_fine, y = FSBI_ref_exceeded)) +
    geom_line(data = linedata, aes(x = pct_fine, y = predicted_prob)) +
    theme_bw() +
    labs(x = "fines < 2.5 mm", y = "FSBI < reference") +
    ggtitle(paste(site_class, "", "order ", str_order))
  
  print(logreg_plot)
  
} #end logreg fun


# apply function----------------------------------------------------------------

logreg_fun(4, "Mountains", 145)
