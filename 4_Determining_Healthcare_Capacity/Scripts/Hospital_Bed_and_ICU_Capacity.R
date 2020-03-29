# Loading in Libraries and Relevant Functions/Packages
library(tidyverse); library(zoo); library(dismo); library(conflicted); library(gbm); 
library(mltools); library(data.table); library(gtools); library(patchwork)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("area", "patchwork")

# Load In Data Used Across Multiple Analyses
world_bank_dd <- read.csv("Data/World_Bank_Country_Metadata.csv", 
                          stringsAsFactors = FALSE, header = TRUE, 
                          fileEncoding = 'UTF-8-BOM') %>%
  select(country_code, region, income_group)
countries <- world_bank_dd$country_code[world_bank_dd$income_group %in% 
                                          c("Low income", "Lower middle income", 
                                            "Upper middle income","High income")]

# Set Working Directory to Specific Folder
setwd("4_Determining_Healthcare_Capacity/")

# Sourcing Functions
source("Functions/Healthcare_Capacity_Functions.R")
source("Scripts/World_Bank_Data_Loading.R")

# Set Seed and Toggle Whether Fresh Run Required
set.seed(1020191)
fresh_run <- FALSE

# Load In World Bank Hospital Beds Data and Changing Column Names 
raw_hospital_beds <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Hospital_Beds.csv", 
                              stringsAsFactors = FALSE, header = TRUE, 
                              fileEncoding = 'UTF-8-BOM')
old_names <- colnames(raw_hospital_beds)[3:length(colnames(raw_hospital_beds))]
new_names <- gsub('X', '', old_names)
colnames(raw_hospital_beds)[3:length(colnames(raw_hospital_beds))] <- new_names

# Appending Region and Income Group, Filtering Away Rows With No Data
raw_hospital_beds <- append_info(raw_hospital_beds, world_bank_dd, countries)
hospital_beds <- raw_hospital_beds %>%
  mutate(region_new = factor(as.numeric(as.factor(region)))) %>%
  mutate(income_group_new = factor(as.numeric(as.factor(income_group)))) %>%
  select(country_name, country_code, region, income_group, region_new, income_group_new, everything())
index <- min(which(!is.na(as.numeric(colnames(hospital_beds)))))
rows_all_NAs <- detect_rows_all_NAs(hospital_beds[index:length(hospital_beds)])
hospital_beds <- hospital_beds[-rows_all_NAs, ]
hospital_beds <- hospital_beds %>%
  gather(year, hospital_beds, -country_name, -country_code, -region, 
         -income_group, -region_new, -income_group_new)

# Most Recent Hospital Bed Estimates
raw_most_recent <- hospital_beds %>%
  filter(!is.na(hospital_beds)) %>%
  group_by(country_code) %>%
  filter(year == max(year))
raw_most_recent <- table(raw_most_recent$year) 

# Adding In Relevant Covariates and One Hot Encoding Where Relevant
model_inputs <- hospital_beds %>%
  left_join(maternal_mortality, by = c("country_code", "year")) %>%
  left_join(electricity, by = c("country_code", "year")) %>%
  left_join(prop_pop, by = c("country_code", "year")) %>%
  left_join(school_ratio, by = c("country_code", "year")) %>%
  left_join(rural, by = c("country_code", "year")) %>%
  left_join(domestic_GDP, by = c("country_code", "year")) %>%
  left_join(infant_mortality, by = c("country_code", "year")) %>%
  left_join(school_enrollment, by = c("country_code", "year")) 
region_one_hot <- unname(as.matrix(one_hot(as.data.table(model_inputs[, c("hospital_beds", "region_new")]), cols = "region_new"))[, -1])
income_one_hot <- unname(as.matrix(one_hot(as.data.table(model_inputs[, c("hospital_beds", "income_group_new")]), cols = "income_group_new"))[, -1])
model_inputs <- model_inputs %>%
  cbind(region_one_hot) %>%
  cbind(income_one_hot)
index <- which(colnames(model_inputs) == "school_enrollment")
colnames(model_inputs)[(index + 1):length(colnames(model_inputs))] <- c("region_1", "region_2", "region_3", "region_4", "region_5", "region_6", "region_7", "income_1", "income_2", "income_3", "income_4") 
comp_model_inputs <- model_inputs %>%
  filter_all(all_vars(!is.na(.))) %>%
  select(-("region_5")) %>% # remove region 5 as no country in North America has complete covariate info 
  mutate(year = as.numeric(year))

# Creating the Dataframe for Input into the GBM Machinery 
gbm_data <- data.frame(hospital_bed = comp_model_inputs$hospital_beds, 
                       maternal_mortality = comp_model_inputs$maternal_mortality,
                       electricity = comp_model_inputs$electricity,
                       prop_pop = comp_model_inputs$prop_pop,
                       school_ratio = comp_model_inputs$school_ratio,
                       rural = comp_model_inputs$rural,
                       domestic_GDP = comp_model_inputs$domestic_GDP,
                       infant_mortality = comp_model_inputs$infant_mortality,
                       school_enrollment = comp_model_inputs$school_enrollment,
                       region_1 = comp_model_inputs$region_1,
                       region_2 = comp_model_inputs$region_2,
                       region_3 = comp_model_inputs$region_3,
                       region_4 = comp_model_inputs$region_4,
                       region_6 = comp_model_inputs$region_6,
                       region_7 = comp_model_inputs$region_7,
                       income_1 = comp_model_inputs$income_1,
                       income_2 = comp_model_inputs$income_2,
                       income_3 = comp_model_inputs$income_3,
                       income_4 = comp_model_inputs$income_4)

# Running the Boosted Regression Tree Algorithm and Examining Raw Outputs
tree_complexity <- 12
bag_fraction <- 0.65
max_trees <- 25000
learning_rate <- 0.01
if (fresh_run == TRUE) {
  world_bank_brt <- gbm.step(data = gbm_data, gbm.x = 2:ncol(gbm_data), gbm.y = 1, family = "gaussian", tree.complexity = tree_complexity, 
                             learning.rate = learning_rate, bag.fraction = bag_fraction, max.trees = max_trees, n.folds = 10)
  saveRDS(world_bank_brt, file = "Outputs/Hospital_Bed_Capacity_BRT.rds")
} else {
  world_bank_brt <- readRDS(file = "Outputs/Hospital_Bed_Capacity_BRT.rds")
}
summary(world_bank_brt)

# Using the Boosted Regression Tree to Predict Hosptial Bed Capacity and Compare to Observed
prediction_table <- model_inputs %>%
  filter_at(vars(-hospital_beds), all_vars(!is.na(.)))
predicted <- predict.gbm(world_bank_brt, prediction_table[, c("maternal_mortality", "electricity", "prop_pop", "school_ratio", "rural", "domestic_GDP", "infant_mortality", "school_enrollment", "region_1", "region_2", "region_3", "region_4", "region_6", "region_7", "income_1", "income_2", "income_3", "income_4")],
                         n.trees = world_bank_brt$gbm.call$best.trees, type = "response")
prediction_table$pred_beds <- predicted
observed_vs_predicted <- prediction_table %>%
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) %>%
  filter(!is.na(hospital_beds)) %>%
  select(income_group, hospital_beds, pred_beds)

a <- ggplot(observed_vs_predicted, aes(colour = income_group, x = hospital_beds, y = pred_beds)) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) + 
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  geom_point(stat = "identity", size = 2) +
  geom_segment(x = 0, y = 0, xend = 13, yend = 13, colour = "black", size = 1, linetype = 2) +
  xlim(c(0, 13)) +
  ylim(c(0, 13)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.3, 0.8), legend.title = element_blank(), 
        axis.title.y = element_text(size = 13.5, vjust = -15), axis.title.x = element_text(size = 13.5, vjust = +0),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold")) +
  labs(x = "Observed Hospital Beds /1000 People", y = "Predicted Hospital Beds\n /1000 People") 

# Selecting the Most Recent Predictions
most_recent <- prediction_table %>%
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) %>%
  mutate(beds_to_use = ifelse(!is.na(hospital_beds), hospital_beds, pred_beds)) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(country_code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(country_name, year, income_group, region, hospital_beds, pred_beds, beds_to_use)
table(most_recent$year)
saveRDS(most_recent, "Outputs/Hospital_Bed_Capacity_Predictions.Rds")

b <- ggplot(most_recent, aes(col = income_group, x = income_group, y = pred_beds)) +
  scale_colour_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) + 
  scale_fill_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) + 
  geom_boxplot(size = 1.5, outlier.shape = NA) +
  geom_jitter(data = most_recent, aes(x = income_group, y = pred_beds, fill = income_group), 
              pch = 21, size = 3, width = 0.2) +
  scale_x_discrete(labels = c("Low Income", "Lower Middle\nIncome", "Upper Middle\nIncome", "High Income")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none", axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +3), 
        axis.title.x = element_text(size = 13.5, vjust = -1), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.75, "cm")) +
  labs(x = "World Bank Income Group", y = "Hospital Beds /1000 People") 

income_status <- most_recent %>%
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) %>%
  group_by(income_group) %>%
  summarise(hosp_mean_pred = mean(pred_beds), hosp_median_pred = median(pred_beds), hosp_lower = quantile(pred_beds, 0.25), hosp_upper = quantile(pred_beds, 0.75)) %>%
  select(-hosp_mean_pred)

# ICU Bed Capacity 
raw_ICU <- read.csv("Data/ICU_Capacity_Data/ICU_Mini_Systematic_Review.csv")
ICU <- raw_ICU %>%
  select(Country_Code, ICU_Per_100_Hospital_Beds) %>%
  rename(ICU_Beds = ICU_Per_100_Hospital_Beds, country_code = Country_Code) %>%
  filter(!is.na(ICU_Beds), ICU_Beds < 20) %>% # filter anomalously high Cambodian datapoint
  left_join(world_bank_dd, by = "country_code") %>%
  select(-region)
ICU_income_strata <- ICU %>%
  group_by(income_group) %>%
  summarise(ICU_median = median(ICU_Beds),
            ICU_lower = quantile(ICU_Beds, 0.25), 
            ICU_upper = quantile(ICU_Beds, 0.75)) %>%
  filter(!is.na(income_group)) %>%
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", 
                                                        "Upper middle income", "High income"))) %>%
  arrange(income_group)
hosp_and_ICU_beds <- income_status %>%
  left_join(ICU_income_strata, by = "income_group")
saveRDS(hosp_and_ICU_beds, "Outputs/Income_Strata_Predicted_Hospital_and_ICU_Beds.Rds")

c <- ggplot(hosp_and_ICU_beds, aes(fill = income_group, x = income_group, y = ICU_median)) +
  scale_fill_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                    values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) + 
  scale_x_discrete(labels = c("Low Income", "Lower Middle\nIncome", "Upper Middle\nIncome", "High Income")) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ICU_lower, ymax = ICU_upper), width = 0.2) + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), 
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13.5, vjust = +2)) + 
  labs(y = "Number ICU Beds /100 Hospital Beds")

# Figure creation
layout <- "AABBB
           AABBB
           AABBB
           CCBBB
           CCBBB"

figure <- a + b + c +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 25, face = "bold"))
figure # save this manually width = 12, height = 6.7

