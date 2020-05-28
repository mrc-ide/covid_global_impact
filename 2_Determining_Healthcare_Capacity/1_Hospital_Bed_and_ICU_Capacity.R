# Loading in Libraries and Relevant Functions/Packages
library(tidyverse); library(zoo); library(dismo); library(conflicted); library(gbm); 
library(mltools); library(data.table); library(gtools); library(patchwork); 
library(squire); library(gridExtra); library(squire)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("area", "patchwork")

# Set Working Directory
setwd("2_Determining_Healthcare_Capacity/")

# Sourcing Functions
source("Functions/Healthcare_Capacity_Functions.R")

# Set Seed and Toggle Whether Fresh Run Required
set.seed(1020191)
fresh_run <- TRUE

# Loading In World Bank Covariates
## Maternal Mortality
maternal_mortality <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Maternal_Mortality_Ratio.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(maternal_mortality)[2:length(colnames(maternal_mortality))] <- gsub('X', '', colnames(maternal_mortality)[2:length(colnames(maternal_mortality))])
maternal_mortality <- maternal_mortality %>%
  gather(year, maternal_mortality, -country_code)

## Electricity
electricity <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Electricity.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(electricity)[2:length(colnames(electricity))] <- gsub('X', '', colnames(electricity)[2:length(colnames(electricity))])
electricity <- electricity %>%
  gather(year, electricity, -country_code)

## Proportion Population 0-14
prop_pop <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Pop_Prop.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(prop_pop)[2:length(colnames(prop_pop))] <- gsub('X', '', colnames(prop_pop)[2:length(colnames(prop_pop))])
prop_pop <- prop_pop %>%
  gather(year, prop_pop, -country_code)

## Teacher Pupil Ratio 
school_ratio <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Teacher_Pupil.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(school_ratio)[2:length(colnames(school_ratio))] <- gsub('X', '', colnames(school_ratio)[2:length(colnames(school_ratio))])
school_ratio <- school_ratio %>%
  gather(year, school_ratio, -country_code)

## Rural
rural <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Rural_Percentage.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(rural)[2:length(colnames(rural))] <- gsub('X', '', colnames(rural)[2:length(colnames(rural))])
rural <- rural %>%
  gather(year, rural, -country_code)

## Domestic Expenditure
domestic_GDP <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Domestic_Healthcare_Expenditure_GDP.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(domestic_GDP)[2:length(colnames(domestic_GDP))] <- gsub('X', '', colnames(domestic_GDP)[2:length(colnames(domestic_GDP))])
domestic_GDP <- domestic_GDP %>%
  gather(year, domestic_GDP, -country_code)

## Infant Mortality
infant_mortality <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Infant_Mortality_Ratio.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(infant_mortality)[2:length(colnames(infant_mortality))] <- gsub('X', '', colnames(infant_mortality)[2:length(colnames(infant_mortality))])
infant_mortality <- infant_mortality %>%
  gather(year, infant_mortality, -country_code)

## School Enrollment
school_enrollment <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_School_Enrollment.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(school_enrollment)[2:length(colnames(school_enrollment))] <- gsub('X', '', colnames(school_enrollment)[2:length(colnames(school_enrollment))])
school_enrollment <- school_enrollment %>%
  gather(year, school_enrollment, -country_code)

# Load In World Bank Data Dictionary With Information About Income Group
world_bank_dd <- read.csv("Data/World_Bank_Country_Metadata.csv", 
                          stringsAsFactors = FALSE, header = TRUE, 
                          fileEncoding = 'UTF-8-BOM') %>%
  select(country_code, region, income_group)
countries <- world_bank_dd$country_code[world_bank_dd$income_group %in% 
                                          c("Low income", "Lower middle income", 
                                            "Upper middle income","High income")]

# Load In World Bank Hospital Beds Data and Changing Column Names 
raw_hospital_beds <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Hospital_Beds.csv", 
                              stringsAsFactors = FALSE, header = TRUE, 
                              fileEncoding = 'UTF-8-BOM')
old_names <- colnames(raw_hospital_beds)[3:length(colnames(raw_hospital_beds))]
new_names <- gsub('X', '', old_names)
colnames(raw_hospital_beds)[3:length(colnames(raw_hospital_beds))] <- new_names

# Appending Region and Income Group
raw_hospital_beds <- append_info(raw_hospital_beds, world_bank_dd, countries)
hospital_beds <- raw_hospital_beds %>%
  mutate(region_new = factor(as.numeric(as.factor(region)))) %>%
  mutate(income_group_new = factor(as.numeric(as.factor(income_group)))) %>%
  select(country_name, country_code, region, income_group, region_new, income_group_new, everything())

# Filtering Away Rows With No Data For Any of the Years 
index <- min(which(!is.na(as.numeric(colnames(hospital_beds)))))
rows_all_NAs <- detect_rows_all_NAs(hospital_beds[index:length(hospital_beds)])
hospital_beds <- hospital_beds[-rows_all_NAs, ]
hospital_beds <- hospital_beds %>%
  gather(year, hospital_beds, -country_name, -country_code, -region, 
         -income_group, -region_new, -income_group_new)

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
  theme(legend.position = c(0.3, 0.8), legend.title = element_blank(), 
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
saveRDS(most_recent, "Outputs/Hospital_Bed_Capacity_Predictions.Rds")

b <- ggplot(most_recent, aes(col = income_group, x = income_group, y = pred_beds)) +
  scale_colour_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) + 
  scale_fill_manual(values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60")) + 
  geom_boxplot(size = 1.5, outlier.shape = NA) +
  geom_jitter(data = most_recent, aes(x = income_group, y = pred_beds, fill = income_group), 
              pch = 21, size = 3, width = 0.2) +
  scale_x_discrete(labels = c("Low Income", "Lower Middle\nIncome", "Upper Middle\nIncome", "High Income")) + 
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, face = "bold"),
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

# Loading In Results from ICU Bed Capacity Systematic Review  
raw_ICU <- read.csv("Data/ICU_Capacity_Data/ICU_Beds_Per_Hospital_Bed_Systematic_Review_Results.csv")
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
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), 
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13.5, vjust = +2)) + 
  labs(y = "Number ICU Beds /100 Hospital Beds")

# Healthcare Quality and Its Impacts on Estimates of the IFR By Age
# Age groups
age_groups <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", 
                "45-50", "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80+")
ages <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 
          62.5, 67.5, 72.5, 77.5, 85)

# Probabilities of symptom type
prob_hosp <- c(0.000744192, 0.000634166, 0.001171109, 0.002394593, 0.005346437,
               0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042, 	
               0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064, 
               0.176634654, 0.18) # see squire documentation for full details
prob_severe <- c(0.05022296, 0.05022296, 0.05022296, 0.05022296, 0.05022296, 
                 0.05022296, 0.05022296, 0.053214942, 0.05974426, 0.074602879, 
                 0.103612417, 0.149427991, 0.223777304, 0.306985918, 0.385779555,
                 0.461217861, 0.709444444) # see squire documentation for full details
prob_non_severe <- 1 - prob_severe

# Probabilities of dying given symptom type and treatment
prob_severe_death_treatment <- rep(0.5, 17) # see squire documentation for full details
prob_severe_death_no_treatment <- rep(0.905, 17) # see squire documentation for full details
prob_non_severe_death_treatment <- c(0.0125702, 0.0125702, 0.0125702, 0.0125702, 0.0125702,
                                     0.0125702, 0.0125702, 0.013361147, 0.015104687, 0.019164124,
                                     0.027477519, 0.041762108, 0.068531658, 0.105302319, 0.149305732,
                                     0.20349534, 0.5804312) # see squire documentation for full details
prob_non_severe_death_poorer_outcomes <- c(rep(0.25, 16), 0.5804312) # see squire documentation for full details
prob_non_severe_death_no_treatment <- rep(0.6, 17) # see squire documentation for full details

# No Healthcare Quality Constraints 
scen_1 <- (prob_hosp * prob_severe * prob_severe_death_treatment) +
  (prob_hosp * prob_non_severe * prob_non_severe_death_treatment)

# No Mechanical Ventilation
scen_2 <- (prob_hosp * prob_severe * prob_severe_death_no_treatment) +
  (prob_hosp * prob_non_severe * prob_non_severe_death_treatment)

# Poorer Outcomes in Those Recieving Oxygen
scen_3 <- (prob_hosp * prob_severe * prob_severe_death_no_treatment) +
  (prob_hosp * prob_non_severe * prob_non_severe_death_poorer_outcomes)

# No Mechanical Ventilation & No Oxygen
scen_4 <- (prob_hosp * prob_severe * prob_severe_death_no_treatment) +
  (prob_hosp * prob_non_severe * prob_non_severe_death_no_treatment)


# Plotting Different Scenarios
scen_df <- data.frame(scenario = rep(c("scen_1", "scen_2", "scen_3", "scen_4"), each = 17),
                      age_group = rep(age_groups, 4),
                      prob = c(scen_1, scen_2, scen_3, scen_4)) %>%
  mutate(age_group = factor(age_group, levels = age_groups)) %>%
  spread(scenario, prob) %>%
  mutate(scen_2 = scen_2 - scen_1,
         scen_3 = scen_3 - scen_2 - scen_1,
         scen_4 = scen_4 - scen_3 - scen_2 - scen_1) %>%
  gather(scenario, prob, -age_group) %>%
  mutate(scenario = factor(scenario, levels = c("scen_4", "scen_3", "scen_2", "scen_1")))

d <- ggplot(scen_df, aes(x = age_group, y = prob, fill = scenario)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(y = "Infection Fatality Ratio", x = "Age Group (Years)") +
  scale_fill_manual(name = "Healthcare Quality",
                    values = c("#F5E3E0", "#E8B4BC", "#D282A6", "#6E4555"),
                    labels = c("No MV & No Oxygen", "No MV & Sub-Optimal Oxygen",  "No MV", "Baseline")) +
  theme(axis.text.y = element_text(size = 12),
        legend.position = "none")

# Calculating IFRs for Each Country Under the Different Healthcare Quality Scenarios 
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta") # representative of each income group
raw_IFRs <- matrix(nrow = 4, ncol = 4)

# Unlimited Healthcare
for (i in 1:4) {
  
  mixing_matrix <- get_mixing_matrix(countries[i])
  std_population <- generate_standard_population(countries[i])
  
  # Unlimited Capacity, Identical Quality Across All Settings
  x <- run_explicit_SEEIR_model(R0 = 3,
                                country = countries[i],
                                contact_matrix_set = mixing_matrix,
                                population = std_population,
                                hosp_bed_capacity = 100000000,
                                ICU_bed_capacity = 100000000,
                                replicates = 20,
                                dt = 0.1,
                                day_return = TRUE)

  output <- format_output(x, var_select = c("deaths", "infections")) %>%
    mutate(t = factor(t)) %>%
    group_by(compartment, replicate) %>%
    summarise(total = sum(y)) %>%
    group_by(compartment) %>%
    summarise(median = median(total))
  
  deaths <- output$median[output$compartment == "deaths"]
  infections <- output$median[output$compartment == "infections"]
  raw_IFRs[i, 1] <- 100 * deaths/infections
  
  # No ICU Capacity, Identical, High Quality Across All Settings
  x <- run_explicit_SEEIR_model(R0 = 3,
                                country = countries[i],
                                contact_matrix_set = mixing_matrix,
                                population = std_population,
                                hosp_bed_capacity = 100000000,
                                ICU_bed_capacity = 0,
                                replicates = 20,
                                dt = 0.1,
                                day_return = TRUE)
  
  output <- format_output(x, var_select = c("deaths", "infections")) %>%
    mutate(t = factor(t)) %>%
    group_by(compartment, replicate) %>%
    summarise(total = sum(y)) %>%
    group_by(compartment) %>%
    summarise(median = median(total))
  
  deaths <- output$median[output$compartment == "deaths"]
  infections <- output$median[output$compartment == "infections"]
  raw_IFRs[i, 2] <- 100 * deaths/infections
  
  # Limited ICU Capacity, Identical, Poorer General Hopsital Bed Care Quality 
  # Across All Settings (Presented Only for LIC and LMIC Though)
  x <- run_explicit_SEEIR_model(R0 = 3,
                                country = countries[i],
                                contact_matrix_set = mixing_matrix,
                                population = std_population,
                                hosp_bed_capacity = 100000000,
                                ICU_bed_capacity = 0,
                                replicates = 20,
                                prob_non_severe_death_treatment = c(rep(0.25, 16), 0.5804312),
                                dt = 0.1,
                                day_return = TRUE)
  
  output <- format_output(x, var_select = c("deaths", "infections")) %>%
    mutate(t = factor(t)) %>%
    group_by(compartment, replicate) %>%
    summarise(total = sum(y)) %>%
    group_by(compartment) %>%
    summarise(median = median(total))
  
  deaths <- output$median[output$compartment == "deaths"]
  infections <- output$median[output$compartment == "infections"]
  raw_IFRs[i, 3] <- 100 * deaths/infections
  
  # No Hospital (Oxygen) or ICU (MV) Capacity
  x <- run_explicit_SEEIR_model(R0 = 3,
                                country = countries[i],
                                contact_matrix_set = mixing_matrix,
                                population = std_population,
                                hosp_bed_capacity = 0,
                                ICU_bed_capacity = 0,
                                replicates = 20,
                                dt = 0.1,
                                day_return = TRUE)
  
  output <- format_output(x, var_select = c("deaths", "infections")) %>%
    mutate(t = factor(t)) %>%
    group_by(compartment, replicate) %>%
    summarise(total = sum(y)) %>%
    group_by(compartment) %>%
    summarise(median = median(total))
  
  deaths <- output$median[output$compartment == "deaths"]
  infections <- output$median[output$compartment == "infections"]
  raw_IFRs[i, 4] <- 100 * deaths/infections
  
  print(i)
}

row.names(raw_IFRs) <- c("LIC", "LMIC", "UMIC", "HIC")
raw_IFRs <- round(raw_IFRs, 2)
IFRs <- data.frame(raw_IFRs)
colnames(raw_IFRs) <- c("Baseline", "No MV", "No MV &\n Poorer\nOutcomes", "No MV &\nNo Oxygen")
e <- grid.table(IFRs)
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
e <- tableGrob(t(IFRs), rows=NULL, theme=tt)

# Figure Creation
layout <- "AABBB
AABBB
AABBB
CCBBB
CCBBB
DDEEE
DDEEE"

figure <- a + b + c + d + e + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 25, face = "bold"))
pdf("Figure_2_Healthcare_Capacity_and_Quality.pdf", width = 13, height = 9)
figure # save this manually width = 13, height = 9
dev.off()

