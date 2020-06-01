# Loading Required Libraries
library(tidyverse); library(zoo); library(squire)

# Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading Functions and Relevant Data
source("Functions/Trigger_Running_Functions.R")
income_strata_healthcare_capacity <- squire::income_strata_healthcare_capacity

# Trigger Thresholds to Use During Model Running
trigger_thresholds <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120,
                        140, 160, 180, 200, 235, 270, 300, 335, 370, 400, 450, 500,
                        600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2250, 2500,
                        2750, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 8000, 10000,
                        20000, 30000, 40000, 50000, 75000, 100000)

# Loading in ICU Capacity
ICU <- income_strata_healthcare_capacity$ICU_beds
LIC_icu <- (ICU[1] * 50000000 /1000) / 2
LMIC_icu <- (ICU[2] * 50000000 /1000) / 2
UMIC_icu <- (ICU[3] * 50000000 /1000) / 2
HIC_icu <- (ICU[4] * 50000000 /1000) / 2

# Defining Parameters Used in All Model Runs
replicates <- 30
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
suppression_reduction <- 0.25
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 30

### 1. Running Without Capacity Constraints to Examine Time In Suppression vs Capacity Required
###     -> For Runs Where We Have an Initial Suppression Based On Known Timings for Income Strata
income_strata <- c("LIC", "LMIC", "UMIC", "HIC")
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
raw_death_triggers <- c(0, 0, 0.044, 0.202)
death_triggers <- round(50 * raw_death_triggers)
a_max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
a_time_in_lockdown <- matrix(nrow = 4, ncol = length(trigger_thresholds))
a_deaths <- matrix(nrow = 4, ncol = length(trigger_thresholds))
a_list <- vector(mode = "list", length = 4)
for (i in 1:length(countries)) {
  pop <- get_population(countries[i])
  pop <- (50000000/sum(pop$n)) * pop$n
  for (j in 1:length(trigger_thresholds)) {
    x <- realistic_run_trigger_threshold(country = countries[i], population = pop,
                                         replicates = replicates,
                                         income_strata = income_strata[i],
                                         initial_trigger_threshold = death_triggers[i],
                                         trigger_threshold = trigger_thresholds[j],
                                         suppression_reduction = suppression_reduction,
                                         suppression_duration = suppression_duration,
                                         mitigation_reduction = mitigation_reduction,
                                         R0 = R0, tt_R0 = tt_R0,
                                         max_lockdowns = max_lockdowns,
                                         hospital_bed_capacity = 10000000,
                                         ICU_bed_capacity = 10000000,
                                         income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                         poorer_outcomes = FALSE)
    a_max_ICU_req[i, j] <- median(get_max_ICU_req(x))
    a_time_in_lockdown[i, j] <- median(get_time_in_lockdown(x))
    a_deaths[i, j] <- median(get_total_deaths(x))
    print(j)
  }
}

colnames(a_max_ICU_req) <- paste0("ICU_inc", as.integer(trigger_thresholds))
a_max_ICU <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), a_max_ICU_req)
a_max_ICU <- tidyr::gather(a_max_ICU, threshold, max_capacity, -setting)

colnames(a_time_in_lockdown) <- paste0("ICU_inc", as.integer(trigger_thresholds))
a_lockdown_time <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), a_time_in_lockdown)
a_lockdown_time <- tidyr::gather(a_lockdown_time, threshold, time_in_lockdown, -setting)

colnames(a_deaths) <- paste0("ICU_inc", as.integer(trigger_thresholds))
a_total_deaths <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), a_deaths)
a_total_deaths <- tidyr::gather(a_total_deaths, threshold, deaths, -setting)

capacity_vs_time <- a_max_ICU %>%
  left_join(a_lockdown_time, by = c("setting", "threshold")) %>%
  left_join(a_total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold, levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) 
saveRDS(capacity_vs_time, "capacity_vs_time.rds")

### 2. Running With Capacity Constraints to Examine Time In Suppression vs Deaths
###     -> For Runs Where We Have an Initial Suppression Based On Known Timings for Income Strata
income_strata <- c("LIC", "LIC", "LMIC", "LMIC", "UMIC", "HIC")
poorer_outcomes <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
countries <- c("Madagascar", "Madagascar", "Nicaragua", "Nicaragua", "Grenada", "Malta")
raw_death_triggers <- c(0, 0, 0, 0, 0.044, 0.202)
death_triggers <- round(50 * raw_death_triggers)
b_max_ICU_req <- matrix(nrow = 6, ncol = length(trigger_thresholds))
b_time_in_lockdown <- matrix(nrow = 6, ncol = length(trigger_thresholds))
b_deaths <- matrix(nrow = 6, ncol = length(trigger_thresholds))
b_list <- vector(mode = "list", length = 6)
for (i in 1:length(countries)) {
  pop <- get_population(countries[i])
  pop <- (50000000/sum(pop$n)) * pop$n
  for (j in 1:length(trigger_thresholds)) {
    x <- realistic_run_trigger_threshold(country = countries[i], population = pop,
                                         replicates = replicates,
                                         income_strata = income_strata[i],
                                         initial_trigger_threshold = death_triggers[i],
                                         trigger_threshold = trigger_thresholds[j],
                                         suppression_reduction = suppression_reduction,
                                         suppression_duration = suppression_duration,
                                         mitigation_reduction = mitigation_reduction,
                                         R0 = R0, tt_R0 = tt_R0,
                                         max_lockdowns = max_lockdowns,
                                         hospital_bed_capacity = NULL,
                                         ICU_bed_capacity = NULL,
                                         income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                         poorer_outcomes = poorer_outcomes[i])
    b_max_ICU_req[i, j] <- median(get_max_ICU_req(x))
    b_time_in_lockdown[i, j] <- median(get_time_in_lockdown(x))
    b_deaths[i, j] <- median(get_total_deaths(x))
    print(j)
  }
}
colnames(b_max_ICU_req) <- paste0("ICU_inc", as.integer(trigger_thresholds))
b_max_ICU <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), b_max_ICU_req)
b_max_ICU <- gather(b_max_ICU, threshold, max_capacity, -setting)

colnames(b_time_in_lockdown) <- paste0("ICU_inc", as.integer(trigger_thresholds))
b_lockdown_time <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), b_time_in_lockdown)
b_lockdown_time <- gather(b_lockdown_time, threshold, time_in_lockdown, -setting)

colnames(b_deaths) <- paste0("ICU_inc", as.integer(trigger_thresholds))
b_total_deaths <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), b_deaths)
b_total_deaths <- gather(b_total_deaths, threshold, deaths, -setting)

deaths_vs_time <- b_max_ICU %>%
  left_join(b_lockdown_time, by = c("setting", "threshold")) %>%
  left_join(b_total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold, levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) 
saveRDS(deaths_vs_time, "deaths_vs_time.rds")

