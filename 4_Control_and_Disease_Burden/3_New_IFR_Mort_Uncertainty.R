# Load Required Libraries
library(squire); library(tidyverse)

# Set Seed
set.seed(10001092)

# Source Functions
source("Functions/Healthcare_Demand_Functions.R")

# Loading Severity Parameter Posterior Draws
severity_params <- readRDS("Outputs/severity_param_sets.rds")
severity_params_LMIC <- readRDS("Outputs/severity_param_sets_lmic.rds")

# Representative Countries for Each of LIC, LMIC, UMIC and HIC and 
# Associated Parameters for Model Running
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
income_strata <- c("LIC", "LMIC", "UMIC", "HIC")
mitigated_R0 <- 3 * 0.55
number_draws <- 500

# Looping Over Severity Parameters and Calculating IFR/Deaths for Infinity Capacity
unlimited_capacity_IFR <- matrix(nrow = number_draws, ncol = 4)
unlimited_capacity_mort <- matrix(nrow = number_draws, ncol = 4)
for (i in 1:4) {
  
  mixing_matrix <- get_mixing_matrix(countries[i])
  std_population <- generate_standard_population(countries[i])

  for (j in 1:number_draws) {
    
    temp_sev_params <- severity_params[[j]]
    x <- run_explicit_SEEIR_model(R0 = mitigated_R0,
                                  contact_matrix_set = mixing_matrix,
                                  population = std_population,
                                  hosp_bed_capacity = 1000000000,
                                  ICU_bed_capacity = 1000000000,
                                  day_return = TRUE,
                                  time_period = 365,
                                  prob_hosp = temp_sev_params$prob_hosp,
                                  prob_severe = temp_sev_params$prob_severe,
                                  prob_non_severe_death_treatment = temp_sev_params$prob_non_severe_death_treatment,
                                  prob_non_severe_death_no_treatment = temp_sev_params$prob_non_severe_death_no_treatment,
                                  prob_severe_death_treatment = temp_sev_params$prob_severe_death_treatment,
                                  prob_severe_death_no_treatment = temp_sev_params$prob_severe_death_no_treatment,
                                  replicates = 10)
    
    output <- format_output(x, var_select = c("deaths", "infections")) %>%
      mutate(t = factor(t)) %>%
      group_by(compartment, replicate) %>%
      summarise(total = sum(y)) %>%
      group_by(compartment) %>%
      summarise(median = median(total))
    deaths <- output$median[output$compartment == "deaths"]
    infections <- output$median[output$compartment == "infections"]
    unlimited_capacity_IFR[j, i] <- deaths/infections
    unlimited_capacity_mort[j, i] <- deaths * (1000000/sum(std_population))
    print(j)
    
  }
}

# Gathering the Unmitigated Capacity Exceedance Dataset
unlimited_capacity_IFR <- data.frame(unlimited_capacity_IFR)
colnames(unlimited_capacity_IFR) <- income_strata
tidy_unlim_IFR <- unlimited_capacity_IFR %>%
  gather(setting, IFR)
tidy_unlim_IFR$scenario <- "unlimited_healthcare"

unlimited_capacity_mort <- data.frame(unlimited_capacity_mort)
colnames(unlimited_capacity_mort) <- income_strata
tidy_unlim_mort <- unlimited_capacity_mort %>%
  gather(setting, mort)
tidy_unlim_mort$scenario <- "unlimited_healthcare"


# Looping Over Severity Parameters and Calculating IFR/Deaths for Limited Capacity
limited_capacity_IFR <- matrix(nrow = number_draws, ncol = 4)
limited_capacity_mort <- matrix(nrow = number_draws, ncol = 4)
for (i in 1:4) {
  
  mixing_matrix <- get_mixing_matrix(countries[i])
  std_population <- generate_standard_population(countries[i])
  hosp_beds <- generate_hosp_beds(std_population, income_strata[i], income_strata_healthcare_capacity)
  ICU_beds <- generate_ICU_beds(std_population, income_strata[i], income_strata_healthcare_capacity)
  
  for (j in 1:number_draws) {
    
    temp_sev_params <- severity_params[[j]]
    x <- run_explicit_SEEIR_model(R0 = mitigated_R0,
                                  contact_matrix_set = mixing_matrix,
                                  population = std_population,
                                  hosp_bed_capacity = hosp_beds,
                                  ICU_bed_capacity = ICU_beds,
                                  day_return = TRUE,
                                  time_period = 365,
                                  prob_hosp = temp_sev_params$prob_hosp,
                                  prob_severe = temp_sev_params$prob_severe,
                                  prob_non_severe_death_treatment = temp_sev_params$prob_non_severe_death_treatment,
                                  prob_non_severe_death_no_treatment = temp_sev_params$prob_non_severe_death_no_treatment,
                                  prob_severe_death_treatment = temp_sev_params$prob_severe_death_treatment,
                                  prob_severe_death_no_treatment = temp_sev_params$prob_severe_death_no_treatment,
                                  replicates = 10)
    
    output <- format_output(x, var_select = c("deaths", "infections")) %>%
      mutate(t = factor(t)) %>%
      group_by(compartment, replicate) %>%
      summarise(total = sum(y)) %>%
      group_by(compartment) %>%
      summarise(median = median(total))
    deaths <- output$median[output$compartment == "deaths"]
    infections <- output$median[output$compartment == "infections"]
    limited_capacity_IFR[j, i] <- deaths/infections
    limited_capacity_mort[j, i] <- deaths * (1000000/sum(std_population))
    print(j)
    
  }
}

# Gathering the Unmitigated Capacity Exceedance Dataset
limited_capacity_IFR <- data.frame(limited_capacity_IFR)
colnames(limited_capacity_IFR) <- income_strata
tidy_lim_IFR <- limited_capacity_IFR %>%
  gather(setting, IFR)
tidy_lim_IFR$scenario <- "limited_healthcare"

limited_capacity_mort <- data.frame(limited_capacity_mort)
colnames(limited_capacity_mort) <- income_strata
tidy_lim_mort <- limited_capacity_mort %>%
  gather(setting, mort)
tidy_lim_mort$scenario <- "limited_healthcare"


# Looping Over Severity Parameters and Calculating IFR/Deaths for Limited Capacity & Poor Outcomes
#   Note: LIC and LMICs only 
poor_outcomes_capacity_IFR <- matrix(nrow = number_draws, ncol = 2)
poor_outcomes_mort <- matrix(nrow = number_draws, ncol = 2)
for (i in 1:2) {
  
  mixing_matrix <- get_mixing_matrix(countries[i])
  std_population <- generate_standard_population(countries[i])
  hosp_beds <- generate_hosp_beds(std_population, income_strata[i], income_strata_healthcare_capacity)
  ICU_beds <- generate_ICU_beds(std_population, income_strata[i], income_strata_healthcare_capacity)
  
  for (j in 1:number_draws) {
    
    temp_sev_params <- severity_params_LMIC[[j]]
    x <- run_explicit_SEEIR_model(R0 = mitigated_R0,
                                  contact_matrix_set = mixing_matrix,
                                  population = std_population,
                                  hosp_bed_capacity = hosp_beds + ICU_beds,
                                  ICU_bed_capacity = 0, 
                                  day_return = TRUE,
                                  time_period = 365,
                                  prob_hosp = temp_sev_params$prob_hosp,
                                  prob_severe = temp_sev_params$prob_severe,
                                  prob_non_severe_death_treatment = temp_sev_params$prob_non_severe_death_treatment,
                                  prob_non_severe_death_no_treatment = temp_sev_params$prob_non_severe_death_no_treatment,
                                  prob_severe_death_treatment = temp_sev_params$prob_severe_death_treatment,
                                  prob_severe_death_no_treatment = temp_sev_params$prob_severe_death_no_treatment,
                                  replicates = 10)
    
    output <- format_output(x, var_select = c("deaths", "infections")) %>%
      mutate(t = factor(t)) %>%
      group_by(compartment, replicate) %>%
      summarise(total = sum(y)) %>%
      group_by(compartment) %>%
      summarise(median = median(total))
    deaths <- output$median[output$compartment == "deaths"]
    infections <- output$median[output$compartment == "infections"]
    poor_outcomes_capacity_IFR[j, i] <- deaths/infections
    poor_outcomes_mort[j, i] <- deaths * (1000000/sum(std_population))
    print(j)
    
  }
}

# Gathering the Unmitigated Capacity Exceedance Dataset
poor_outcomes_capacity_IFR <- data.frame(poor_outcomes_capacity_IFR)
colnames(poor_outcomes_capacity_IFR) <- income_strata[1:2]
tidy_po_IFR <- poor_outcomes_capacity_IFR %>%
  gather(setting, IFR)
tidy_po_IFR$scenario <- "poorer_outcomes"

poor_outcomes_mort <- data.frame(poor_outcomes_mort)
colnames(poor_outcomes_mort) <- income_strata[1:2]
tidy_po_mort <- poor_outcomes_mort %>%
  gather(setting, mort)
tidy_po_mort$scenario <- "poorer_outcomes"


# Combining the three Dataframes Together and Saving
overall_IFR <- rbind(tidy_unlim_IFR, tidy_lim_IFR, tidy_po_IFR)
overall_IFR$setting <- factor(overall_IFR$setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))
overall_IFR$scenario <- factor(overall_IFR$scenario, levels = c("unlimited_healthcare", "limited_healthcare", "poorer_outcomes"))
saveRDS(overall_IFR, file = "Outputs/IFR_uncertainty.rds")

check_overall_IFR <- overall_IFR %>%
  group_by(setting, scenario) %>%
  summarise(median = median(IFR),
            lower = quantile(IFR, 0.25),
            upper = quantile(IFR, 0.75))

overall_mort <- rbind(tidy_unlim_mort, tidy_lim_mort, tidy_po_mort)
overall_mort$setting <- factor(overall_mort$setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))
overall_mort$scenario <- factor(overall_mort$scenario, levels = c("unlimited_healthcare", "limited_healthcare", "poorer_outcomes"))
saveRDS(overall_mort, file = "Outputs/Mort_uncertainty.rds")

check_overall_mort <- overall_mort %>%
  group_by(setting, scenario) %>%
  summarise(median = median(mort),
            lower = quantile(mort, 0.25),
            upper = quantile(mort, 0.75))

