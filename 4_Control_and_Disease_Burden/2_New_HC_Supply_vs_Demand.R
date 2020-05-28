# Load Required Libraries
library(squire); library(tidyverse)

# Set Seed
set.seed(10001092)

# Source Functions
source("Functions/Healthcare_Demand_Functions.R")

# Loading Severity Parameter Posterior Draws
severity_params <- readRDS("Outputs/severity_param_sets.rds")

# Representative Countries for Each of LIC, LMIC, UMIC and HIC and 
# Associated Parameters for Model Running
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
income_strata <- c("LIC", "LMIC", "UMIC", "HIC")
unmitigated_R0 <- 3
mitigated_R0 <- 3 * 0.55
number_draws <- 500

# Looping Over Severity Parameters and Calculating ICU Capacity Exceedance for Unmitigated Epidemic
unmitigated_capacity_exceed <- matrix(nrow = number_draws, ncol = 4)
for (i in 1:4) {
  
  mixing_matrix <- get_mixing_matrix(countries[i])
  std_population <- generate_standard_population(countries[i])
  ICU_beds <- generate_ICU_beds(std_population, income_strata[i], income_strata_healthcare_capacity)

  for (j in 1:number_draws) {
    
    temp_sev_params <- severity_params[[j]]
    x <- run_explicit_SEEIR_model(R0 = unmitigated_R0,
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

    ICU_dem <- format_output(x, var_select = "ICU_demand") %>%
      group_by(replicate) %>%
      summarise(max = max(y))
    unmitigated_capacity_exceed[j, i] <- median(ICU_dem$max)/ICU_beds
    print(j)
    
  }
}

# Gathering the Unmitigated Capacity Exceedance Dataset
unmitigated_capacity_exceed <- data.frame(unmitigated_capacity_exceed)
colnames(unmitigated_capacity_exceed) <- income_strata
tidy_unmit <- unmitigated_capacity_exceed %>%
  gather(setting, multiple)
tidy_unmit$scenario <- "unmitigated"

# Looping Over Severity Parameters and Calculating ICU Capacity Exceedance for Unmitigated Epidemic
mitigated_capacity_exceed <- matrix(nrow = number_draws, ncol = 4)
for (i in 1:4) {
  
  mixing_matrix <- get_mixing_matrix(countries[i])
  std_population <- generate_standard_population(countries[i])
  ICU_beds <- generate_ICU_beds(std_population, income_strata[i], income_strata_healthcare_capacity)
  
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
    
    ICU_dem <- format_output(x, var_select = "ICU_demand") %>%
      group_by(replicate) %>%
      summarise(max = max(y))
    mitigated_capacity_exceed[j, i] <- median(ICU_dem$max)/ICU_beds
    print(j)
    
  }
}

# Gathering the Mitigated Capacity Exceedance Dataset
mitigated_capacity_exceed <- data.frame(mitigated_capacity_exceed)
colnames(mitigated_capacity_exceed) <- income_strata
tidy_mit <- mitigated_capacity_exceed %>%
  gather(setting, multiple)
tidy_mit$scenario <- "mitigated"

# Combining the Two Dataframes Together and Saving
overall <- rbind(tidy_unmit, tidy_mit)
overall$setting <- factor(overall$setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))
overall$scenario <- factor(overall$scenario, levels = c("unmitigated", "mitigated"))

saveRDS(overall, file = "Outputs/HC_stretch_uncertainty.csv")

check_overall <- overall %>%
  group_by(setting, scenario) %>%
  summarise(median = median(multiple))


