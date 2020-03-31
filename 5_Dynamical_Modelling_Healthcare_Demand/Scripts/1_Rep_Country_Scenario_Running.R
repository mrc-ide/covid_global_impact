# Load Required Packages
library(tidyverse); library(socialmixr); library(extraDistr); library(tictoc); 
library(zoo); library(readxl)

# Loading In Data Used In Multiple Analyses
pop_WPP <- read.csv("Data/WPP_demog_matrix.csv") # WPP Population
contact_matrices <- readRDS("Data/contact_matrices.rds")
severe_parameters <- read.csv("Data/severe_parameters.csv", stringsAsFactors = FALSE)
beds_per_capita_df <- readRDS("4_Determining_Healthcare_Capacity/Outputs/Income_Strata_Predicted_Hospital_and_ICU_Beds.Rds") # Hospital Beds Per 1000 Population

# Set Working Directory 
setwd("5_Dynamical_Modelling_Healthcare_Demand/")
source("Functions/Dynamical_Modelling_Functions.R")

# Loading in Data and Sourcing Required Functions 
R0_req <- 3 # R0 (paper uses 2.7, 3 and 3.5 - corresponds to 4, 3.5 and 3 day doubling times)
mitigation_raw <- read.csv("Data/mitigation_strategies.csv") %>%
  filter(R0 == R0_req)

# Choose Specific Country to Run Analyses For 
country_indicator <- 4
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
raw_country_pop <- unlist(pop_WPP[which(pop_WPP$`Region..subregion..country.or.area..` == countries[country_indicator]), 13:33])
country_pop <- c(raw_country_pop[1:15], sum(raw_country_pop[16:length(raw_country_pop)])) * 1000
millions <- 20 # standardise to 20 million population
country_pop <- 1000000 * millions * country_pop / sum(country_pop) 
country_pop <- round(country_pop)

# Loading In and Processing Severity Parameters
prob_hosp <- calc_severity_parameters(severe_parameters$prob_hosp, raw_country_pop) # prob hospitalisation by age
prob_critical <- calc_severity_parameters(severe_parameters$prob_critical, raw_country_pop) # prob critical care requirement by age
prob_critical_given_hosp <- prob_critical/prob_hosp # prob critical care requirement given hospitalisation by age
prob_death <- calc_severity_parameters(severe_parameters$prob_death, raw_country_pop) # prob death by age

# Loading In Hospital Beds By Income Strata
beds_per_million_df <- beds_per_capita_df %>%
  mutate(median_hosp = 1000000 * hosp_median_pred/1000, lower_hosp = 1000000 * hosp_lower/1000, upper_hosp = 1000000 * hosp_upper/1000) %>%
  select(income_group, median_hosp, lower_hosp, upper_hosp, ICU_median, ICU_lower, ICU_upper) %>% # converting from beds per 1000 to beds per million population
  mutate(ICU_median = median_hosp * ICU_median/100, ICU_lower = lower_hosp * ICU_lower/100, ICU_upper = upper_hosp * ICU_upper/100)
beds_per_million <- beds_per_million_df[country_indicator, -1]

# Loading in Raw Asymmetric and Unbalanced Contact Matrix, Processing to Produce Required Model Input
matrix_usage <- mitigation_raw$matrix[country_indicator]
contact_matrix <- contact_matrices[[matrix_usage]]
contact_matrix <- generate_contact_matrix(country_pop, contact_matrix) # per capita rates, standardised for demography
mixing_matrix <- t(t(contact_matrix)/country_pop) # divided by population, model input
matrices_set <- aperm(array(c(mixing_matrix, mixing_matrix, mixing_matrix), # create set of matrices for use at different timepoints
                            dim = c(dim(mixing_matrix), 3)), 
                      c(3, 1, 2))

# Loading Model and Initialising Required Data/Parameters
N_age <- length(country_pop) # number of age groups
time_period <- 300 # time period to run over
dt <- 0.1 # timestep
S0 <- country_pop # number of starting susceptibles
E0 <- rep(0, N_age) # number of starting exposed 
I0 <- rep(1, N_age) # number of starting infectious
R0 <- rep(0, N_age) # number of starting recovered
dur_E <- 4.58 # mean duration of incubation period
dur_I <- 2.09 # mean duration of infectious period
number_betas <- 2 # number of beta values (model is flexible and can change beta over time)
tt_beta <- c(0, 200) # timepoints to implement different betas at
matrices_set <- matrices_set # mixing matrices (model is flexible and can change the matrices over time)
tt_matrix <- c(0, 100, 250) # timepoints to implement the different matrices at 
delay_onset_hospital <- 5  # time between symptom onset (assumed E->I transition) and hospital admission
time_in_hospital <- 8  # duration of hospitalisation if not requiring critical care
time_in_critical_care <- 16  # duration of hospitalisation if requiring critical care
delay_onset_death <- 21  # time between symtom onset (assumed E->I transition) and death
suppress_triggers <- c(1, 2, 4, 8, 16, 32) # threshold (weekly deaths per million) required to trigger suppression
suppress_adjusted <- suppress_triggers * millions # convert per million triggers to equivalent in raw deaths for specified population size
replicates <- 1 # number of model replicates to run 

# Running Model 100 Times to Calculate the Mean Timing of Suppression Triggers Across Multiple Runs 
number_model_runs <- 100
index <- matrix(nrow = number_model_runs, ncol = length(suppress_triggers)) # tracking time when suppression triggers would be initiated
infections <- vector(mode = "numeric", length = number_model_runs) # tracking total infections per run
hospitalisations <- vector(mode = "numeric", length = number_model_runs) # tracking total hospitalisations per run
critical_care <- vector(mode = "numeric", length = number_model_runs) # tracking total requiring critical care per run
hospital_stays <- vector(mode = "numeric", length = number_model_runs) # tracking total hospital (normal + ICU) bed occupancy in person days per run
ICU_stays <-  vector(mode = "numeric", length = number_model_runs) # tracking total ICU bed occupancy in person days per run
deaths_total <- vector(mode = "numeric", length = number_model_runs) # tracking total deaths per run 
for (z in 1:number_model_runs) {
 
  # Running the Model 
  model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                  dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                  number_betas = number_betas, tt_beta = tt_beta, 
                                  matrices_set = matrices_set, tt_matrix = tt_matrix, plot = TRUE,
                                  contact_matrix = contact_matrix, time_period = time_period, 
                                  replicates = replicates)
  
  # Processing and Storing Incidence of Infections
  incidence_infections <- model_output$infection_incidence
  daily_incidence <- get_daily_incidence(incidence_infections)
  infections[z] <- sum(daily_incidence) 

  # Calculating and Storing Hospitalisations
  raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = daily_incidence))
  hospital_incidence <- matrix(unlist(raw_hosp_incidence), ncol = N_age, byrow = FALSE)
  hospitalisations[z] <- sum(hospital_incidence)
  
  # Calculating and Storing ICU Admissions
  raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = hospital_incidence))
  ICU_incidence <- matrix(unlist(raw_ICU_incidence), ncol = N_age, byrow = FALSE)
  critical_care[z] <- sum(ICU_incidence)
  
  # Calculating and Storing Deaths
  raw_death_incidence <- Map(calc_deaths, seq_len(ncol(daily_incidence)), prob_death, MoreArgs = list(infection_incidence = daily_incidence))
  death_incidence <- matrix(unlist(raw_death_incidence), ncol = N_age, byrow = FALSE)
  deaths_total[z] <- sum(death_incidence)
  
  # Generating Estimates of Bed Occupancy Matrix of Bed Occupancy
  hosp_and_deaths <- generate_hosp_req_and_deaths(delay_onset_hospital = delay_onset_hospital, 
                                                  time_in_hospital = time_in_hospital, 
                                                  time_in_critical_care = time_in_critical_care, 
                                                  delay_onset_death = delay_onset_death, 
                                                  hospital_incidence = hospital_incidence, 
                                                  ICU_incidence = ICU_incidence, 
                                                  death_incidence = death_incidence,
                                                  time_period = time_period,
                                                  death_logical = TRUE)
  hosp_demand <- apply(hosp_and_deaths$hosp_matrix, 2, sum)
  hospital_stays[z] <- sum(hosp_demand)
  
  ICU_demand <- apply(hosp_and_deaths$ICU_matrix, 2, sum)
  ICU_stays[z] <- sum(ICU_demand)
  
  # Calculating When Suppression Strategies Would Be Triggered With Different Thresholds
  deaths <- apply(hosp_and_deaths$death_matrix, 2, sum)
  weekly_deaths <- rollapply(deaths, 7, sum)
  for (j in 1:length(suppress_adjusted)) {
    index[z, j] <- min(which(weekly_deaths >= suppress_adjusted[j])) 
  }
  print(z)
}

# Checking Output For Errors or Inconsistencies 
attack_rate_age <- apply(daily_incidence, 2, sum)/country_pop
plot(attack_rate_age, type = "l", ylim = c(0, 1))
attack_rate <- infections/sum(country_pop)
hospitalisation_rate <- 100 * hospitalisations/infections
overall_mortality_rate <- 100 * deaths_total/infections
prop_critical_care <- critical_care/hospitalisations
hospital_bed_occupancy <- hospital_stays/hospitalisations
hospital_bed_occupancy_check <- prop_critical_care * time_in_critical_care + (1 - prop_critical_care) * time_in_hospital
ICU_bed_occupancy_check <- ICU_stays/critical_care
doubling_time <- check_doubling_time(incidence_infections, dt)

# Exploring the Impact of Suppression Strategies Triggered at Different Timepoints
mean_trigger_time <- c(10000, round(apply(index, 2, mean)))

# Matrices for the New Strategies
suppress_reduction <- 0.25
suppression_matrices_set <- aperm(array(c(mixing_matrix, mixing_matrix * suppress_reduction, mixing_matrix * suppress_reduction), 
  dim = c(dim(mixing_matrix), 3)),
  c(3, 1, 2))

# Creating Storage Vectors for Incidence, Hospitalisation and Deaths
lower_incidence_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
median_incidence_output <- matrix(ncol = length(mean_trigger_time), nrow = time_period)
upper_incidence_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
lower_death_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
median_death_output <- matrix(ncol = length(mean_trigger_time), nrow = time_period)
upper_death_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)

lower_inc_hospitalisation_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
median_inc_hospitalisation_output <- matrix(ncol = length(mean_trigger_time), nrow = time_period)
upper_inc_hospitalisation_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
lower_inc_ICU_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
median_inc_ICU_output <- matrix(ncol = length(mean_trigger_time), nrow = time_period)
upper_inc_ICU_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)

lower_occ_hospitalisation_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
median_occ_hospitalisation_output <- matrix(ncol = length(mean_trigger_time), nrow = time_period)
upper_occ_hospitalisation_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
lower_occ_ICU_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)
median_occ_ICU_output <- matrix(ncol = length(mean_trigger_time), nrow = time_period)
upper_occ_ICU_output <-  matrix(ncol = length(mean_trigger_time), nrow = time_period)

# Running the Model Multiple Times for Each Trigger Scenario 
replicates <- 100
for (i in 1:length(mean_trigger_time)) {
  temp_incidence_storage <- matrix(ncol = replicates, nrow = time_period)
  temp_inc_hospitalisation_storage <- matrix(ncol = replicates, nrow = time_period) 
  temp_inc_ICU_storage <- matrix(ncol = replicates, nrow = time_period)
  temp_occ_hospitalisation_storage <- matrix(ncol = replicates, nrow = time_period) 
  temp_occ_ICU_storage <- matrix(ncol = replicates, nrow = time_period)
  temp_death_storage <- matrix(ncol = replicates, nrow = time_period)
  tt_matrix <- c(0, mean_trigger_time[i]/dt, 1000000)
  model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                  dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                  number_betas = number_betas, tt_beta = tt_beta, 
                                  matrices_set = suppression_matrices_set, tt_matrix = tt_matrix, plot = FALSE,
                                  contact_matrix = contact_matrix, time_period = time_period, replicates = replicates)
  incidence_replicates <- model_output$infection_incidence
  for (j in 1:replicates) {
    temp_incidence_infections <- data.frame(time = model_output$time, incidence = incidence_replicates[[j]])
    temp_daily_incidence <- get_daily_incidence(temp_incidence_infections)
    temp_incidence_storage[, j] <- apply(temp_daily_incidence, 1, sum)
    
    temp_raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(temp_daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = temp_daily_incidence))
    temp_hospital_incidence <- matrix(unlist(temp_raw_hosp_incidence), ncol = N_age, byrow = FALSE)
    temp_inc_hospitalisation_storage[, j] <- apply(temp_hospital_incidence, 1, sum)
    
    temp_raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(temp_daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = temp_hospital_incidence))
    temp_ICU_incidence <- matrix(unlist(temp_raw_ICU_incidence), ncol = N_age, byrow = FALSE)
    temp_inc_ICU_storage[, j] <- apply(temp_ICU_incidence, 1, sum)
    
    temp_raw_death_incidence <- Map(calc_deaths, seq_len(ncol(temp_daily_incidence)), prob_death, MoreArgs = list(infection_incidence = temp_daily_incidence))
    temp_death_incidence <- matrix(unlist(temp_raw_death_incidence), ncol = N_age, byrow = FALSE)
    temp_death_storage[, j] <- apply(temp_death_incidence, 1, sum)
    
    temp_hosp_and_deaths <- generate_hosp_req_and_deaths(delay_onset_hospital = delay_onset_hospital, 
                                                         time_in_hospital = time_in_hospital, 
                                                         time_in_critical_care = time_in_critical_care,
                                                         delay_onset_death = delay_onset_death, 
                                                         hospital_incidence = temp_hospital_incidence, 
                                                         ICU_incidence = temp_ICU_incidence, 
                                                         death_incidence = temp_death_incidence,
                                                         time_period = time_period,
                                                         death_logical = FALSE)
    temp_occ_hospitalisation_storage[, j] <- apply(temp_hosp_and_deaths$hosp_matrix, 2, sum)
    temp_occ_ICU_storage[, j] <- apply(temp_hosp_and_deaths$ICU_matrix, 2, sum)
    print(j)
  }
  lower_incidence_output[, i] <- apply(temp_incidence_storage, 1, quantile, 0.025)
  median_incidence_output[, i] <- apply(temp_incidence_storage, 1, median)
  upper_incidence_output[, i] <- apply(temp_incidence_storage, 1, quantile, 0.975)
  lower_death_output[, i] <-  apply(temp_death_storage, 1, quantile, 0.025)
  median_death_output[, i] <- apply(temp_death_storage, 1, median)
  upper_death_output[, i] <-  apply(temp_death_storage, 1, quantile, 0.975)
  
  lower_inc_hospitalisation_output[, i] <- apply(temp_inc_hospitalisation_storage, 1, quantile, 0.025)
  median_inc_hospitalisation_output[, i] <- apply(temp_inc_hospitalisation_storage, 1, median)
  upper_inc_hospitalisation_output[, i] <- apply(temp_inc_hospitalisation_storage, 1, quantile, 0.975)
  lower_inc_ICU_output[, i] <- apply(temp_inc_ICU_storage, 1, quantile, 0.025)
  median_inc_ICU_output[, i] <- apply(temp_inc_ICU_storage, 1, median)
  upper_inc_ICU_output[, i] <- apply(temp_inc_ICU_storage, 1, quantile, 0.975)
  
  lower_occ_hospitalisation_output[, i] <- apply(temp_occ_hospitalisation_storage, 1, quantile, 0.025)
  median_occ_hospitalisation_output[, i] <- apply(temp_occ_hospitalisation_storage, 1, median)
  upper_occ_hospitalisation_output[, i] <- apply(temp_occ_hospitalisation_storage, 1, quantile, 0.975)
  lower_occ_ICU_output[, i] <- apply(temp_occ_ICU_storage, 1, quantile, 0.025)
  median_occ_ICU_output[, i] <- apply(temp_occ_ICU_storage, 1, median)
  upper_occ_ICU_output[, i] <- apply(temp_occ_ICU_storage, 1, quantile, 0.975)

}

values <- c("lower_inc", "median_inc", "upper_inc", "lower_death", "median_death", "upper_death", 
            "lower_hosp_inc", "median_hosp_inc", "upper_hosp_inc", "lower_ICU_inc", "median_ICU_inc", "upper_ICU_inc",
            "lower_hosp_occ", "median_hosp_occ", "upper_hosp_occ", "lower_ICU_occ", "median_ICU_occ", "upper_ICU_occ")
values <- rep(values, each = time_period)
x <- rbind(lower_incidence_output, median_incidence_output, upper_incidence_output,
           lower_death_output, median_death_output, upper_death_output, 
           lower_inc_hospitalisation_output, median_inc_hospitalisation_output, upper_inc_hospitalisation_output, 
           lower_inc_ICU_output, median_inc_ICU_output, upper_inc_ICU_output, 
           lower_occ_hospitalisation_output, median_occ_hospitalisation_output, upper_occ_hospitalisation_output,
           lower_occ_ICU_output, median_occ_ICU_output, upper_occ_ICU_output)
x <- data.frame(country = rep(country_indicator, 18 * time_period), 
                R0 = rep(R0_req, 18 * time_period), 
                value = values, 
                time = seq(1, time_period), 
                x)
colnames(x) <- c("country", "R0", "output_type", "time", "unmitigated", paste0("supp_", suppress_triggers, "_time"))
write.csv(x, file = paste0("Outputs/Raw_Rep_Country_Outputs/Suppression_Country_", country_indicator, "_R0_", R0_req, "_Model_Outputs.csv"))

# Plotting the Suppression Output 
strats <- c("None", paste0("Supp_", suppress_triggers))
multiple <- length(lower_occ_hospitalisation_output[, 1])

## Hospitalisation
hospitalisation_suppression_output <- data.frame(time = seq(1, time_period, 1),
                                                 measure = c(rep("lower", multiple), rep("median", multiple), rep("upper", multiple)),
                                                 rbind(lower_occ_hospitalisation_output, median_occ_hospitalisation_output, upper_occ_hospitalisation_output))
colnames(hospitalisation_suppression_output) <- c("time", "measure", strats)
hospitalisation_suppression_output_plot <- hospitalisation_suppression_output %>%
  mutate(day = ceiling(time)) %>%
  select(day, everything(), -time) %>%
  gather(strategy, incidence, -day, -measure) %>%
  mutate(strategy = factor(strategy, levels = strats)) %>%
  group_by(strategy, measure, day) %>%
  summarise(incidence = sum(incidence)) %>%
  spread(measure, incidence)

ggplot(data = hospitalisation_suppression_output_plot, aes(x = day, y = median/millions/10, colour = strategy)) +
  geom_line(size = 2) +
  geom_ribbon(data = hospitalisation_suppression_output_plot, aes(x = day, ymin = lower/millions/10, ymax = upper/millions/10, colour = strategy, fill = strategy, border = NULL), alpha = 0.2, linetype = 0) + 
  labs(x = "Time (Days)", y = "Hospital Bed Occupancy Per Day\n Per 100,000 Population") +
  annotate("rect", xmin = min(hospitalisation_suppression_output_plot$day), xmax = max(hospitalisation_suppression_output_plot$day), ymin = beds_per_million$lower_hosp/10, ymax = beds_per_million$upper_hosp/10, alpha = 0.2, fill = "grey") + 
  geom_segment(x = 0, y = beds_per_million$median_hosp/10, xend = time_period, yend = beds_per_million$median_hosp/10, colour = "black", size = 1, linetype = 2) +
  theme_bw() +
  scale_colour_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                        breaks = strats,
                        labels = c("No Suppression", paste0(suppress_triggers/10))) +  
  scale_fill_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                        breaks = strats,
                        labels = c("No Suppression", paste0(suppress_triggers/10))) +  
  lims(y = c(0, max(hospitalisation_suppression_output_plot$upper/millions/10))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", axis.title.y = element_text(size = 13.5, vjust = +3), 
        axis.title.x = element_text(size = 13.5, vjust = +0),
        legend.title = element_text(size = 12, face = "bold"),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))

## ICU Admissions
ICU_suppression_output <- data.frame(time = seq(1, time_period, 1),
                                     measure = c(rep("lower", multiple), rep("median", multiple), rep("upper", multiple)),
                                     rbind(lower_occ_ICU_output, median_occ_ICU_output, upper_occ_ICU_output))
colnames(ICU_suppression_output) <- c("time", "measure", strats)
ICU_suppression_output_plot <- ICU_suppression_output %>%
  mutate(day = ceiling(time)) %>%
  select(day, everything(), -time) %>%
  gather(strategy, incidence, -day, -measure) %>%
  mutate(strategy = factor(strategy, levels = strats)) %>%
  group_by(strategy, measure, day) %>%
  summarise(incidence = sum(incidence)) %>%
  spread(measure, incidence)

ggplot(data = ICU_suppression_output_plot, aes(x = day, y = median/millions/10, colour = strategy)) +
  geom_line(size = 2) +
  geom_ribbon(data = ICU_suppression_output_plot, aes(x = day, ymin = lower/millions/10, ymax = upper/millions/10, colour = strategy, fill = strategy, border = NULL), alpha = 0.2, linetype = 0) + 
  labs(x = "Time (Days)", y = "ICU Bed Occupancy Per Day\n Per 100,000 Population") +
  annotate("rect", xmin = min(ICU_suppression_output_plot$day), xmax = max(ICU_suppression_output_plot$day), ymin = beds_per_million$ICU_lower/10, ymax = beds_per_million$ICU_upper/10, alpha = 0.2, fill = "grey") + 
  geom_segment(x = 0, y = beds_per_million$ICU_median/10, xend = time_period, yend = beds_per_million$ICU_median/10, colour = "black", size = 1, linetype = 2) +
  theme_bw() +
  scale_colour_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                        breaks = strats,
                        labels = c("No Suppression", paste0(suppress_triggers/10))) +  
  scale_fill_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                      breaks = strats,
                      labels = c("No Suppression", paste0(suppress_triggers/10))) +  
  lims(y = c(0, max(ICU_suppression_output_plot$upper/millions/10))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", axis.title.y = element_text(size = 13.5, vjust = +3), 
        axis.title.x = element_text(size = 13.5, vjust = +0),
        legend.title = element_text(size = 12, face = "bold"),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))

# Mitigation Scenario Exploration 
mitigation <- mitigation_raw[country_indicator, ]
social_dist <- mitigation$social_dist
social_dist_plus <- mitigation$with_shield
replicates <- 100

# Loading In and Changing Mixing Matrix for Various Mitigation Scenarios  
matrix_usage <- mitigation_raw$matrix[country_indicator]
contact_matrix <- contact_matrices[[matrix_usage]] # contact_matrices[[13]] has 0 values in there
contact_matrix <- generate_contact_matrix(country_pop, contact_matrix) # per capita rates, standardised for demography
mixing_matrix <- t(t(contact_matrix)/country_pop) # divided by population, model input

# Generating the Different Sets of Matrices 
tt_matrix <- c(0, 100000, 1000000)
standard_matrices <- aperm(array(c(mixing_matrix, 
                                   mixing_matrix, 
                                   mixing_matrix), dim = c(dim(mixing_matrix), 3)), c(3, 1, 2))
mitigation_matrices <- aperm(array(c(mixing_matrix * social_dist,
                                     mixing_matrix * social_dist,
                                     mixing_matrix * social_dist), dim = c(dim(mixing_matrix), 3)), c(3, 1, 2))
mitigation_plus_matrix <- mixing_matrix
mitigation_plus_matrix[1:14, 1:14] <- mitigation_plus_matrix[1:14, 1:14] * social_dist_plus
mitigation_plus_matrix[15:16, 1:16] <- mitigation_plus_matrix[15:16, 1:16] * 0.4
mitigation_plus_matrix[1:14, 15:16] <- mitigation_plus_matrix[1:14, 15:16] * 0.4
mitigation_plus_matrices <- aperm(array(c(mitigation_plus_matrix, 
                                          mitigation_plus_matrix, 
                                          mitigation_plus_matrix), dim = c(dim(mitigation_plus_matrix), 3)), c(3, 1, 2))

# Generating the Output for No Mitigation
no_mitigation_infections <-  matrix(ncol = replicates, nrow = time_period)
no_mitigation_deaths <- matrix(ncol = replicates, nrow = time_period)
no_mitigation_inc_hospitalisations <- matrix(ncol = replicates, nrow = time_period)
no_mitigation_inc_ICUs <- matrix(ncol = replicates, nrow = time_period)
no_mitigation_occ_hospitalisations <- matrix(ncol = replicates, nrow = time_period)
no_mitigation_occ_ICUs <- matrix(ncol = replicates, nrow = time_period)

model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                number_betas = number_betas, tt_beta = tt_beta, 
                                matrices_set = standard_matrices, tt_matrix = tt_matrix, plot = FALSE,
                                contact_matrix = contact_matrix, time_period = time_period, replicates = replicates)
incidence_replicates <- model_output$infection_incidence
for (j in 1:replicates) {
  temp_incidence_infections <- data.frame(time = model_output$time, incidence = incidence_replicates[[j]])
  temp_daily_incidence <- get_daily_incidence(temp_incidence_infections)
  no_mitigation_infections[, j] <- apply(temp_daily_incidence, 1, sum)

  temp_raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(temp_daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = temp_daily_incidence))
  temp_hospital_incidence <- matrix(unlist(temp_raw_hosp_incidence), ncol = N_age, byrow = FALSE)
  no_mitigation_inc_hospitalisations[, j] <- apply(temp_daily_incidence, 1, sum)
  
  temp_raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(temp_daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = temp_hospital_incidence))
  temp_ICU_incidence <- matrix(unlist(temp_raw_ICU_incidence), ncol = N_age, byrow = FALSE)
  no_mitigation_inc_ICUs[, j] <- apply(temp_ICU_incidence, 1, sum)
  
  temp_raw_death_incidence <- Map(calc_deaths, seq_len(ncol(temp_daily_incidence)), prob_death, MoreArgs = list(infection_incidence = temp_daily_incidence))
  temp_death_incidence <- matrix(unlist(temp_raw_death_incidence), ncol = N_age, byrow = FALSE)
  no_mitigation_deaths[, j] <- apply(temp_death_incidence, 1, sum)
  
  temp_hosp_and_deaths <- generate_hosp_req_and_deaths(delay_onset_hospital = delay_onset_hospital, 
                                                       time_in_hospital = time_in_hospital, 
                                                       time_in_critical_care = time_in_critical_care,
                                                       delay_onset_death = delay_onset_death, 
                                                       hospital_incidence = temp_hospital_incidence, 
                                                       ICU_incidence = temp_ICU_incidence, 
                                                       death_incidence = temp_death_incidence,
                                                       time_period = time_period,
                                                       death_logical = FALSE)
  
  no_mitigation_occ_hospitalisations[, j] <- apply(temp_hosp_and_deaths$hosp_matrix, 2, sum)
  no_mitigation_occ_ICUs[, j] <- apply(temp_hosp_and_deaths$ICU_matrix, 2, sum)
  print(j)
}

no_mitigation_infections_lower_output <- apply(no_mitigation_infections, 1, quantile, 0.025)
no_mitigation_infections_median_output <- apply(no_mitigation_infections, 1, median)
no_mitigation_infections_upper_output <- apply(no_mitigation_infections, 1, quantile, 0.975)

no_mitigation_deaths_lower_output <- apply(no_mitigation_deaths, 1, quantile, 0.025)
no_mitigation_deaths_median_output <- apply(no_mitigation_deaths, 1, median)
no_mitigation_deaths_upper_output <- apply(no_mitigation_deaths, 1, quantile, 0.975)

no_mitigation_hosp_inc_lower_output <- apply(no_mitigation_inc_hospitalisations, 1, quantile, 0.025)
no_mitigation_hosp_inc_median_output <- apply(no_mitigation_inc_hospitalisations, 1, median)
no_mitigation_hosp_inc_upper_output <- apply(no_mitigation_inc_hospitalisations, 1, quantile, 0.975)

no_mitigation_ICU_inc_lower_output <- apply(no_mitigation_inc_ICUs, 1, quantile, 0.025)
no_mitigation_ICU_inc_median_output <- apply(no_mitigation_inc_ICUs, 1, median)
no_mitigation_ICU_inc_upper_output <- apply(no_mitigation_inc_ICUs, 1, quantile, 0.975)

no_mitigation_hosp_occ_lower_output <- apply(no_mitigation_occ_hospitalisations, 1, quantile, 0.025)
no_mitigation_hosp_occ_median_output <- apply(no_mitigation_occ_hospitalisations, 1, median)
no_mitigation_hosp_occ_upper_output <- apply(no_mitigation_occ_hospitalisations, 1, quantile, 0.975)

no_mitigation_ICU_occ_lower_output <- apply(no_mitigation_occ_ICUs, 1, quantile, 0.025)
no_mitigation_ICU_occ_median_output <- apply(no_mitigation_occ_ICUs, 1, median)
no_mitigation_ICU_occ_upper_output <- apply(no_mitigation_occ_ICUs, 1, quantile, 0.975)

outputs <- c("infections_lower", "infections_median", "infections_upper",
             "deaths_lower", "deaths_median", "deaths_upper",
             "hosp_inc_lower", "hosp_inc_median", "hosp_inc_upper",
             "ICU_inc_lower", "ICU_inc_median", "ICU_inc_upper",
             "hosp_occ_lower", "hosp_occ_median", "hosp_occ_upper",
             "ICU_occ_lower", "ICU_occ_median", "ICU_occ_upper")
no_mitigation <- data.frame(country = rep(country_indicator, time_period),
                            R0 = rep(R0_req, time_period),
                            strategy = rep("unmitigated", time_period),
                            time = seq(1, time_period),
                       cbind(no_mitigation_infections_lower_output, no_mitigation_infections_median_output, no_mitigation_infections_upper_output,
                             no_mitigation_deaths_lower_output, no_mitigation_deaths_median_output, no_mitigation_deaths_upper_output,
                             no_mitigation_hosp_inc_lower_output, no_mitigation_hosp_inc_median_output, no_mitigation_hosp_inc_upper_output,
                             no_mitigation_ICU_inc_lower_output, no_mitigation_ICU_inc_median_output, no_mitigation_ICU_inc_upper_output,
                             no_mitigation_hosp_occ_lower_output, no_mitigation_hosp_occ_median_output, no_mitigation_hosp_occ_upper_output,
                             no_mitigation_ICU_occ_lower_output, no_mitigation_ICU_occ_median_output, no_mitigation_ICU_occ_upper_output))
colnames(no_mitigation) <- c("country", "R0", "strategy", "time", outputs)

# Generating the Output for Standard Mitigation
std_mitigation_infections <-  matrix(ncol = replicates, nrow = time_period)
std_mitigation_deaths <- matrix(ncol = replicates, nrow = time_period)
std_mitigation_inc_hospitalisations <- matrix(ncol = replicates, nrow = time_period)
std_mitigation_inc_ICUs <- matrix(ncol = replicates, nrow = time_period)
std_mitigation_occ_hospitalisations <- matrix(ncol = replicates, nrow = time_period)
std_mitigation_occ_ICUs <- matrix(ncol = replicates, nrow = time_period)

model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                number_betas = number_betas, tt_beta = tt_beta, 
                                matrices_set = mitigation_matrices, tt_matrix = tt_matrix, plot = FALSE,
                                contact_matrix = contact_matrix, time_period = time_period, replicates = replicates)
incidence_replicates <- model_output$infection_incidence
for (j in 1:replicates) {
  temp_incidence_infections <- data.frame(time = model_output$time, incidence = incidence_replicates[[j]])
  temp_daily_incidence <- get_daily_incidence(temp_incidence_infections)
  std_mitigation_infections[, j] <- apply(temp_daily_incidence, 1, sum)
  
  temp_raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(temp_daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = temp_daily_incidence))
  temp_hospital_incidence <- matrix(unlist(temp_raw_hosp_incidence), ncol = N_age, byrow = FALSE)
  std_mitigation_inc_hospitalisations[, j] <- apply(temp_hospital_incidence, 1, sum)
  
  temp_raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(temp_daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = temp_hospital_incidence))
  temp_ICU_incidence <- matrix(unlist(temp_raw_ICU_incidence), ncol = N_age, byrow = FALSE)
  std_mitigation_inc_ICUs[, j] <- apply(temp_ICU_incidence, 1, sum)
  
  temp_raw_death_incidence <- Map(calc_deaths, seq_len(ncol(temp_daily_incidence)), prob_death, MoreArgs = list(infection_incidence = temp_daily_incidence))
  temp_death_incidence <- matrix(unlist(temp_raw_death_incidence), ncol = N_age, byrow = FALSE)
  std_mitigation_deaths[, j] <- apply(temp_death_incidence, 1, sum)
  
  temp_hosp_and_deaths <- generate_hosp_req_and_deaths(delay_onset_hospital = delay_onset_hospital, 
                                                       time_in_hospital = time_in_hospital, 
                                                       time_in_critical_care = time_in_critical_care,
                                                       delay_onset_death = delay_onset_death, 
                                                       hospital_incidence = temp_hospital_incidence, 
                                                       ICU_incidence = temp_ICU_incidence, 
                                                       death_incidence = temp_death_incidence,
                                                       time_period = time_period,
                                                       death_logical = FALSE)
  
  std_mitigation_occ_hospitalisations[, j] <- apply(temp_hosp_and_deaths$hosp_matrix, 2, sum)
  std_mitigation_occ_ICUs[, j] <- apply(temp_hosp_and_deaths$ICU_matrix, 2, sum)
  print(j)
}

std_mitigation_infections_lower_output <- apply(std_mitigation_infections, 1, quantile, 0.025)
std_mitigation_infections_median_output <- apply(std_mitigation_infections, 1, median)
std_mitigation_infections_upper_output <- apply(std_mitigation_infections, 1, quantile, 0.975)

std_mitigation_deaths_lower_output <- apply(std_mitigation_deaths, 1, quantile, 0.025)
std_mitigation_deaths_median_output <- apply(std_mitigation_deaths, 1, median)
std_mitigation_deaths_upper_output <- apply(std_mitigation_deaths, 1, quantile, 0.975)

std_mitigation_hosp_inc_lower_output <- apply(std_mitigation_inc_hospitalisations, 1, quantile, 0.025)
std_mitigation_hosp_inc_median_output <- apply(std_mitigation_inc_hospitalisations, 1, median)
std_mitigation_hosp_inc_upper_output <- apply(std_mitigation_inc_hospitalisations, 1, quantile, 0.975)

std_mitigation_ICU_inc_lower_output <- apply(std_mitigation_inc_ICUs, 1, quantile, 0.025)
std_mitigation_ICU_inc_median_output <- apply(std_mitigation_inc_ICUs, 1, median)
std_mitigation_ICU_inc_upper_output <- apply(std_mitigation_inc_ICUs, 1, quantile, 0.975)

std_mitigation_hosp_occ_lower_output <- apply(std_mitigation_occ_hospitalisations, 1, quantile, 0.025)
std_mitigation_hosp_occ_median_output <- apply(std_mitigation_occ_hospitalisations, 1, median)
std_mitigation_hosp_occ_upper_output <- apply(std_mitigation_occ_hospitalisations, 1, quantile, 0.975)

std_mitigation_ICU_occ_lower_output <- apply(std_mitigation_occ_ICUs, 1, quantile, 0.025)
std_mitigation_ICU_occ_median_output <- apply(std_mitigation_occ_ICUs, 1, median)
std_mitigation_ICU_occ_upper_output <- apply(std_mitigation_occ_ICUs, 1, quantile, 0.975)

outputs <- c("infections_lower", "infections_median", "infections_upper",
             "deaths_lower", "deaths_median", "deaths_upper",
             "hosp_inc_lower", "hosp_inc_median", "hosp_inc_upper",
             "ICU_inc_lower", "ICU_inc_median", "ICU_inc_upper",
             "hosp_occ_lower", "hosp_occ_median", "hosp_occ_upper",
             "ICU_occ_lower", "ICU_occ_median", "ICU_occ_upper")
std_mitigation <- data.frame(country = rep(country_indicator, time_period),
                             R0 = rep(R0_req, time_period),
                             strategy = rep("std_mitigation", time_period),
                             time = seq(1, time_period), 
                       cbind(std_mitigation_infections_lower_output, std_mitigation_infections_median_output, std_mitigation_infections_upper_output,
                             std_mitigation_deaths_lower_output, std_mitigation_deaths_median_output, std_mitigation_deaths_upper_output,
                             std_mitigation_hosp_inc_lower_output, std_mitigation_hosp_inc_median_output, std_mitigation_hosp_inc_upper_output,
                             std_mitigation_ICU_inc_lower_output, std_mitigation_ICU_inc_median_output, std_mitigation_ICU_inc_upper_output,
                             std_mitigation_hosp_occ_lower_output, std_mitigation_hosp_occ_median_output, std_mitigation_hosp_occ_upper_output,
                             std_mitigation_ICU_occ_lower_output, std_mitigation_ICU_occ_median_output, std_mitigation_ICU_occ_upper_output))
colnames(std_mitigation) <- c("country", "R0", "strategy", "time", outputs)


# Generating the Output for Standard Mitigation and Elderly Protection
plus_mitigation_infections <-  matrix(ncol = replicates, nrow = time_period)
plus_mitigation_deaths <- matrix(ncol = replicates, nrow = time_period)
plus_mitigation_inc_hospitalisations <- matrix(ncol = replicates, nrow = time_period)
plus_mitigation_inc_ICUs <- matrix(ncol = replicates, nrow = time_period)
plus_mitigation_occ_hospitalisations <- matrix(ncol = replicates, nrow = time_period)
plus_mitigation_occ_ICUs <- matrix(ncol = replicates, nrow = time_period)

model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                number_betas = number_betas, tt_beta = tt_beta, 
                                matrices_set = mitigation_plus_matrices, tt_matrix = tt_matrix, plot = FALSE,
                                contact_matrix = contact_matrix, time_period = time_period, replicates = replicates)
incidence_replicates <- model_output$infection_incidence
for (j in 1:replicates) {
  temp_incidence_infections <- data.frame(time = model_output$time, incidence = incidence_replicates[[j]])
  temp_daily_incidence <- get_daily_incidence(temp_incidence_infections)
  plus_mitigation_infections[, j] <- apply(temp_daily_incidence, 1, sum)
  
  temp_raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(temp_daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = temp_daily_incidence))
  temp_hospital_incidence <- matrix(unlist(temp_raw_hosp_incidence), ncol = N_age, byrow = FALSE)
  plus_mitigation_inc_hospitalisations[, j] <- apply(temp_hospital_incidence, 1, sum)
  
  temp_raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(temp_daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = temp_hospital_incidence))
  temp_ICU_incidence <- matrix(unlist(temp_raw_ICU_incidence), ncol = N_age, byrow = FALSE)
  plus_mitigation_inc_ICUs[, j] <- apply(temp_ICU_incidence, 1, sum)
  
  temp_raw_death_incidence <- Map(calc_deaths, seq_len(ncol(temp_daily_incidence)), prob_death, MoreArgs = list(infection_incidence = temp_daily_incidence))
  temp_death_incidence <- matrix(unlist(temp_raw_death_incidence), ncol = N_age, byrow = FALSE)
  plus_mitigation_deaths[, j] <- apply(temp_death_incidence, 1, sum)
  
  temp_hosp_and_deaths <- generate_hosp_req_and_deaths(delay_onset_hospital = delay_onset_hospital, 
                                                       time_in_hospital = time_in_hospital, 
                                                       time_in_critical_care = time_in_critical_care,
                                                       delay_onset_death = delay_onset_death, 
                                                       hospital_incidence = temp_hospital_incidence, 
                                                       ICU_incidence = temp_ICU_incidence, 
                                                       death_incidence = temp_death_incidence,
                                                       time_period = time_period,
                                                       death_logical = FALSE)
  
  plus_mitigation_occ_hospitalisations[, j] <- apply(temp_hosp_and_deaths$hosp_matrix, 2, sum)
  plus_mitigation_occ_ICUs[, j] <- apply(temp_hosp_and_deaths$ICU_matrix, 2, sum)
  print(j)
}

plus_mitigation_infections_lower_output <- apply(plus_mitigation_infections, 1, quantile, 0.025)
plus_mitigation_infections_median_output <- apply(plus_mitigation_infections, 1, median)
plus_mitigation_infections_upper_output <- apply(plus_mitigation_infections, 1, quantile, 0.975)

plus_mitigation_deaths_lower_output <- apply(plus_mitigation_deaths, 1, quantile, 0.025)
plus_mitigation_deaths_median_output <- apply(plus_mitigation_deaths, 1, median)
plus_mitigation_deaths_upper_output <- apply(plus_mitigation_deaths, 1, quantile, 0.975)

plus_mitigation_hosp_inc_lower_output <- apply(plus_mitigation_inc_hospitalisations, 1, quantile, 0.025)
plus_mitigation_hosp_inc_median_output <- apply(plus_mitigation_inc_hospitalisations, 1, median)
plus_mitigation_hosp_inc_upper_output <- apply(plus_mitigation_inc_hospitalisations, 1, quantile, 0.975)

plus_mitigation_ICU_inc_lower_output <- apply(plus_mitigation_inc_ICUs, 1, quantile, 0.025)
plus_mitigation_ICU_inc_median_output <- apply(plus_mitigation_inc_ICUs, 1, median)
plus_mitigation_ICU_inc_upper_output <- apply(plus_mitigation_inc_ICUs, 1, quantile, 0.975)

plus_mitigation_hosp_occ_lower_output <- apply(plus_mitigation_occ_hospitalisations, 1, quantile, 0.025)
plus_mitigation_hosp_occ_median_output <- apply(plus_mitigation_occ_hospitalisations, 1, median)
plus_mitigation_hosp_occ_upper_output <- apply(plus_mitigation_occ_hospitalisations, 1, quantile, 0.975)

plus_mitigation_ICU_occ_lower_output <- apply(plus_mitigation_occ_ICUs, 1, quantile, 0.025)
plus_mitigation_ICU_occ_median_output <- apply(plus_mitigation_occ_ICUs, 1, median)
plus_mitigation_ICU_occ_upper_output <- apply(plus_mitigation_occ_ICUs, 1, quantile, 0.975)

outputs <- c("infections_lower", "infections_median", "infections_upper",
             "deaths_lower", "deaths_median", "deaths_upper",
             "hosp_inc_lower", "hosp_inc_median", "hosp_inc_upper",
             "ICU_inc_lower", "ICU_inc_median", "ICU_inc_upper",
             "hosp_occ_lower", "hosp_occ_median", "hosp_occ_upper",
             "ICU_occ_lower", "ICU_occ_median", "ICU_occ_upper")
plus_mitigation <- data.frame(country = rep(country_indicator, time_period),
                              R0 = rep(R0_req, time_period),
                              strategy = rep("plus_mitigation", time_period),
                              time = seq(1, time_period),
                        cbind(plus_mitigation_infections_lower_output, plus_mitigation_infections_median_output, plus_mitigation_infections_upper_output,
                              plus_mitigation_deaths_lower_output, plus_mitigation_deaths_median_output, plus_mitigation_deaths_upper_output,
                              plus_mitigation_hosp_inc_lower_output, plus_mitigation_hosp_inc_median_output, plus_mitigation_hosp_inc_upper_output,
                              plus_mitigation_ICU_inc_lower_output, plus_mitigation_ICU_inc_median_output, plus_mitigation_ICU_inc_upper_output,
                              plus_mitigation_hosp_occ_lower_output, plus_mitigation_hosp_occ_median_output, plus_mitigation_hosp_occ_upper_output,
                              plus_mitigation_ICU_occ_lower_output, plus_mitigation_ICU_occ_median_output, plus_mitigation_ICU_occ_upper_output))
colnames(plus_mitigation) <- c("country", "R0", "strategy", "time", outputs)

x <- rbind(no_mitigation, std_mitigation, plus_mitigation)
write.csv(x, file =  paste0("Outputs/Raw_Rep_Country_Outputs/Mitigation_Country_", country_indicator, "_R0_", R0_req, "_Model_Outputs.csv"))

# Plotting the Output for Hospitalisations
colours <- c("#191308", "#454B66", "#9CA3DB")  # c("#a4243b", "#bd632f", "#d8973c")
hosp_overall <- data.frame(time = seq(1, time_period, 1),
                           scenario = c(rep("a_nothing", time_period), rep("b_mitigation", time_period), rep("c_mitigation_plus", time_period)),
                           lower = c(no_mitigation_hosp_occ_lower_output, std_mitigation_hosp_occ_lower_output, plus_mitigation_hosp_occ_lower_output),
                           median =  c(no_mitigation_hosp_occ_median_output, std_mitigation_hosp_occ_median_output, plus_mitigation_hosp_occ_median_output),
                           upper =  c(no_mitigation_hosp_occ_upper_output, std_mitigation_hosp_occ_upper_output, plus_mitigation_hosp_occ_upper_output))
hosp_overall$scenario <- factor(hosp_overall$scenario)

ggplot(data = hosp_overall, aes(x = time, y = median/millions/10, colour = scenario)) +
  geom_line(size = 2) +
  geom_ribbon(data = hosp_overall, aes(x = time, ymin = lower/millions/10, ymax = upper/millions/10, colour = scenario, fill = scenario, border = NULL), alpha = 0.2, linetype = 0) + 
  labs(x = "Time (Days)", y = "Hospital Bed Occupancy Per Day\n Per 100,000 Population") +
  theme_bw() +
  lims(y = c(0, max(hosp_overall$upper/millions/10))) +
  scale_colour_manual(values = colours,
                      breaks = c("a_nothing", "b_mitigation", "c_mitigation_plus"),
                      labels = c("No Mitigation", "Social Distancing\nWhole Population", "Enhanced Social\nDistancing of\nthe Elderly"), 
                      name = "Control Strategy") +
  scale_fill_manual(values = colours,
                    breaks = c("a_nothing", "b_mitigation", "c_mitigation_plus"),
                    labels = c("No Mitigation", "Social Distancing\nWhole Population", "Enhanced Social\nDistancing of\nthe Elderly"), 
                    name = "Control Strategy") +
  annotate("rect", xmin = min(hosp_overall$time), xmax = max(hosp_overall$time), ymin = beds_per_million$lower_hosp/10, ymax = beds_per_million$upper_hosp/10, alpha = 0.2, fill = "grey") + 
  geom_segment(x = 0, y = beds_per_million$median_hosp/10, xend = time_period, yend = beds_per_million$median_hosp/10, colour = "black", size = 1, linetype = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", legend.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +3), axis.title.x = element_text(size = 13.5, vjust = +0),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))

# Plotting the Output for ICU 
ICU_overall <- data.frame(time = seq(1, time_period, 1),
                          scenario = c(rep("a_nothing", time_period), rep("b_mitigation", time_period), rep("c_mitigation_plus", time_period)),
                          lower = c(no_mitigation_ICU_occ_lower_output, std_mitigation_ICU_occ_lower_output, plus_mitigation_ICU_occ_lower_output),
                          median =  c(no_mitigation_ICU_occ_median_output, std_mitigation_ICU_occ_median_output, plus_mitigation_ICU_occ_median_output),
                          upper =  c(no_mitigation_ICU_occ_upper_output, std_mitigation_ICU_occ_upper_output, plus_mitigation_ICU_occ_upper_output))
ggplot(data = ICU_overall, aes(x = time, y = median/millions/10, colour = scenario)) +
  geom_line(size = 2) +
  geom_ribbon(data = ICU_overall, aes(x = time, ymin = lower/millions/10, ymax = upper/millions/10, colour = scenario, fill = scenario, border = NULL), alpha = 0.2, linetype = 0) + 
  labs(x = "Time (Days)", y = "ICU Bed Occupancy Per Day\n Per 100,000 Population") +
  theme_bw() +
  lims(y = c(0, max(ICU_overall$upper/millions/10))) +
  scale_colour_manual(values = colours,
                      breaks = c("a_nothing", "b_mitigation", "c_mitigation_plus"),
                      labels = c("No Mitigation", "Social Distancing\nWhole Population", "Enhanced Social\nDistancing of\nthe Elderly"), 
                      name = "Control Strategy") +
  scale_fill_manual(values = colours,
                    breaks = c("a_nothing", "b_mitigation", "c_mitigation_plus"),
                    labels = c("No Mitigation", "Social Distancing\nWhole Population", "Enhanced Social\nDistancing of\nthe Elderly"), 
                    name = "Control Strategy") +
  annotate("rect", xmin = min(ICU_overall$time), xmax = max(ICU_overall$time), ymin = beds_per_million$ICU_lower/10, ymax = beds_per_million$ICU_upper/10, alpha = 0.2, fill = "grey") + 
  geom_segment(x = 0, y = beds_per_million$ICU_median/10, xend = time_period, yend = beds_per_million$ICU_median/10, colour = "black", size = 1, linetype = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", legend.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +3), axis.title.x = element_text(size = 13.5, vjust = +0),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))

# Suppression Followed By Lifting
suppress_reduction <- 0.25
time_for_triggers <- c(10000, mean_trigger_time[5]) # corresponding 0.8 deaths per 100,000 per week shutdown initiated
lifted_suppression_matrices_set <- aperm(array(c(mixing_matrix, 
                                                 mixing_matrix * suppress_reduction, 
                                                 mixing_matrix), 
                                        dim = c(dim(mixing_matrix), 3)),
                                  c(3, 1, 2))

# Creating Storage Vectors for Incidence, Hospitalisation and Deaths
time_period_new <- 300
lower_incidence_output <-  matrix(ncol = length(time_for_triggers), nrow = time_period_new)
median_incidence_output <- matrix(ncol = length(time_for_triggers), nrow = time_period_new)
upper_incidence_output <-  matrix(ncol = length(time_for_triggers), nrow = time_period_new)
lower_occ_ICU_output <-  matrix(ncol = length(time_for_triggers), nrow = time_period_new)
median_occ_ICU_output <- matrix(ncol = length(time_for_triggers), nrow = time_period_new)
upper_occ_ICU_output <-  matrix(ncol = length(time_for_triggers), nrow = time_period_new)

replicates <- 100
for (i in 1:length(time_for_triggers)) {
  temp_incidence_storage <- matrix(ncol = replicates, nrow = time_period)
  temp_inc_hospitalisation_storage <- matrix(ncol = replicates, nrow = time_period) 
  temp_inc_ICU_storage <- matrix(ncol = replicates, nrow = time_period)
  temp_occ_ICU_storage <- matrix(ncol = replicates, nrow = time_period)
  temp_death_storage <- matrix(ncol = replicates, nrow = time_period)
  tt_matrix <- c(0, time_for_triggers[i]/dt, (time_for_triggers[i] + 90)/dt)
  model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                  dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                  number_betas = number_betas, tt_beta = tt_beta, 
                                  matrices_set = lifted_suppression_matrices_set, tt_matrix = tt_matrix, plot = FALSE,
                                  contact_matrix = contact_matrix, time_period = time_period_new, replicates = replicates)
  incidence_replicates <- model_output$infection_incidence
  for (j in 1:replicates) {
    temp_incidence_infections <- data.frame(time = model_output$time, incidence = incidence_replicates[[j]])
    temp_daily_incidence <- get_daily_incidence(temp_incidence_infections)
    temp_incidence_storage[, j] <- apply(temp_daily_incidence, 1, sum)
    
    temp_raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(temp_daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = temp_daily_incidence))
    temp_hospital_incidence <- matrix(unlist(temp_raw_hosp_incidence), ncol = N_age, byrow = FALSE)
    temp_inc_hospitalisation_storage[, j] <- apply(temp_hospital_incidence, 1, sum)
    
    temp_raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(temp_daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = temp_hospital_incidence))
    temp_ICU_incidence <- matrix(unlist(temp_raw_ICU_incidence), ncol = N_age, byrow = FALSE)
    temp_inc_ICU_storage[, j] <- apply(temp_ICU_incidence, 1, sum)
    
    temp_raw_death_incidence <- Map(calc_deaths, seq_len(ncol(temp_daily_incidence)), prob_death, MoreArgs = list(infection_incidence = temp_daily_incidence))
    temp_death_incidence <- matrix(unlist(temp_raw_death_incidence), ncol = N_age, byrow = FALSE)
    temp_death_storage[, j] <- apply(temp_death_incidence, 1, sum)
    
    temp_hosp_and_deaths <- generate_hosp_req_and_deaths(delay_onset_hospital = delay_onset_hospital, 
                                                         time_in_hospital = time_in_hospital, 
                                                         time_in_critical_care = time_in_critical_care,
                                                         delay_onset_death = delay_onset_death, 
                                                         hospital_incidence = temp_hospital_incidence, 
                                                         ICU_incidence = temp_ICU_incidence, 
                                                         death_incidence = temp_death_incidence,
                                                         time_period = time_period,
                                                         death_logical = FALSE)
    temp_occ_ICU_storage[, j] <- apply(temp_hosp_and_deaths$ICU_matrix, 2, sum)
    print(j)
  }
  lower_incidence_output[, i] <- apply(temp_incidence_storage, 1, quantile, 0.025)
  median_incidence_output[, i] <- apply(temp_incidence_storage, 1, median)
  upper_incidence_output[, i] <- apply(temp_incidence_storage, 1, quantile, 0.975)
  
  lower_occ_ICU_output[, i] <- apply(temp_occ_ICU_storage, 1, quantile, 0.025)
  median_occ_ICU_output[, i] <- apply(temp_occ_ICU_storage, 1, median)
  upper_occ_ICU_output[, i] <- apply(temp_occ_ICU_storage, 1, quantile, 0.975)
}

## Incidence
lifted_incidence_suppression_output <- data.frame(country_indicator = rep(country_indicator, time_period_new * 3),
                                                  R0 = rep(R0_req, 3), 
                                                  time = rep(seq(1, time_period_new, 1), 3),
                                                  measure = c(rep("lower", time_period_new), rep("median", time_period_new), rep("upper", time_period_new)),
                                                  rbind(lower_occ_ICU_output, median_occ_ICU_output, upper_occ_ICU_output))
colnames(lifted_incidence_suppression_output) <- c("country", "R0", "time", "measure", "none", "incomplete")
write.csv(lifted_incidence_suppression_output, file =  paste0("Outputs/Raw_Rep_Country_Outputs/Lifted_Suppression_Country_", country_indicator, "_R0_", R0_req, "_Model_Outputs.csv"))

lifted_incidence_suppression_output_plot <- lifted_incidence_suppression_output %>%
  mutate(day = ceiling(time)) %>%
  select(day, everything(), -time) %>%
  gather(strategy, incidence, -day, -measure) %>%
  group_by(strategy, measure, day) %>%
  summarise(incidence = sum(incidence)) %>%
  spread(measure, incidence)

ggplot(data = lifted_incidence_suppression_output_plot, aes(x = day, y = median/millions/10, colour = strategy)) +
  geom_rect(xmin = time_for_triggers[2], xmax = time_for_triggers[2] + 90, fill = "#EAEAEA", ymin = 0, ymax = 4500, colour="white", size = 0.5, alpha = 0.1) +
  geom_line(size = 2) +
  geom_ribbon(data = lifted_incidence_suppression_output_plot, aes(x = day, ymin = lower/millions/10, ymax = upper/millions/10, colour = strategy, fill = strategy, border = NULL), alpha = 0.2, linetype = 0) + 
  labs(x = "Time (Days)", y = "Infection Incidence Per Day\n Per 100,000 Population") +
  theme_bw() +
  scale_colour_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                        breaks = c("none", "incomplete"),
                        labels = c("No Suppression", "Suppression then lifting")) + 
  scale_fill_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                      breaks = c("none", "incomplete"),
                      labels = c("No Suppression", "Suppression then lifting")) +  
  lims(y = c(0, max(lifted_incidence_suppression_output_plot$upper/millions/10))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", axis.title.y = element_text(size = 13.5, vjust = +3), 
        axis.title.x = element_text(size = 13.5, vjust = +0),
        legend.title = element_text(size = 12, face = "bold"),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))

