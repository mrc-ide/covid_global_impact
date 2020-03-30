# Load Required Packages
library(tidyverse); library(socialmixr); library(extraDistr); library(tictoc); library(zoo);
source("Functions/Functions.R")

# Loading In Relevant Data
pop_WPP <- read.csv("Data/WPP_demog_matrix.csv")
severe_parameters <- read.csv("Data/severe_parameters.csv", stringsAsFactors = FALSE)
contact_matrices <- readRDS("Data/contact_matrices.rds")

# Loading Model and Initialising Required Data/Parameters
time_period <- 300 # time period to run over
dt <- 0.1 # timestep
dur_E <- 4.58 # mean duration of incubation period
dur_I <- 2.09 # mean duration of infectious period
R0_req <- 3 # R0 
number_betas <- 2 # number of beta values (model is flexible and can change beta over time)
tt_beta <- c(0, 200) # timepoints to implement different betas at
tt_matrix <- c(0, 100, 250) # timepoints to implement the different matrices at 
delay_onset_hospital <- 5  # time between symptom onset (assumed E->I transition) and hospital admission
time_in_hospital <- 8  # duration of hospitalisation if not requiring critical care
time_in_critical_care <- 16  # duration of hospitalisation if requiring critical care
delay_onset_death <- 21  # time between symtom onset (assumed E->I transition) and death
millions <- 20 # standardise to 20 million population

# Loading In the Required Population and Scaling to 25 Million and Initialising Model Populations
country_index <- which(pop_WPP$Region..subregion..country.or.area.. == "Zambia")
raw_country_pop <- unlist(pop_WPP[country_index, 13:33])
country_pop <- c(raw_country_pop[1:15], sum(raw_country_pop[16:length(raw_country_pop)])) * 1000
country_pop <- 1000000 * millions * country_pop / sum(country_pop) 
country_pop <- round(country_pop)
N_age <- length(country_pop) # number of age groups
S0 <- country_pop # number of starting susceptibles
E0 <- rep(0, N_age) # number of starting exposed 
I0 <- rep(1, N_age) # number of starting infectious
R0 <- rep(0, N_age) # number of starting recovered
  
# Loading in Raw Asymmetric and Unbalanced Contact Matrix, Processing to Produce Required Model Input
matrix_usage <- pop_WPP$Matrix[country_index]
contact_matrix <- as.matrix(contact_matrices[[matrix_usage]])
contact_matrix <- generate_contact_matrix(country_pop, contact_matrix) # per capita rates, standardised for demography
mixing_matrix <- t(t(contact_matrix)/country_pop) # divided by population, model input
matrices_set <- aperm(array(c(mixing_matrix, 
                              mixing_matrix, 
                              mixing_matrix), # create set of matrices for use at different timepoints
                              dim = c(dim(mixing_matrix), 3)), c(3, 1, 2))
  
# Loading In and Processing Severity Parameters
prob_hosp <- calc_severity_parameters(severe_parameters$prob_hosp, raw_country_pop) # prob hospitalisation by age
prob_critical <- calc_severity_parameters(severe_parameters$prob_critical, raw_country_pop) # prob critical care requirement by age
prob_critical_given_hosp <- prob_critical/prob_hosp # prob critical care requirement given hospitalisation by age
prob_death <- calc_severity_parameters(severe_parameters$prob_death, raw_country_pop) # prob death by age
  
# Running Model Multiple Times to Calculate the Mean Timing of Suppression Triggers Across Multiple Runs 
number_model_runs <- 20
suppress_triggers <- 10 # threshold (weekly deaths per million) required to trigger suppression
index <- matrix(nrow = number_model_runs, ncol = length(suppress_triggers)) # tracking time when suppression triggers would be initiated
suppress_adjusted <- suppress_triggers * millions # convert per million triggers to equivalent in raw deaths for specified population size
suppress_reduction <- 0.25
for (z in 1:number_model_runs) {
  
  # Number of model replicates to run 
  replicates <- 1
    
  # Running the Model 
  model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                  dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                  number_betas = number_betas, tt_beta = tt_beta, 
                                  matrices_set = matrices_set, tt_matrix = tt_matrix, plot = FALSE,
                                  contact_matrix = contact_matrix, time_period = time_period, 
                                  replicates = replicates)
  
  # Processing and Storing Incidence of Infections
  incidence_infections <- model_output$infection_incidence
  daily_incidence <- get_daily_incidence(incidence_infections)
    
  # Calculating and Storing Hospitalisations
  raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = daily_incidence))
  hospital_incidence <- matrix(unlist(raw_hosp_incidence), ncol = N_age, byrow = FALSE)
    
  # Calculating and Storing ICU Admissions
  raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = hospital_incidence))
  ICU_incidence <- matrix(unlist(raw_ICU_incidence), ncol = N_age, byrow = FALSE)
    
  # Calculating and Storing Deaths
  raw_death_incidence <- Map(calc_deaths, seq_len(ncol(daily_incidence)), prob_death, MoreArgs = list(infection_incidence = daily_incidence))
  death_incidence <- matrix(unlist(raw_death_incidence), ncol = N_age, byrow = FALSE)
    
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
  
  # Calculating When Suppression Strategies Would Be Triggered With Different Thresholds
  deaths <- apply(hosp_and_deaths$death_matrix, 2, sum)
  weekly_deaths <- rollapply(deaths, 7, sum)
  for (q in 1:length(suppress_adjusted)) {
    index[z, q] <- min(which(weekly_deaths >= suppress_adjusted[q])) 
  }
}

# Storing the Unmitigated Output
unmitigated_incidence <- model_output$infection_incidence
unmitigated_daily_incidence <- get_daily_incidence(incidence_infections)
number_timepoints <- seq(1, length(unmitigated_daily_incidence$`0_5`))

# Running and Storing the Mitigated Output 
mitigation_matrices_set <- aperm(array(c(mixing_matrix * 0.55, 
                                         mixing_matrix * 0.55, 
                                         mixing_matrix * 0.55), 
                                        dim = c(dim(mixing_matrix), 3)), c(3, 1, 2))

mitigated_model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                          dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                          number_betas = number_betas, tt_beta = tt_beta, 
                                          matrices_set = mitigation_matrices_set, tt_matrix = tt_matrix, plot = FALSE,
                                          contact_matrix = contact_matrix, time_period = time_period, 
                                          replicates = replicates)

mitigated_incidence <- mitigated_model_output$infection_incidence
mitigated_daily_incidence <- get_daily_incidence(mitigated_incidence)

# Running and Storing the Suppression Output Create New Matrices for Suppression
mean_trigger_time <- 58
suppression_matrices_set <- aperm(array(c(mixing_matrix, 
                                          mixing_matrix * 0.25, 
                                          mixing_matrix * 0.25), 
                                          dim = c(dim(mixing_matrix), 3)), c(3, 1, 2))
# Running the Model With Suppression Matrix At Time Given By the Trigger Time
tt_matrix <- c(0, 58/dt, 10000)
suppression_model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                number_betas = number_betas, tt_beta = tt_beta, 
                                matrices_set = suppression_matrices_set, tt_matrix = tt_matrix, plot = FALSE,
                                contact_matrix = contact_matrix, time_period = time_period, replicates = replicates)
suppression_incidence <- suppression_model_output$infection_incidence
suppression_daily_incidence <- get_daily_incidence(suppression_incidence)

# Zambia Output 
Zambia <- cbind(Time = rep(number_timepoints, 3), 
                Scenario = c(rep("Unmitigated", time_period), rep("Mitigation", time_period), rep("Suppression", time_period)),
                rbind(unmitigated_daily_incidence, mitigated_daily_incidence, suppression_daily_incidence))
write.csv(Zambia, file = "Zambia_long.csv")

z <- Zambia %>%
  gather(age_group, incidence, -Time, -Scenario) %>%
  group_by(Scenario, Time) %>%
  summarise(inc = sum(incidence))
 

ggplot(z, aes(x = Time, y = inc, colour = Scenario)) +
  geom_line()
