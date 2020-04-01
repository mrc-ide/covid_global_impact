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

# Sourcing Functions
source("Functions/Dynamical_Modelling_Functions.R")

# Loading Model and Initialising Required Data/Parameters
time_period <- 150 # time period to run over
dt <- 0.2 # timestep
dur_E <- 4.58 # mean duration of incubation period
dur_I <- 2.09 # mean duration of infectious period
number_betas <- 2 # number of beta values (model is flexible and can change beta over time)
tt_beta <- c(0, 200) # timepoints to implement different betas at
tt_matrix <- c(0, 100, 250) # timepoints to implement the different matrices at 
delay_onset_hospital <- 5  # time between symptom onset (assumed E->I transition) and hospital admission
time_in_hospital <- 8  # duration of hospitalisation if not requiring critical care
time_in_critical_care <- 16  # duration of hospitalisation if requiring critical care
delay_onset_death <- 21  # time between symtom onset (assumed E->I transition) and death
suppress_triggers <- c(2, 16) # threshold (weekly deaths per million) required to trigger suppression
millions <- 20 # standardise to 10 million population
suppress_adjusted <- suppress_triggers * millions # convert per million triggers to equivalent in raw deaths for specified population size
number_model_runs <- 25 # number model runs to work out when triggers should be implemented
suppress_reduction <- 0.25
countries <- length(pop_WPP$Index)

# Running for All Countries and Storing the Output
R0_req <- 2.7 # R0 
country_suppression_output <- matrix(nrow = countries, ncol = 18)
for(i in 1:countries){
  
  # Loading In the Required Population and Scaling to 25 Million and Initialising Model Populations
  raw_country_pop <- unlist(pop_WPP[i, 13:33])
  country_pop <- c(raw_country_pop[1:15], sum(raw_country_pop[16:length(raw_country_pop)])) * 1000
  country_pop <- 1000000 * millions * country_pop / sum(country_pop) 
  country_pop <- round(country_pop)
  N_age <- length(country_pop) # number of age groups
  S0 <- country_pop # number of starting susceptibles
  E0 <- rep(0, N_age) # number of starting exposed 
  I0 <- rep(1, N_age) # number of starting infectious
  R0 <- rep(0, N_age) # number of starting recovered
  
  # Loading in Raw Asymmetric and Unbalanced Contact Matrix, Processing to Produce Required Model Input
  matrix_usage <- pop_WPP$Matrix[i]
  contact_matrix <- as.matrix(contact_matrices[[matrix_usage]])
  contact_matrix <- generate_contact_matrix(country_pop, contact_matrix) # per capita rates, standardised for demography
  mixing_matrix <- t(t(contact_matrix)/country_pop) # divided by population, model input
  matrices_set <- aperm(array(c(mixing_matrix, mixing_matrix, mixing_matrix), # create set of matrices for use at different timepoints
                              dim = c(dim(mixing_matrix), 3)), c(3, 1, 2))
  
  # Loading In and Processing Severity Parameters
  prob_hosp <- calc_severity_parameters(severe_parameters$prob_hosp, raw_country_pop) # prob hospitalisation by age
  prob_critical <- calc_severity_parameters(severe_parameters$prob_critical, raw_country_pop) # prob critical care requirement by age
  prob_critical_given_hosp <- prob_critical/prob_hosp # prob critical care requirement given hospitalisation by age
  prob_death <- calc_severity_parameters(severe_parameters$prob_death, raw_country_pop) # prob death by age
  
  # Running Model Multiple Times to Calculate the Mean Timing of Suppression Triggers Across Multiple Runs 
  index <- matrix(nrow = number_model_runs, ncol = length(suppress_triggers)) # tracking time when suppression triggers would be initiated
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
  
  # Calculate the Average Time At Which Different Triggers Are Reached
  mean_trigger_time <- c(10000, round(apply(index, 2, mean)))
    
  # Create New Matrices for Suppression
  suppression_matrices_set <- aperm(array(c(mixing_matrix, 
                                            mixing_matrix * suppress_reduction, 
                                            mixing_matrix * suppress_reduction), 
                                          dim = c(dim(mixing_matrix), 3)), c(3, 1, 2))
  # Storage for Required Outputs 
  incidence_output <- vector(mode = "numeric", length = length(mean_trigger_time))
  death_output <- vector(mode = "numeric", length = length(mean_trigger_time))
  
  hospitalisation_total_output <- vector(mode = "numeric", length = length(mean_trigger_time))
  ICU_total_output <- vector(mode = "numeric", length = length(mean_trigger_time))
  hospitalisation_peak_output <- vector(mode = "numeric", length = length(mean_trigger_time))
  ICU_peak_output <- vector(mode = "numeric", length = length(mean_trigger_time))

  # Running the Model Multiple Times for Each Trigger Scenario 
  for (k in 1:length(mean_trigger_time)) {
    
    # Number of model replicates to run 
    replicates <- 3
    
    # Generating Storage Vectors and Matrices
    temp_incidence_storage <- vector(mode = "numeric", length = replicates) 
    temp_death_storage <- vector(mode = "numeric", length = replicates) 
    temp_total_hospitalisation_storage <- vector(mode = "numeric", length = replicates)  
    temp_total_ICU_storage <- vector(mode = "numeric", length = replicates) 
    temp_peak_hospitalisation_storage <- vector(mode = "numeric", length = replicates)  
    temp_peak_ICU_storage <- vector(mode = "numeric", length = replicates) 
    
    # Running the Model With Suppression Matrix At Time Given By the Trigger Time
    tt_matrix <- c(0, mean_trigger_time[k]/dt, 1000000)
    model_output <- run_SEEIR_model(dt = dt, N_age = N_age, S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                                    dur_E = dur_E, dur_I = dur_I, R0_req = R0_req, 
                                    number_betas = number_betas, tt_beta = tt_beta, 
                                    matrices_set = suppression_matrices_set, tt_matrix = tt_matrix, plot = FALSE,
                                    contact_matrix = contact_matrix, time_period = time_period, replicates = replicates)
    incidence_replicates <- model_output$infection_incidence
    
    # Iterating Over Replicate Model Runs and Storing Relevant Output
    for (j in 1:replicates) {
      
      temp_incidence_infections <- data.frame(time = model_output$time, incidence = incidence_replicates[[j]])
      temp_daily_incidence <- get_daily_incidence(temp_incidence_infections)
      temp_incidence_storage[j] <- sum(temp_daily_incidence)
      
      temp_raw_hosp_incidence <- Map(calc_hospitalisation, seq_len(ncol(temp_daily_incidence)), prob_hosp, MoreArgs = list(infection_incidence = temp_daily_incidence))
      temp_hospital_incidence <- matrix(unlist(temp_raw_hosp_incidence), ncol = N_age, byrow = FALSE)
      temp_total_hospitalisation_storage[j] <- sum(temp_hospital_incidence)
      
      temp_raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(temp_daily_incidence)), prob_critical_given_hosp, MoreArgs = list(hospitalisation_incidence = temp_hospital_incidence))
      temp_ICU_incidence <- matrix(unlist(temp_raw_ICU_incidence), ncol = N_age, byrow = FALSE)
      temp_total_ICU_storage[j] <- sum(temp_ICU_incidence)
      
      temp_raw_death_incidence <- Map(calc_deaths, seq_len(ncol(temp_daily_incidence)), prob_death, MoreArgs = list(infection_incidence = temp_daily_incidence))
      temp_death_incidence <- matrix(unlist(temp_raw_death_incidence), ncol = N_age, byrow = FALSE)
      temp_death_storage[j] <- sum(temp_death_incidence)
      
      temp_hosp_and_deaths <- generate_hosp_req_and_deaths(delay_onset_hospital = delay_onset_hospital, 
                                                           time_in_hospital = time_in_hospital, 
                                                           time_in_critical_care = time_in_critical_care,
                                                           delay_onset_death = delay_onset_death, 
                                                           hospital_incidence = temp_hospital_incidence, 
                                                           ICU_incidence = temp_ICU_incidence, 
                                                           death_incidence = temp_death_incidence,
                                                           time_period = time_period,
                                                           death_logical = FALSE)
      
      temp_peak_hospitalisation_storage[j] <- max(apply(temp_hosp_and_deaths$hosp_matrix, 2, sum))
      temp_peak_ICU_storage[j] <- max(apply(temp_hosp_and_deaths$ICU_matrix, 2, sum))    
    }
    
    # Generating and Storing the Outputs 
    incidence_output[k] <- median(temp_incidence_storage) * (sum(1000 * raw_country_pop)/(1000000 * millions)) 
    death_output[k] <- median(temp_death_storage) * (sum(1000 * raw_country_pop)/(1000000 * millions)) 
    
    hospitalisation_total_output[k] <- median(temp_total_hospitalisation_storage) * (sum(1000 * raw_country_pop)/(1000000 * millions)) 
    ICU_total_output[k] <- median(temp_total_ICU_storage) * (sum(1000 * raw_country_pop)/(1000000 * millions)) 
    
    hospitalisation_peak_output[k] <- median(temp_peak_hospitalisation_storage) * (sum(1000 * raw_country_pop)/(1000000 * millions)) 
    ICU_peak_output[k] <- median(temp_peak_ICU_storage) * (sum(1000 * raw_country_pop)/(1000000 * millions)) 
    
  }
  country_suppression_output[i, ] <- c(incidence_output, death_output, 
                                       hospitalisation_total_output, hospitalisation_peak_output,
                                       ICU_total_output, ICU_peak_output)
  print(i)
}
country_suppression_output <- data.frame(country = pop_WPP$Region..subregion..country.or.area..,  country_suppression_output)
colnames(country_suppression_output) <- c("Country", 
                                         "Unmit_Inc_Abs", "Supp_0.2_Inc_Abs", "Supp_1.6_Inc_Abs",
                                         "Unmit_Death_Abs", "Supp_0.2_Death_Abs", "Supp_1.6_Death_Abs",
                                         "Unmit_Total_Hosp_Abs", "Supp_0.2_Total_Hosp_Abs", "Supp_1.6_Total_Hosp_Abs",
                                         "Unmit_Max_Hosp_Abs", "Supp_0.2_Max_Hosp_Abs", "Supp_1.6_Max_Hosp_Abs",
                                         "Unmit_Total_ICU_Abs", "Supp_0.2_Total_ICU_Abs", "Supp_1.6_Total_ICU_Abs",
                                         "Unmit_Max_ICU_Abs", "Supp_0.2_Max_ICU_Abs", "Supp_1.6_Max_ICU_Abs")
saveRDS(country_suppression_output, file = paste0("Outputs/Raw_All_Countries_Supp_Output/All_Countries_Suppression_Output_R0_", R0_req, ".Rds"))
