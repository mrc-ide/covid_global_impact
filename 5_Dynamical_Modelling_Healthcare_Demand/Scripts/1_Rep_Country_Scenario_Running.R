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
R0_req <- 3.5 # R0 (paper uses 2.7, 3 and 3.5 - corresponds to 4, 3.5 and 3 day doubling times)
mitigation_raw <- read.csv("Data/mitigation_strategies.csv") %>%
  filter(R0 == R0_req)

# Choose Specific Country to Run Analyses For 
1