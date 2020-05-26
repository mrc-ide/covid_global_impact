# Loading Required Libraries
library(tidyverse); library(zoo); library(squire)

# Sourcing Functions for Running Model With Threshold Based Triggers
source("trigger_running_function.R")
setwd("5_Suppression_Triggering_and_Exploration/")
income_strata_healthcare_capacity <- squire::income_strata_healthcare_capacity

# Trigger Thresholds to Use During Model Running
trigger_thresholds <- c(1, 5, 20, 50, 100, 200, 500, 1000, 2000, 3000, 5000, 10000, 20000) 

# Loading in ICU Capacity
ICU <- income_strata_healthcare_capacity$ICU_beds
LIC_icu <- (ICU[1] * 50000000 /1000) / 2
LMIC_icu <- (ICU[2] * 50000000 /1000) / 2
UMIC_icu <- (ICU[3] * 50000000 /1000) / 2
HIC_icu <- (ICU[4] * 50000000 /1000) / 2

# Defining Parameters Used in All Model Runs
replicates <- 2
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
suppression_reduction <- 0.25
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 40

### Running With Capacity Constraints to Examine Time In Suppression vs Deaths
###  -> For Runs Where We Have an Initial Suppression Based On Known Timings for Income Strata
income_strata <- "LMIC"
poorer_outcomes <- FALSE
countries <- "Nicaragua"
raw_death_triggers <- 0.00243
death_triggers <- round(50 * raw_death_triggers)
max_ICU_req <- matrix(nrow = 3, ncol = replicates)
time_in_lockdown <-  matrix(nrow = 3, ncol = replicates)
deaths <-  matrix(nrow = 3, ncol = replicates)
pop <- get_population(countries)
pop <- (50000000/sum(pop$n)) * pop$n
set.seed(10000)
counter <- 1
outputs <- list()
for (j in 8:10) {
  x <- realistic_run_trigger_threshold(country = countries, population = pop,
                                       replicates = replicates,
                                       income_strata = income_strata,
                                       initial_trigger_threshold = death_triggers,
                                       trigger_threshold = trigger_thresholds[j],
                                       suppression_reduction = suppression_reduction,
                                       suppression_duration = suppression_duration,
                                       mitigation_reduction = mitigation_reduction,
                                       R0 = R0, tt_R0 = tt_R0,
                                       max_lockdowns = max_lockdowns,
                                       dt = 0.1, 
                                       hospital_bed_capacity = NULL,
                                       ICU_bed_capacity = NULL,
                                       income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                       poorer_outcomes = poorer_outcomes)
  max_ICU_req[counter, ] <- get_max_ICU_req(x)
  time_in_lockdown[counter, ] <- get_time_in_lockdown(x)
  deaths[counter, ] <- get_total_deaths(x)
  outputs[[counter]] <- x
  print(counter)
  counter <- counter + 1
}

# initial trigger time seems diff

one <- outputs[[1]]
index <- one$index
one_output <- one$model_output[, , 1]
one_deaths <- one_output[, index$delta_D]
one_deaths <- apply(one_deaths, 1, sum)
plot(one_deaths, type = "l")
one_infections <- one_output[, index$n_E2_I]
one_infections <- apply(one_infections, 1, sum)
sum(one_infections)/50000000

two <- outputs[[2]]
index <- two$index
two_output <- two$model_output[, , 1]
two_deaths <- two_output[, index$delta_D]
two_deaths <- apply(two_deaths, 1, sum)
plot(two_deaths, type = "l")
two_infections <- two_output[, index$n_E2_I]
two_infections <- apply(two_infections, 1, sum)
sum(two_infections)/50000000

three <- outputs[[3]]
index <- three$index
three_output <- three$model_output[, , 1]
three_deaths <- three_output[, index$delta_D]
three_deaths <- apply(three_deaths, 1, sum)
plot(three_deaths, type = "l")
three_infections <- three_output[, index$n_E2_I]
three_infections <- apply(three_infections, 1, sum)
sum(three_infections)/50000000


plot(apply(time_in_lockdown, 1, median), apply(deaths, 1, median), 
     ylim = c(0, 280000), col = c("black", "red", "blue"), pch = 20, cex = 3, ylab = "deaths", xlab = "time in lockdown")

plot(one_deaths, type = "l", ylim = c(0, 800))
lines(two_deaths, col = "red")
lines(three_deaths, col = "blue")



time_in_lockdown
deaths
