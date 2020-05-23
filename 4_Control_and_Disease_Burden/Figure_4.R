
library(devtools)
install.packages("viridis")
install.packages("Rcolorbrewer")

devtools::install_github("mrc-ide/squire",ref="non_odin_triggering")
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse);
library(ggplot2);
library(squire)

library(ggthemes)
rm(list=ls())
setwd("C:/Users/Patrick/Imperial College London/ncov - Documents/2019-nCoV/LMIC/LMIC Parameters")
severity_param_sets <- readRDS("severity_param_sets.rds")
severity_param_sets_lmic <- readRDS("severity_param_sets_lmic.rds")
load("income_strata_healthcare_capacity.rda")

ndraws=500
parameter_list<-severity_param_sets[1:ndraws]
parameter_list_lmic<-severity_param_sets_lmic[1:ndraws]


severity_param_sets_lmic[[1]]$prob_non_severe_death_treatment
severity_param_sets[[1]]$prob_non_severe_death_treatment


severity_param_sets_lmic[[1]]$prob_non_severe_death_treatment
severity_param_sets_lmic[[1]]$prob_non_severe_death_no_treatment



R0<-3
dt<-0.1
sensitivity_runs_var_select <- function(params) {
  # initial run
  r <- suppressWarnings(do.call(run_explicit_SEEIR_model, params[[1]]))
  t <- seq(from = 1, to = r$parameters$time_period / r$parameters$dt)
  
  #### use the following to work out locations of the vars you want
  index <- squire:::odin_index(r$model)
  
  #### this will get the indices for the infections, deaths, cumulative deaths.
  #### you must have t and time here
  to_keep <- c(index$t, index$time, index$n_E2_I, index$delta_D, index$D)
  
  #### here are others if you want them
  # list(deaths = "delta_D",
  #      infections = "n_E2_I",
  #      hospital_occupancy = c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2", "IRec1", "IRec2"),
  #      ICU_occupancy = c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2"),
  #      hospital_demand = c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2", "IRec1", "IRec2",
  #                          "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1","IOxNotGetDie2"),
  #      ICU_demand = c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
  #                     "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2"))
  
  # assign to our results
  out <- list()
  out[[1]] <- r
  out[[1]]$output <- out[[1]]$output[,to_keep,, drop=FALSE]
  
  # running and storing the model output for each of the different initial seeding cases
  for(i in 2:length(params)) {
    message(paste(i," "), appendLF = "FALSE")
    suppressWarnings(do.call(r$model$set_user, params[[i]]))
    beta <- beta_est_explicit(dur_IMild = r$parameters$dur_IMild,
                              dur_ICase = r$parameters$dur_ICase,
                              prob_hosp = params[[i]]$prob_hosp,
                              mixing_matrix =  squire:::process_contact_matrix_scaled_age(r$parameters$contact_matrix_set[[1]], params[[i]]$population),
                              R0 = params[[i]]$R0
    )
    r$model$set_user(beta_set = beta)
    r$output <- r$model$run(t, replicate = 1)
    out[[i]] <- r
    out[[i]]$output <- out[[i]]$output[,to_keep, , drop=FALSE]
  }
  
  outarray <- array(NA, dim = c(nrow(out[[1]]$output), ncol(out[[1]]$output), length(params)))
  for(i in 1:length(out)){
    outarray[,,i] <- out[[i]]$output
  }
  colnames(outarray) <- names(out[[1]]$output[1,,1])
  r$output <- outarray
  
  return(r)
  
}

format_output_new <- function(x, var_select = NULL, reduce_age = TRUE,
                              combine_compartments = TRUE, date_0 = NULL){
  
  new_index <- function(x) {
    
    index <- squire:::odin_index(x$model)
    nms <- names(x$output[1,,1])
    all_names_simp <- gsub("\\[.*?]", "", nms)
    found <- which(names(index) %in% all_names_simp)
    
    for(i in found) {
      index[[i]] <- which(all_names_simp == names(index[i]))
    }
    
    return(index)
  }
  
  # Get relevant model details
  nt <- nrow(x$output)
  index <- new_index(x)
  all_names <- names(x$output[1,,1])
  all_names_simp <- gsub("\\[.*?]", "", all_names)
  
  # Multi/Single Compartment Variables
  single_compartments <- c("S", "IMild", "R", "D", "n_E2_I", "n_E2_ICase1", "n_E2_IMild", "delta_D")
  multi_compartments <- c("E", "ICase", "IOxGetLive", "IOxGetDie", "IOxNotGetLive", "IOxNotGetDie",
                          "IMVGetLive", "IMVGetDie", "IMVNotGetLive", "IMVNotGetDie", "IRec")
  
  # Summary Values and Relevant Compartments
  summary_variables <- c("deaths", "infections", "hospital_occupancy", "ICU_occupancy", "hospital_demand", "ICU_demand")
  summary_variable_compartments <- list(deaths = "delta_D",
                                        infections = "n_E2_I",
                                        hospital_occupancy = c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2", "IRec1", "IRec2"),
                                        ICU_occupancy = c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2"),
                                        hospital_demand = c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2", "IRec1", "IRec2",
                                                            "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1","IOxNotGetDie2"),
                                        ICU_demand = c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
                                                       "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2"))
  
  # Check var_select contains only variables described above
  if(sum(!(var_select %in% c(single_compartments, multi_compartments, summary_variables))) > 0) {
    stop("Selected variable are not all present in output. Either specify a compartment:\n\n",
         paste0(c(paste0(single_compartments, collapse = ","), "\n\n",
                  paste0(multi_compartments, collapse = ","))),
         "\n\nor a summary compartment:\n\n",
         paste0(summary_variables, collapse = ", "))
  }
  
  # Disaggregating var_select into compartments and summary variables
  compartments <- var_select[!(var_select %in% summary_variables)]
  compartments <- if (identical(compartments, character(0))) NULL else compartments
  summaries <- var_select[var_select %in% summary_variables]
  summaries <- if (identical(summaries, character(0))) NULL else summaries
  
  # Extracting relevant columns for compartment variables
  # -> if var_select = NULL extract all compartments
  # -> if var_select = names specific compartments, extract those
  # -> if var_select = summary variables but no specific compartments, return empty list
  if(is.null(var_select)) {
    compartments <- unique(all_names_simp[!all_names_simp %in% c("step", "time")])
    compartment_output_list <- lapply(compartments, function(j) {
      temp <- x$output[,unlist(index[j]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    })
    names(compartment_output_list) <- compartments
  } else if (!is.null(var_select) & !is.null(compartments)) {
    number_variables <- length(compartments)
    new_compartments <- c()
    for (i in 1:number_variables) {
      if (compartments[i] %in% single_compartments) {
        new_compartments <- c(new_compartments, compartments[i])
      } else {
        temp <- unique(all_names_simp[grepl(paste0("^", compartments[i], "[1-2]"), all_names_simp)])
        new_compartments <- c(new_compartments, temp)
      }
    }
    compartment_output_list <- lapply(new_compartments, function(j) {
      temp <- x$output[,unlist(index[j]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    })
    names(compartment_output_list) <- new_compartments
  } else {
    compartment_output_list <- list()
  }
  
  # summaries
  if (!is.null(var_select) & !is.null(summaries)) {
    summaries_output_list <- vector(mode = "list", length = length(summaries))
    for (i in 1:length(summaries)) {
      indices <- which(summary_variables %in% summaries[i])
      temp_compartments <- summary_variable_compartments[[indices]]
      temp <- x$output[,unlist(index[temp_compartments]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      summaries_output_list[[i]] <- squire:::odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    }
    names(summaries_output_list) <- summaries
  } else {
    summaries_output_list <- list()
  }
  
  # combining outputs for compartments and overall summaries into 1 list
  output_list <- c(compartment_output_list, summaries_output_list)
  vars <- names(output_list)
  n_age_groups <- 17
  
  # generating df of extracted compartment/summary outputs, disaggregated by age or not
  if (reduce_age == TRUE) {
    out <- data.frame("t" = as.numeric(x$output[,index$time,]),
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), nt)),
                      "compartment" = as.character(mapply(rep, vars, nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  } else {
    out <- data.frame("t" = rep(as.numeric(x$output[,index$time, ]), n_age_groups), # ASK OJ TO CHECK THIS
                      "age_group" = rep(1:n_age_groups, each = nt), ##### NEED TO CHANGE ####
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), n_age_groups * nt)),
                      "compartment" = as.character(mapply(rep, vars, n_age_groups*nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  }
  
  # If combine_compartments is TRUE, sum compartments of same type e.g.
  # E1 and E2 together
  if (combine_compartments == TRUE & reduce_age == FALSE) {
    out <- out %>%
      dplyr::mutate(compartment = gsub("[1-2]$", "", .data$compartment)) %>%
      dplyr::group_by(.data$replicate, .data$age_group, .data$compartment, .data$t) %>%
      dplyr::summarise(y = sum(.data$y)) %>%
      dplyr::ungroup()
  } else if (combine_compartments == TRUE & reduce_age == TRUE) {
    out <- out %>%
      dplyr::mutate(compartment = gsub("[1-2]$", "", .data$compartment)) %>%
      dplyr::group_by(.data$replicate, .data$compartment, .data$t) %>%
      dplyr::summarise(y = sum(.data$y)) %>%
      dplyr::ungroup()
  }
  
  # replacting time with date if date_0 is provided
  if(!is.null(date_0)){
    stopifnot(inherits(date_0, "Date"))
    out$date <- as.Date(out$t + date_0,
                        format = "%d/%m/%y")
  }
  
  return(out)
}

params_LIC_mit_FC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe", "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$prob_severe<-x$prob_severe/x$prob_hosp
  x$replicates <- 1
  # add in any other parameters you want here
  x$country <- "Madagascar"
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$population <- squire::get_population(x$country)$n
  x$population <-x$population /sum(x$population )*1000000
  x$hosp_bed_capacity<-sum(x$population)
  x$ICU_bed_capacity<-sum(x$population)
  x$R0 <- 0.55*R0
  x$dt<-dt
  return(x)
})

params_LIC_mit_TC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe",
                "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$replicates <- 1
  x$prob_severe<-x$prob_severe/x$prob_hosp
  # add in any other parameters you want here
  x$country <- "Madagascar"
  
  x$population <- squire::get_population(x$country)
  x$population <-x$population$n /sum(x$population$n)*1000000
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$hosp_bed_capacity<-income_strata_healthcare_capacity$hosp_beds[1]*1000
  x$ICU_bed_capacity<-income_strata_healthcare_capacity$ICU_beds[1]*1000
  x$R0 <- R0*0.55
  x$dt<-dt
  return(x)
})

params_LIC_mit_TC_po  <- lapply(parameter_list_lmic, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe",
                "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$replicates <- 1
  
  # add in any other parameters you want here
  x$country <- "Madagascar"
  x$prob_severe<-x$prob_severe/x$prob_hosp
  x$population <- squire::get_population(x$country)
  x$population <-x$population$n /sum(x$population$n)*1000000
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$hosp_bed_capacity<-(income_strata_healthcare_capacity$hosp_beds[1]+income_strata_healthcare_capacity$ICU_beds[1])*1000
  x$ICU_bed_capacity<-0
  x$R0 <- R0*0.55
  x$dt<-dt
  return(x)
})

params_LMIC_mit_FC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe", "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$prob_severe<-x$prob_severe/x$prob_hosp
  x$replicates <- 1
  # add in any other parameters you want here
  x$country <- "Nicaragua"
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$population <- squire::get_population(x$country)$n
  x$population <-x$population /sum(x$population)*1000000
  x$hosp_bed_capacity<-sum(x$population)
  x$ICU_bed_capacity<-sum(x$population)
  x$R0 <- 0.55*R0
  x$dt<-dt
  return(x)
})

params_LMIC_mit_TC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe",
                "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$replicates <- 1
  x$prob_severe<-x$prob_severe/x$prob_hosp
  # add in any other parameters you want here
  x$country <- "Nicaragua"
  
  x$population <- squire::get_population(x$country)
  x$population <-x$population$n /sum(x$population$n)*1000000
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$hosp_bed_capacity<-income_strata_healthcare_capacity$hosp_beds[2]*1000
  x$ICU_bed_capacity<-income_strata_healthcare_capacity$ICU_beds[2]*1000
  x$R0 <- R0*0.55
  x$dt<-dt
  return(x)
})

params_LMIC_mit_TC_po  <- lapply(parameter_list_lmic, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe",
                "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$replicates <- 1
  x$prob_severe<-x$prob_severe/x$prob_hosp
  # add in any other parameters you want here
  x$country <- "Nicaragua"
  x$population <- squire::get_population(x$country)
  x$population <-x$population$n /sum(x$population$n)*1000000
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$hosp_bed_capacity<-(income_strata_healthcare_capacity$hosp_beds[2]+income_strata_healthcare_capacity$ICU_beds[2])*1000
  x$ICU_bed_capacity<-0
  x$R0 <- R0*0.55
  x$dt<-dt
  return(x)
})


params_UMIC_mit_FC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe", "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$prob_severe<-x$prob_severe/x$prob_hosp
  x$replicates <- 1
  # add in any other parameters you want here
  x$country <- "Grenada"
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$population <- squire::get_population(x$country)$n
  x$population <-x$population /sum(x$population )*1000000
  x$hosp_bed_capacity<-sum(x$population)
  x$ICU_bed_capacity<-sum(x$population)
  x$R0 <- 0.55*R0
  x$dt<-dt
  return(x)
})

params_UMIC_mit_TC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe",
                "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$replicates <- 1
  
  # add in any other parameters you want here
  x$country <- "Grenada"
  x$prob_severe<-x$prob_severe/x$prob_hosp
  x$population <- squire::get_population(x$country)
  x$population <-x$population$n /sum(x$population$n)*1000000
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$hosp_bed_capacity<-income_strata_healthcare_capacity$hosp_beds[3]*1000
  x$ICU_bed_capacity<-income_strata_healthcare_capacity$ICU_beds[3]*1000
  x$R0 <- R0*0.55
  x$dt<-dt
  return(x)
})

params_HIC_mit_FC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe", "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$prob_severe<-x$prob_severe/x$prob_hosp
  x$replicates <- 1
  # add in any other parameters you want here
  x$country <- "Malta"
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$population <- squire::get_population(x$country)$n
  x$population <-x$population /sum(x$population )*1000000
  x$hosp_bed_capacity<-sum(x$population)
  x$ICU_bed_capacity<-sum(x$population)
  x$R0 <- 0.55*R0
  x$dt<-dt
  return(x)
})

params_HIC_mit_TC  <- lapply(parameter_list, function(x) {
  x <- tail(as.list(x), -2)
  names(x) <- c("prob_hosp", "prob_severe",
                "prob_non_severe_death_treatment",
                "prob_severe_death_treatment","prob_severe_death_no_treatment",
                "prob_non_severe_death_no_treatment")
  x$replicates <- 1
  x$prob_severe<-x$prob_severe/x$prob_hosp
  # add in any other parameters you want here
  x$country <- "Malta"
  x$population <- squire::get_population(x$country)
  x$population <-x$population$n /sum(x$population$n)*1000000
  x$contact_matrix_set=squire::get_mixing_matrix(x$country)
  x$hosp_bed_capacity<-income_strata_healthcare_capacity$hosp_beds[4]*1000
  x$ICU_bed_capacity<-income_strata_healthcare_capacity$ICU_beds[4]*1000
  x$R0 <- R0*0.55
  x$dt<-dt
  return(x)
})


LIC_mit_FC<- sensitivity_runs_var_select(params_LIC_mit_FC)
LIC_mit_TC<- sensitivity_runs_var_select(params_LIC_mit_TC)
LIC_mit_TC_po<- sensitivity_runs_var_select(params_LIC_mit_TC_po)
LIC_mit_FC$parameters$replicates=ndraws
LIC_mit_TC$parameters$replicates=ndraws
LIC_mit_TC_po$parameters$replicates=ndraws


LMIC_mit_FC<- sensitivity_runs_var_select(params_LMIC_mit_FC)
LMIC_mit_TC<- sensitivity_runs_var_select(params_LMIC_mit_TC)
LMIC_mit_TC_po<- sensitivity_runs_var_select(params_LMIC_mit_TC_po)
LMIC_mit_FC$parameters$replicates=ndraws
LMIC_mit_TC$parameters$replicates=ndraws
LMIC_mit_TC_po$parameters$replicates=ndraws

LMIC_mit_FC$parameters
LMIC_mit_TC$parameters

UMIC_mit_FC<- sensitivity_runs_var_select(params_UMIC_mit_FC)
UMIC_mit_TC<- sensitivity_runs_var_select(params_UMIC_mit_TC)
UMIC_mit_FC$parameters$replicates=ndraws
UMIC_mit_TC$parameters$replicates=ndraws

HIC_mit_FC<- sensitivity_runs_var_select(params_HIC_mit_FC)
HIC_mit_TC<- sensitivity_runs_var_select(params_HIC_mit_TC)
HIC_mit_FC$parameters$replicates=ndraws
HIC_mit_TC$parameters$replicates=ndraws


LIC_mit_FC_out <- format_output_new(LIC_mit_FC, var_select = c("deaths"), date_0 = Sys.Date())
LIC_mit_FC_out <- LIC_mit_FC_out %>%
  mutate(replicate = factor(replicate))

LIC_mit_FC_out_inf <- format_output_new(LIC_mit_FC, var_select = c("infections"), date_0 = Sys.Date())
LIC_mit_FC_out_inf <- LIC_mit_FC_out_inf %>%
  mutate(replicate = factor(replicate))

LIC_mit_TC_out <- format_output_new(LIC_mit_TC, var_select = c("deaths"), date_0 = Sys.Date())
LIC_mit_TC_out <- LIC_mit_TC_out %>%
  mutate(replicate = factor(replicate))

LIC_mit_TC_out_inf <- format_output_new(LIC_mit_TC, var_select = c("infections"), date_0 = Sys.Date())
LIC_mit_TC_out_inf <- LIC_mit_TC_out_inf %>%
  mutate(replicate = factor(replicate))

LIC_mit_TC_po_out <- format_output_new(LIC_mit_TC_po, var_select = c("deaths"), date_0 = Sys.Date())
LIC_mit_TC_po_out <- LIC_mit_TC_po_out %>%
  mutate(replicate = factor(replicate))

LIC_mit_TC_po_out_inf <- format_output_new(LIC_mit_TC_po, var_select = c("infections"), date_0 = Sys.Date())
LIC_mit_TC_po_out_inf <- LIC_mit_TC_po_out_inf %>%
  mutate(replicate = factor(replicate))


LMIC_mit_FC_out <- format_output_new(LMIC_mit_FC, var_select = c("deaths"), date_0 = Sys.Date())
LMIC_mit_FC_out <- LMIC_mit_FC_out %>%
  mutate(replicate = factor(replicate))

LMIC_mit_FC_out_inf <- format_output_new(LMIC_mit_FC, var_select = c("infections"), date_0 = Sys.Date())
LMIC_mit_FC_out_inf <- LMIC_mit_FC_out_inf %>%
  mutate(replicate = factor(replicate))

LMIC_mit_TC_out <- format_output_new(LMIC_mit_TC, var_select = c("deaths"), date_0 = Sys.Date())
LMIC_mit_TC_out <- LMIC_mit_TC_out %>%
  mutate(replicate = factor(replicate))

LMIC_mit_TC_out_inf <- format_output_new(LMIC_mit_TC, var_select = c("infections"), date_0 = Sys.Date())
LMIC_mit_TC_out_inf <- LMIC_mit_TC_out_inf %>%
  mutate(replicate = factor(replicate))

LMIC_mit_TC_po_out <- format_output_new(LMIC_mit_TC_po, var_select = c("deaths"), date_0 = Sys.Date())
LMIC_mit_TC_po_out <- LMIC_mit_TC_po_out %>%
  mutate(replicate = factor(replicate))

LMIC_mit_TC_po_out_inf <- format_output_new(LMIC_mit_TC_po, var_select = c("infections"), date_0 = Sys.Date())
LMIC_mit_TC_po_out_inf <- LMIC_mit_TC_po_out_inf %>%
  mutate(replicate = factor(replicate))


UMIC_mit_FC_out <- format_output_new(UMIC_mit_FC, var_select = c("deaths"), date_0 = Sys.Date())
UMIC_mit_FC_out <- UMIC_mit_FC_out %>%
  mutate(replicate = factor(replicate))

UMIC_mit_FC_out_inf <- format_output_new(UMIC_mit_FC, var_select = c("infections"), date_0 = Sys.Date())
UMIC_mit_FC_out_inf <- UMIC_mit_FC_out_inf %>%
  mutate(replicate = factor(replicate))

UMIC_mit_TC_out <- format_output_new(UMIC_mit_TC, var_select = c("deaths"), date_0 = Sys.Date())
UMIC_mit_TC_out <- UMIC_mit_TC_out %>%
  mutate(replicate = factor(replicate))

UMIC_mit_TC_out_inf <- format_output_new(UMIC_mit_TC, var_select = c("infections"), date_0 = Sys.Date())
UMIC_mit_TC_out_inf <- UMIC_mit_TC_out_inf %>%
  mutate(replicate = factor(replicate))



HIC_mit_FC_out <- format_output_new(HIC_mit_FC, var_select = c("deaths"), date_0 = Sys.Date())
HIC_mit_FC_out <- HIC_mit_FC_out %>%
  mutate(replicate = factor(replicate))

HIC_mit_FC_out_inf <- format_output_new(HIC_mit_FC, var_select = c("infections"), date_0 = Sys.Date())
HIC_mit_FC_out_inf <- HIC_mit_FC_out_inf %>%
  mutate(replicate = factor(replicate))

HIC_mit_TC_out <- format_output_new(HIC_mit_TC, var_select = c("deaths"), date_0 = Sys.Date())
HIC_mit_TC_out <- HIC_mit_TC_out %>%
  mutate(replicate = factor(replicate))

HIC_mit_TC_out_inf <- format_output_new(HIC_mit_TC, var_select = c("infections"), date_0 = Sys.Date())
HIC_mit_TC_out_inf <- HIC_mit_TC_out_inf %>%
  mutate(replicate = factor(replicate))


HIC_inf<-data.frame("Income"="HIC","HS"="Infinite capacity",
               "IFR"=sapply(1:ndraws,function(z){sum(HIC_mit_FC_out$y[HIC_mit_FC_out$replicate==z])/sum(HIC_mit_FC_out_inf$y[HIC_mit_FC_out_inf$replicate==z])}),
               "Mort"=sapply(1:ndraws,function(z){sum(HIC_mit_FC_out$y[HIC_mit_FC_out$replicate==z])})
               )
HIC_tc<-data.frame("Income"="HIC","HS"="true capacity",
                   "IFR"=sapply(1:ndraws,function(z){sum(HIC_mit_TC_out$y[HIC_mit_TC_out$replicate==z])/sum(HIC_mit_TC_out_inf$y[HIC_mit_TC_out_inf$replicate==z])}),
                   "Mort"=sapply(1:ndraws,function(z){sum(HIC_mit_TC_out$y[HIC_mit_TC_out$replicate==z])})
)


UMIC_inf<-data.frame("Income"="UMIC","HS"="Infinite capacity",
                    "IFR"=sapply(1:ndraws,function(z){sum(UMIC_mit_FC_out$y[UMIC_mit_FC_out$replicate==z])/sum(UMIC_mit_FC_out_inf$y[UMIC_mit_FC_out_inf$replicate==z])}),
                    "Mort"=sapply(1:ndraws,function(z){sum(UMIC_mit_FC_out$y[UMIC_mit_FC_out$replicate==z])})
)
UMIC_tc<-data.frame("Income"="UMIC","HS"="true capacity",
                   "IFR"=sapply(1:ndraws,function(z){sum(UMIC_mit_TC_out$y[UMIC_mit_TC_out$replicate==z])/sum(UMIC_mit_TC_out_inf$y[UMIC_mit_TC_out_inf$replicate==z])}),
                   "Mort"=sapply(1:ndraws,function(z){sum(UMIC_mit_TC_out$y[UMIC_mit_TC_out$replicate==z])})
)

LMIC_inf<-data.frame("Income"="LMIC","HS"="Infinite capacity",
                     "IFR"=sapply(1:ndraws,function(z){sum(LMIC_mit_FC_out$y[LMIC_mit_FC_out$replicate==z])/sum(LMIC_mit_FC_out_inf$y[LMIC_mit_FC_out_inf$replicate==z])}),
                     "Mort"=sapply(1:ndraws,function(z){sum(LMIC_mit_FC_out$y[LMIC_mit_FC_out$replicate==z])})
)
LMIC_tc<-data.frame("Income"="LMIC","HS"="true capacity",
                    "IFR"=sapply(1:ndraws,function(z){sum(LMIC_mit_TC_out$y[LMIC_mit_TC_out$replicate==z])/sum(LMIC_mit_TC_out_inf$y[LMIC_mit_TC_out_inf$replicate==z])}),
                    "Mort"=sapply(1:ndraws,function(z){sum(LMIC_mit_TC_out$y[LMIC_mit_TC_out$replicate==z])})
)

LIC_inf<-data.frame("Income"="LIC","HS"="Infinite capacity",
                     "IFR"=sapply(1:ndraws,function(z){sum(LIC_mit_FC_out$y[LIC_mit_FC_out$replicate==z])/sum(LIC_mit_FC_out_inf$y[LIC_mit_FC_out_inf$replicate==z])}),
                     "Mort"=sapply(1:ndraws,function(z){sum(LIC_mit_FC_out$y[LIC_mit_FC_out$replicate==z])})
)
LIC_tc<-data.frame("Income"="LIC","HS"="true capacity",
                    "IFR"=sapply(1:ndraws,function(z){sum(LIC_mit_TC_out$y[LIC_mit_TC_out$replicate==z])/sum(LIC_mit_TC_out_inf$y[LIC_mit_TC_out_inf$replicate==z])}),
                    "Mort"=sapply(1:ndraws,function(z){sum(LIC_mit_TC_out$y[LIC_mit_TC_out$replicate==z])})
)

LIC_po<-data.frame("Income"="LIC","HS"="Poor outcomes",
                   "IFR"=sapply(1:ndraws,function(z){sum(LIC_mit_TC_po_out$y[LIC_mit_TC_po_out$replicate==z])/sum(LIC_mit_TC_po_out_inf$y[LIC_mit_TC_po_out_inf$replicate==z])}),
                   "Mort"=sapply(1:ndraws,function(z){sum(LIC_mit_TC_po_out$y[LIC_mit_TC_po_out$replicate==z])})
)

LMIC_po<-data.frame("Income"="LMIC","HS"="Poor outcomes",
                   "IFR"=sapply(1:ndraws,function(z){sum(LMIC_mit_TC_po_out$y[LMIC_mit_TC_po_out$replicate==z])/sum(LMIC_mit_TC_po_out_inf$y[LMIC_mit_TC_po_out_inf$replicate==z])}),
                   "Mort"=sapply(1:ndraws,function(z){sum(LMIC_mit_TC_po_out$y[LMIC_mit_TC_po_out$replicate==z])})
)

all<-rbind(HIC_inf,UMIC_inf,LMIC_inf,LIC_inf,HIC_tc,UMIC_tc,LMIC_tc,LIC_tc,LMIC_po,LIC_po)
all$Income<-factor(all$Income,
                            levels = c("LIC","LMIC","UMIC","HIC"))

all$HS<-factor(all$HS,
                   levels = c("Infinite capacity","true capacity","Poor outcomes"))


write.csv(all,"IFR_mortality_uncertainty.csv",row.names=F)
check<-read.csv("IFR_mortality_uncertainty.csv")
check$Income<-factor(all$Income,
                   levels = c("LIC","LMIC","UMIC","HIC"))

check$HS<-factor(all$HS,
               levels = c("Infinite capacity","true capacity","Poor outcomes"))

g <- ggplot(check, aes(Income, IFR))
g + geom_boxplot(aes(fill=factor(HS))) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

g <- ggplot(all, aes(Income, Mort))
g + geom_boxplot(aes(fill=factor(HS))) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



mpg

g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(aes(fill=factor(cyl))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

unmit<-run_explicit_SEEIR_model(country="India",R0=3,hosp_bed_capacity=1000000000000,ICU_bed_capacity = 100000000000000000000,dt=0.01)

out<-format_output(unmit, var_select = "deaths")
out_inf<-format_output(unmit, var_select = "infections")
sum(out$y)/sum(out_inf$y)

plot(r,var_select = "deaths")


?run_explicit_SEEIR_model
# Get the mixing matrix
pop <- get_population("India")
std_population <- pop$n/sum(pop$n)*100000000
contact_matrix <- get_mixing_matrix("India")

no_action<-run_explicit_SEEIR_model(country="India",
  tt_contact_matrix = c(0,30,210),
contact_matrix_set = list(contact_matrix,
                          contact_matrix*0.55,contact_matrix),
population = std_population)

  t1 <- calibrate(country = "India",
                  population<-std_population,
                  contact_matrix_set=contact_matrix,
                  hosp_bed_capacity=1000000000000,
                  ICU_bed_capacity = 100000000000000000000,
                  deaths = 10,
                  reporting_fraction = 1,
                  dt = 0.1,
                  time_period = 400,
                  replicates=50,
                  R0=3)

  p2 <- projections(r = t1, R0_change = c(0.55, 1), tt_R0 = c(0, 180))
  p3 <- projections(r = t1, R0_change = c(0.25, 1), tt_R0 = c(0, 180))
  p4 <- projections(r = t1, R0_change = c(0.8, 1), tt_R0 = c(0, 180))
  
projection_plotting(r_list = list(t1,p2,p3,p4),
                    scenarios = c("Unmitigated","Mitigation","Supp_lift","no_action"),
                    var_select = c("deaths"),
                    add_parms_to_scenarios = FALSE,
                    ci = TRUE,summarise = TRUE)

  R0 = 2.5,
time_period = 400,
dt = 0.1,
replicates = 5
)

