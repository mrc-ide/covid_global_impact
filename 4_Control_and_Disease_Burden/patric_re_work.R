
#library(devtools)
#devtools::install_github("mrc-ide/squire",ref="odin_patch")
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse);
library(ggplot2);
library(squire)
rm(list=ls())
setwd("C:/Users/Patrick/Imperial College London/ncov - Documents/2019-nCoV/LMIC/LMIC Parameters")
severity_param_sets <- readRDS("severity_param_sets.rds")
severity_param_sets_lmic <- readRDS("severity_param_sets_lmic.rds")
load("income_strata_healthcare_capacity.rda")

ndraws=100
parameter_list<-severity_param_sets[1:ndraws]
parameter_list_lmic<-severity_param_sets_lmic[1:ndraws]
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


quantile(sapply(1:ndraws,function(z){sum(LIC_mit_FC_out$y[LIC_mit_FC_out$replicate==z])/sum(LIC_mit_FC_out_inf$y[LIC_mit_FC_out_inf$replicate==z])}),c(0.025,0.5,0.975))

quantile(sapply(1:ndraws,function(z){sum(LIC_mit_TC_out$y[LIC_mit_TC_out$replicate==z])/sum(LIC_mit_TC_out_inf$y[LIC_mit_TC_out_inf$replicate==z])}),c(0.025,0.5,0.975))

quantile(sapply(1:ndraws,function(z){sum(LIC_mit_TC_po_out$y[LIC_mit_TC_po_out$replicate==z])/sum(LIC_mit_TC_po_out_inf$y[LIC_mit_TC_po_out_inf$replicate==z])}),c(0.025,0.5,0.975))


quantile(sapply(1:ndraws,function(z){sum(LMIC_mit_FC_out$y[LMIC_mit_FC_out$replicate==z])/sum(LMIC_mit_FC_out_inf$y[LMIC_mit_FC_out_inf$replicate==z])}),c(0.025,0.5,0.975))

quantile(sapply(1:ndraws,function(z){sum(LMIC_mit_TC_out$y[LMIC_mit_TC_out$replicate==z])/sum(LMIC_mit_TC_out_inf$y[LMIC_mit_TC_out_inf$replicate==z])}),c(0.025,0.5,0.975))

quantile(sapply(1:ndraws,function(z){sum(LMIC_mit_TC_po_out$y[LMIC_mit_TC_po_out$replicate==z])/sum(LMIC_mit_TC_po_out_inf$y[LMIC_mit_TC_po_out_inf$replicate==z])}),c(0.025,0.5,0.975))


quantile(sapply(1:ndraws,function(z){sum(UMIC_mit_FC_out$y[UMIC_mit_FC_out$replicate==z])/sum(UMIC_mit_FC_out_inf$y[UMIC_mit_FC_out_inf$replicate==z])}),c(0.025,0.5,0.975))

quantile(sapply(1:ndraws,function(z){sum(UMIC_mit_TC_out$y[UMIC_mit_TC_out$replicate==z])/sum(UMIC_mit_TC_out_inf$y[UMIC_mit_TC_out_inf$replicate==z])}),c(0.025,0.5,0.975))


quantile(sapply(1:ndraws,function(z){sum(HIC_mit_FC_out$y[HIC_mit_FC_out$replicate==z])/sum(HIC_mit_FC_out_inf$y[HIC_mit_FC_out_inf$replicate==z])}),c(0.025,0.5,0.975))

quantile(sapply(1:ndraws,function(z){sum(HIC_mit_TC_out$y[HIC_mit_TC_out$replicate==z])/sum(HIC_mit_TC_out_inf$y[HIC_mit_TC_out_inf$replicate==z])}),c(0.025,0.5,0.975))

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
mpg

g <- ggplot(all, aes(Income, IFR))
g + geom_boxplot(aes(fill=factor(HS))) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


install.packages("ggthemes")
library(ggthemes)
mpg

g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(aes(fill=factor(cyl))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")



quantile(sapply(1:ndraws,function(z){sum(LIC_mit_TC_out$y[LIC_mit_TC_out$replicate==z])}),c(0.025,0.5,0.975))
quantile(sapply(1:ndraws,function(z){sum(LIC_mit_TC_po_out$y[LIC_mit_TC_po_out$replicate==z])}),c(0.025,0.5,0.975))

quantile(sapply(1:ndraws,function(z){sum(LIC_mit_FC_out$y[LIC_mit_FC_out$replicate==z])}),c(0.025,0.5,0.975))        
         #/sapply(1:ndraws,function(z){sum(LIC_mit_TC_out_inf$y[LIC_mit_TC_out_inf$replicate==z])}))


LIC_mit_TC_po_out <- format_output_new(LIC_mit_TC_po, var_select = c("deaths"), date_0 = Sys.Date())
LIC_mit_TC_po_out <- LIC_mit_TC_po_out %>%
  mutate(replicate = factor(replicate))

LIC_mit_FC_out$scenario <- "FC"
LIC_mit_TC_out$scenario <- "FT"
LIC_mit_TC_po_out$scenario <- "FT_po"
LIC <- rbind(LIC_mit_FC_out,LIC_mit_TC_out,LIC_mit_TC_po_out)
summarised_LIC <- LIC %>% group_by(compartment,t, scenario) %>%
  summarise(ymin = quantile(y,c(0.025,0.975))[1],
            ymax = quantile(y,c(0.025,0.975))[2],
            y=mean(y))


ggplot(summarised_LIC,
       aes(x=t, y=y, group=interaction(scenario),
           ymin = ymin, ymax = ymax,
           fill = scenario, color = scenario)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme_bw()



x <- format_output_new(LIC_mit_FC, var_select = c("deaths"), date_0 = Sys.Date())
x <- x %>%
  mutate(replicate = factor(replicate))

y <- format_output_new(LIC_mit_FC, var_select = c("infections"), date_0 = Sys.Date())
y <- y %>%
  mutate(replicate = factor(replicate))


x$parameters




x <- run_explicit_SEEIR_model(country = "United Kingdom",
                              R0 = 2.4, dt = 0.1,
                              hosp_bed_capacity = 1000000000,
                              ICU_bed_capacity = 1000000000, 
                              replicates = 2,
                              prob_non_severe_death_no_treatment = rep(0, 17))
colnames(x$output[,,1])
plot(rowSums(x$output[,grep("IMVGetDie1",colnames(x$output[,,1])),1]))

deaths <- format_output(x, var_select = "deaths")
deaths <- deaths %>%
  group_by(replicate) %>%
  summarise(death = sum(y))

deaths

x <- run_explicit_SEEIR_model(country = "United Kingdom",
                              R0 = 2.4, dt = 0.1,
                              hosp_bed_capacity = 10000000000,
                              ICU_bed_capacity = 100000000000)

deaths <- format_output(x, var_select = "deaths")

deaths <- deaths %>%
  group_by(replicate) %>%
  summarise(death = sum(y))

infections <- format_output(x, var_select = "infections")

infections <- infections %>%
  group_by(replicate) %>%
  summarise(infections = sum(y))

deaths/infections


deaths


??run_explicit_SEEIR_model

r<-run_explicit_SEEIR_model(country = "United Kingdom",R0=2.4)

format(r,vars="deaths")

params_LIC_mit_FC[[1]]$prob_severe
params_LIC_mit_FC[[1]]$prob_hosp

params_LIC_mit_FC[[10]]

params_LIC_mit_FC[[1]]$prob_severe
params_LIC_mit_TC
params_LIC_mit_TC

params<-params_LIC_mit_FC
r$parameters$prob_hosp
r$parameters$prob_severe_death_no_treatment
r$parameters$prob_non_severe_death_no_treatment
r$parameters$prob_severe_death_treatment
  
r$parameters$ICU_bed_capacity


?run_explicit_SEEIR_model
sensitivity_runs <- function(params) {

  # initial run
  r <- do.call(run_explicit_SEEIR_model, params[[1]])
  t <- seq(from = 1, to = r$parameters$time_period / r$parameters$dt)


  # assign to our results
  out <- list()
  out[[1]] <- r
  # running and storing the model output for each of the different initial seeding cases
  for(i in 2:length(params)) {
    print(i)
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
  }

  outarray <- array(NA, dim = c(nrow(out[[1]]$output), ncol(out[[1]]$output), length(params)))
  for(i in 1:length(out)){
    outarray[,,i] <- out[[i]]$output
  }
  colnames(outarray) <- names(r$output[1,,1])
  r$output <- outarray



  return(r)


}

LIC_mit_TC_old<- sensitivity_runs(params_LIC_mit_TC)
LIC_mit_FC_old<- sensitivity_runs(params_LIC_mit_FC)
LIC_mit_TC_old$parameters$replicates=ndraws
LIC_mit_FC_old$parameters$replicates=ndraws


x <- format_output_new(LIC_mit_FC_old, var_select = "deaths", date_0 = Sys.Date())
x <- x %>%
  mutate(replicate = factor(replicate))

y<- format_output_new(LIC_mit_TC_old, var_select = "deaths", date_0 = Sys.Date())
y <- y %>%
  mutate(replicate = factor(replicate))

summary(sapply(1:ndraws,function(z){sum(x$y[x$replicate==z])})*66.5)

summary(sapply(1:ndraws,function(z){sum(y$y[y$replicate==z])})*66.5)


seeding_cases

# tehre is this. unfortunately you'll get less control over plotting
get <- projection_plotting(list(LIC_mit_FC_old, LIC_mit_TC_old),
                           scenarios = c("FC","TC"),
                           var_select = c("deaths"),
                           add_parms_to_scenarios = FALSE,
                           summarise = TRUE, ci = TRUE)
get
## or use the abridged


colnames(LIC_mit_FC$output)
sum(LIC_mit_FC$output[7282,37:53,1])
LIC_mit_FC$output[7282,37:53,1]/LIC_mit_FC$output[7282,29:36,1]
params_LIC_mit_FC
LIC_mit_FC$output[7282,29:36,1]
LIC_mit_TC<- sensitivity_runs_var_select(params_LIC_mit_TC)
LIC_mit_TC_po<- sensitivity_runs_var_select(params_LIC_mit_TC_po)
LIC_mit_FC<- sensitivity_runs_var_select(params_LIC_mit_FC)
LIC_mit_TC$parameters$replicates=ndraws
LIC_mit_FC$parameters$replicates=ndraws
LIC_mit_TC_po$parameters$replicates=ndraws
LIC_mit_TC$parameters


LIC_mit_TC$parameters


x <- format_output_new(LIC_mit_FC, var_select = c("deaths"), date_0 = Sys.Date())
x <- x %>%
  mutate(replicate = factor(replicate))

y <- format_output_new(LIC_mit_FC, var_select = c("infections"), date_0 = Sys.Date())
y <- y %>%
  mutate(replicate = factor(replicate))
y

sapply(1:ndraws,function(c){sum(x$y[x$replicate==c])})


y<- format_output_new(LIC_mit_TC, var_select = c("deaths"), date_0 = Sys.Date())
y <- y %>%
  mutate(replicate = factor(replicate))


z<- format_output_new(LIC_mit_TC, var_select = c("infections"), date_0 = Sys.Date())
z <- z %>%
  mutate(replicate = factor(replicate))

summary(y$y)
#y$
severity_param_sets[[10]]$prob_hosp/severity_param_sets[[10]]$Adj_IFR
severity_param_sets[[10]]$prob_severe/severity_param_sets[[10]]$Adj_IFR

severity_param_sets[[9]]$prob_hosp/severity_param_sets[[10]]$prob_hosp
severity_param_sets[[9]]$prob_severe/severity_param_sets[[10]]$prob_severe
severity_param_sets[[9]]$/severity_param_sets[[10]]$prob_non_severe_death_no_treatment

severity_param_sets[[9]]
severity_param_sets[[10]]


severity_param_sets[[10]]$prob_hosp
mean(sapply(1:ndraws,function(c){sum(x$y[x$replicate==c])})*66.5)

mean(sapply(1:ndraws,function(c){sum(x$y[x$replicate==c])})/sapply(1:ndraws,function(t){sum(y$y[y$replicate==t])}))


sapply(1:ndraws,function(c){sum(x$y[x$replicate==c])})

# and then to plot these together
unique(x$compartment)


