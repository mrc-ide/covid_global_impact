calc_severity_parameters <- function(prob, raw_pop) {
  prop_75_80 <- raw_pop[16]/sum(raw_pop[16:length(raw_pop)])
  prop_80_plus <- 1 - prop_75_80
  prob[length(prob) - 1] <- prop_75_80 * prob[length(prob)-1] + prop_80_plus * prob[length(prob)]
  prob <- prob[-length(prob)]
  return(prob)
}

generate_contact_matrix <- function(country_population, mixing_matrix) {
  if (length(country_population) != dim(mixing_matrix)[1]) {
    return("Dimensions of demography and matrix don't match up")
  }
  
  # Convert Unbalanced Matrix of Per-Capita Rates to Total Number of Contacts
  # Between Diff Age Groups and Balance By Taking the Mean of i->j and j->i
  MIJ <- t(sapply(seq(country_population),function(x){
    mixing_matrix[x,] * country_population[x]
  }))
  adjust_mat <- (MIJ + t(MIJ))/2 # symmetric and balanced
  
  # Convert to New Per-Capita Rates By Dividing By Population
  # Resulting Matrix Is Asymmetric But Balanced 
  # Asymmetric in that c_ij != c_ji BUT Total Number of Contacts i->j and j->i
  # Is Balanced (so when we divide by pop at end, will be balanced)
  new_mix_mat <- t(sapply(seq(country_population), function(x) {
    adjust_mat[x, ] / country_population[x]
  }))
  
  # Adjusting to create input for model i.e. per capita rates divided by
  # population to give the number of contacts made on each individual 
  return(new_mix_mat)
}

get_required_beta <- function(duration_infectiousness, mixing_matrix, R0) {
  ng_eigen <- Re(eigen(mixing_matrix)$val[1])
  # remember: R0 <- ng_eigen * beta * duration_infectiousness
  beta <- R0/(ng_eigen * duration_infectiousness)
  return(beta)
}

calc_hospitalisation <- function(index, prob_hosp, infection_incidence) {
  temp <- unname(unlist(infection_incidence[, index]))
  hosp_vector <- vector(mode = "numeric", length = length(temp))
  for (i in 1:length(temp)) {
    hosp_vector[i] <- rbinom(n = 1, size = temp[i], prob_hosp)
  }
  return(hosp_vector)
} 

calc_ICU <- function(index, prob_ICU_given_hosp, hospitalisation_incidence) {
  temp <- hospitalisation_incidence[, index]
  ICU_vector <- vector(mode = "numeric", length = length(temp))
  for (i in 1:length(temp)) {
    ICU_vector[i] <- rbinom(n = 1, size = temp[i], prob_ICU_given_hosp)
  }
  return(ICU_vector)
}

calc_ICU_red <- function(index, prob_ICU_given_hosp, hospitalisation_incidence) {
  temp <- hospitalisation_incidence[index]
  ICU_vector <- vector(mode = "numeric", length = length(temp))
  for (i in 1:length(temp)) {
    ICU_vector[i] <- rbinom(n = 1, size = temp[i], prob_ICU_given_hosp)
  }
  return(ICU_vector)
}

calc_deaths <- function(index, prob_death, infection_incidence) {
  temp <- unname(unlist(infection_incidence[, index]))
  death_vector <- vector(mode = "numeric", length = length(temp))
  for (i in 1:length(temp)) {
    death_vector[i] <- rbinom(n = 1, size = temp[i], prob_death)
  }
  return(death_vector)
}

run_SEEIR_model <- function(dt, N_age, S0, E0, I0, R0, dur_E, dur_I, R0_req, number_betas, tt_beta, 
                             matrices_set, tt_matrix, plot, contact_matrix, time_period, replicates) {
  
  # Load Model Instance
  basic_SEEIR <- odin::odin("Model/SEEIR_Stochastic_Model.R")
  
  # Convert and Generate Parameters As Required
  gamma_E <- 2 * 1/dur_E
  gamma_I <- 1/dur_I
  beta <- get_required_beta(dur_I, contact_matrix, R0_req) # beta <- R0_req/(ng_eigen * dur_I)
  beta_set <- rep(beta, number_betas)

  # Collate Parameters Into List
  pars <- list(S0 = S0, E0 = E0, I0 = I0, R0 = R0, 
               gamma_E = gamma_E, gamma_I = gamma_I, 
               tt_beta = tt_beta, beta_set = beta_set,
               N_age = N_age,
               tt_matrix = tt_matrix, mix_mat_set = matrices_set, 
               dt = dt)
  
  # Running the Model
  mod <- basic_SEEIR(user = pars)
  t <- seq(from = 1, to = time_period/dt)
  tmp <- mod$run(t, replicate = replicates)
  results <- mod$transform_variables(tmp)
  
  # Different Output Depending On Number Replicates Required
  if (replicates == 1) {
    S <- apply(results$S[, , 1], 1, sum)
    E <- apply(results$E1[, , 1], 1, sum) + apply(results$E2[, , 1], 1, sum)
    I <- apply(results$I[, , 1], 1, sum)
    R <- apply(results$R[, , 1], 1, sum)
    
    # Outputs
    epidemic_final_size <- max(R)/max(S)
    incidence_infections <- data.frame(time = results$time, incidence = results$n_EI[, , 1])
    
    model_output <- data.frame(t = results$time, S = S, E = E, I = I, R = R)
    model_output <- model_output %>%
      gather(class, value, - t)
    
    # Plotting
    if (plot == TRUE) {
      plot(apply(results$R, 2, max)/apply(results$S, 2, max), type = "l", ylim = c(0, 1))
      a <- ggplot(model_output, aes(x = t, y = value, colour = class)) +
        geom_line(size = 2) +
        labs(x = "Time (Days)", y = "") +
        theme_bw() +
        lims(y = c(0, max(model_output$value))) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              legend.position = "right", legend.title = element_blank(),
              axis.title.y = element_text(size = 13.5, vjust = +3), axis.title.x = element_text(size = 13.5, vjust = +0),
              plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
              legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))
      print(a)
    }
    return(list(final_size = epidemic_final_size,
                infection_incidence = incidence_infections,
                model_output_full = model_output))
  } else {
    output_list <- list()
    for (j in 1:replicates) {
      output_list[[j]] <- results$n_EI[, , j]
      time <- results$time[, 1]
    }
    return(list(infection_incidence = output_list,
                time = time))
  }
}

# Convert Raw Model Output (Timestep <1 Day) Into Daily Incidence
get_daily_incidence <- function(incidence_infections) {
  age_groups <- c("0_5", "5_10", "10_15", "15_20", "20_25", "25_30", "30_35", "35_40", "40_45", "45_50", "50_55", "55_60", "60_65", "65_70", "70-75", "75+")
  colnames(incidence_infections) <- c("time", age_groups)
  incidence_infections <- incidence_infections %>%
    mutate(day = ceiling(time)) %>%
    select(day, everything(), -time) %>%
    gather(age_group, value, -day) %>%
    mutate(age_group = factor(age_group, levels = age_groups)) %>%
    group_by(age_group, day) %>%
    summarise(incidence = sum(value)) %>%
    spread(age_group, incidence) %>%
    select(-day)
}

# Generate Matrices of Hosp Stay and Deaths Over Time From Incidence Over Time
generate_hosp_req_and_deaths <- function(delay_onset_hospital, time_in_hospital, time_in_critical_care, delay_onset_death,
                                         hospital_incidence, ICU_incidence, death_incidence, time_period, death_logical) {
  
  total_hosp_incidence <- apply(hospital_incidence, 1, sum)
  hosp_duration <- list(mode = "numeric")
  
  total_ICU_incidence <- apply(ICU_incidence, 1, sum)
  ICU_duration <- list(mode = "numeric")
  
  total_death_incidence <- apply(death_incidence, 1, sum)
  death_date <- list(mode = "numeric")
  
  for (i in 1:length(total_hosp_incidence)) {
    temp_hosp_duration <- runif(n = total_hosp_incidence[i], min = time_in_hospital, max = time_in_hospital)
    temp_ICU_duration <- runif(n = total_ICU_incidence[i], min = time_in_critical_care - time_in_hospital, max = time_in_critical_care - time_in_hospital)
    temp_death_time_post_onset <- runif(n = total_death_incidence[i], min = delay_onset_death, max = delay_onset_death)
    hosp_duration[[i]] <- temp_hosp_duration
    ICU_duration[[i]] <- temp_ICU_duration
    if (total_ICU_incidence[i] == 0) {
      hosp_duration[[i]] <-  hosp_duration[[i]]
    } else {
      hosp_duration[[i]][1:total_ICU_incidence[i]] <- time_in_critical_care
    }
    death_date[[i]] <- temp_death_time_post_onset
  }

  hosp_matrix <- matrix(0, nrow = sum(total_hosp_incidence), ncol = time_period)
  counter_hosp <- 1
  ICU_matrix <- matrix(0, nrow = sum(total_ICU_incidence), ncol = time_period)
  counter_ICU <- 1
  death_matrix <- matrix(0, nrow = sum(total_death_incidence), ncol = time_period)
  counter_dead <- 1
  for (i in 1:length(hosp_duration)) {
    temp_hosp <- hosp_duration[[i]]
    number_hosp <- length(temp_hosp)
    current_time <- i
    if (number_hosp == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_hosp != 0) {
      for (j in 1:length(temp_hosp)) {
        if ((current_time + delay_onset_hospital + temp_hosp[j] - 1) >= time_period) {
          if((current_time + delay_onset_hospital >= time_period)) {
            hosp_matrix[counter_hosp, current_time:time_period] <- 0
          } else {
            hosp_matrix[counter_hosp, (current_time + delay_onset_hospital):time_period] <- 1
          }
          counter_hosp <- counter_hosp + 1
        } else {
          hosp_matrix[counter_hosp, (current_time + delay_onset_hospital):(current_time + delay_onset_hospital + temp_hosp[j] - 1)] <- rep(1, temp_hosp[j])
          counter_hosp <- counter_hosp + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
    
    temp_ICU <- ICU_duration[[i]]
    number_ICU <- length(temp_ICU)
    current_time <- i
    if (number_ICU == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_ICU != 0) {
      for (j in 1:length(temp_ICU)) {
        if ((current_time + delay_onset_hospital + time_in_hospital + temp_ICU[j] - 1) >= time_period) {
          if((current_time + delay_onset_hospital + time_in_hospital >= time_period)) {
            ICU_matrix[counter_ICU, current_time:time_period] <- 0
          } else {
            ICU_matrix[counter_ICU, (current_time + delay_onset_hospital + time_in_hospital):time_period] <- 1
          }
          counter_ICU <- counter_ICU + 1
        } else {
          ICU_matrix[counter_ICU, (current_time + delay_onset_hospital + time_in_hospital):(current_time + delay_onset_hospital + time_in_hospital + temp_ICU[j] - 1)] <- rep(1, temp_ICU[j])
          counter_ICU <- counter_ICU + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
    
    if (death_logical == TRUE) {
      temp_dead <- death_date[[i]]
      number_dead <- length(temp_dead)
      if (number_dead == 0) {
        ## do nothing as no one dead
      }
      else if (number_dead != 0) {
        for (j in 1:length(temp_dead)) {
          if ((current_time + temp_dead[j] - 1) >= time_period) {
            death_matrix[counter_dead, time_period] <- 1
            counter_dead <- counter_dead + 1
          } else {
            death_matrix[counter_dead, (current_time + temp_dead[j] - 1)] <- 1
            counter_dead <- counter_dead + 1 
          }
        }
      }
      else {
        print("this is a weird condition")
      }
    }
  }
  
  if (death_logical == TRUE) {
    return(list(hosp_duration = hosp_duration,
                hosp_matrix = hosp_matrix,
                ICU_duration = ICU_duration,
                ICU_matrix = ICU_matrix,
                death_date = death_date,
                death_matrix = death_matrix))
  } else {
    return(list(hosp_duration = hosp_duration,
                hosp_matrix = hosp_matrix,
                ICU_duration = ICU_duration,
                ICU_matrix = ICU_matrix))
  }

}

check_doubling_time <- function(infection_incidence, dt) {
  time <- infection_incidence[, 1]
  infections <- apply(infection_incidence[, -1], 1, sum)
  time_stop <- which(infections == max(infections))
  time_start <- 50/dt
  temp_time <- time[(time_stop-time_start):(time_stop - 10/dt)]
  temp_infections <- infections[(time_stop-time_start):(time_stop - 10/dt)]
  plot(temp_time, log(temp_infections))
  coefficients <- coef(summary(lm(log(temp_infections) ~ temp_time)))
  return(log(2)/coefficients[2, 1])
}

