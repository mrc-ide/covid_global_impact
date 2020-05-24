# Loading Rrequired Libraries
library(tidyverse); library(zoo); library(patchwork)

# Sourcing Required Functions
setwd("5_Suppression_Triggering_and_Exploration/")
source("trigger_running_function.R")

no_constraints_overall <- readRDS("no_constraints_overall_df.rds")
x <- no_constraints_overall

a <- ggplot(x, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  xlim(c(0, 0.8)) +
  theme_bw() +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown") +
  guides(colour = "none")

x <-  no_constraints_overall %>%
  filter(!(setting == "LIC" & threshold == "ICU_inc200"),
         !(setting == "LIC" & threshold == "ICU_inc270"),
         !(setting == "LIC" & threshold == "ICU_inc500"),
         !(setting == "LIC" & threshold == "ICU_inc800"),
         !(setting == "LMIC" & threshold == "ICU_inc335"),
         !(setting == "LMIC" & threshold == "ICU_inc2500"),
         !(setting == "UMIC" & threshold == "ICU_inc400"),
         !(setting == "UMIC" & threshold == "ICU_inc800"),
         !(setting == "UMIC" & threshold == "ICU_inc3000"),
         !(setting == "HIC" & threshold == "ICU_inc700"))

b <- ggplot(x, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  xlim(c(0, 0.8)) +
  theme_bw() +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown") +
  guides(colour = "none")

a + b

constraints_overall <- readRDS("constraints_overall_df.rds")
y <- constraints_overall

f <- ggplot(y, aes(x = time_in_lockdown, y = 1000 *deaths/50000000, col = setting)) +
  geom_path(size = 2, aes(linetype = setting)) +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor",
                                 "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#fcb15b", "#B7C0EE", "#FB7171", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  scale_linetype_manual(values = c(5, 1, 5, 1, 1, 1)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  xlim(c(0, 0.8)) +
  labs(y = "Deaths Per 1000 Population", x = "Proportion of Time in Lockdown") +
  guides(linetype = "none", colour = "none")


y <- constraints_overall %>%
  filter(!(setting == "LMIC" & threshold == "ICU_inc50"),
         !(setting == "LMIC" & threshold == "ICU_inc120"),
         !(setting == "LMIC" & threshold == "ICU_inc800"),
         !(setting == "LMIC" & threshold == "ICU_inc1000"),
         !(setting == "LMIC" & threshold == "ICU_inc2250"),
         !(setting == "LMIC" & threshold == "ICU_inc370"),
         !(setting == "LMIC" & threshold == "ICU_inc270"),
         !(setting == "LMIC" & threshold == "ICU_inc500"),
         !(setting == "LMIC" & threshold == "ICU_inc600"),
         !(setting == "LMIC" & threshold == "ICU_inc180"),
         !(setting == "LMIC" & threshold == "ICU_inc90"),
         !(setting == "LIC" & threshold == "ICU_inc70"),
         !(setting == "LIC" & threshold == "ICU_inc160"),
         !(setting == "LIC" & threshold == "ICU_inc80"),
         !(setting == "LIC" & threshold == "ICU_inc100"),
         !(setting == "LIC" & threshold == "ICU_inc700"),
         !(setting == "LIC" & threshold == "ICU_inc450"),
         !(setting == "LIC" & threshold == "ICU_inc270"),
         !(setting == "LIC" & threshold == "ICU_inc370"),
         !(setting == "LIC" & threshold == "ICU_inc140"),
         !(setting == "LIC" & threshold == "ICU_inc200"),
         !(setting == "LIC" & threshold == "ICU_inc600"),
         !(setting == "LIC_poor" & threshold == "ICU_inc70"),
         !(setting == "LIC_poor" & threshold == "ICU_inc90"),
         !(setting == "LIC_poor" & threshold == "ICU_inc80"),
         !(setting == "LIC_poor" & threshold == "ICU_inc100"),
         !(setting == "LIC_poor" & threshold == "ICU_inc140"),
         !(setting == "LIC_poor" & threshold == "ICU_inc800"),
         !(setting == "LIC_poor" & threshold == "ICU_inc600"),
         !(setting == "LIC_poor" & threshold == "ICU_inc700"),
         !(setting == "LIC_poor" & threshold == "ICU_inc370"),
         !(setting == "LIC_poor" & threshold == "ICU_inc450"),
         !(setting == "LIC_poor" & threshold == "ICU_inc270"),
         !(setting == "LIC_poor" & threshold == "ICU_inc200"),
         !(setting == "LIC_poor" & threshold == "ICU_inc335"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc50"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc90"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc60"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc200"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc100"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc180"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc270"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc370"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc500"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc600"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc800"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc1000"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc2250"),
         !(setting == "UMIC" & threshold == "ICU_inc60"),
         !(setting == "UMIC" & threshold == "ICU_inc20"),
         !(setting == "UMIC" & threshold == "ICU_inc70"),
         !(setting == "UMIC" & threshold == "ICU_inc1500"),
         !(setting == "UMIC" & threshold == "ICU_inc140"),
         !(setting == "UMIC" & threshold == "ICU_inc235"),
         !(setting == "UMIC" & threshold == "ICU_inc370"),
         !(setting == "UMIC" & threshold == "ICU_inc450"),
         !(setting == "UMIC" & threshold == "ICU_inc800"),
         !(setting == "UMIC" & threshold == "ICU_inc2000"),
         !(setting == "UMIC" & threshold == "ICU_inc500"),
         !(setting == "HIC" & threshold == "ICU_inc180"),
         !(setting == "HIC" & threshold == "ICU_inc270"),
         !(setting == "HIC" & threshold == "ICU_inc70"),
         !(setting == "HIC" & threshold == "ICU_inc180"),
         !(setting == "HIC" & threshold == "ICU_inc80"),
         !(setting == "HIC" & threshold == "ICU_inc90"),
         !(setting == "HIC" & threshold == "ICU_inc335"),
         !(setting == "HIC" & threshold == "ICU_inc200"),
         !(setting == "HIC" & threshold == "ICU_inc300"),
         !(setting == "HIC" & threshold == "ICU_inc700"),
         !(setting == "HIC" & threshold == "ICU_inc900"),
         !(setting == "HIC" & threshold == "ICU_inc1250"),
         !(setting == "HIC" & threshold == "ICU_inc1750"))

g <- ggplot(y, aes(x = time_in_lockdown, y = 1000 *deaths/50000000, col = setting)) +
  geom_path(size = 2, aes(linetype = setting)) +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor",
                                 "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#fcb15b", "#B7C0EE", "#FB7171", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  scale_linetype_manual(values = c(5, 1, 5, 1, 1, 1)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  xlim(c(0, 0.8)) +
  labs(y = "Deaths Per 1000 Population", x = "Proportion of Time in Lockdown") +
  guides(linetype = "none", colour = "none")

f + g

# Plotting the Output
layout <- "AACCCC
AADDDD
BBEEEE
BBFFFF"

e + f + a + b + c + d +
  plot_layout(design = layout)

# Running for LIC but for the initial illustrative plot
income_strata <- "LIC"
trigger_threshold <- 30
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")
suppression_reduction <- c(0.25, 0.15, 0.05)

med_LIC <- run_trigger_threshold(country = country, population = pop,
                                 replicates = replicates,
                                 income_strata = income_strata,
                                 trigger_threshold = trigger_threshold,
                                 suppression_reduction = suppression_reduction[2],
                                 suppression_duration = suppression_duration,
                                 mitigation_reduction = mitigation_reduction,
                                 R0 = R0, tt_R0 = tt_R0,
                                 max_lockdowns = max_lockdowns,
                                 hospital_bed_capacity = 10000000,
                                 ICU_bed_capacity = 10000000,
                                 income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                 poorer_outcomes = FALSE)

high_LIC <- run_trigger_threshold(country = country, population = pop,
                                  replicates = replicates,
                                  income_strata = income_strata,
                                  trigger_threshold = trigger_threshold,
                                  suppression_reduction = suppression_reduction[3],
                                  suppression_duration = suppression_duration,
                                  mitigation_reduction = mitigation_reduction,
                                  R0 = R0, tt_R0 = tt_R0,
                                  max_lockdowns = max_lockdowns,
                                  hospital_bed_capacity = 10000000,
                                  ICU_bed_capacity = 10000000,
                                  income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                  poorer_outcomes = FALSE)

mid <- process_output(med_LIC, index)
mid_time <- round(apply(med_LIC$time_in_lockdown[1:5500, ], 1, median))
mid$scenario <- "mid"

high <- process_output(high_LIC, index)
high_time <- round(apply(high_LIC$time_in_lockdown[1:5500, ], 1, median))
high$scenario <- "high"

first_mid <- mid_time
first_mid[800:5500] <- 0
first_high <- high_time
first_high[800:5500] <- 0
first_lockdown <- c(first_mid, first_high)

overall <- rbind(mid, high) %>%
  cbind(lockdown = first_lockdown) %>%
  filter(time > 25 & time < 120)

g <- ggplot(overall, aes(x = time, y = median, colour = scenario)) +
  geom_ribbon(aes(ymin = rep(0, length(lockdown)), ymax = lockdown * max(overall$upper)),
              fill = "grey", alpha = 0.2, colour = NA) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("#95A78D", "#C8C6AF")) +
  scale_fill_manual(values = c("#95A78D", "#C8C6AF")) +
  geom_line(aes(x = 94, y = lockdown * max(overall$upper)),
            size = 1, colour = "#C8C6AF") +
  geom_line(aes(x = 109, y = lockdown * max(overall$upper)),
            size = 1, colour = "#95A78D") +
  theme_bw() +
  theme(legend.position = "none")

# Illustrative Plot Part 2
replicates <- 20
income_strata <- "LIC"
trigger_threshold <- 30
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")

low_thresh_LIC <- run_trigger_threshold(country = country, population = pop,
                                        replicates = replicates,
                                        income_strata = income_strata,
                                        trigger_threshold = trigger_threshold,
                                        suppression_reduction = suppression_reduction[2],
                                        suppression_duration = suppression_duration,
                                        mitigation_reduction = mitigation_reduction,
                                        R0 = R0, tt_R0 = tt_R0,
                                        max_lockdowns = max_lockdowns,
                                        hospital_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000,
                                        income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                        poorer_outcomes = FALSE)

trigger_threshold <- 500
high_thresh_LIC <- run_trigger_threshold(country = country, population = pop,
                                         replicates = replicates,
                                         income_strata = income_strata,
                                         trigger_threshold = trigger_threshold,
                                         suppression_reduction = suppression_reduction[2],
                                         suppression_duration = suppression_duration,
                                         mitigation_reduction = mitigation_reduction,
                                         R0 = R0, tt_R0 = tt_R0,
                                         max_lockdowns = max_lockdowns,
                                         hospital_bed_capacity = 10000000,
                                         ICU_bed_capacity = 10000000,
                                         income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                         poorer_outcomes = FALSE)

low_thresh <- process_output(low_thresh_LIC, index)
low_thresh$scenario <- "low"
low_thresh_time <- round(apply(low_thresh_LIC$time_in_lockdown[1:5500, ], 1, median))

high_thresh <- process_output(high_thresh_LIC, index)
high_thresh$scenario <- "high"
high_thresh_time <- round(apply(high_thresh_LIC$time_in_lockdown[1:5500, ], 1, median))

h <- ggplot(low_thresh, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#C884A5", alpha = 0.2, colour = NA) +
  geom_line(colour = "#C884A5") +
  geom_ribbon(aes(ymin = 0, ymax = low_thresh_time * 600), alpha = 0.1) +
  labs(x = "") +
  theme_bw()

i <- ggplot(high_thresh, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = high_thresh_time * 7800), alpha = 0.1) +
  geom_line(colour = "#EB55A2") +
  labs(x = "") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#EB55A2", alpha = 0.2, colour = NA) +
  theme_bw()

layout <- "AAABBB
           AAACCC
           DDFFFF
           DDFFFF
           DDGGGG
           DDGGGG
           EEHHHH
           EEHHHH
           EEIIII
           EEIIII"

g + h + i + e + f + a + b + c + d +
  plot_layout(design = layout)


