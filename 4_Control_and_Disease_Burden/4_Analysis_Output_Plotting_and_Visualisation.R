# Loading Required Libraries 
library(readxl); library(lubridate); library(dplyr); library(tidyverse);
library(ggplot2); library(squire); library(ggpubr);library(gridExtra);
library(patchwork); library(zoo)

# Set Seed
set.seed(10001092)

# Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source Functions
source("Functions/Healthcare_Demand_Functions.R")

# Running the Model and Tracking Deaths Over Time for Different Control Scenarios
std_population <- generate_standard_population("Nicaragua")
contact_matrix <- get_mixing_matrix("Nicaragua")
millions <- sum(std_population)/1000000
t1 <- align(country = "Nicaragua",
            population = std_population,
            contact_matrix_set = contact_matrix,
            hosp_bed_capacity = 1000000000000,
            ICU_bed_capacity = 1000000000000,
            deaths = 10,
            reporting_fraction = 1,
            dt = 0.1,
            time_period = 400,
            replicates = 100,
            R0 = 3,
            day_return = TRUE)
t1_deaths <- format_output(t1, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
t1_deaths$t <- seq(0, length(t1_deaths$t) - 1, 1)
t1_deaths$scenario <- "Unmitigated"

p2 <- projections(r = t1, R0_change = c(0.55, 1), tt_R0 = c(0, 180))
p2_deaths <- format_output(p2, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
p2_deaths$t <- seq(0, length(p2_deaths$t) - 1, 1)
p2_deaths$scenario <- "Mitigated"

p3 <- projections(r = t1, R0_change = c(0.25, 1), tt_R0 = c(0, 180))
p3_deaths <- format_output(p3, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
p3_deaths$t <- seq(0, length(p3_deaths$t) - 1, 1)
p3_deaths$scenario <- "Suppression Then Lift"

p4 <- projections(r = t1, R0_change = c(0.8, 1), tt_R0 = c(0, 180))
p4_deaths <- format_output(p4, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
p4_deaths$t <- seq(0, length(p4_deaths$t) - 1, 1)
p4_deaths$scenario <- "No Action"

overall <- rbind(t1_deaths, p2_deaths, p3_deaths, p4_deaths)
overall$t <- as.numeric(overall$t)
overall$scenario <- factor(overall$scenario, levels = c("Unmitigated", "No Action", "Mitigated", "Suppression Then Lift"))

epidem <- ggplot(overall, aes(x = t, y = median/millions, col = scenario)) + 
  geom_line(size = 2) +
  theme_bw() +
  scale_colour_manual(values = c("#D8DBE2", "#A9BCD0", "#58A4B0", "#373F51")) +
  scale_fill_manual(values = c("#D8DBE2", "#A9BCD0", "#58A4B0", "#373F51")) +
  labs(y = "Number of Deaths (Daily)", x = "Time (Days)") +
  theme(legend.position = "none")


# Running the Model and Tracking Deaths Over Time for Unmitigated/Mitigated Control Scenarios
# and Various Assumptions About Healthcare Capacity/Quality

## unmitigated, unlimited healthcare
raw_unmit_unlim <- run_explicit_SEEIR_model(country = "Nicaragua",
                                        population = std_population,
                                        contact_matrix_set = contact_matrix,
                                        hosp_bed_capacity = 1000000000000,
                                        ICU_bed_capacity = 1000000000000,
                                        R0 = 3, replicates = 50,
                                        day_return = TRUE)
unmit_unlim <- format_output(raw_unmit_unlim, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
unmit_unlim$scenario <- "unmitigated_unlimited"
get_IFR(raw_unmit_unlim)

## unmitigated, limited healthcare
hosp_beds <- generate_hosp_beds(std_population, "LMIC", income_strata_healthcare_capacity)
ICU_beds <- generate_ICU_beds(std_population, "LMIC", income_strata_healthcare_capacity)
raw_unmit_limited <- run_explicit_SEEIR_model(country = "Nicaragua",
                                          population = std_population,
                                          contact_matrix_set = contact_matrix,
                                          hosp_bed_capacity = hosp_beds,
                                          ICU_bed_capacity = ICU_beds,
                                          R0 = 3, replicates = 50,
                                          day_return = TRUE)
unmit_limited <- format_output(raw_unmit_limited, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
unmit_limited$scenario <- "Unmitigated Limited Healthcare"
get_IFR(raw_unmit_limited)

## unmitigated, limited healthcare
raw_unmit_po <- run_explicit_SEEIR_model(country = "Nicaragua",
                                     population = std_population,
                                     contact_matrix_set = contact_matrix,
                                     hosp_bed_capacity = hosp_beds + ICU_beds,
                                     ICU_bed_capacity = 0, 
                                     prob_non_severe_death_treatment = c(rep(0.25, 16), 0.5804312),
                                     R0 = 3, replicates = 50,
                                     day_return = TRUE)
unmit_po <- format_output(raw_unmit_po, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
unmit_po$scenario <- "Unmitigated Poorer Outcomes"
get_IFR(raw_unmit_po)

## mitigated, unlimited healthcare
raw_mit_unlim <- run_explicit_SEEIR_model(country = "Nicaragua",
                                      population = std_population,
                                      contact_matrix_set = contact_matrix,
                                      hosp_bed_capacity = 1000000000000,
                                      ICU_bed_capacity = 1000000000000,
                                      R0 = 3 * 0.55, replicates = 50,
                                      day_return = TRUE)
mit_unlim <- format_output(raw_mit_unlim, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
mit_unlim$scenario <- "Mitigated Unlimited Healthcare"
get_IFR(raw_mit_unlim)

## mitigated, limited healthcare
raw_mit_limited <- run_explicit_SEEIR_model(country = "Nicaragua",
                                        population = std_population,
                                        contact_matrix_set = contact_matrix,
                                        hosp_bed_capacity = hosp_beds,
                                        ICU_bed_capacity = ICU_beds,
                                        R0 = 3 * 0.55, replicates = 50,
                                        day_return = TRUE)
mit_limited <- format_output(raw_mit_limited, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
mit_limited$scenario <- "Mitigated Limited Healthcare"
get_IFR(raw_mit_limited)

## mitigated, limited healthcare, poorer outcomes
raw_mit_po <- run_explicit_SEEIR_model(country = "Nicaragua",
                                   population = std_population,
                                   contact_matrix_set = contact_matrix,
                                   hosp_bed_capacity = hosp_beds + ICU_beds,
                                   ICU_bed_capacity = 0,
                                   prob_non_severe_death_treatment = c(rep(0.25, 16), 0.5804312),
                                   R0 = 3 * 0.55, replicates = 50,
                                   day_return = TRUE)
mit_po <- format_output(raw_mit_po, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y))
mit_po$scenario <- "Mitigated Poorer Outcomes"
get_IFR(raw_mit_po)

overall_pt_2 <- rbind(unmit_po, unmit_limited, unmit_unlim, mit_po, mit_limited, mit_unlim)
overall_pt_2$t <- as.numeric(overall_pt_2$t)
epidem_pt_2 <- ggplot(overall_pt_2, aes(x = t, y = median/millions, col = scenario)) + 
  geom_line(size = 2) +
  theme_bw() +
  scale_colour_manual(values = c("#D9AC64", "#E0785F", "#78A069", 
                                 "#D9AC64", "#E0785F", "#78A069")) +
  scale_fill_manual(values = c("#D9AC64", "#E0785F", "#78A069", 
                               "#D9AC64", "#E0785F", "#78A069")) +
  theme(legend.position = "none")

# Plotting Supply vs Demand for Each Income Strata
HC_stretch <- readRDS("Outputs/HC_stretch_uncertainty.rds") %>%
  mutate(setting = factor(setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))) %>%
  mutate(scenario = factor(scenario, levels = c("unmitigated", "mitigated")))

HS_stretch_plot <- ggplot(HC_stretch, aes(setting, multiple, col = interaction(setting, scenario))) +
  geom_boxplot(position = position_dodge2(preserve = "single", padding = 0.15), size = 1, coef = 100) +
  scale_colour_manual(values = c("#D8DBE2", "#D8DBE2", "#D8DBE2", "#D8DBE2",
                                 "#58A4B0", "#58A4B0", "#58A4B0", "#58A4B0")) + 
  labs(x = "", y = "Multiple By Which ICU Demand Exceeds Capaciy") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position="none") +
  guides(fill = guide_legend(ncol = 2, position = "bottom"))


# Plotting IFR for Each Income Strata 
IFR <- readRDS("Outputs/IFR_uncertainty.rds") %>%
  mutate(setting = factor(setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))) %>%
  mutate(scenario = factor(scenario, levels = c("unlimited_healthcare", "limited_healthcare", "poorer_outcomes")))

IFR_plot <- ggplot(IFR, aes(setting, IFR, col = interaction(setting, scenario))) +
  geom_boxplot(position = position_dodge2(preserve = "single", padding = 0.25), size = 1, coef = 100) +
  scale_colour_manual(values = c("#78A069", "#78A069", "#78A069", "#78A069", 
                                 "#D9AC64", "#D9AC64", "#D9AC64", "#D9AC64", 
                                 "#E0785F", "#E0785F")) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position="none") +
  labs(x = "", y = "Infection Fatality Rate")

# Plotting the Overall Mortality for Each Income Strata
mort <- readRDS("Outputs/Mort_uncertainty.rds") %>%
  mutate(setting = factor(setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))) %>%
  mutate(scenario = factor(scenario, levels = c("unlimited_healthcare", "limited_healthcare", "poorer_outcomes")))

mort_plot <- ggplot(mort, aes(setting, mort, col = interaction(setting, scenario))) +
  geom_boxplot(position = position_dodge2(preserve = "single", padding = 0.25), size = 1, coef = 100) +
  scale_colour_manual(values = c("#78A069", "#78A069", "#78A069", "#78A069", 
                                 "#D9AC64", "#D9AC64", "#D9AC64", "#D9AC64", 
                                 "#E0785F", "#E0785F")) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position="none") +
  labs(x = "", y = "Total Mortality (Deaths per Million)") + 
  guides(fill = guide_legend(ncol = 3, position = "bottom"))


# Plotting all the graphs together
layout <- c("AAAAAAABBBBBBB
             CCCCCDDDDEEEEE")
epidem + epidem_pt_2 + HS_stretch_plot + IFR_plot + mort_plot +
  plot_layout(design = layout) 
