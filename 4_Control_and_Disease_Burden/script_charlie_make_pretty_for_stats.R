# Loading Required Libraries 
library(readxl); library(lubridate); library(dplyr); library(tidyverse);
library(ggplot2); library(squire); library(ggpubr);library(gridExtra);
library(patchwork)

# Setting Working Directory and Removing Any Environment Contents
rm(list = ls())
setwd("C:/Users/cw1716/Imperial College London/ncov - Documents/2019-nCoV/LMIC/LMIC Parameters")
load("income_strata_healthcare_capacity.rda")

# Plotting Supply vs Demand for Each Income Strata
HC_stretch <- read.csv("HC_stretch_uncertainty.csv") %>%
  mutate(Income = factor(Income, levels = c("LIC","LMIC","UMIC","HIC"))) %>%
  mutate(strat = factor(strat, levels = c("Unmitigated","Mitigated")))

HC_stretch_numbers <- HC_stretch %>%
  filter(HC_stretch$strat == "Mitigated") %>%
  group_by(Income) %>%
  summarise(median = median(demand/supply),
            lower = quantile(demand/supply, 0.025),
            upper = quantile(demand/supply, 0.975))

HS_stretch_plot <- ggplot(HC_stretch, aes(Income, demand/supply, 
                                          col = interaction(Income, strat))) +
  geom_boxplot(position = position_dodge2(preserve = "single",
                                          padding = 0.15), size = 1, coef = 100) +
  scale_colour_manual(values = c("#D8DBE2", "#D8DBE2", "#D8DBE2", "#D8DBE2",
                                 "#58A4B0", "#58A4B0", "#58A4B0", "#58A4B0")) + 
  labs(x = "", y = "Multiple By Which ICU Demand Exceeds Capaciy") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position="none") +
  guides(fill = guide_legend(ncol = 2, position = "bottom"))


# Plotting IFR for Each Income Strata 
IFR_mort <- read.csv("IFR_mortality_uncertainty.csv") %>%
  mutate(Income = factor(Income, levels = c("LIC","LMIC","UMIC","HIC"))) %>%
  mutate(HS = factor(HS, levels = c("Infinite capacity","true capacity","Poor outcomes")))
IFR_plot <- ggplot(IFR_mort, aes(Income, IFR, 
                                 col = interaction(Income, HS))) +
  geom_boxplot(position = position_dodge2(preserve = "single",
                                         padding = 0.25), size = 1, , coef = 100) +
  scale_colour_manual(values = c("#78A069", "#78A069", "#78A069", "#78A069", 
                                 "#D9AC64", "#D9AC64", "#D9AC64", "#D9AC64", 
                                 "#E0785F", "#E0785F")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position="none") +
  labs(x = "", y = "Infection Fatality Rate")

# Plotting the Overall Mortality for Each Income Strata
mort_plot <- ggplot(IFR_mort, aes(Income, Mort, 
                                  col = interaction(Income, HS))) +
  geom_boxplot(position = position_dodge2(preserve = "single",
                                          padding = 0.25), size = 1, coef = 100) +
  scale_colour_manual(values = c("#78A069", "#78A069", "#78A069", "#78A069", 
                                 "#D9AC64", "#D9AC64", "#D9AC64", "#D9AC64", 
                                 "#E0785F", "#E0785F")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position="none") +
  labs(x = "", y = "Total Mortality (Deaths per Million") + 
  guides(fill = guide_legend(ncol = 3, position = "bottom"))

#c("#78A069", "#D9AC64", "#E0785F")

# Plotting these 3 Graphs Together
x <- HS_stretch_plot + IFR_plot + mort_plot
x

# Running the Epidemic Trajectory Plots Tracking Deaths Over Time, Unlimited Healthcare Capacity
pop <- get_population("India")
std_population <- pop$n/sum(pop$n) * 100000000
contact_matrix <- get_mixing_matrix("India")
t1 <- calibrate(country = "India",
                population= std_population,
                contact_matrix_set = contact_matrix,
                hosp_bed_capacity = 1000000000000,
                ICU_bed_capacity = 100000000000000000000,
                deaths = 10,
                reporting_fraction = 1,
                dt = 0.1,
                time_period = 400,
                replicates = 50,
                R0 = 3)
t1_deaths <- format_output(t1, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
t1_deaths$scenario <- "Unmitigated"
p2 <- projections(r = t1, R0_change = c(0.55, 1), tt_R0 = c(0, 180))
p2_deaths <- format_output(p2, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
p2_deaths$scenario <- "Mitigated"
p3 <- projections(r = t1, R0_change = c(0.25, 1), tt_R0 = c(0, 180))
p3_deaths <- format_output(p3, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
# p3_deaths_t <- as.numeric(p3_deaths$t[seq(from = 1, to = length(p3_deaths$median), by = 10)])/10
# p3_deaths_y <- unname(tapply(p3_deaths$median, (seq_along(p3_deaths$median)-1) %/% 10, sum))
p3_deaths$scenario <- "Suppression Then Lift"

p4 <- projections(r = t1, R0_change = c(0.8, 1), tt_R0 = c(0, 180))
p4_deaths <- format_output(p4, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975)) 
# p4_deaths_t <- as.numeric(p4_deaths$t[seq(from = 1, to = length(p4_deaths$median), by = 10)])/10
# p4_deaths_y <- unname(tapply(p4_deaths$median, (seq_along(p4_deaths$median)-1) %/% 10, sum))
p4_deaths$scenario <- "No Action"

overall <- rbind(t1_deaths, p2_deaths, p3_deaths, p4_deaths)
overall$t <- as.numeric(overall$t)
overall$scenario <- factor(overall$scenario, levels = c("Unmitigated", "No Action", "Mitigated", "Suppression Then Lift"))


epidem <- ggplot(overall, aes(x = t, y = median, col = scenario)) + 
  #geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.2, colour = NA) +
  geom_line(size = 2) +
  theme_bw() +
  scale_colour_manual(values = c("#D8DBE2", "#A9BCD0", "#58A4B0", "#373F51")) +
  scale_fill_manual(values = c("#D8DBE2", "#A9BCD0", "#58A4B0", "#373F51")) +
  labs(y = "Number of Deaths (Daily)", x = "Time (Days)") +
  theme(legend.position = "none")

scale_colour_manual(values = c("#A27E8E", "#A77464", "#88292F", "#A2A79E")) 
scale_fill_manual(values = c("#A27E8E", "#A77464", "#88292F", "#A2A79E")) 

# Running the Epidemic Trajectory Plots Tracking Deaths Over Time, Examining Deaths Due to Healthcare Capacity Being Exceeded
## mitigated, unlimited healthcare
raw_mit_inf <- run_explicit_SEEIR_model(country = "India",
                                   population = std_population,
                                   contact_matrix_set = contact_matrix,
                                   hosp_bed_capacity = 1000000000000,
                                   ICU_bed_capacity = 1000000000000,
                                   R0 = 3 * 0.55, replicates = 50)
mit_inf <- format_output(raw_mit_inf, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
mit_inf$scenario <- "Mitigated Unlimited Healthcare"
## mitigated, limited healthcare
raw_mit_true <- run_explicit_SEEIR_model(country = "India",
                                     population = std_population,
                                     contact_matrix_set = contact_matrix,
                                     hosp_bed_capacity = income_strata_healthcare_capacity$hosp_beds[2] * 1000,
                                     ICU_bed_capacity = income_strata_healthcare_capacity$ICU_beds[2] * 1000,
                                     R0 = 3 * 0.55, replicates = 50)
mit_true <- format_output(raw_mit_true, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
mit_true$scenario <- "Mitigated Limited Healthcare"
## mitigated, limited healthcare, poorer outcomes
raw_mit_po <- run_explicit_SEEIR_model(country = "India",
                                   population = std_population,
                                   contact_matrix_set = contact_matrix,
                                   hosp_bed_capacity = (income_strata_healthcare_capacity$hosp_beds[2]+income_strata_healthcare_capacity$ICU_beds[2])*1000,
                                   ICU_bed_capacity = 0,
                                   prob_non_severe_death_treatment = c(rep(0.25, 16), 0.5804312),
                                   R0 = 3 * 0.55, replicates = 50)
mit_po <- format_output(raw_mit_po, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
mit_po$scenario <- "Mitigated Poorer Outcomes"
## unmitigated, unlimited healthcare
raw_unmit_inf <- run_explicit_SEEIR_model(country = "India",
                                      population = std_population,
                                      contact_matrix_set = contact_matrix,
                                      hosp_bed_capacity = 1000000000000,
                                      ICU_bed_capacity = 100000000000000000000,
                                      R0 = 3, replicates = 50)
unmit_inf <- format_output(raw_unmit_inf, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
unmit_inf$scenario <- "Unmitigated Unlimited Healthcare"
## unmitigated, limited healthcare
raw_unmit_true <- run_explicit_SEEIR_model(country = "India",
                                       population = std_population,
                                       contact_matrix_set = contact_matrix,
                                       hosp_bed_capacity = income_strata_healthcare_capacity$hosp_beds[2] * 1000,
                                       ICU_bed_capacity = income_strata_healthcare_capacity$ICU_beds[2] * 1000,
                                       R0 = 3, replicates = 50)
unmit_true <- format_output(raw_unmit_true, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
unmit_true$scenario <- "Unmitigated Limited Healthcare"
## unmitigated, limited healthcare
raw_unmit_po <- run_explicit_SEEIR_model(country = "India",
                                     population = std_population,
                                     contact_matrix_set = contact_matrix,
                                     hosp_bed_capacity = (income_strata_healthcare_capacity$hosp_beds[2]+
                                                            income_strata_healthcare_capacity$ICU_beds[2]) * 1000,
                                     ICU_bed_capacity = 0,
                                     prob_non_severe_death_treatment = c(rep(0.25, 16), 0.5804312),
                                     R0 = 3, replicates = 50)
unmit_po <- format_output(raw_unmit_po, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
unmit_po$scenario <- "Unmitigated Poorer Outcomes"

overall_pt_2 <- rbind(unmit_po, unmit_true, unmit_inf, mit_po, mit_true, mit_inf)
overall_pt_2$t <- as.numeric(overall_pt_2$t)
epidem_pt_2 <- ggplot(overall_pt_2, aes(x = t, y = median, col = scenario)) + 
  #geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.2, colour = NA) +
  geom_line(size = 2) +
  theme_bw() +
  scale_colour_manual(values = c("#D9AC64", "#E0785F", "#78A069", 
                                 "#D9AC64", "#E0785F", "#78A069")) +
  scale_fill_manual(values = c("#D9AC64", "#E0785F", "#78A069", 
                               "#D9AC64", "#E0785F", "#78A069")) +
  theme(legend.position = "none")

layout <- c("AAAAAAABBBBBBB
             CCCCCDDDDEEEEE")
epidem + epidem_pt_2 + HS_stretch_plot + IFR_plot + mort_plot +
  plot_layout(design = layout) 
