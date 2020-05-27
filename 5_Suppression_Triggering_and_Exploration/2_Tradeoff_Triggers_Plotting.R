# Loading Rrequired Libraries
library(tidyverse); library(zoo); library(patchwork); library(squire)

# Sourcing Required Functions
setwd("5_Suppression_Triggering_and_Exploration/")
source("Functions/Trigger_Running_Functions.R")

# Set Seed
set.seed(100001)

# Defining Parameters Used in All Model Runs
replicates <- 50
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
suppression_reduction <- 0.25
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 30
r <- run_explicit_SEEIR_model("United Kingdom")
index <-  squire:::odin_index(r$model)

# Example Showing How Strength of Suppression Affects Time Required in Lockdown
income_strata <- "LIC"
trigger_threshold <- 60
country <- "Madagascar"
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")

weak_supp <- run_trigger_threshold(country = country, population = pop,
                                   replicates = replicates,
                                   income_strata = income_strata,
                                   trigger_threshold = trigger_threshold,
                                   suppression_reduction = 0.15,
                                   suppression_duration = suppression_duration,
                                   mitigation_reduction = mitigation_reduction,
                                   R0 = R0, tt_R0 = tt_R0,
                                   max_lockdowns = max_lockdowns,
                                   hospital_bed_capacity = 10000000,
                                   ICU_bed_capacity = 10000000,
                                   income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                   poorer_outcomes = FALSE)

strong_supp <- run_trigger_threshold(country = country, population = pop,
                                     replicates = replicates,
                                     income_strata = income_strata,
                                     trigger_threshold = trigger_threshold,
                                     suppression_reduction = 0.05,
                                     suppression_duration = suppression_duration,
                                     mitigation_reduction = mitigation_reduction,
                                     R0 = R0, tt_R0 = tt_R0,
                                     max_lockdowns = max_lockdowns,
                                     hospital_bed_capacity = 10000000,
                                     ICU_bed_capacity = 10000000,
                                     income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                     poorer_outcomes = FALSE)


# Processing Outputs
index <- weak_supp$index
ICU_occupancy_indices <- c(index$IMVGetLive1, index$IMVGetLive2, index$IMVGetDie1, index$IMVGetDie2)

# Processing and Plotting Output
weak <- process_output(weak_supp, index, ICU_occupancy_indices, "weak")
strong <- process_output(strong_supp, index, ICU_occupancy_indices, "strong")
overall <- rbind(weak, strong) %>%
  mutate(lockdown = ifelse(time > 80, 0, lockdown)) %>%
  filter(time > 25 & time < 120)

a <- ggplot(overall, aes(x = time, y = median, colour = scenario)) +
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
  labs(x = "Time (Days)", y = "ICU Occupancy") +
  theme(legend.position = "none", panel.grid = element_blank())

# Example Showing How Strength of Suppression Affects Time Required in Lockdown
income_strata <- "LIC"
low_trigger_threshold <- 30
high_trigger_threshold <- 500
country <- "Madagascar"
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")

low_thresh <- run_trigger_threshold(country = country, population = pop,
                                    replicates = replicates,
                                    income_strata = income_strata,
                                    trigger_threshold = low_trigger_threshold,
                                    suppression_reduction = 0.15,
                                    suppression_duration = suppression_duration,
                                    mitigation_reduction = mitigation_reduction,
                                    R0 = R0, tt_R0 = tt_R0,
                                    max_lockdowns = max_lockdowns,
                                    hospital_bed_capacity = 10000000,
                                    ICU_bed_capacity = 10000000,
                                    income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                    poorer_outcomes = FALSE)

high_thresh <- run_trigger_threshold(country = country, population = pop,
                                     replicates = replicates,
                                     income_strata = income_strata,
                                     trigger_threshold = high_trigger_threshold,
                                     suppression_reduction = 0.15,
                                     suppression_duration = suppression_duration,
                                     mitigation_reduction = mitigation_reduction,
                                     R0 = R0, tt_R0 = tt_R0,
                                     max_lockdowns = max_lockdowns,
                                     hospital_bed_capacity = 10000000,
                                     ICU_bed_capacity = 10000000,
                                     income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                     poorer_outcomes = FALSE)

# Processing and Plotting Output
low <- process_output(low_thresh, index, ICU_occupancy_indices, "low")
high <- process_output(high_thresh, index, ICU_occupancy_indices, "high")

b <- ggplot(low, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#C884A5", alpha = 0.2, colour = NA) +
  geom_line(colour = "#C884A5") +
  geom_ribbon(aes(ymin = 0, ymax = low$lockdown * 600), alpha = 0.1) +
  labs(x = "") +
  theme_bw()

c <- ggplot(high, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = high$lockdown * 7800), alpha = 0.1) +
  geom_line(colour = "#EB55A2") +
  labs(x = "") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#EB55A2", alpha = 0.2, colour = NA) +
  theme_bw()

# Loading in ICU Capacity
ICU <- income_strata_healthcare_capacity$ICU_beds
LIC_icu <- (ICU[1] * 50000000 /1000) / 2
LMIC_icu <- (ICU[2] * 50000000 /1000) / 2
UMIC_icu <- (ICU[3] * 50000000 /1000) / 2
HIC_icu <- (ICU[4] * 50000000 /1000) / 2

# Running for LIC
income_strata <- "LIC"
trigger_threshold <- 26
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")

LIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                       replicates = replicates,
                                       income_strata = income_strata,
                                       initial_trigger_threshold = death_triggers,
                                       trigger_threshold = trigger_threshold,
                                       suppression_reduction = suppression_reduction,
                                       suppression_duration = suppression_duration,
                                       mitigation_reduction = mitigation_reduction,
                                       R0 = R0, tt_R0 = tt_R0,
                                       max_lockdowns = max_lockdowns,
                                       hospital_bed_capacity = 10000000,
                                       ICU_bed_capacity = 10000000,
                                       income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                       poorer_outcomes = FALSE)

max_LIC <- get_max_ICU_req(LIC)
time_LIC <- get_time_in_lockdown(LIC)
LIC <- process_output(LIC, index, ICU_occupancy_indices, "LIC")
f <- ggplot(LIC, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = lockdown * LIC_icu * 2), alpha = 0.1) +
  geom_line(col = "#B7C0EE", size = 1) +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = LIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = LIC_icu * 2), linetype = "dashed", size = 0.5)

# Running for LMIC
income_strata <- "LMIC"
trigger_threshold <- 47
country <- "Nicaragua"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
LMIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                        replicates = replicates,
                                        income_strata = income_strata,
                                        initial_trigger_threshold = death_triggers,
                                        trigger_threshold = trigger_threshold,
                                        suppression_reduction = suppression_reduction,
                                        suppression_duration = suppression_duration,
                                        mitigation_reduction = mitigation_reduction,
                                        R0 = R0, tt_R0 = tt_R0,
                                        max_lockdowns = max_lockdowns,
                                        hospital_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000,
                                        income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                        poorer_outcomes = FALSE)

max_LMIC <- get_max_ICU_req(LMIC)
time_LIMC <- get_time_in_lockdown(LMIC)
LMIC <- process_output(LMIC, index, ICU_occupancy_indices, "LMIC")
g <- ggplot(LMIC, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = lockdown * LMIC_icu * 2), alpha = 0.1) +
  geom_line(col = "#7067CF", size = 1) +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = LMIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = LMIC_icu * 2), linetype = "dashed", size = 0.5)


# Running for UMIC
income_strata <- "UMIC"
trigger_threshold <- 103
country <- "Grenada"
raw_death_trigger <- 0.044
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
UMIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                        replicates = replicates,
                                        income_strata = income_strata,
                                        initial_trigger_threshold = death_triggers,
                                        trigger_threshold = trigger_threshold,
                                        suppression_reduction = suppression_reduction,
                                        suppression_duration = suppression_duration,
                                        mitigation_reduction = mitigation_reduction,
                                        R0 = R0, tt_R0 = tt_R0,
                                        max_lockdowns = max_lockdowns,
                                        hospital_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000,
                                        income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                        poorer_outcomes = FALSE)

max_UMIC <- get_max_ICU_req(UMIC)
time_UMIC <- get_time_in_lockdown(UMIC)
UMIC <- process_output(UMIC, index, ICU_occupancy_indices, "UMIC")
h <- ggplot(UMIC, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = lockdown * UMIC_icu * 2), alpha = 0.1) +
  geom_line(col = "#362E91", size = 1) +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = UMIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = UMIC_icu * 2), linetype = "dashed", size = 0.5)


# Running for HIC
income_strata <- "HIC"
trigger_threshold <- 213
country <- "Malta"
raw_death_trigger <- 0.202
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
HIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                       replicates = replicates,
                                       income_strata = income_strata,
                                       initial_trigger_threshold = death_triggers,
                                       trigger_threshold = trigger_threshold,
                                       suppression_reduction = suppression_reduction,
                                       suppression_duration = suppression_duration,
                                       mitigation_reduction = mitigation_reduction,
                                       R0 = R0, tt_R0 = tt_R0,
                                       max_lockdowns = max_lockdowns,
                                       hospital_bed_capacity = 10000000,
                                       ICU_bed_capacity = 10000000,
                                       income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                       poorer_outcomes = FALSE)

max_HIC <- get_max_ICU_req(HIC)
time_HIC <- get_time_in_lockdown(HIC)
HIC <- process_output(HIC, index, ICU_occupancy_indices, "HIC")
i <- ggplot(HIC, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = lockdown * HIC_icu * 2), alpha = 0.1) +
  geom_line(col = "#241F60", size = 1) +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = HIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = HIC_icu * 2), linetype = "dashed", size = 0.5)

# Plotting Time In Lockdown vs Maximum ICU Capacity Required 
time_vs_ICU_capacity <- readRDS("Outputs/capacity_vs_time.rds") %>%
  group_by(setting, time_in_lockdown) %>%
  filter(max_capacity == min(max_capacity))
x <- time_vs_ICU_capacity
d <- ggplot(x, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  xlim(c(0, 0.85)) +
  theme_bw() +
  geom_point(aes(x = median(time_LIC), y = LIC_icu), colour = "#B7C0EE", size = 5) +
  geom_point(aes(x = median(time_LIMC), y = LMIC_icu), colour = "#7067CF", size = 5) +
  geom_point(aes(x = median(time_UMIC), y = UMIC_icu), colour = "#362E91", size = 5) +
  geom_point(aes(x = median(time_HIC), y = HIC_icu), colour = "#241F60", size = 5) +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown") +
  guides(colour = "none")

# Plotting Time In Lockdown vs Number of Deaths 
time_vs_deaths <- readRDS("Outputs/deaths_vs_time.rds") %>%
  group_by(setting, time_in_lockdown) %>%
  filter(deaths == min(deaths)) 
y <- time_vs_deaths
e <- ggplot(y, aes(x = time_in_lockdown, y = 1000 *deaths/50000000, col = setting)) +
  geom_path(size = 2, aes(linetype = setting)) +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor",
                                 "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#fcb15b", "#B7C0EE", "#FB7171", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  scale_linetype_manual(values = c(5, 1, 5, 1, 1, 1)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  xlim(c(0, 0.85)) +
  labs(y = "Deaths Per 1000 Population", x = "Proportion of Time in Lockdown") +
  guides(linetype = "none", colour = "none")

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

figure <- a + b + c + d + e + f + g + h + i +
  plot_layout(design = layout)
figure 
# width = 12, height = 9 
