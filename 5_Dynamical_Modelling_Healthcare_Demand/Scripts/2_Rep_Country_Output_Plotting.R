# Load Required Packages
library(tidyverse); library(socialmixr); library(extraDistr); library(tictoc); 
library(zoo); library(readxl); library(patchwork)

# Loading Data and Assigning Variables From Other Analyses
millions <- 20 # results are standardised to 20 million population
suppress_triggers <- c(1, 2, 4, 8, 16, 32) # threshold (weekly deaths per million) required to trigger suppression
strats <- c("unmitigated", paste0("supp_", suppress_triggers, "_time"))
raw_healthcare_capacity <- readRDS("4_Determining_Healthcare_Capacity/Outputs/Income_Strata_Predicted_Hospital_and_ICU_Beds.Rds")
healthcare_capacity <- raw_healthcare_capacity %>%
  mutate(hosp_median_pred = 1000000 * hosp_median_pred/1000, hosp_lower = 1000000 * hosp_lower/1000, hosp_upper = 1000000 * hosp_upper/1000) %>%
  select(income_group, hosp_median_pred, hosp_lower, hosp_upper, ICU_median, ICU_lower, ICU_upper) %>% # converting from beds per 1000 to beds per million population
  mutate(ICU_median = hosp_median_pred * ICU_median/100, ICU_lower = hosp_lower * ICU_lower/100, ICU_upper = hosp_upper * ICU_upper/100)
profiles <- read.csv("Data/severe_parameters.csv", stringsAsFactors = TRUE)

# Setting Working Directory to Current Analysis Strand
setwd("5_Dynamical_Modelling_Healthcare_Demand/")

# Creating Combinations of all the Different Parameter Combinations in Script 1
country_indicator <- c(1, 2, 3, 4)
R0 <- c(2.7, 3, 3.5)
strategy <- c("Mitigation", "Suppression")
combinations <- expand.grid(country_indicator, R0, strategy)

# Loading In Data Generated In Script 1
file_names <- list()
object_names <- list()
storage_list <- list()
for (i in 1:length(combinations[, 1])) {
  temp_country <- combinations[i, 1]
  temp_R0 <- combinations[i, 2]
  temp_strategy <- combinations[i, 3]
  temp_filename <- paste0("Outputs/Raw_Rep_Country_Outputs/", temp_strategy, "_Country_", temp_country, "_R0_", temp_R0, "_Model_Outputs.csv")
  temp_object_name <- paste0(temp_strategy, "_", temp_country, "_", temp_R0)
  file_names[[temp_object_name]] <- temp_filename
  object_names[[i]] <- temp_object_name
  assign(object_names[[i]], read.csv(file_names[[temp_object_name]])[, -1])
  storage_list[[temp_object_name]] <- assign(object_names[[i]], read.csv(file_names[[temp_object_name]])[, -1])
}
saveRDS(storage_list, "Outputs/All_Rep_Country_Outputs.rds")

# Generic Plotting Utilities
scaled_colour_mit <- scale_colour_manual(values = c("#F8756C", "#A2A5B5", "#50AFD8"),
                                         breaks = c("unmitigated", "std_mitigation", "plus_mitigation"),
                                         labels = c("No Mitigation", "Social Distancing\nWhole Population", "Enhanced Social\nDistancing of\nthe Elderly"), 
                                         name = "Control Strategy") 
scaled_fill_mit <-   scale_fill_manual(values = c("#F8756C", "#A2A5B5", "#50AFD8"),
                                       breaks = c("unmitigated", "std_mitigation", "plus_mitigation"),
                                       labels = c("No Mitigation", "Social Distancing\nWhole Population", "Enhanced Social\nDistancing of\nthe Elderly"), 
                                       name = "Control Strategy")
scale_colour_supp <- scale_colour_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                                           breaks = strats,
                                           labels = c("No Suppression", paste0(suppress_triggers/10)))
scale_fill_supp <- scale_fill_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                                       breaks = strats,
                                       labels = c("No Suppression", paste0(suppress_triggers/10)))  

# Plotting Mitigation Output for R03 and ICU Occupancy
counter <- 1
mit_names <- c("a", "b", "c", "d")
for (i in 5:8) {
  x <- assign(object_names[[i]], read.csv(file_names[[i]])[, -1])
  beds <- healthcare_capacity[counter, ]
  x$strategy <- factor(x$strategy, levels = c("unmitigated", "std_mitigation", "plus_mitigation"))
  assign(mit_names[counter], ggplot(data = x, aes(x = time, y = ICU_occ_median/millions/10, colour = strategy)) +
    geom_line(size = 1) +
    geom_ribbon(aes(x = time, ymin = ICU_occ_lower/millions/10, 
                    ymax = ICU_occ_upper/millions/10, colour = strategy, 
                    fill = strategy, border = NULL), alpha = 0.2, linetype = 0) + 
    labs(x = "Time (Days)", y = "ICU Bed Occupancy Per Day\n Per 100,000 Population") +
    theme_bw() +
    lims(y = c(0, max(x$ICU_occ_upper/millions/10))) +
    scaled_colour_mit +
    scaled_fill_mit + 
    annotate("rect", xmin = min(x$time), xmax = max(x$time), 
             ymin = beds$ICU_lower/10, ymax = beds$ICU_upper/10, 
             alpha = 0.2, fill = "grey") + 
    geom_segment(x = 0, y = beds$ICU_median/10, xend = max(x$time), 
                 yend = beds$ICU_median/10, colour = "black", size = 0.5, linetype = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "right", legend.title = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 13.5, vjust = +3), axis.title.x = element_text(size = 13.5, vjust = +0),
          plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
          legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold")))
  counter <- counter + 1
}

# Plotting Suppression Output for R03 and ICU Occupancy
lifting_1 <- read.csv(file = "Outputs/Raw_Rep_Country_Outputs/Lifted_Suppression_Country_1_R0_3_Model_Outputs.csv")[, -1]
lifting_1 <- lifting_1 %>% 
  mutate(measure = paste0(measure, "_ICU_occ")) %>%
  rename(output_type = measure) %>%
  select(-none)

lifting_2 <- read.csv(file = "Outputs/Raw_Rep_Country_Outputs/Lifted_Suppression_Country_2_R0_3_Model_Outputs.csv") 
lifting_2 <- lifting_2 %>% 
  mutate(measure = paste0(measure, "_ICU_occ")) %>%
  rename(output_type = measure) %>%
  select(-none)

lifting_3 <- read.csv(file = "Outputs/Raw_Rep_Country_Outputs/Lifted_Suppression_Country_3_R0_3_Model_Outputs.csv") 
lifting_3 <- lifting_3 %>% 
  mutate(measure = paste0(measure, "_ICU_occ")) %>%
  rename(output_type = measure) %>%
  select(-none)

lifting_4 <- read.csv(file = "Outputs/Raw_Rep_Country_Outputs/Lifted_Suppression_Country_4_R0_3_Model_Outputs.csv") 
lifting_4 <- lifting_4 %>% 
  mutate(measure = paste0(measure, "_ICU_occ")) %>%
  rename(output_type = measure) %>%
  select(-none)

counter <- 1
supp_names <- c("e", "f", "g", "h")
for (i in 17:20) {
  x <- assign(object_names[[i]], read.csv(file_names[[i]])[, -1])
  if (i == 17) {
    x <- x %>%
      filter(output_type == "lower_ICU_occ" | output_type == "median_ICU_occ" | output_type == "upper_ICU_occ") %>%
      mutate(supp_8_time = lifting_1$incomplete) %>%
      gather(strategy, incidence, -country, -R0, -output_type, -time) %>%
      spread(output_type, incidence) 
  } else if (i == 18) {
    x <- x %>%
      filter(output_type == "lower_ICU_occ" | output_type == "median_ICU_occ" | output_type == "upper_ICU_occ") %>%
      mutate(supp_8_time = lifting_2$incomplete) %>%
      gather(strategy, incidence, -country, -R0, -output_type, -time) %>%
      spread(output_type, incidence) 
  } else if (i == 19) {
    x <- x %>%
      filter(output_type == "lower_ICU_occ" | output_type == "median_ICU_occ" | output_type == "upper_ICU_occ") %>%
      mutate(supp_8_time = lifting_3$incomplete) %>%
      gather(strategy, incidence, -country, -R0, -output_type, -time) %>%
      spread(output_type, incidence) 
  } else {
    x <- x %>%
      filter(output_type == "lower_ICU_occ" | output_type == "median_ICU_occ" | output_type == "upper_ICU_occ") %>%
      mutate(supp_8_time = lifting_4$incomplete) %>%
      gather(strategy, incidence, -country, -R0, -output_type, -time) %>%
      spread(output_type, incidence) 
  }
  x$strategy <- factor(x$strategy, levels = strats)
  beds <- healthcare_capacity[counter, ]
  assign(supp_names[counter], ggplot(data = x, aes(x = time, y = median_ICU_occ/millions/10, colour = strategy)) +
    geom_line(size = 1) +
    geom_ribbon(aes(x = time, ymin = lower_ICU_occ/millions/10, ymax = upper_ICU_occ/millions/10, 
                    colour = strategy, fill = strategy, border = NULL), alpha = 0.2, linetype = 0) + 
    labs(x = "Time (Days)", y = "ICU Bed Occupancy Per Day\n Per 100,000 Population") +
    annotate("rect", xmin = min(x$time), xmax = max(x$time), ymin = beds$ICU_lower/10, ymax = beds$ICU_upper/10, alpha = 0.2, fill = "grey") + 
    geom_segment(x = 0, y = beds$ICU_median/10, xend = max(x$time), yend = beds$ICU_median/10, colour = "black", size = 0.5, linetype = 2) +
    theme_bw() +
    scale_colour_supp +
    scale_fill_supp + 
    lims(y = c(0, max(x$upper_ICU_occ/millions/10))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "right", axis.title.y = element_text(size = 13.5, vjust = +3), 
          axis.title.x = element_text(size = 13.5, vjust = +0),
          legend.title = element_text(size = 12, face = "bold"),
          plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
          legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold")))
  counter <- counter + 1
}

# 18 X 7 
layout <- "ABCD
           ABCD
           EFGH
           EFGH"

figure <-  a + b + c + d + 
  e + f + g + h +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 25, face = "bold"))
figure

# LICs
LIC_ICU_capacity <- healthcare_capacity$ICU_median[1]/10
unmit_LIC <- c(max(Mitigation_1_2.7$ICU_occ_median[Mitigation_1_2.7$strategy == "unmitigated"]/millions/10),
               max(Mitigation_1_3$ICU_occ_median[Mitigation_1_3$strategy == "unmitigated"]/millions/10),
               max(Mitigation_1_3.5$ICU_occ_median[Mitigation_1_3.5$strategy == "unmitigated"]/millions/10))
std_mit_LIC <- c(max(Mitigation_1_2.7$ICU_occ_median[Mitigation_1_2.7$strategy == "std_mitigation"]/millions/10),
                 max(Mitigation_1_3$ICU_occ_median[Mitigation_1_3$strategy == "std_mitigation"]/millions/10),
                 max(Mitigation_1_3.5$ICU_occ_median[Mitigation_1_3.5$strategy == "std_mitigation"]/millions/10))
plus_mit_LIC <- c(max(Mitigation_1_2.7$ICU_occ_median[Mitigation_1_2.7$strategy == "plus_mitigation"]/millions/10),
                  max(Mitigation_1_3$ICU_occ_median[Mitigation_1_3$strategy == "plus_mitigation"]/millions/10),
                  max(Mitigation_1_3.5$ICU_occ_median[Mitigation_1_3.5$strategy == "plus_mitigation"]/millions/10))
LIC_capacity_exceed <- c(unmit_LIC/LIC_ICU_capacity,
                         std_mit_LIC/LIC_ICU_capacity, 
                         plus_mit_LIC/LIC_ICU_capacity)

# LMICs
LMIC_ICU_capacity <- healthcare_capacity$ICU_median[2]/10
unmit_LMIC <- c(max(Mitigation_2_2.7$ICU_occ_median[Mitigation_2_2.7$strategy == "unmitigated"]/millions/10),
               max(Mitigation_2_3$ICU_occ_median[Mitigation_2_3$strategy == "unmitigated"]/millions/10),
               max(Mitigation_2_3.5$ICU_occ_median[Mitigation_2_3.5$strategy == "unmitigated"]/millions/10))
std_mit_LMIC <- c(max(Mitigation_2_2.7$ICU_occ_median[Mitigation_2_2.7$strategy == "std_mitigation"]/millions/10),
                 max(Mitigation_2_3$ICU_occ_median[Mitigation_2_3$strategy == "std_mitigation"]/millions/10),
                 max(Mitigation_2_3.5$ICU_occ_median[Mitigation_2_3.5$strategy == "std_mitigation"]/millions/10))
plus_mit_LMIC <- c(max(Mitigation_2_2.7$ICU_occ_median[Mitigation_2_2.7$strategy == "plus_mitigation"]/millions/10),
                  max(Mitigation_2_3$ICU_occ_median[Mitigation_2_3$strategy == "plus_mitigation"]/millions/10),
                  max(Mitigation_2_3.5$ICU_occ_median[Mitigation_2_3.5$strategy == "plus_mitigation"]/millions/10))
LMIC_capacity_exceed <- c(unmit_LMIC/LMIC_ICU_capacity,
                          std_mit_LMIC/LMIC_ICU_capacity, 
                          plus_mit_LMIC/LMIC_ICU_capacity)

# UMICs
UMIC_ICU_capacity <- healthcare_capacity$ICU_median[3]/10
unmit_UMIC <- c(max(Mitigation_3_2.7$ICU_occ_median[Mitigation_3_2.7$strategy == "unmitigated"]/millions/10),
                max(Mitigation_3_3$ICU_occ_median[Mitigation_3_3$strategy == "unmitigated"]/millions/10),
                max(Mitigation_3_3.5$ICU_occ_median[Mitigation_3_3.5$strategy == "unmitigated"]/millions/10))
std_mit_UMIC <- c(max(Mitigation_3_2.7$ICU_occ_median[Mitigation_3_2.7$strategy == "std_mitigation"]/millions/10),
                  max(Mitigation_3_3$ICU_occ_median[Mitigation_3_3$strategy == "std_mitigation"]/millions/10),
                  max(Mitigation_3_3.5$ICU_occ_median[Mitigation_3_3.5$strategy == "std_mitigation"]/millions/10))
plus_mit_UMIC <- c(max(Mitigation_3_2.7$ICU_occ_median[Mitigation_3_2.7$strategy == "plus_mitigation"]/millions/10),
                   max(Mitigation_3_3$ICU_occ_median[Mitigation_3_3$strategy == "plus_mitigation"]/millions/10),
                   max(Mitigation_3_3.5$ICU_occ_median[Mitigation_3_3.5$strategy == "plus_mitigation"]/millions/10))
UMIC_capacity_exceed <- c(unmit_UMIC/UMIC_ICU_capacity,
                          std_mit_UMIC/UMIC_ICU_capacity, 
                          plus_mit_UMIC/UMIC_ICU_capacity)

# HIC
HIC_ICU_capacity <- healthcare_capacity$ICU_median[4]/10
unmit_HIC <- c(max(Mitigation_4_2.7$ICU_occ_median[Mitigation_4_2.7$strategy == "unmitigated"]/millions/10),
                max(Mitigation_4_3$ICU_occ_median[Mitigation_4_3$strategy == "unmitigated"]/millions/10),
                max(Mitigation_4_3.5$ICU_occ_median[Mitigation_4_3.5$strategy == "unmitigated"]/millions/10))
std_mit_HIC <- c(max(Mitigation_4_2.7$ICU_occ_median[Mitigation_4_2.7$strategy == "std_mitigation"]/millions/10),
                  max(Mitigation_4_3$ICU_occ_median[Mitigation_4_3$strategy == "std_mitigation"]/millions/10),
                  max(Mitigation_4_3.5$ICU_occ_median[Mitigation_4_3.5$strategy == "std_mitigation"]/millions/10))
plus_mit_HIC <- c(max(Mitigation_4_2.7$ICU_occ_median[Mitigation_4_2.7$strategy == "plus_mitigation"]/millions/10),
                   max(Mitigation_4_3$ICU_occ_median[Mitigation_4_3$strategy == "plus_mitigation"]/millions/10),
                   max(Mitigation_4_3.5$ICU_occ_median[Mitigation_4_3.5$strategy == "plus_mitigation"]/millions/10))
HIC_capacity_exceed <- c(unmit_HIC/HIC_ICU_capacity,
                          std_mit_HIC/HIC_ICU_capacity, 
                          plus_mit_HIC/HIC_ICU_capacity)

names(LIC_capacity_exceed) <- c("Unmit_Low", "Unmit_Med", "Unmit_High", "Mit_Low", "Mit_Med", "Mit_High", "+Mit_Low", "+Mid_Med", "+Mit_High")
names(LMIC_capacity_exceed) <- c("Unmit_Low", "Unmit_Med", "Unmit_High", "Mit_Low", "Mit_Med", "Mit_High", "+Mit_Low", "+Mid_Med", "+Mit_High")
names(UMIC_capacity_exceed) <- c("Unmit_Low", "Unmit_Med", "Unmit_High", "Mit_Low", "Mit_Med", "Mit_High", "+Mit_Low", "+Mid_Med", "+Mit_High")
names(HIC_capacity_exceed) <- c("Unmit_Low", "Unmit_Med", "Unmit_High", "Mit_Low", "Mit_Med", "Mit_High", "+Mit_Low", "+Mid_Med", "+Mit_High")

LIC_capacity_exceed
LMIC_capacity_exceed
UMIC_capacity_exceed
HIC_capacity_exceed

# Plotting the Severity Parameters Used Here
age_groups_raw <-c("0_5", "5_10", "10_15", "15_20", "20_25", "25_30", "30_35", "35_40", "40_45", "45_50", "50_55", "55_60", "60_65", "65_70", "70_75", "75_80", "80+")
profiles$age_group <- factor(profiles$age_group, levels = age_groups_raw)
profiles <- profiles %>% 
  gather(value, prob, -age_group) %>%
  mutate(value = factor(value, levels = c("prob_hosp", "prob_critical", "prob_death")))
levels(profiles$value)[levels(profiles$value) == "prob_hosp"] <- "Prop. Hospitalised"
levels(profiles$value)[levels(profiles$value) == "prob_critical"] <- "Prop. Critical Care"
levels(profiles$value)[levels(profiles$value) == "prob_death"] <- "Prop. Dying"

ggplot(data = profiles, aes(x = age_group, y = prob, fill = value)) +
  geom_bar(stat = "identity") + 
  labs(y = "Proportion", x = "Age") +
  scale_x_discrete(breaks = age_groups_raw, 
                   labels = c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80")) +
  scale_fill_manual(name = "Proportion",
                    values = c("#FFBD26", "#FF5B32", "#F9423B")) +
  facet_grid(~value) +
  theme_bw() +
  theme(legend.position = "bottom")

