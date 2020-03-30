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
    geom_line(size = 2) +
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
                 yend = beds$ICU_median/10, colour = "black", size = 1, linetype = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "right", legend.title = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 13.5, vjust = +3), axis.title.x = element_text(size = 13.5, vjust = +0),
          plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
          legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold")))
  counter <- counter + 1
}

ncol_supp <- 3
ncol_mit <- 2
theme(axis.title.y = element_text(size = 13.5, vjust = +1)) 
guides(colour = guide_legend(ncol = ncol_mit), fill = guide_legend(ncol = ncol_mit))
guides(colour = guide_legend(ncol = ncol_supp), fill = guide_legend(ncol = ncol_supp))


# Plotting Suppression Output for R03 and ICU Occupancy
counter <- 1
supp_names <- c("e", "f", "g", "h")
for (i in 17:20) {
  x <- assign(object_names[[i]], read.csv(file_names[[i]])[, -1])
  x <- x %>%
    gather(strategy, incidence, -country, -R0, -output_type, -time) %>%
    spread(output_type, incidence) 
  x$strategy <- factor(x$strategy, levels = strats)
  beds <- healthcare_capacity[counter, ]
  assign(supp_names[counter], ggplot(data = x, aes(x = time, y = median_ICU_occ/millions/10, colour = strategy)) +
    geom_line(size = 2) +
    geom_ribbon(aes(x = time, ymin = lower_ICU_occ/millions/10, ymax = upper_ICU_occ/millions/10, 
                    colour = strategy, fill = strategy, border = NULL), alpha = 0.2, linetype = 0) + 
    labs(x = "Time (Days)", y = "ICU Bed Occupancy Per Day\n Per 100,000 Population") +
    annotate("rect", xmin = min(x$time), xmax = max(x$time), ymin = beds$ICU_lower/10, ymax = beds$ICU_upper/10, alpha = 0.2, fill = "grey") + 
    geom_segment(x = 0, y = beds$ICU_median/10, xend = max(x$time), yend = beds$ICU_median/10, colour = "black", size = 1, linetype = 2) +
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

layout <- "ABCD
           ABCD
           EFGH
           EFGH"

figure <- e + f + g + h +
  a + b + c + d +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 25, face = "bold"),
        legend.position = "bottom", legend.box = "horizontal")
figure

# # LICs
# LIC_ICU_capacity <- beds_per_million_df$ICU_median[1]/10
# LIC_mitigation <- ICU_LIC_mitigation$data
# LIC_unmitigated <- LIC_mitigation[LIC_mitigation$scenario == "a_nothing", ]
# LIC_mitigated <- LIC_mitigation[LIC_mitigation$scenario == "b_mitigation", ]
# LIC_mitigated_plus <- LIC_mitigation[LIC_mitigation$scenario == "c_mitigation_plus", ]
# 
# LIC_unmitigated_peak <- max(LIC_unmitigated$median/millions/10)
# LIC_mitigated_peak <- max(LIC_mitigated$median/millions/10)
# LIC_mitigated_plus_peak <- max(LIC_mitigated_plus$median/millions/10)
# 
# LIC_capacity_exceed <- c(LIC_unmitigated_peak/LIC_ICU_capacity,
#                          LIC_mitigated_peak/LIC_ICU_capacity, 
#                          LIC_mitigated_plus_peak/LIC_ICU_capacity)
# 
# # LMICs
# LMIC_ICU_capacity <- beds_per_million_df$ICU_median[2]/10
# LMIC_mitigation <- ICU_LMIC_mitigation$data
# LMIC_unmitigated <- LMIC_mitigation[LMIC_mitigation$scenario == "a_nothing", ]
# LMIC_mitigated <- LMIC_mitigation[LMIC_mitigation$scenario == "b_mitigation", ]
# LMIC_mitigated_plus <- LMIC_mitigation[LMIC_mitigation$scenario == "c_mitigation_plus", ]
# 
# LMIC_unmitigated_peak <- max(LMIC_unmitigated$median/millions/10)
# LMIC_mitigated_peak <- max(LMIC_mitigated$median/millions/10)
# LMIC_mitigated_plus_peak <- max(LMIC_mitigated_plus$median/millions/10)
# 
# LMIC_capacity_exceed <- c(LMIC_unmitigated_peak/LMIC_ICU_capacity, 
#                           LMIC_mitigated_peak/LMIC_ICU_capacity, 
#                           LMIC_mitigated_plus_peak/LMIC_ICU_capacity)
# 
# # UMIC
# UMIC_ICU_capacity <- beds_per_million_df$ICU_median[3]/10
# UMIC_mitigation <- ICU_UMIC_mitigation$data
# UMIC_unmitigated <- UMIC_mitigation[UMIC_mitigation$scenario == "a_nothing", ]
# UMIC_mitigated <- UMIC_mitigation[UMIC_mitigation$scenario == "b_mitigation", ]
# UMIC_mitigated_plus <- UMIC_mitigation[UMIC_mitigation$scenario == "c_mitigation_plus", ]
# 
# UMIC_unmitigated_peak <- max(UMIC_unmitigated$median/millions/10)
# UMIC_mitigated_peak <- max(UMIC_mitigated$median/millions/10)
# UMIC_mitigated_plus_peak <- max(UMIC_mitigated_plus$median/millions/10)
# 
# UMIC_capacity_exceed <- c(UMIC_unmitigated_peak/UMIC_ICU_capacity, 
#                           UMIC_mitigated_peak/UMIC_ICU_capacity, 
#                           UMIC_mitigated_plus_peak/UMIC_ICU_capacity)
# 
# # HIC
# HIC_ICU_capacity <- beds_per_million_df$ICU_median[4]/10
# HIC_mitigation <- ICU_HIC_mitigation$data
# HIC_unmitigated <- HIC_mitigation[HIC_mitigation$scenario == "a_nothing", ]
# HIC_mitigated <- HIC_mitigation[HIC_mitigation$scenario == "b_mitigation", ]
# HIC_mitigated_plus <- HIC_mitigation[HIC_mitigation$scenario == "c_mitigation_plus", ]
# 
# HIC_unmitigated_peak <- max(HIC_unmitigated$median/millions/10)
# HIC_mitigated_peak <- max(HIC_mitigated$median/millions/10)
# HIC_mitigated_plus_peak <- max(HIC_mitigated_plus$median/millions/10)
# 
# HIC_capacity_exceed <- c(HIC_unmitigated_peak/HIC_ICU_capacity, 
#                          HIC_mitigated_peak/HIC_ICU_capacity, 
#                          HIC_mitigated_plus_peak/HIC_ICU_capacity)
# 
# Table of ICU Bed Demand Exceeding 
# x <- data.frame(LIC = LIC_capacity_exceed, LMIC = LMIC_capacity_exceed,
#                 UMIC = UMIC_capacity_exceed, HIC = HIC_capacity_exceed)
# row.names(x) <- c("Unmitigated", "Mitigation", "Mitigation w/ Elderly Shielding")
