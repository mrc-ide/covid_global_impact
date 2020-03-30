library(tidyverse)

profiles <- read.csv("C:/Users/cw1716/Documents/COVID_2019/LMIC_Impact_Projection/SEIR_Modelling/very_basic_SEIR/Data/severe_parameters.csv", stringsAsFactors = TRUE)
age_groups_raw <-c("0_5", "5_10", "10_15", "15_20", "20_25", "25_30", "30_35", "35_40", "40_45", "45_50", "50_55", "55_60", "60_65", "65_70", "70_75", "75_80", "80+")
profiles$age_group <- factor(profiles$age_group, levels = age_groups_raw)

profiles <- profiles %>% 
  # mutate(age_group = as.numeric(age_group)) %>%
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


  geom_rect(xmin = time_for_triggers[2], xmax = time_for_triggers[2] + 90, fill = "#EAEAEA", ymin = 0, ymax = 4500, colour="white", size = 0.5, alpha = 0.1) +
  geom_line(size = 2) +
  geom_ribbon(data = lifted_incidence_suppression_output_plot, aes(x = day, ymin = lower/millions/10, ymax = upper/millions/10, colour = strategy, fill = strategy, border = NULL), alpha = 0.2, linetype = 0) + 
  labs(x = "Time (Days)", y = "Infection Incidence Per Day\n Per 100,000 Population") +
  theme_bw() +
  scale_colour_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                        breaks = c("none", "incomplete"),
                        labels = c("No Suppression", "Suppression then lifting")) + 
  scale_fill_discrete(name = "Deaths Per 100,000\nPopulation Per Week\nTo Trigger Suppression",
                      breaks = c("none", "incomplete"),
                      labels = c("No Suppression", "Suppression then lifting")) +  
  lims(y = c(0, max(lifted_incidence_suppression_output_plot$upper/millions/10))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", axis.title.y = element_text(size = 13.5, vjust = +3), 
        axis.title.x = element_text(size = 13.5, vjust = +0),
        legend.title = element_text(size = 12, face = "bold"),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))

