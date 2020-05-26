
# Loading Required Libraries 
library(readxl); library(lubridate); library(dplyr); library(tidyverse);
library(ggplot2); library(squire); library(ggpubr);library(gridExtra);
library(patchwork)

# Setting Working Directory and Removing Any Environment Contents
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Running the Epidemic Trajectory Plots Tracking Deaths Over Time, Unlimited Healthcare Capacity
pop <- get_population("India")
std_population <- pop$n/sum(pop$n) * 100000000
contact_matrix <- get_mixing_matrix("India")
t1 <- align(country = "India",
                population= std_population,
                contact_matrix_set = contact_matrix,
                hosp_bed_capacity = 100000000000000000,
                ICU_bed_capacity = 100000000000000000000,
                deaths = 10,
                reporting_fraction = 1,
                dt = 0.1,
                time_period = 700,
                replicates = 50,
                R0 = 3)
## RUN UNMITIGATED
t1_deaths <- format_output(t1, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
t1_deaths$scenario <- "Unmitigated"
## RUN POORLY MITIGATED
p1 <- projections(r = t1, R0_change = c(0.8, 1), tt_R0 = c(0, 360))
p1_deaths <- format_output(p1, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
p1_deaths$scenario <- "Poorly mitigated (20% reduction for 1 year)"


## RUN WELL-MITIGATED
p2 <- projections(r = t1, R0_change = c(0.55, 1), tt_R0 = c(0, 360))
p2_deaths <- format_output(p2, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
p2_deaths$scenario <- "Optimal single wave (45% reduction for 1 year)"
## RUN SECOND WAVE
p3 <- projections(r = t1, R0_change = c(0.4, 1), tt_R0 = c(0, 360))
p3_deaths <- format_output(p3, var_select = "deaths") %>%
  mutate(t = factor(t)) %>%
  group_by(t) %>%
  summarise(median = median(y),
            lower = quantile(y, 0.025),
            upper = quantile(y, 0.975))
p3_deaths$scenario <- "Second wave (60% reduction for 1 year)"

overall <- rbind(t1_deaths,p1_deaths,p2_deaths, p3_deaths)
overall$t <- as.numeric(overall$t)
overall$scenario <- factor(overall$scenario, levels = c("Unmitigated","Poorly mitigated (20% reduction for 1 year)", "Optimal single wave (45% reduction for 1 year)", "Second wave (60% reduction for 1 year)"))

tiff("FigureS1_Optimal_mitigation_example.tif",res=300,height=5,width=7,unit="in")
 ggplot(overall, aes(x = t/10, y = median/10, col = scenario)) + 
  #geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.2, colour = NA) +
  geom_line(size = 2) +
  theme_bw() +
  scale_colour_manual(values = c("#D8DBE2", "#A9BCD0", "#58A4B0", "#373F51")) +
  scale_fill_manual(values = c("#D8DBE2", "#A9BCD0", "#58A4B0", "#373F51")) +
  labs(y = "Number of Deaths/million (Daily)", x = "Time (Days)")+
  theme(legend.position = c(.6, .85),legend.text = element_text(size = 15),legend.title = element_blank())
dev.off()

 