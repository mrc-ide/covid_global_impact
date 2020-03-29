# Loading Required Libraries
library(tidyverse); library(lattice); library(viridisLite); library(RColorBrewer); 
library(patchwork); library(cowplot);

# Loading In Data Used Across Multiple Analysis Strands
world_bank_metadata <- read.csv("Data/World_Bank_Country_Metadata.csv", fileEncoding = 'UTF-8-BOM') %>%
  select(TableName, income_group)
world_bank_countries <- world_bank_metadata %>%
  filter(income_group != "") %>%
  select(TableName)
world_bank_countries <- as.character(unlist(world_bank_countries))
world_bank_countries <- world_bank_countries[order(world_bank_countries)]

# Set Working Data
setwd("2_Comorbidity_Exploration/")

# Various Naming Bits and Bobs
age_levels_raw <- c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                    "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", 
                    "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 plus")
age_levels <- c("0 to 5", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", 
                "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 plus")
age_levels_final <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
income_groups <- c("Low income", "Lower middle income", "Upper middle income", "High income")

# Replacing and Checking Problem Spelling in Comorbidity Data
#   The datasets have different naming conventions for countries - use World Bank as the
#   standard here.
comorbidity_data <- read.csv("Data/IHME_GBD_Comorbidity_Data.csv", stringsAsFactors = FALSE)
comorbidity_countries <- comorbidity_data %>%
  select(location_name) 
comorbidity_countries <- unique(comorbidity_countries)
comorbidity_countries <- as.character(unlist(comorbidity_countries))
comorbidity_countries <- comorbidity_countries[order(comorbidity_countries)]
comorbidity_problem_spellings <- comorbidity_countries[which(!(comorbidity_countries %in% world_bank_countries))]
comorbidity_problem_spellings <- comorbidity_problem_spellings[order(comorbidity_problem_spellings)]
comorbidity_correct <- c("Brunei Darussalam", "Cabo Verde", "Congo, Rep.", "Congo, Dem. Rep.",
                         "Egypt, Arab Rep.", "Micronesia, Fed. Sts.", "Iran, Islamic Rep.", "Kyrgyz Republic",
                         "Lao PDR", "North Macedonia", "Korea, Dem. People's Rep.", "Palestine", "St. Lucia",
                         "St. Vincent and the Grenadines", "Slovak Republic", "Korea, Rep.", "Eswatini",
                         "Syrian Arab Republic", "Taiwan", "Bahamas, The", "Gambia, The", "Venezuela, RB", 
                         "Virgin Islands (U.S.)", "Yemen, Rep.")
for(i in 1:length(comorbidity_data$location_name)) {
  if (comorbidity_data$location_name[i] %in% comorbidity_problem_spellings) {
    index <- which(comorbidity_problem_spellings %in% comorbidity_data$location_name[i])
    comorbidity_data$location_name[i] <- comorbidity_correct[index]
    print(i)
  }
}
comorbidity_countries <- comorbidity_data %>%
  select(location_name) 
comorbidity_countries <- unique(comorbidity_countries)
comorbidity_countries <- as.character(unlist(comorbidity_countries))
comorbidity_countries <- comorbidity_countries[order(comorbidity_countries)]
comorbidity_problem_spellings <- comorbidity_countries[which(!(comorbidity_countries %in% world_bank_countries))]

# Loading In and Processing Comorbidity Data
como_df <- comorbidity_data %>%
  filter(age_name != "All Ages") %>%
  mutate(country = location_name) %>%
  select(country, age_name, cause_name, val) %>%
  mutate(age_name = factor(age_name, levels = age_levels_raw)) %>%
  left_join(world_bank_metadata, by = c("country" = "TableName")) %>%
  mutate(income_group = factor(income_group, levels = income_groups)) %>%
  filter(!is.na(income_group)) %>%
  spread(age_name, val) %>%
  mutate(`0 to 5` = (0.2 * `<1 year` + 0.8 * `1 to 4`)) %>%
  select(-`<1 year`, -`1 to 4`) %>%
  gather(age_group, value, -cause_name, -income_group, -country) %>%
  mutate(age_group = factor(age_group, levels = age_levels),
         income_group = case_when(income_group == "Low income" ~ "LIC",
                                  income_group == "Lower middle income" ~ "LMIC", 
                                  income_group == "Upper middle income" ~ "UMIC",
                                  income_group == "High income" ~ "HIC")) %>%
  mutate(income_group = factor(income_group, levels = c("LIC", "LMIC", "UMIC", "HIC"))) %>%
  mutate(age_new = age_group)
levels(como_df$age_new) <- age_levels_final
como_df <- como_df %>%
  mutate(age_group = age_new) %>% 
  select(-age_new) 

# COPD 5.2, 5.8
COPD <- como_df %>%
  filter(cause_name == "Chronic obstructive pulmonary disease") %>% 
  group_by(income_group, age_group) %>%
  summarise(mean = mean(value)) %>%
  select(income_group, age_group, mean) 
COPD_Plot <- ggplot(COPD, aes(income_group, age_group, fill = mean)) + 
  geom_tile() +
  labs(y = "Age Group", x = "Income Group") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.26), breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25),
                       guide = "legend") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +5), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm")) + 
  guides(fill = guide_legend(reverse = TRUE)) 

# Diabetes 5.2, 5.8
diabetes <- como_df %>%
  filter(cause_name == "Diabetes and kidney diseases") %>% 
  group_by(income_group, age_group) %>%
  summarise(mean = mean(value)) %>%
  select(income_group, age_group, mean) 
diabetes_plot <- ggplot(diabetes, aes(income_group, age_group, fill = mean)) + 
  geom_tile() +
  labs(y = "Age Group", x = "Income Group") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.625), breaks = c(0, 0.125, 0.25, 0.375, 0.5, 0.625),
                       guide = "legend") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +5), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm")) + 
  guides(fill = guide_legend(reverse = TRUE)) 


# CVD 5.2, 5.8 or 6.38, 6.55
CVD <- como_df %>%
  filter(cause_name == "Cardiovascular diseases") %>% 
  group_by(income_group, age_group) %>%
  summarise(mean = mean(value)) %>%
  select(income_group, age_group, mean) 
CVD_plot <- ggplot(CVD, aes(income_group, age_group, fill = mean)) + 
  geom_tile() +
  labs(y = "Age Group", x = "Income Group") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.505), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                       guide = "legend") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +5), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"))  + 
  guides(fill = guide_legend(reverse = TRUE)) 

# HIV
HIV <- como_df %>%
  filter(cause_name == "HIV/AIDS") %>% 
  group_by(income_group, age_group) %>%
  summarise(mean = mean(value)) %>%
  select(income_group, age_group, mean) 
HIV_plot <- ggplot(HIV, aes(income_group, age_group, fill = mean)) + 
  geom_tile() +
  labs(y = "Age Group", x = "Income Group") +
  scale_fill_distiller(palette = "PuBu", direction = 1,
                       limits = c(0, 0.045), breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05),
                       guide = "legend") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +5), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"))  +
  guides(fill = guide_legend(reverse = TRUE)) 

# Tuberculosis
Tuberculosis <- como_df %>%
  filter(cause_name == "Tuberculosis") %>% 
  group_by(income_group, age_group) %>%
  summarise(mean = mean(value)) %>%
  select(income_group, age_group, mean) 
tuberculosis_plot <- ggplot(Tuberculosis, aes(income_group, age_group, fill = mean)) + 
  geom_tile() +
  labs(y = "Age Group", x = "Income Group") +
  scale_fill_distiller(palette = "PuBu", direction = 1,
                       limits = c(0, 0.5), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                       guide = "legend") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +5), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"))  +
  guides(fill = guide_legend(reverse = TRUE)) 

# Nutrition
Nutrition <- como_df %>%
  filter(cause_name == "Nutritional deficiencies") %>% 
  group_by(income_group, age_group) %>%
  summarise(mean = mean(value)) %>%
  select(income_group, age_group, mean) 
nutrition_plot <- ggplot(Nutrition, aes(income_group, age_group, fill = mean)) + 
  geom_tile() +
  labs(y = "Age Group", x = "Income Group") +
  scale_fill_distiller(palette = "PuBu", direction = 1,
                       limits = c(0, 0.55), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 13.5, vjust = +5), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"))  +
  guides(fill = guide_legend(reverse = TRUE)) 


# Figure creation: 11.06 x 6.53 
layout <- "ABC
           CDE"

figure <- diabetes_plot + CVD_plot + COPD_Plot +
          HIV_plot + tuberculosis_plot + nutrition_plot +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 25, face = "bold"))
figure
