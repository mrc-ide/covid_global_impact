# Loading Required Libraries
library(tidyverse); library(lattice); library(viridisLite); library(RColorBrewer); library(patchwork);
library(cowplot);

# Various Naming Bits and Bobs
age_levels_raw <- c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34",
                    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", 
                    "70 to 74", "75 to 79", "80 plus")
age_levels <- c("0 to 5", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34",
                "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", 
                "70 to 74", "75 to 79", "80 plus")
age_levels_WPP <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                    "70-74", "75-79", "80+")
income_groups <- c("Low income", "Lower middle income", "Upper middle income", "High income")

# Sorting Out Naming of Countries
world_bank_metadata <- read.csv("C:/Users/cw1716/Documents/COVID_2019/LMIC_Impact_Projection/Hospital Beds & ICU Capacity/Data/World_Bank_Country_Metadata.csv",
                                fileEncoding = 'UTF-8-BOM')
world_bank_metadata <- world_bank_metadata %>%
  select(TableName, income_group)
world_bank_countries <- world_bank_metadata %>%
  filter(income_group != "") %>%
  select(TableName)
world_bank_countries <- as.character(unlist(world_bank_countries))
world_bank_countries <- world_bank_countries[order(world_bank_countries)]

# Replacing and Checking Problem Spelling in Comorbidity Data
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

# Replacing and Checking the Problem Spellings in the WPP Data
WPP <- read.csv("C:/Users/cw1716/Documents/COVID_2019/LMIC_Impact_Projection/SEIR_Modelling/very_basic_SEIR/Data/WPP_demog_matrix.csv")
WPP <- WPP %>%
  mutate(Region..subregion..country.or.area.. = as.character(Region..subregion..country.or.area..))
WPP_countries <- WPP %>%
  select(Region..subregion..country.or.area..)
WPP_countries <- unique(WPP_countries)
WPP_countries <- as.character(unlist(WPP_countries))
WPP_countries <- WPP_countries[order(WPP_countries)]
WPP_problem_spellings <- WPP_countries[which(!(WPP_countries %in% world_bank_countries))]
WPP_problem_spellings <- WPP_problem_spellings[order(WPP_problem_spellings)]
WPP_correct <- c("Bahamas, The", "Taiwan", "Curaçao", "Czech Republic", "Egypt, Arab Rep.", "French Guiana",
                 "Gambia, The", "Guadeloupe", "Iran, Islamic Rep.", "Korea, Dem. People's Rep.", "Martinique",
                 "Mayotte", "Micronesia, Fed. Sts.", "Reunion", "Slovak Republic", "Palestine", 
                 "Virgin Islands (U.S.)", "Venezuela, RB", "Western Sahara", "Yemen, Rep.")
for(i in 1:length(WPP$Region..subregion..country.or.area..)) {
  if (WPP$Region..subregion..country.or.area..[i] %in% WPP_problem_spellings) {
    index <- which(WPP_problem_spellings %in% WPP$Region..subregion..country.or.area..[i])
    WPP$Region..subregion..country.or.area..[i] <- WPP_correct[index]
    print(i)
  }
}
WPP_countries <- WPP %>%
  select(Region..subregion..country.or.area..) 
WPP_countries <- unique(WPP_countries)
WPP_countries <- as.character(unlist(WPP_countries))
WPP_countries <- WPP_countries[order(WPP_countries)]
WPP_problem_spellings <- WPP_countries[which(!(WPP_countries %in% world_bank_countries))]

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
levels(como_df$age_new) <- age_levels_WPP
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
COPD_Plot  

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

# Population Linking
WPP_pop <- WPP[, c(3, 13:33)]
age_group_names <- colnames(WPP_pop)[-1]
age_group_names <- gsub("X", "", age_group_names)
age_group_names <- gsub("\\.", "-", age_group_names)
colnames(WPP_pop) <- c("country", age_group_names)
WPP_pop <- WPP_pop %>%
  mutate(`80+` = `80-84` + `85-89` + `90-94` + `95-99` + `100-`) %>%
  select(-`80-84`, -`85-89`, -`90-94`, -`95-99`, -`100-`) %>%
  gather(age_group, population, -country)
total_pop_WPP <- WPP_pop %>%
  group_by(country) %>%
  summarise(total_pop = sum(population) * 1000)
WPP_como <- como_df %>% 
  left_join(total_pop_WPP, by = c("country" = "country")) %>%
  left_join(WPP_pop, by = c("country" = "country", "age_group" = "age_group")) %>%
  mutate(age_population = population * 1000) %>%
  select(-population)

# Final Epidemic Size Linking
final_size <- read.csv("Data/Global_unmitigated_and_mitigated_epidemics_2.4_to_3.3.csv")
final_size <- final_size %>%
  filter(R0 == 3) %>%
  select(Country, Strategy, grep("infected_", colnames(final_size))) %>%
  mutate(Country = as.character(Country))
names_index <- grep("infected_", colnames(final_size))
colnames(final_size)[names_index] <- age_levels_WPP
final_size_countries <- final_size$Country[order(final_size$Country)]
final_size <- final_size %>%
  gather(age_group, number_infected, -Country, - Strategy) %>%
  mutate(number_infected = number_infected * 1000)

final_size_problem_spellings <- unique(final_size_countries[which(!(final_size_countries %in% WPP_como$country))])
final_size_correct <- c("Aruba", "Bahamas, The", "Channel Islands", "Taiwan", "Curaçao", "Czech Republic",
                        "Egypt, Arab Rep.", "French Guiana", "French Polynesia", "Gambia, The", "Guadeloupe",
                        "Hong Kong", "Iran, Islamic Rep.", "Korea, Dem. People's Rep.", "Macao", "Martinique",
                        "Mayotte", "Micronesia (Fed. States of)", "New Caledonia", "Réunion", "Slovak Republic",
                        "Palestine", "Virgin Islands (U.S.)", "Venezuela, RB", "Western Sahara", "Yemen, Rep.")
for(i in 1:length(final_size$Country)) {
  if (final_size$Country[i] %in% final_size_problem_spellings) {
    index <- which(final_size_problem_spellings %in% final_size$Country[i])
    final_size$Country[i] <- final_size_correct[index]
    print(i)
  }
}
final_size_countries <- final_size$Country
final_size_countries <- unique(final_size_countries)
final_size_countries <- as.character(unlist(final_size_countries))
final_size_countries <- final_size_countries[order(final_size_countries)]
final_size_problem_spellings <- final_size_countries[which(!(final_size_countries %in% WPP_como$country))]

# Creating the Final Df
causes <- c("Cardiovascular diseases", "Chronic obstructive pulmonary disease", "Diabetes and kidney diseases",
            "HIV/AIDS", "Tuberculosis", "Nutritional deficiencies", "Malaria")
final_df <- WPP_como %>%
  left_join(final_size, by = c("country" = "Country", "age_group" = "age_group")) %>%
  filter(!is.na(number_infected), cause_name %in% causes) %>%
  rename(percent_comorb = value) %>%
  mutate(percent_infected_and_comorb = number_infected * percent_comorb/total_pop) %>%
  mutate(Strategy = factor(Strategy, levels = c("Unmitigated", "Social distancing whole population", "Enhanced social distancing of elderly")))
included_countries <- unique(final_df$country)[order(unique(final_df$country))]
missing_countries <- final_size_countries[!(final_size_countries %in% included_countries)]

HIV_age <- final_df %>%
  filter(cause_name == "Nutritional deficiencies") %>%
  group_by(income_group, Strategy, age_group) %>%
  summarise(mean = mean(percent_infected_and_comorb)) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))
  
ggplot(HIV_age, aes(x = income_group, y = mean, fill = age_group)) +
  geom_bar(stat = "identity") + 
  scale_fill_viridis_d(direction = -1) +
  facet_grid(~Strategy)

total_pop <- final_df %>%
  filter(cause_name == "HIV/AIDS") %>%
  group_by(country) %>%
  summarise(total_pop = sum(population))


summary_final_df <- final_df %>%
  group_by(cause_name, income_group) %>%
  summarise(total = sum(number_infected_and_comorb))

income_status_total <- summary_final_df %>%
  group_by(income_group) %>%
  summarise(total = sum(total))

b <- summary_final_df %>%
  left_join(income_status_total, by = "income_group") %>%
  mutate(prop = total.x/total.y)

ggplot(b, aes(x = income_group, y = prop, fill = cause_name)) +
  geom_bar(stat = "identity")

sum(is.na(final_df$population))

total_pop <- final_df %>%
  group_by(country) %>%
  summarise(total_pop = sum(population))

final_df <- final_df %>%
  mutate(percent_pop_inf_comorb = percent_comorb * number_infected / population)
  


summary_final_df <- final_df %>%
  group_by(cause_name, income_group) %>%
  summarise(total = sum(number_infected_and_comorb))

income_status_total <- summary_final_df %>%
  group_by(cause_name) %>%
  summarise(total = sum(total))

b <- summary_final_df %>%
  left_join(income_status_total, by = c("cause_name" = "cause_name")) %>%
  mutate(prop = total.x/total.y)

ggplot(b, aes(x = cause_name, y = prop, fill = income_group)) +
  geom_bar(stat = "identity")


# OrRd
# YlGn
# PuBu
# Greys
# 
# coul <- viridis(100)
# levelplot(b, col.regions = coul) # try cm.colors() or terrain.colors()
# 
# heatmap(b)
# 
# 
# # Malaria
# Malaria <- como_df %>%
#   filter(cause_name == "Malaria") %>%
#   group_by(income_group, age_group) %>%
#   summarise(mean = mean(value)) %>%
#   select(income_group, age_group, mean)
# ggplot(Malaria, aes(income_group, age_group, fill = mean)) +
#   geom_tile() +
#   labs(y = "Age Group", x = "Income Group") +
#   scale_x_discrete(labels = c("Low Income", "Lower Middle\nIncome", "Upper Middle\nIncome", "High Income")) +
#   scale_fill_distiller(palette = "OrRd", direction = 1) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text.x = element_text(size = 12, face = "bold"),
#         axis.text.y = element_text(size = 12, face = "bold"),
#         axis.title.y = element_text(size = 13.5, vjust = +3),
#         axis.title.x = element_text(size = 13.5, vjust = -1),
#         plot.margin = margin(0.2, 0.2, 0.2, 0.75, "cm"))
# 
# 
# # Resp 5.2, 5.8
# resp <- como_df %>%
#   filter(cause_name == "Chronic respiratory diseases") %>%
#   group_by(income_group, age_group) %>%
#   summarise(mean = mean(value)) %>%
#   select(income_group, age_group, mean)
# ggplot(resp, aes(income_group, age_group, fill = mean)) +
#   geom_tile() +
#   scale_x_discrete(labels = c("Low\nIncome", "Lower\nMiddle\nIncome", "Upper\nMiddle\nIncome", "High\nIncome")) +
#   scale_fill_distiller(palette = "OrRd", direction = 1,
#                        limits = c(0, 0.31), breaks = c(0, 0.1, 0.2, 0.3)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text.x = element_text(size = 11, face = "bold"),
#         axis.text.y = element_text(size = 11, face = "bold"),
#         axis.title.y = element_text(size = 13.5, vjust = +5),
#         axis.title.x = element_blank(),
#         plot.margin = margin(0.2, 0.2, 0.2, 0.75, "cm"),
#         legend.title = element_blank(), legend.text = element_text(size = 12),
#         legend.key.size = unit(1, "cm"))
# 
