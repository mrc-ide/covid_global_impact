# Loading Required Libraries
library(tidyverse); library(lattice); library(viridisLite); library(RColorBrewer); 
library(patchwork); library(cowplot);

# Loading In Data Used Across Multiple Analysis Strands
final_size <- read.csv("Data/Global_unmitigated_and_mitigated_epidemics_2.4_to_3.5.csv")
WPP <- read.csv("Data/WPP_demog_matrix.csv")
world_bank_metadata <- read.csv("Data/World_Bank_Country_Metadata.csv", fileEncoding = 'UTF-8-BOM') %>%
  select(TableName, income_group, region)
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
age_levels_WPP <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
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
  gather(age_group, value, -cause_name, -income_group, -country, -region) %>%
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

# Replacing and Checking the Problem Spellings in the WPP Data
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
            "HIV/AIDS", "Tuberculosis", "Nutritional deficiencies")
final_df <- WPP_como %>%
  left_join(final_size, by = c("country" = "Country", "age_group" = "age_group")) %>%
  filter(!is.na(number_infected), cause_name %in% causes) %>%
  rename(prop_comorb = value) %>%
  mutate(number_comorb = prop_comorb * age_population)%>%
  mutate(number_infected_and_comorb = number_infected * prop_comorb) %>%
  mutate(Strategy = factor(Strategy, levels = c("Unmitigated", "Social distancing whole population", "Enhanced social distancing of elderly"))) %>%
  mutate(age_group = factor(age_group, levels = age_levels_final))
included_countries <- unique(final_df$country)[order(unique(final_df$country))]
missing_countries <- final_size_countries[!(final_size_countries %in% included_countries)]
wb_missing_countries <- world_bank_countries[!(world_bank_countries %in% final_size_countries)]
fs_missing_countries <- final_size_countries[!(final_size_countries %in% world_bank_countries)]

# CVD
CVD_age <- final_df %>%
  filter(cause_name == "Cardiovascular diseases") %>%
  group_by(income_group, Strategy, age_group) %>%
  summarise(total_infected_with_comorb = sum(number_infected_and_comorb), total_pop = sum(total_pop)) %>%
  mutate(prop_comorb_and_inf = total_infected_with_comorb/total_pop) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))
averted <- sum(CVD_age$total_infected_with_comorb[CVD_age$Strategy == "Unmitigated"]) - sum(CVD_age$total_infected_with_comorb[CVD_age$Strategy == "Enhanced social distancing of elderly"])
HIC_averted <- sum(CVD_age$total_infected_with_comorb[CVD_age$Strategy == "Unmitigated" & CVD_age$income_group == "HIC"]) - 
  sum(CVD_age$total_infected_with_comorb[CVD_age$Strategy == "Enhanced social distancing of elderly" & CVD_age$income_group == "HIC"])
HIC_averted/averted

a <- ggplot(CVD_age, aes(x = income_group, y = prop_comorb_and_inf, fill = age_group)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Strategy) + 
  theme_bw() +
  labs(y = "Prop. Pop", x = "") +
  scale_fill_manual(breaks = levels(CVD_age$age_group),
                    values = rev(magma(length(levels(CVD_age$age_group))))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

CVD <- final_df %>%
  filter(cause_name == "Cardiovascular diseases") %>% 
  filter(Strategy == "Unmitigated") %>%
  group_by(income_group, age_group) %>%
  summarise(prop_comorb = sum(number_comorb)/sum(age_population))

b <- ggplot(CVD, aes(income_group, age_group, fill = prop_comorb)) + 
  geom_tile() +
  labs(y = "", x = "") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.51), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                       guide = "legend") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"), legend.position = "left", 
        panel.border=element_rect(fill = NA, colour = "black", size = 0.5))  +
  guides(fill = guide_legend(reverse = TRUE, label.position = "left")) 


# HIV/AIDs
hiv_age <- final_df %>%
  filter(cause_name == "HIV/AIDS") %>%
  group_by(income_group, Strategy, age_group) %>%
  summarise(total_infected_with_comorb = sum(number_infected_and_comorb), total_pop = sum(total_pop)) %>%
  mutate(prop_comorb_and_inf = total_infected_with_comorb/total_pop) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))

c <- ggplot(hiv_age, aes(x = income_group, y = prop_comorb_and_inf, fill = age_group)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Strategy) + 
  theme_bw() +
  labs(y = "Prop. Pop", x = "") +
  scale_fill_manual(breaks = levels(hiv_age$age_group),
                    values = rev(magma(length(levels(hiv_age$age_group))))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

HIV <- final_df %>%
  filter(cause_name == "HIV/AIDS") %>% 
  filter(Strategy == "Unmitigated") %>%
  group_by(income_group, age_group) %>%
  summarise(prop_comorb = sum(number_comorb)/sum(age_population))
d <- ggplot(HIV, aes(income_group, age_group, fill = prop_comorb)) + 
  geom_tile() +
  labs(y = "", x = "") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.045), breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05),
                       guide = "legend") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"), legend.position = "left",
        panel.border=element_rect(fill = NA, colour = "black", size = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE, label.position = "left")) 

# Nutrition
nutrition_age <- final_df %>%
  filter(cause_name == "Nutritional deficiencies") %>%
  group_by(income_group, Strategy, age_group) %>%
  summarise(total_infected_with_comorb = sum(number_infected_and_comorb), total_pop = sum(total_pop)) %>%
  mutate(prop_comorb_and_inf = total_infected_with_comorb/total_pop) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))
  
e <- ggplot(nutrition_age, aes(x = income_group, y = prop_comorb_and_inf, fill = age_group)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Strategy) + 
  theme_bw() +
  labs(y = "Prop. Pop", x = "Income Group") +
  scale_fill_manual(breaks = levels(nutrition_age$age_group),
                    values = rev(magma(length(levels(nutrition_age$age_group))))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

Nutrition <- final_df %>%
  filter(cause_name == "Nutritional deficiencies") %>% 
  filter(Strategy == "Unmitigated") %>%
  group_by(income_group, age_group) %>%
  summarise(prop_comorb = sum(number_comorb)/sum(age_population))
f <- ggplot(Nutrition, aes(income_group, age_group, fill = prop_comorb)) + 
  geom_tile() +
  labs(y = "", x = "") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.55), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", angle = -90, hjust = 0, vjust = 0.25),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"), legend.position = "left",
        panel.border=element_rect(fill = NA, colour = "black", size = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE, label.position = "left")) 

# 9.5 by 7.8
layout <- "ABBBBBBB
CDDDDDDD
EFFFFFFF"

figure <- b + a + d + c + f + e +
  plot_layout(design = layout)
figure 

# HIV/AIDs by Region
hiv_age_region <- final_df %>%
  filter(cause_name == "HIV/AIDS") %>%
  group_by(region, Strategy, age_group) %>%
  summarise(total_infected_with_comorb = sum(number_infected_and_comorb), total_pop = sum(total_pop)) %>%
  mutate(prop_comorb_and_inf = total_infected_with_comorb/total_pop) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))

supp <- ggplot(hiv_age_region, aes(x = region, y = prop_comorb_and_inf, fill = age_group)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Strategy) + 
  theme_bw() +
  labs(y = "Prop. Pop", x = "") +
  scale_x_discrete(labels = c("East Asia & Pacific" = "E.Asia",
                              "Europe & Central Asia" = "Europe",
                              "Latin America & Caribbean" = "L.America",
                              "Middle East & North Africa" = "M.East",
                              "North America" = "N.America",
                              "South Asia" = "S.Asia",
                              "Sub-Saharan Africa" = "SSA")) +
  scale_fill_manual(breaks = levels(hiv_age_region$age_group),
                    values = rev(magma(length(levels(hiv_age_region$age_group))))) +
  theme(legend.position = "right")

income_group_pop <- final_df %>%
  filter(Strategy == "Unmitigated") %>%
  filter(cause_name == "Chronic obstructive pulmonary disease") %>% 
  group_by(income_group) %>%
  summarise(total_pop = sum(age_population))
 
income_group_pop$total_pop[4]/sum(income_group_pop$total_pop)

#####
COPD_age <- final_df %>%
  filter(cause_name == "Chronic obstructive pulmonary disease") %>% 
  group_by(income_group, Strategy, age_group) %>%
  summarise(total_infected_with_comorb = sum(number_infected_and_comorb), total_pop = sum(total_pop)) %>%
  mutate(prop_comorb_and_inf = total_infected_with_comorb/total_pop) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))
averted <- sum(COPD_age$total_infected_with_comorb[COPD_age$Strategy == "Unmitigated"]) - sum(COPD_age$total_infected_with_comorb[COPD_age$Strategy == "Enhanced social distancing of elderly"])
HIC_averted <- sum(COPD_age$total_infected_with_comorb[COPD_age$Strategy == "Unmitigated" & COPD_age$income_group == "HIC"]) - 
  sum(COPD_age$total_infected_with_comorb[COPD_age$Strategy == "Enhanced social distancing of elderly" & COPD_age$income_group == "HIC"])
HIC_averted/averted


h <- ggplot(COPD_age, aes(x = income_group, y = prop_comorb_and_inf, fill = age_group)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Strategy) + 
  theme_bw() +
  labs(y = "Prop. Pop", x = "Income Group") +
  scale_fill_manual(breaks = levels(COPD_age$age_group),
                    values = rev(magma(length(levels(COPD_age$age_group))))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

COPD <- final_df %>%
  filter(cause_name == "Chronic obstructive pulmonary disease") %>% 
  filter(Strategy == "Unmitigated") %>%
  group_by(income_group, age_group) %>%
  summarise(prop_comorb = sum(number_comorb)/sum(age_population))
g <- ggplot(COPD, aes(income_group, age_group, fill = prop_comorb)) + 
  geom_tile() +
  labs(y = "", x = "") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.3), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", angle = -90, hjust = 0, vjust = 0.25),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"), legend.position = "left",
        panel.border=element_rect(fill = NA, colour = "black", size = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE, label.position = "left")) 

# Diabetes
diabetes_age <- final_df %>%
  filter(cause_name == "Diabetes and kidney diseases") %>% 
  group_by(income_group, Strategy, age_group) %>%
  summarise(total_infected_with_comorb = sum(number_infected_and_comorb), total_pop = sum(total_pop)) %>%
  mutate(prop_comorb_and_inf = total_infected_with_comorb/total_pop) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))
averted <- sum(diabetes_age$total_infected_with_comorb[diabetes_age$Strategy == "Unmitigated"]) - sum(diabetes_age$total_infected_with_comorb[diabetes_age$Strategy == "Enhanced social distancing of elderly"])
HIC_averted <- sum(diabetes_age$total_infected_with_comorb[diabetes_age$Strategy == "Unmitigated" & diabetes_age$income_group == "HIC"]) - 
  sum(diabetes_age$total_infected_with_comorb[diabetes_age$Strategy == "Enhanced social distancing of elderly" & diabetes_age$income_group == "HIC"])
HIC_averted/averted

j <- ggplot(diabetes_age, aes(x = income_group, y = prop_comorb_and_inf, fill = age_group)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Strategy) + 
  theme_bw() +
  labs(y = "Prop. Pop", x = "Income Group") +
  scale_fill_manual(breaks = levels(COPD_age$age_group),
                    values = rev(magma(length(levels(COPD_age$age_group))))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

diabetes <- final_df %>%
  filter(cause_name == "Diabetes and kidney diseases") %>% 
  filter(Strategy == "Unmitigated") %>%
  group_by(income_group, age_group) %>%
  summarise(prop_comorb = sum(number_comorb)/sum(age_population))

i <- ggplot(diabetes, aes(income_group, age_group, fill = prop_comorb)) + 
  geom_tile() +
  labs(y = "", x = "") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.6), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", angle = -90, hjust = 0, vjust = 0.25),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"), legend.position = "left",
        panel.border=element_rect(fill = NA, colour = "black", size = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE, label.position = "left")) 



# Diabetes
tb_age <- final_df %>%
  filter(cause_name == "Tuberculosis") %>%
  group_by(income_group, Strategy, age_group) %>%
  summarise(total_infected_with_comorb = sum(number_infected_and_comorb), total_pop = sum(total_pop)) %>%
  mutate(prop_comorb_and_inf = total_infected_with_comorb/total_pop) %>%
  mutate(age_group = factor(age_group, levels = rev(age_levels_WPP)))

l <- ggplot(tb_age, aes(x = income_group, y = prop_comorb_and_inf, fill = age_group)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Strategy) + 
  theme_bw() +
  labs(y = "Prop. Pop", x = "Income Group") +
  scale_fill_manual(breaks = levels(tb_age$age_group),
                    values = rev(magma(length(levels(tb_age$age_group))))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

tb <- final_df %>%
  filter(cause_name == "Tuberculosis") %>%
  filter(Strategy == "Unmitigated") %>%
  group_by(income_group, age_group) %>%
  summarise(prop_comorb = sum(number_comorb)/sum(age_population))

k <- ggplot(tb, aes(income_group, age_group, fill = prop_comorb)) + 
  geom_tile() +
  labs(y = "", x = "") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0, 0.5), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", angle = -90, hjust = 0, vjust = 0.25),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"), plot.title = element_text(size = 14, face = "bold"),
        legend.key.width = unit(0.5, "cm"), legend.position = "left",
        panel.border=element_rect(fill = NA, colour = "black", size = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE, label.position = "left")) 

# 9.5 by 7.8
layout <- "ABBBBBBB
CDDDDDDD
EFFFFFFF"

figure <- g + h + i + j + k + l +
  plot_layout(design = layout)
figure 
