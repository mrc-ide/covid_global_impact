# Replacing and Checking the Problem Spellings in the WPP Data
WPP <- read.csv("Data/WPP_demog_matrix.csv")
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




age_levels_WPP <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")


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

