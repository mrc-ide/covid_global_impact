# Loading Required Packages 
library(tidyverse)

# Loading In WPP Data First
raw_pop_WPP <- read.csv("Data/WPP_demog_matrix.csv") # WPP Population
pop_WPP <- raw_pop_WPP[, c(3, 13:33)] %>%
  rename(Country = `Region..subregion..country.or.area..`) %>%
  mutate(Total_Population = 1000 * rowSums(select(., -Country))) %>%
  select(Country, Total_Population)

world_bank_metadata <- read.csv("Data/World_Bank_Country_Metadata.csv", fileEncoding = 'UTF-8-BOM') %>%
  select(TableName, income_group, region) %>%
  rename(Country = TableName)

# Set Working Directory to Specific Subdirectory 
setwd("5_Dynamical_Modelling_Healthcare_Demand/")

# Loading In Suppression Results 
R0_2.7 <- readRDS("Outputs/Raw_All_Countries_Supp_Output/All_Countries_Suppression_Output_R0_2.7.Rds") %>%
  mutate(R0 = 2.7) %>%
  select(Country, R0, everything()) %>%
  gather(variable, total_number, -Country, -R0) %>%
  separate(variable, c("Strategy", "Variable"), 
           "(?<=Unmit)_|(?<=Supp_0.2)_|(?<=Supp_1.6)_", 
           extra = "merge", remove = TRUE) %>%
  spread(Variable, total_number) %>%
  select(Country, R0, Strategy, Inc_Abs, Death_Abs, Total_Hosp_Abs, Max_Hosp_Abs, Total_ICU_Abs, Max_ICU_Abs) %>%
  rename(Incidence = Inc_Abs, Deaths = Death_Abs, 
         Total_Hospitalied = Total_Hosp_Abs, Peak_Hospital_Bed_Demand = Max_Hosp_Abs,
         Total_Critical = Total_ICU_Abs, Peak_Critical_Care_Demand = Max_ICU_Abs) %>%
  mutate(Strategy = factor(Strategy, levels = c("Unmit", "Supp_0.2", "Supp_1.6"))) %>%
  arrange(Strategy, R0, Country) %>%
  left_join(pop_WPP, by = "Country") %>%
  select(Country, R0, Strategy, Total_Population, everything())
R0_3.0 <- readRDS("Outputs/Raw_All_Countries_Supp_Output/All_Countries_Suppression_Output_R0_3.Rds") %>%
  mutate(R0 = 3.0) %>%
  select(Country, R0, everything()) %>%
  gather(variable, total_number, -Country, -R0) %>%
  separate(variable, c("Strategy", "Variable"), 
           "(?<=Unmit)_|(?<=Supp_0.2)_|(?<=Supp_1.6)_", 
           extra = "merge", remove = TRUE) %>%
  spread(Variable, total_number) %>%
  select(Country, R0, Strategy, Inc_Abs, Death_Abs, Total_Hosp_Abs, Max_Hosp_Abs, Total_ICU_Abs, Max_ICU_Abs) %>%
  rename(Incidence = Inc_Abs, Deaths = Death_Abs, 
         Total_Hospitalied = Total_Hosp_Abs, Peak_Hospital_Bed_Demand = Max_Hosp_Abs,
         Total_Critical = Total_ICU_Abs, Peak_Critical_Care_Demand = Max_ICU_Abs) %>%
  mutate(Strategy = factor(Strategy, levels = c("Unmit", "Supp_0.2", "Supp_1.6"))) %>%
  arrange(Strategy, R0, Country) %>%
  left_join(pop_WPP, by = "Country") %>%
  select(Country, R0, Strategy, Total_Population, everything())
R0_3.5 <- readRDS("Outputs/Raw_All_Countries_Supp_Output/All_Countries_Suppression_Output_R0_3.5.Rds") %>%
  mutate(R0 = 3.5) %>%
  select(Country, R0, everything()) %>%
  gather(variable, total_number, -Country, -R0) %>%
  separate(variable, c("Strategy", "Variable"), 
           "(?<=Unmit)_|(?<=Supp_0.2)_|(?<=Supp_1.6)_", 
           extra = "merge", remove = TRUE) %>%
  spread(Variable, total_number) %>%
  select(Country, R0, Strategy, Inc_Abs, Death_Abs, Total_Hosp_Abs, Max_Hosp_Abs, Total_ICU_Abs, Max_ICU_Abs) %>%
  rename(Incidence = Inc_Abs, Deaths = Death_Abs, 
         Total_Hospitalied = Total_Hosp_Abs, Peak_Hospital_Bed_Demand = Max_Hosp_Abs,
         Total_Critical = Total_ICU_Abs, Peak_Critical_Care_Demand = Max_ICU_Abs) %>%
  mutate(Strategy = factor(Strategy, levels = c("Unmit", "Supp_0.2", "Supp_1.6"))) %>%
  arrange(Strategy, R0, Country) %>%
  left_join(pop_WPP, by = "Country") %>%
  select(Country, R0, Strategy, Total_Population, everything())

overall <- rbind(R0_2.7, R0_3.0, R0_3.5)
write.csv(overall, file = "Outputs/Combined_All_Countries_Suppression_Output.csv")

WPP_problem_spellings <- as.character(unique(R0_3.0$Country[!(R0_3.0$Country %in% world_bank_metadata$Country)]))
WPP_correct <- c("Bahamas, The", "Taiwan", "Curaçao", "Czech Republic", "Egypt, Arab Rep.", "French Guiana",
                 "Gambia, The", "Guadeloupe", "Iran, Islamic Rep.", "Korea, Dem. People's Rep.", "Martinique",
                 "Mayotte", "Micronesia, Fed. Sts.", "Reunion", "Slovak Republic", "Palestine", 
                 "Virgin Islands (U.S.)", "Venezuela, RB", "Western Sahara", "Yemen, Rep.")
R0_2.7$Country <- as.character(R0_2.7$Country)
for(i in 1:length(R0_2.7$Country)) {
  if (R0_2.7$Country[i] %in% WPP_problem_spellings) {
    index <- which(WPP_problem_spellings %in% R0_2.7$Country[i])
    R0_2.7$Country[i] <- WPP_correct[index]
    print(i)
  }
}
R0_3.0$Country <- as.character(R0_3.0$Country)
for(i in 1:length(R0_3.0$Country)) {
  if (R0_3.0$Country[i] %in% WPP_problem_spellings) {
    index <- which(WPP_problem_spellings %in% R0_3.0$Country[i])
    R0_3.0$Country[i] <- WPP_correct[index]
    print(i)
  }
}
R0_3.5$Country <- as.character(R0_3.5$Country)
for(i in 1:length(R0_3.5$Country)) {
  if (R0_3.5$Country[i] %in% WPP_problem_spellings) {
    index <- which(WPP_problem_spellings %in% R0_3.5$Country[i])
    R0_3.5$Country[i] <- WPP_correct[index]
    print(i)
  }
}

NAs_R0_3.0 <- R0_3.0 %>%
  left_join(world_bank_metadata, by = "Country") %>%
  filter(Strategy == "Unmit") %>%
  filter(is.na(region))
length(unique(NAs_R0_3.0$Country))
sum(NAs_R0_3.0$Incidence)

region_R0_2.7 <- R0_2.7 %>%
  left_join(world_bank_metadata, by = "Country") %>%
  filter(!is.na(region)) %>% 
  select(Country, region, Strategy, Incidence, Deaths) %>%
  group_by(Strategy, region) %>%
  summarise(total_incidence = round(sum(Incidence)), total_deaths = round(sum(Deaths))) %>%
  pivot_wider(id_cols = region, 
              names_from = Strategy, 
              values_from = c("total_incidence", "total_deaths")) 

region_R0_3.0 <- R0_3.0 %>%
  left_join(world_bank_metadata, by = "Country") %>%
  filter(!is.na(region)) %>% 
  select(Country, region, Strategy, Incidence, Deaths) %>%
  group_by(Strategy, region) %>%
  summarise(total_incidence = round(sum(Incidence)), total_deaths = round(sum(Deaths))) %>%
  pivot_wider(id_cols = region, 
              names_from = Strategy, 
              values_from = c("total_incidence", "total_deaths")) 

region_R0_3.5 <- R0_3.5 %>%
  left_join(world_bank_metadata, by = "Country") %>%
  filter(!is.na(region)) %>% 
  select(Country, region, Strategy, Incidence, Deaths) %>%
  group_by(Strategy, region) %>%
  summarise(total_incidence = round(sum(Incidence)), total_deaths = round(sum(Deaths))) %>%
  pivot_wider(id_cols = region, 
              names_from = Strategy, 
              values_from = c("total_incidence", "total_deaths")) 


colnames(region_numbers) <- c("Region", 
                              "Unmitigated Incidence", "Suppression 0.2 Incidence", "Suppression 1.6 Incidence",
                              "Unmitigated Deaths", "Suppression 0.2 Deaths", "Suppression 1.6 Deaths",
                              "Unmitigated Hospitalisations", "Suppression 0.2 Hospitalisations", "Suppression 1.6 Hospitalisations",
                              "Unmitigated Critical Care", "Suppression 0.2 Critical Care", "Suppression 1.6 Critical Care")

write.csv(region_numbers, file = "Outputs/All_Countries_Suppression/Region_Specific_Numbers.csv")

