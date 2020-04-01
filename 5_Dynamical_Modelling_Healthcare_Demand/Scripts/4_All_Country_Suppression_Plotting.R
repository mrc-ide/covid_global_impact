# Loading Required Packages 
library(tidyverse)

# Loading In WPP Data First
raw_pop_WPP <- read.csv("Data/WPP_demog_matrix.csv") # WPP Population
pop_WPP <- raw_pop_WPP[, c(3, 13:33)] %>%
  rename(Country = `Region..subregion..country.or.area..`) %>%
  mutate(Total_Population = 1000 * rowSums(select(., -Country))) %>%
  select(Country, Total_Population)

world_bank_metadata <- read.csv("Data/World_Bank_Country_Metadata.csv", fileEncoding = 'UTF-8-BOM') %>%
  select(TableName, income_group) %>%
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

overall <- cbind(R0_2.7, R0_3.0, R0_3.5)
overall <- overall[order(overall$R0), ] 
write.csv(overall, file = "Outputs/Combined_All_Countries_Suppression_Output.csv")

region_R0_3.0 <- R0_3.0 %>%
  left_join(world_bank_metadata, by = "Country")

region_R0_3.0$Country[which(is.na(region_R0_3.0$income_group))]

abs_numbers_0.2 <- country_suppression_output %>%
  select(Country, Supp_0.2_Inc_Abs, Supp_0.2_Death_Abs, Supp_0.2_Total_Hosp_Abs, Supp_0.2_Max_Hosp_Abs, Supp_0.2_Total_ICU_Abs, Supp_0.2_Max_ICU_Abs) %>%
  mutate(Supp_0.2_Inc_Abs = round(Supp_0.2_Inc_Abs),
         Supp_0.2_Death_Abs = round(Supp_0.2_Death_Abs),
         Supp_0.2_Total_Hosp_Abs = round(Supp_0.2_Total_Hosp_Abs),
         Supp_0.2_Max_Hosp_Abs = round(Supp_0.2_Max_Hosp_Abs),
         Supp_0.2_Total_ICU_Abs = round(Supp_0.2_Total_ICU_Abs),
         Supp_0.2_Max_ICU_Abs = round(Supp_0.2_Max_ICU_Abs))

abs_numbers_1.6 <- country_suppression_output %>%
  select(Country, Supp_1.6_Inc_Abs, Supp_1.6_Death_Abs, Supp_1.6_Total_Hosp_Abs, Supp_1.6_Max_Hosp_Abs, Supp_1.6_Total_ICU_Abs, Supp_1.6_Max_ICU_Abs) %>%
  mutate(Supp_1.6_Inc_Abs = round(Supp_1.6_Inc_Abs),
         Supp_1.6_Death_Abs = round(Supp_1.6_Death_Abs),
         Supp_1.6_Total_Hosp_Abs = round(Supp_1.6_Total_Hosp_Abs),
         Supp_1.6_Max_Hosp_Abs = round(Supp_1.6_Max_Hosp_Abs),
         Supp_1.6_Total_ICU_Abs = round(Supp_1.6_Total_ICU_Abs),
         Supp_1.6_Max_ICU_Abs = round(Supp_1.6_Max_ICU_Abs))
write.csv(abs_numbers_1.6, file = "Outputs/All_Countries_Suppression/Absolute_Numbers_16.csv")

abs_numbers_unmitigated <- country_suppression_output %>%
  select(Country, Unmit_Inc_Abs, Unmit_Death_Abs, Unmit_Total_Hosp_Abs, Unmit_Max_Hosp_Abs, Unmit_Total_ICU_Abs, Unmit_Max_ICU_Abs) %>%
  mutate(Unmit_Inc_Abs = round(Unmit_Inc_Abs),
         Unmit_Death_Abs = round(Unmit_Death_Abs),
         Unmit_Total_Hosp_Abs = round(Unmit_Total_Hosp_Abs),
         Unmit_Max_Hosp_Abs = round(Unmit_Max_Hosp_Abs),
         Unmit_Total_ICU_Abs = round(Unmit_Total_ICU_Abs),
         Unmit_Max_ICU_Abs = round(Unmit_Max_ICU_Abs))
write.csv(abs_numbers_unmitigated, file = "Outputs/All_Countries_Suppression/Absolute_Numbers_Unmitigated.csv")

x <- country_suppression_output %>%
  select(Unmit_Death_Abs, Supp_0.2_Death_Abs, Supp_1.6_Death_Abs)

sum(x[, 1] * 1000) - sum(x[, 2] * 1000)
sum(x[, 1] * 1000) - sum(x[, 3] * 1000)

region <- data.frame(Country = pop_WPP$Region..subregion..country.or.area.., Region = pop_WPP$region)
region_numbers <- country_suppression_output %>%
  select(-Unmit_Max_Hosp_Abs, -Supp_0.2_Max_Hosp_Abs, -Supp_1.6_Max_Hosp_Abs,
         -Unmit_Max_ICU_Abs, -Supp_0.2_Max_ICU_Abs, -Supp_1.6_Max_ICU_Abs) %>%
  left_join(region, by = c("Country" = "Country")) %>%
  filter(!is.na(Region)) %>%
  select(Country, Region, everything()) %>%
  gather(metric, value, -Country, -Region) %>%
  group_by(Region, metric) %>%
  summarise(sum = sum(value)) %>%
  spread(metric, sum) %>%
  select(Region, 
         Unmit_Inc_Abs, Supp_0.2_Inc_Abs, Supp_1.6_Inc_Abs,
         Unmit_Death_Abs, Supp_0.2_Death_Abs, Supp_1.6_Death_Abs,
         Unmit_Total_Hosp_Abs, Supp_0.2_Total_Hosp_Abs, Supp_1.6_Total_Hosp_Abs,
         Unmit_Total_ICU_Abs, Supp_0.2_Total_ICU_Abs, Supp_1.6_Total_ICU_Abs)

colnames(region_numbers) <- c("Region", 
                              "Unmitigated Incidence", "Suppression 0.2 Incidence", "Suppression 1.6 Incidence",
                              "Unmitigated Deaths", "Suppression 0.2 Deaths", "Suppression 1.6 Deaths",
                              "Unmitigated Hospitalisations", "Suppression 0.2 Hospitalisations", "Suppression 1.6 Hospitalisations",
                              "Unmitigated Critical Care", "Suppression 0.2 Critical Care", "Suppression 1.6 Critical Care")

write.csv(region_numbers, file = "Outputs/All_Countries_Suppression/Region_Specific_Numbers.csv")

