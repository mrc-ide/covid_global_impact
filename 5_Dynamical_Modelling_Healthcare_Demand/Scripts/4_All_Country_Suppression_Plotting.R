
country_suppression_output <- readRDS(file = paste0("Outputs/Raw_All_Countries_Supp_Output/All_Countries_Suppression_Output_R0_", R0_req, ".Rds"))

abs_numbers_0.2 <- country_suppression_output %>%
  select(Country, Supp_0.2_Inc_Abs, Supp_0.2_Death_Abs, Supp_0.2_Total_Hosp_Abs, Supp_0.2_Max_Hosp_Abs, Supp_0.2_Total_ICU_Abs, Supp_0.2_Max_ICU_Abs) %>%
  mutate(Supp_0.2_Inc_Abs = round(Supp_0.2_Inc_Abs),
         Supp_0.2_Death_Abs = round(Supp_0.2_Death_Abs),
         Supp_0.2_Total_Hosp_Abs = round(Supp_0.2_Total_Hosp_Abs),
         Supp_0.2_Max_Hosp_Abs = round(Supp_0.2_Max_Hosp_Abs),
         Supp_0.2_Total_ICU_Abs = round(Supp_0.2_Total_ICU_Abs),
         Supp_0.2_Max_ICU_Abs = round(Supp_0.2_Max_ICU_Abs))
write.csv(abs_numbers_0.2, file = "Outputs/All_Countries_Suppression/Absolute_Numbers_02.csv")

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

