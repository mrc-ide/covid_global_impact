rm(list =ls())

library(devtools)
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(squire)
library(janitor)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### UNCERTAINTY IN SEVERITY
severity_param_sets<-readRDS("severity_param_sets.rds")
### import ACAPS inputs and format dates corrects
ACAPS_inputs <- read_xlsx("ACAPS_suppression_measures_inputs.xlsx",sheet="Results")%>%
  mutate(Date_suppression= excel_numeric_to_date(as.numeric(Date_suppression)),
  Date_lastACAPSifnosuppression= excel_numeric_to_date(as.numeric(Date_lastACAPSifnosuppression)),
  WPP_Population=as.numeric(WPP_Population))

###download ECDC data
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                 na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE) %>%
  rename(ecdc_country = countriesAndTerritories) %>%
  mutate(dateRep=as.Date(dateRep, format = "%d/%m/%y"))%>%
  select(dateRep, ecdc_country, countryterritoryCode, cases, deaths)

#### find cases and deaths at date of suppression
suppressed_countries <- data %>%
  left_join(ACAPS_inputs, by = c("countryterritoryCode" = "Code"))%>%
  filter(ACAPS_suppression_number==1)%>%
  mutate(country=Country_ECDC,end_exp_growth=Date_suppression)%>%
  group_by(country,countryterritoryCode,Region,Income_group,WPP_Population,ACAPS_suppression_number,ACAPS_reference,
           Description,source,Date_suppression,Date_lastACAPSifnosuppression,end_exp_growth)%>%
 filter(dateRep <= end_exp_growth) %>%
  summarise(cases_at_suppression = sum(cases), deaths_at_suppression = sum(deaths),cases_at_end_exponential=sum(cases),deaths_at_end_exponential = sum(deaths))


### check for missing countries in "suppressed countries" of these all had reported no cases at date of suppression (apart from Hong Kong which is not disaggregated in ECDC data####
`%notin%` <- Negate(`%in%`)
zero_suppressors<-ACAPS_inputs$Code[ACAPS_inputs$ACAPS_suppression_number==1][ACAPS_inputs$Code[ACAPS_inputs$ACAPS_suppression_number==1]%notin%suppressed_countries$countryterritoryCode]
zero_suppressors<-zero_suppressors[-c(1)]
suppressed_at_zero<-ACAPS_inputs%>%
  filter(ACAPS_inputs$Code%in%zero_suppressors)%>%
  mutate(country=Country_ECDC,end_exp_growth=Date_suppression,cases_at_suppression=0,deaths_at_suppression=0,cases_at_end_exponential=0,deaths_at_end_exponential=0)%>%
  group_by(country,Code,Region,Income_group,WPP_Population,ACAPS_suppression_number,ACAPS_reference,
         Description,source,Date_suppression,Date_lastACAPSifnosuppression,end_exp_growth,cases_at_suppression,deaths_at_suppression,cases_at_end_exponential,deaths_at_end_exponential)

###combine all suppressed countries

suppressed_countries<-rbind(suppressed_countries,suppressed_at_zero)
  
##outputs for first 5 columns in table 1

##summarise across all countries (first row)
summary_suppression<-suppressed_countries%>%
  filter(!is.na(WPP_Population))%>%
  group_by(country)%>%
  summarise(Date_suppression=Date_suppression,"cases_per_m"=cases_at_suppression/WPP_Population*1000,"deaths_per_m"=deaths_at_suppression/WPP_Population*1000)%>%
  summarise("total_lock"=n(),
            "median_date"=median(Date_suppression),"min_date"=min(Date_suppression),"max_date"=max(Date_suppression),
            "median_case"=median(cases_per_m),"low_case"=min(cases_per_m),
            "high_case"=max(cases_per_m),
            "median_death"=median(deaths_per_m),"low_death"=min(deaths_per_m),
            "high_death"=max(deaths_per_m),
  )
##summarise stratified by region (rows 2-8)
summary_suppression_by_region<-suppressed_countries%>%
  filter(!is.na(WPP_Population))%>%
  group_by(country,Region)%>%
  summarise(Date_suppression=Date_suppression,"cases_per_m"=cases_at_suppression/WPP_Population*1000,"deaths_per_m"=deaths_at_suppression/WPP_Population*1000)%>%
  group_by(Region)%>%
  summarise("total_lock"=n(),
            "median_date"=median(Date_suppression),"min_date"=min(Date_suppression),"max_date"=max(Date_suppression),
            "median_case"=median(cases_per_m),"low_case"=min(cases_per_m),
            "high_case"=max(cases_per_m),
            "median_death"=median(deaths_per_m),"low_death"=min(deaths_per_m),
            "high_death"=max(deaths_per_m),
  )

  ##summarise stratified by income strata (rows 9-13)
summary_suppression_by_income<-suppressed_countries%>%
  filter(!is.na(WPP_Population))%>%
  group_by(country,Income_group)%>%
  summarise(Date_suppression=Date_suppression,"cases_per_m"=cases_at_suppression/WPP_Population*1000,"deaths_per_m"=deaths_at_suppression/WPP_Population*1000)%>%
  group_by(Income_group)%>%
  summarise("total_lock"=n(),
            "median_date"=median(Date_suppression),"min_date"=min(Date_suppression),"max_date"=max(Date_suppression),
            "median_case"=median(cases_per_m),"low_case"=min(cases_per_m),
            "high_case"=max(cases_per_m),
            "median_death"=median(deaths_per_m),"low_death"=min(deaths_per_m),
            "high_death"=max(deaths_per_m),
  )

### FIND COUNTRIES YET TO IMPLEMENT SUPRESSION AND CASES AND DEATHS AT DATE OF LAST ACAPS ENTRY
no_suppression_to_date_countries<- data %>%
  left_join(ACAPS_inputs, by = c("countryterritoryCode" = "Code"))%>%
  filter(ACAPS_suppression_number==3)%>%
  mutate(country=Country_ECDC,end_exp_growth= Date_lastACAPSifnosuppression) %>%
  group_by(country,countryterritoryCode,Region,Income_group,WPP_Population,ACAPS_suppression_number,ACAPS_reference,
           Description,source,Date_suppression,Date_lastACAPSifnosuppression,end_exp_growth)%>%
  filter(dateRep <= end_exp_growth) %>%
  summarise(cases_at_end_exponential = sum(cases), deaths_at_end_exponential = sum(deaths))

### COUNTRIES WITH EARLY EPIDEMICS (CHINA/IRAN/JAPAN/SOUTH KOREA/SINGAPORE) AND SUBNATIONAL POLICIES (BRAZIL/USA) ASSUMED TO GROW EXPONENTIALLY Up TO 10 DEATHS
undetermined_countries<-data %>%
  left_join(ACAPS_inputs, by = c("countryterritoryCode" = "Code"))%>%
  filter(ACAPS_suppression_number==2)%>%
  mutate(country=Country_ECDC)%>%
  group_by(country,countryterritoryCode,Region,Income_group,WPP_Population,ACAPS_suppression_number,ACAPS_reference,
           Description,source,Date_suppression,Date_lastACAPSifnosuppression)%>%
  arrange(ecdc_country,dateRep)%>%
  mutate(deaths=cumsum(deaths),cases=cumsum(cases))%>%
  filter(deaths>=10)%>%
  summarise(end_exp_growth=min(dateRep),deaths_at_end_exponential=min(deaths),cases_at_end_exponential=min(cases))

to_merge_at_suppresion<-suppressed_countries%>%
  ungroup()%>%
  mutate("Code"=countryterritoryCode)%>%
  select(Code,cases_at_suppression,deaths_at_suppression)


### GET ALL COUNTRIES WITH >=3 DEATHS DURING ASSUMED EXPONENTIAL PHASE
merged_for_sim<-tbl_df(rbind(suppressed_countries,no_suppression_to_date_countries,undetermined_countries))%>%
  filter(deaths_at_end_exponential>2&!is.na(WPP_Population))

### CALCULATE INFECTION DURING ASSUMED EXPONENTIAL PHASE
Rs=c(rep(c(2.4,3,3.5),each=100))
infections_output<-array(dim=c(length(merged_for_sim$country),300))
for(i in 1:length(merged_for_sim$country)){
  print(merged_for_sim$country[i])
  for(j in 1:300){
    print(j)
    cali<-align(country=merged_for_sim$country[i],deaths = merged_for_sim$deaths_at_end_exponential[i], reporting_fraction = 1,
                    seeding_age_groups = c("35-40", "40-45", "45-50", "50-55"),
                    R0=Rs[j],
                    prob_hosp=severity_param_sets[[j]]$prob_hosp,
                    prob_severe=severity_param_sets[[j]]$prob_severe,
                    min_seeding_cases = 5, max_seeding_cases = 50,
                    replicates = 2, dt = 0.1)
    
    
    x<-format_output(cali, var_select = "infections")
    x <- x %>%
      mutate(replicate = factor(replicate)) %>%
      group_by(replicate)%>%
      filter(t < 0,replicate==1)%>%
      summarise(infections=sum(y))
    infections_output[i,j]<-x$infections[1]
  }
}

### SUMMARISE SIMULATIONS AND INFECTION ASCERTAINMENT

merged_for_sim$Infections_median<-apply(infections_output, 1, FUN=median, na.rm=TRUE)
merged_for_sim$Infections_low<-apply(infections_output, 1, FUN=quantile, na.rm=TRUE,prob=0.025)
merged_for_sim$Infections_high<-apply(infections_output, 1, FUN=quantile, na.rm=TRUE,prob=0.975)


merged_for_sim$Ascertainment_fraction_median<-NA
merged_for_sim$Ascertainment_fraction_low<-NA
merged_for_sim$Ascertainment_fraction_high<-NA


merged_for_sim$Ascertainment_fraction_median<-merged_for_sim$cases_at_end_exponential/merged_for_sim$Infections_median
merged_for_sim$Ascertainment_fraction_high<-merged_for_sim$cases_at_end_exponential/merged_for_sim$Infections_low
merged_for_sim$Ascertainment_fraction_low<-merged_for_sim$cases_at_end_exponential/merged_for_sim$Infections_high


### INPUTS FOR FINAL TWO COLUMNS OF TABLE 1 ####
summary_inf_frac_income<-merged_for_sim%>%
  group_by(Income_group)%>%
  summarise("CAR"=median(Ascertainment_fraction_median),"CAR_max"=max(Ascertainment_fraction_median),"CAR_min"=min(Ascertainment_fraction_median),"count"=n())

summary_inf_frac_region<-merged_for_sim%>%
  group_by(Region)%>%
  summarise("CAR"=median(Ascertainment_fraction_median),"CAR_max"=max(Ascertainment_fraction_median),"CAR_min"=min(Ascertainment_fraction_median),"count"=n())


summary_inf_frac<-merged_for_sim%>%
  summarise("CAR"=median(Ascertainment_fraction_median),"CAR_max"=max(Ascertainment_fraction_median),"CAR_min"=min(Ascertainment_fraction_median),"count"=n())


paste(round(summary_inf_frac_income$CAR*100,2),"% (",round(summary_inf_frac_income$CAR_min*100,2),"%-",round(summary_inf_frac_income$CAR_max*100,2),"%)",sep="")
paste(round(summary_inf_frac_region$CAR*100,2),"% (",round(summary_inf_frac_region$CAR_min*100,2),"%-",round(summary_inf_frac_region$CAR_max*100,2),"%)",sep="")
paste(round(summary_inf_frac$CAR*100,2),"% (",round(summary_inf_frac$CAR_min*100,2),"%-",round(summary_inf_frac$CAR_max*100,2),"%)",sep="")

### GET COUNTRY LEVEL UNCERTAINTY SUMMARIES
Ascertainment_estimates<-merged_for_sim%>%
  mutate("Code"=countryterritoryCode)%>%
  select(Code,end_exp_growth,cases_at_end_exponential,deaths_at_end_exponential,Ascertainment_fraction_median,Ascertainment_fraction_low,Ascertainment_fraction_high)

### MERGE EVERYTHING AND OUTPUT
ACAPS_with_ECDC<-merge(ACAPS_inputs,to_merge_at_suppresion,by=c("Code"),all.x = TRUE)     
all_merged=merge(ACAPS_with_ECDC,Ascertainment_estimates,by=c("Code"),all.x = TRUE)
write.csv(all_merged,"Outputs_for_ACAPS_appendix.csv",row.names = F)

