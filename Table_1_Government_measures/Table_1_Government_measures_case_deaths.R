rm(list =ls())

library(devtools)
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(squire)
library(janitor)
library(triangle)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### import ACAPS inputs and format dates corrects
ACAPS_inputs <- read_xlsx("Data/ACAPS_suppression_measures_inputs.xlsx",sheet="Results")%>%
  mutate(Date_suppression= excel_numeric_to_date(as.numeric(Date_suppression)),
  Date_lastACAPSifnosuppression= excel_numeric_to_date(as.numeric(Date_lastACAPSifnosuppression)),
  WPP_Population=as.numeric(WPP_Population))

###download ECDC data
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                 na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE) %>%
  rename(ecdc_country = countriesAndTerritories) %>%
  mutate(dateRep=as.Date(dateRep, format = "%d/%m/%y"))%>%
  select(dateRep, ecdc_country, countryterritoryCode, cases, deaths)

suppressed_countries <- data %>%
  left_join(ACAPS_inputs, by = c("countryterritoryCode" = "Code"))

#### find cases and deaths at date of suppression
suppressed_countries <- data %>%
  left_join(ACAPS_inputs, by = c("countryterritoryCode" = "Code"))%>%
  filter(ACAPS_suppression_number==1)%>%
  mutate(Country_ECDC=ecdc_country,Code=countryterritoryCode,end_exp_growth=Date_suppression)%>%
  group_by(Country_ECDC,Code,Region,Income_group,WPP_Population,Date_suppression,Date_lastACAPSifnosuppression,end_exp_growth)%>%
  filter(dateRep <= end_exp_growth) %>%
  summarise(cases_at_suppression = sum(cases), deaths_at_suppression = sum(deaths),cases_at_end_exponential=sum(cases),deaths_at_end_exponential = sum(deaths))%>%
  ungroup() %>%
  select(Country_ECDC,Code,Region,Income_group,WPP_Population,Date_suppression,Date_lastACAPSifnosuppression,cases_at_suppression,deaths_at_suppression,end_exp_growth,cases_at_end_exponential,deaths_at_end_exponential)

### check for missing countries in "suppressed countries" of these all had reported no cases at date of suppression (apart from Hong Kong which is not disaggregated in ECDC data####
`%notin%` <- Negate(`%in%`)
zero_suppressors<-ACAPS_inputs$Code[ACAPS_inputs$ACAPS_suppression_number==1][ACAPS_inputs$Code[ACAPS_inputs$ACAPS_suppression_number==1]%notin%suppressed_countries$Code]
zero_suppressors<-zero_suppressors[-c(1)]
suppressed_at_zero<-ACAPS_inputs%>%
  filter(ACAPS_inputs$Code%in%zero_suppressors)%>%
  mutate(end_exp_growth=Date_suppression,cases_at_suppression=0,deaths_at_suppression=0,cases_at_end_exponential=0,deaths_at_end_exponential=0)%>%
  select(Country_ECDC,Code,Region,Income_group,WPP_Population,Date_suppression,Date_lastACAPSifnosuppression,cases_at_suppression,deaths_at_suppression,end_exp_growth,cases_at_end_exponential,deaths_at_end_exponential)

###combine all suppressed countries

suppressed_countries<-rbind(suppressed_countries,suppressed_at_zero)

##outputs for first 5 columns in table 1

##summarise across all countries (first row)
summary_suppression<-suppressed_countries%>%
  filter(!is.na(WPP_Population))%>%
  group_by(Code)%>%
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
  group_by(Code,Region)%>%
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
  group_by(Code,Income_group)%>%
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
  mutate(Country_ECDC=ecdc_country,Code=countryterritoryCode,end_exp_growth=Date_lastACAPSifnosuppression,cases_at_suppression=NA,deaths_at_suppression=NA)%>%
  group_by(Country_ECDC,Code,Region,Income_group,WPP_Population,Date_suppression,Date_lastACAPSifnosuppression,cases_at_suppression,deaths_at_suppression,end_exp_growth)%>%
  filter(dateRep <= end_exp_growth) %>%
  summarise(cases_at_end_exponential = sum(cases), deaths_at_end_exponential = sum(deaths))


### COUNTRIES WITH EARLY EPIDEMICS (CHINA/IRAN/JAPAN/SOUTH KOREA/SINGAPORE) AND SUBNATIONAL POLICIES (BRAZIL/USA) ASSUMED TO GROW EXPONENTIALLY Up TO 10 DEATHS
undetermined_countries<-data %>%
  left_join(ACAPS_inputs, by = c("countryterritoryCode" = "Code"))%>%
  filter(ACAPS_suppression_number==2)%>%
  mutate(Country_ECDC=ecdc_country,Code=countryterritoryCode,cases_at_suppression=NA,deaths_at_suppression=NA)%>%
  group_by(Country_ECDC,Code,Region,Income_group,WPP_Population,Date_suppression,Date_lastACAPSifnosuppression,cases_at_suppression,deaths_at_suppression)%>%
  arrange(Country_ECDC,dateRep)%>%
  mutate(deaths=cumsum(deaths),cases=cumsum(cases))%>%
  filter(deaths>=10)%>%
  summarise(end_exp_growth=min(dateRep),deaths_at_end_exponential=min(deaths),cases_at_end_exponential=min(cases))

all_no_suppress<-rbind(no_suppression_to_date_countries,ungroup(undetermined_countries))


all_with_ECDC<-rbind(suppressed_countries,all_no_suppress)



### GET ALL COUNTRIES WITH >=3 DEATHS DURING ASSUMED EXPONENTIAL PHASE
cases_per_death<-all_with_ECDC%>%
  filter(deaths_at_end_exponential>2)%>%
  group_by(Code)%>%
  summarise(cases_per_death=cases_at_end_exponential/deaths_at_end_exponential)



all_with_ECDC<-merge(all_with_ECDC,cases_per_death,all.x = TRUE)%>%
  select(Code,cases_at_suppression,deaths_at_suppression,
         end_exp_growth,cases_at_end_exponential,deaths_at_end_exponential,cases_per_death)




### MERGE EVERYTHING AND OUTPUT
ACAPS_with_ECDC<-merge(ACAPS_inputs,all_with_ECDC,by=c("Code"),all.x = TRUE)     

##summarise across all countries (first row)
summary_suppression<-ACAPS_with_ECDC%>%
  filter(!is.na(cases_per_death))%>%
   summarise("total_exp"=n(),
            "median_CPD"=median(cases_per_death),"min_CPD"=min(cases_per_death),"max_CPD"=max(cases_per_death)
             )


##summarise stratified by region (rows 2-8)
summary_suppression_by_region<-ACAPS_with_ECDC%>%
  filter(!is.na(cases_per_death))%>%
  group_by(Region)%>%
  summarise("total_exp"=n(),
            "median_CPD"=median(cases_per_death),"min_CPD"=min(cases_per_death),"max_CPD"=max(cases_per_death)
  )

##summarise stratified by income (rows 10-14)
summary_suppression_by_income<-ACAPS_with_ECDC%>%
  filter(!is.na(cases_per_death))%>%
  group_by(Income_group)%>%
  summarise("total_exp"=n(),
            "median_CPD"=median(cases_per_death),"min_CPD"=min(cases_per_death),"max_CPD"=max(cases_per_death)
  )

## Table 1 last two rows
#row one
summary_suppression$total_exp
paste(round(summary_suppression$median_CPD,1)," (",round(summary_suppression$min_CPD,1)," - ",round(summary_suppression$max_CPD,1),")",sep="")

#row 2-8
summary_suppression_by_region$total_exp
paste(round(summary_suppression_by_region$median_CPD,1)," (",round(summary_suppression_by_region$min_CPD,1)," - ",round(summary_suppression_by_region$max_CPD,1),")",sep="")
#row 10-14
summary_suppression_by_income$total_exp
paste(round(summary_suppression_by_income$median_CPD,1)," (",round(summary_suppression_by_income$min_CPD,1)," - ",round(summary_suppression_by_income$max_CPD,1),")",sep="")


write.csv(ACAPS_with_ECDC,"Outputs/Outputs_for_ACAPS_appendix.csv",row.names = F)

