
rm(list =ls())
library(dplyr)
library(RColorBrewer)
library(rdhs)
library(readxl)
library(rgdal)
library(digest)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### UK as examplar of HIC ####
library(tidyverse)
UK_hh <- read.csv("Data/UK_Household_Size_Stratified_By_Age.csv")
UK_hh <- UK_hh %>%
  mutate(num_hh = Total/Size) %>%
  mutate(old = ifelse(Over_65 > 0, "Old", "Young")) %>%
  select(-Over_65) %>%
  group_by(old) %>%
  summarise(avg = sum(num_hh * Size)/sum(num_hh))


### LOG INTO DHS ###
set_rdhs_config("patrick.walker@imperial.ac.uk","Modelling impact of covid-19 in LMICS",config_path = "~/.rdhs.json",password_prompt = TRUE)


### IDENTIFY ALL LATEST DHS DATASETS SINCE 2010
dat<-dhs_datasets(fileFormat = "FL",surveyYearStart = 2010,surveyType ="DHS")
countries=unique(dat$DHS_CountryCode)
latest_dat<-list()
for(i in seq_along(countries)){
  latest_dat[[i]]=dat[which(dat$FileType%in%unique((dat$FileType))[c(3,4,6,7)]&dat$DHS_CountryCode==countries[i]&dat$SurveyYear==max(dat$SurveyYear[which(dat$DHS_CountryCode==countries[i])])),]
}

### IDENTIFY ALL HOUSEHOLD MEMBER DATASETS
HMR_datasets<-latest_dat[[1]][which(latest_dat[[1]]$FileType=="Household Member Recode"),]
for(i in 2:length(countries)){
    HMR_datasets<-rbind(HMR_datasets,latest_dat[[i]][which(latest_dat[[i]]$FileType=="Household Member Recode"),])
  
}

### COUNTRIES WITH DATA 
countries<-c(HMR_datasets$CountryName)
averages_65s<-array(dim=length(countries))
### EXTRACT RELEVANT DATA
hmr_qu <- search_variables(HMR_datasets$FileName,c("HV102","HV103","HV104","HV105","HV010","HV121","HV122","HV205","HV009","HV025","HV005"))
hm_ex <- extract_dhs(hmr_qu)

for(i in seq_along(hm_ex)){
hmr_surv<-hm_ex[[i]]
hmr_surv$over65=0
hmr_surv$over65[hmr_surv$hv105>=65&hmr_surv$hv105<98]=1
averages_65s[i]=sum(hmr_surv$hv005[hmr_surv$over65==1]*hmr_surv$hv009[hmr_surv$over65==1])/sum(hmr_surv$hv005[hmr_surv$over65==1])
}

## GET WB GDP AND WPP DEMOGRAPHY FOR DHS COUNTRIES
GDP<-read_xlsx(".Data/GDP_by_year.xlsx",sheet=2)
demog<-read_xlsx(".Data/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES_DHS_countries.xlsx",sheet="DHS_countries")

### UK GDP FROM "GDP_by_year" worksheet from GDP_by_year.xlsx
UKGDP<-45973.5735
### UK TOTAL AND OVER65 POPN   FROM "Estimates" worksheet from WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES_DHS_countries.xlsx
UKpop<- 67886.004 
UK_o65<-12663.012 
### TOTAL AND OVER65 POPULATION OF DHS COUNTRIES
tot_pop_sizes=rowSums(demog[,3:23])
over65_pop_sizes<-rowSums(demog[,16:23])
prop<-over65_pop_sizes/tot_pop_sizes

### COLOURS AND SCALER OF CIRCLE SIZE
col.sch=brewer.pal(4, "YlOrRd")
scaler=1.5

### MAKE PLOT
tiff("HHvsGDP.tif",height=7,width=7,res=300,units="in")
par(mar=c(5,5,1,1))
plot(GDP$`2016`,averages_65s,ylim=c(1,14),xlim=c(0,50000),cex=log((over65_pop_sizes))/scaler,pch=16,ylab="Average HH size of a person aged 65+",xlab="Per-capita GDP",col="white",cex.lab=1.5,las=1,cex.axis=1.5)
points(GDP$`2016`[prop>0.03&prop<0.05],averages_65s[prop>0.03&prop<0.05],cex=log((over65_pop_sizes)[prop>0.03&prop<0.05])/scaler,bg=col.sch[2],pch=21)
points(GDP$`2016`[prop>0.05&prop<0.1],averages_65s[prop>0.05&prop<0.1],cex=log((over65_pop_sizes[prop>0.05&prop<0.1]))/scaler,bg=col.sch[3],pch=21)
points(GDP$`2016`[prop<0.03],averages_65s[prop<0.03],cex=log((over65_pop_sizes)[prop<0.03])/scaler,bg=col.sch[1],pch=21)
points(GDP$`2016`[prop>0.1],averages_65s[prop>0.1],cex=log((over65_pop_sizes)[prop>0.1])/scaler,bg=col.sch[4],pch=21)
points(UKGDP,UK_hh$avg[1],cex=(log(UK_o65))/scaler,bg=col.sch[4],pch=21)
text(UKGDP,UK_hh$avg[1],"UK")
points(25000,13,cex=log(100)/1.5,bg="grey",pch=21)
points(25000,12,cex=log(1000)/1.5,bg="grey",pch=21)
points(25000,10.65,cex=log(10000)/1.5,bg="grey",pch=21)
points(25000,8.95,cex=log(100000)/1.5,bg="grey",pch=21)
text(32000,13,"100,000",cex=1.5)
text(32000,12,"1 million",cex=1.5)
text(32500,10.65,"10 million",cex=1.5)
text(33500,8.95,"100 million",cex=1.5)
legend(38000,14,c("<3%","3-5%","5-10%","10+"),fill=col.sch,bty="n",cex=1.5)
text(27000,14,"Size of 65+ population",cex=1.2)
text(45000,14,"% population 65+ ",cex=1.2)
dev.off()



