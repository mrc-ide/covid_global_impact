rm(list=ls())

library(tidyverse); library(patchwork); library(scales); library(ggforce); library(readxl)
library(ggplot2)
library(ggpubr);library(gridExtra);
library(RColorBrewer)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
raw_df <- read.csv("Global_mitigation_strategies.csv")

world_bank_data_dictionary <- read.csv("Data/World_Bank_Country_Metadata.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
world_bank_data_dictionary <- world_bank_data_dictionary %>%
  select(country_code, region, income_group, TableName,GDP)

raw_df <- raw_df %>%
  left_join(world_bank_data_dictionary, by = c("country" = "TableName"))


raw_df$income_group[raw_df$income_group=="Low income"]="LIC"
raw_df$income_group[raw_df$income_group=="Lower middle income"]="LMIC"
raw_df$income_group[raw_df$income_group=="Upper middle income"]="UMIC"
raw_df$income_group[raw_df$income_group=="High income"]="HIC"

raw_df$income_group<-factor(raw_df$income_group,
                            levels = c("LIC","LMIC","UMIC","HIC"))

############### FIGURE 1 PLOT ####################################
infected_income_df <- raw_df %>%
  filter(R0==3.0,Strategy =="Unmitigated"&!is.na(income_group))%>%
  group_by(Strategy,income_group)%>%
  summarise_at(c(names(raw_df)[c(grep("infected_",names(raw_df)))],names(raw_df)[c(grep("total_pop",names(raw_df)))]),funs(sum))
leg_labs<-c(paste(seq(0,75,by=5),"-",seq(4,79,by=5),sep=""),"80+")
names(infected_income_df)[3:(length(names(infected_income_df))-1)]<-leg_labs

infect_income_gather<-gather(infected_income_df,"Age","sum",-c(Strategy,income_group,total_pop))
infect_income_gather$Age<-factor(infect_income_gather$Age,levels = rev(unique(infect_income_gather$Age)))



deaths_income_df <- raw_df %>%
  filter(R0==3.0,Strategy =="Unmitigated"&!is.na(income_group))%>%
  group_by(Strategy,income_group)%>%
  summarise_at(c(names(raw_df)[c(grep("deaths_",names(raw_df)))],names(raw_df)[c(grep("total_pop",names(raw_df)))]),funs(sum))
deaths_income_gather<-gather(deaths_income_df,"type","sum",-c(Strategy,income_group,total_pop))
deaths_income_gather$type<-factor(deaths_income_gather$type,levels = rev(unique(deaths_income_gather$type)))


hospital_income_df <- raw_df %>%
  filter(R0==3.0,Strategy =="Unmitigated"&!is.na(income_group))%>%
  group_by(Strategy,income_group)%>%
  summarise_at(c(names(raw_df)[c(grep("hospitalisations_",names(raw_df)))],names(raw_df)[c(grep("total_pop",names(raw_df)))]),funs(sum))
hospital_income_gather<-gather(hospital_income_df,"type","sum",-c(Strategy,income_group,total_pop))
hospital_income_gather$type<-factor(hospital_income_gather$type,levels = rev(unique(hospital_income_gather$type)))
                                      


critical_income_df <- raw_df %>%
  filter(R0==3.0,Strategy =="Unmitigated"&!is.na(income_group))%>%
  group_by(Strategy,income_group)%>%
  summarise_at(c(names(raw_df)[c(grep("critical_",names(raw_df)))],names(raw_df)[c(grep("total_pop",names(raw_df)))]),funs(sum))
critical_income_gather<-gather(critical_income_df,"type","sum",-c(Strategy,income_group,total_pop))
critical_income_gather$type<-factor(critical_income_gather$type,levels = rev(unique(critical_income_gather$type)))


theme_set(theme_pubr(legend="right"))
ax_size=17                 
infected_plot<-ggplot(infect_income_gather, aes(x = income_group, y = sum/total_pop*1000, fill = Age))+
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(direction = -1,option="plasma")+
  #facet_grid(~Strategy)+
  #position = position_dodge(0.9) +
   #theme(legend.position="none")+
  theme(axis.text = element_text(size = 14),axis.title=element_text(size=ax_size),legend.title=element_text(size=16),legend.text=element_text(size=14),legend.key.size = unit(0.6, "cm"))+
  labs(x = "Income group", y = "Infections per 1000 population") 
legend<-get_legend(infected_plot)
infected_plot<- infected_plot + theme(legend.position="none")

hospital_plot<-ggplot(hospital_income_gather, aes(x = income_group, y = sum/total_pop*1000, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(direction = -1,option="plasma")+
  #facet_grid(~Strategy)+
  #position = position_dodge(0.9) +
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 14),axis.title=element_text(size=ax_size))+
  labs(x = "Income group", y = "Requiring hospital per 1000 population") 
  
  

critical_plot<-ggplot(critical_income_gather, aes(x = income_group, y = sum/total_pop*1000, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(direction = -1,option="plasma") +
  #facet_grid(~Strategy)+
  #position = position_dodge(0.9) +
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 14),axis.title=element_text(size=ax_size))+
  labs(x = "Income group", y = "Requiring critical care per 1000 population") 


  
death_plot<-ggplot(deaths_income_gather, aes(x = income_group, y = sum/total_pop*1000, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(direction = -1,option="plasma") +
  #facet_grid(~Strategy)+
  #position = position_dodge(0.9) +
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 14),axis.title=element_text(size=ax_size))+
  labs(x = "Income group", y = "Deaths per 1000 population") 


tiff("Figure1FtoI_unmitigated_patterns.tif",height=5,width=50/3,unit="in",res=300)
grid.arrange(infected_plot,hospital_plot,critical_plot, death_plot,legend,ncol=5,nrow=1,
            # layout_matrix = cbind(c(1,2), c(3,4),c(5,5)),
             widths = c(2.7, 2.7,2.7, 2.7,1)
             )
dev.off()
########################### END OF FIGURE 1 PLOT ################################


#### TABLE S7 OUTPUTS   #########################
### SOCIAL CONTACTS COLUMNS ###
reductions_summary<-raw_df%>%
  filter(!is.na(income_group))%>%
  group_by(R0,Strategy,income_group)%>%
  summarise(median_red=median(Reductions),high_red=max(Reductions),low_red=min(Reductions))
paste(round(reductions_summary$median_red*100,1),"% (",round(reductions_summary$low_red*100,1),"%-",round(reductions_summary$high_red*100,1),"%)",sep="")
summary(raw_df$Reductions[raw_df$Strategy==unique(raw_df$Strategy)[2]&raw_df$R0==2.3])
### REDUCTIONS IN INFECTIONS AND DEATHS WITH MITIGATION ###
miti_reduction_df<-data.frame(Country=raw_df$country[raw_df$Strategy==unique(raw_df$Strategy)[2]],income=raw_df$income_group,R0=raw_df$R0[raw_df$Strategy==unique(raw_df$Strategy)[2]],
                              infection_reduction=1-raw_df$total_infected[raw_df$Strategy==unique(raw_df$Strategy)[2]]/raw_df$total_infected[raw_df$Strategy=="Unmitigated"],
                              death_reduction=1-raw_df$total_deaths[raw_df$Strategy==unique(raw_df$Strategy)[2]]/raw_df$total_deaths[raw_df$Strategy=="Unmitigated"]
)
miti_reductions_summary<-miti_reduction_df%>%
  filter(!is.na(income))%>%
  group_by(R0,income)%>%
  summarise(median_red_inf=median(infection_reduction),high_red_inf=max(infection_reduction),low_red_inf=min(infection_reduction),
            median_red_death=median(death_reduction),high_red_death=max(death_reduction),low_red_death=min(death_reduction))

paste(round(miti_reductions_summary$median_red_inf*100,1),"% (",round(miti_reductions_summary$low_red_inf*100,1),"%-",round(miti_reductions_summary$high_red_inf*100,1),"%)",sep="")
paste(round(miti_reductions_summary$median_red_death*100,1),"% (",round(miti_reductions_summary$low_red_death*100,1),"%-",round(miti_reductions_summary$high_red_death*100,1),"%)",sep="")

### REDUCTIONS IN INFECTIONS AND DEATHS WITH MITIGATION + PROTECTING ELDERLY###
miti_eld_reduction_df<-data.frame(Country=raw_df$country[raw_df$Strategy==unique(raw_df$Strategy)[3]],income=raw_df$income_group,R0=raw_df$R0[raw_df$Strategy==unique(raw_df$Strategy)[3]],
                              infection_reduction=1-raw_df$total_infected[raw_df$Strategy==unique(raw_df$Strategy)[3]]/raw_df$total_infected[raw_df$Strategy=="Unmitigated"],
                              death_reduction=1-raw_df$total_deaths[raw_df$Strategy==unique(raw_df$Strategy)[3]]/raw_df$total_deaths[raw_df$Strategy=="Unmitigated"]
)
miti_eld_reductions_summary<-miti_eld_reduction_df%>%
  filter(!is.na(income))%>%
  group_by(R0,income)%>%
  summarise(median_red_inf=median(infection_reduction),high_red_inf=max(infection_reduction),low_red_inf=min(infection_reduction),
            median_red_death=median(death_reduction),high_red_death=max(death_reduction),low_red_death=min(death_reduction))

paste(round(miti_eld_reductions_summary$median_red_inf*100,1),"% (",round(miti_eld_reductions_summary$low_red_inf*100,1),"%-",round(miti_eld_reductions_summary$high_red_inf*100,1),"%)",sep="")
paste(round(miti_eld_reductions_summary$median_red_death*100,1),"% (",round(miti_eld_reductions_summary$low_red_death*100,1),"%-",round(miti_eld_reductions_summary$high_red_death*100,1),"%)",sep="")


##### END OF TABLE OUTPUTS #####