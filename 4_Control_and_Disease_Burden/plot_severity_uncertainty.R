
library(devtools)
install.packages("viridis")
install.packages("Rcolorbrewer")

devtools::install_github("mrc-ide/squire",ref="non_odin_triggering")
library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse);
library(ggplot2);
library(squire)
library(ggpubr)
library(ggthemes)
library(gridExtra)
rm(list=ls())
setwd("C:/Users/Patrick/Imperial College London/ncov - Documents/2019-nCoV/LMIC/LMIC Parameters")
severity_param_sets <- readRDS("severity_param_sets.rds")
severity_param_sets_lmic <- readRDS("severity_param_sets_lmic.rds")
load("income_strata_healthcare_capacity.rda")

ndraws=500
parameter_list<-severity_param_sets[1:ndraws]
parameter_list_lmic<-severity_param_sets_lmic[1:ndraws]

age_cats<-c(paste(seq(0,75,by=5),"-",seq(5,80,by=5),sep=""),"80+")

french




uncertainty_df<-data.frame("age"=character(),"type"=character(),"draw"=numeric(),
                          "value"<-numeric()
                           )

for(i in 1:500){
  IFR_draw_df<-data.frame("age"=age_cats,"type"="Fatality risk (good access to care)","draw"=i,"value"=severity_param_sets[[i]]$Adj_IFR)
  PIH_draw_df<-data.frame("age"=age_cats,"type"="Requiring hospitalisation","draw"=i,"value"=severity_param_sets[[i]]$prob_hosp)
  PIC_draw_df<-data.frame("age"=age_cats,"type"="Requiring critical care","draw"=i,"value"=severity_param_sets[[i]]$prob_severe*severity_param_sets[[i]]$prob_hosp)
  uncertainty_df<-rbind(uncertainty_df,IFR_draw_df,PIH_draw_df,PIC_draw_df)
}
uncertainty_df$age<-factor(uncertainty_df$age,levels=age_cats)

verity_ages=c(seq(2.5,77.5,by=5),85)
french<-tbl_df(read.csv("French PIH Estimates.csv",fileEncoding="UTF-8-BOM"))
LID<-read.table("lancet_prob_sev_prep.txt",header=T)
LID
french_ages=c(10,seq(25,85,by=10))

plot_compare<-data.frame("age"=numeric(),"estimate"=character(),"mid"=numeric(),"low"=numeric(),"high"=numeric())

LID_est<-data.frame("age"=c(LID$age[1:7],80),
                  "estimate"="Bi et al.",
                  "mid"=LID$mid,
                  "low"=LID$low,
                  "high"=LID$high
)

verity_est<-data.frame("age"=verity_ages,
                       "estimate"="Verity et al.",
                       "mid"=sapply(1:17,function(z){quantile(uncertainty_df$value[uncertainty_df$age==unique(uncertainty_df$age)[z]&uncertainty_df$type =="Requiring hospitalisation"],c(0.5))})*100,
                       "low"=sapply(1:17,function(z){quantile(uncertainty_df$value[uncertainty_df$age==unique(uncertainty_df$age)[z]&uncertainty_df$type =="Requiring hospitalisation"],c(0.0255))}*100),
                       "high"=sapply(1:17,function(z){quantile(uncertainty_df$value[uncertainty_df$age==unique(uncertainty_df$age)[z]&uncertainty_df$type =="Requiring hospitalisation"],c(0.975))}*100)
)
french_est<-data.frame("age"=french_ages,
                       "estimate"="Salje et al.",
                       "mid"=french$mean*100,
                       "low"=french$lower*100,
                       "high"=french$upper*100
)


plot_compare<-rbind(verity_est,french_est,LID_est)


windows(height=10,width=20)
  
  tiff("compare_uncertainty.tif",res=200,height=5,width=10,unit="in")
  gA <- ggplotGrob(uncert_plot)
  gB <- ggplotGrob(compare_plot)
  grid::grid.newpage()
  grid::grid.draw(cbind(gA, gB))
  dev.off()


french_add<-data.frame("Ages"=age_cats,"mean"=NA,"upper"=NA,"lower"=NA)

compare_plot<-ggplot(plot_compare, aes(x=age, y=mid, group=estimate, color=estimate)) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=.1) +
  theme_pubr() +
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle=65, vjust=0.65),legend.title = element_blank(),
        legend.position = c(.3, .85),legend.text = element_text(size = 15))+
  labs(x = "Age", y = "Proportion of infections hospitalised") 

compare_plot

french_add[,]

gA <- ggplotGrob(uncert_plot)
gB <- ggplotGrob(compare_plot)
grid::grid.newpage()
grid::grid.draw(cbind(gA, gB))


french


french_plot<-ggplot(french, aes(x=Age, y=mean)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  theme_pubr() +
  geom_point()+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
labs(x = "Age category", y = "Proportion of infections hospitalised") 

uncert_plot <- ggplot(uncertainty_df, aes(age, value))+
geom_boxplot(aes(fill=factor(type))) +
  theme_pubr() +
  theme(legend.title = element_blank(),
  axis.text.x = element_text(angle=65, vjust=0.6),
  legend.position = c(.45, .85),legend.text = element_text(size = 15))+
labs(x = "Age category", y = "Proportion of infections") 
  

uncertainty_df
