rm(list=ls())

library(dplyr)
library(triangle)
library(ggplot2);
library(squire)
library(ggpubr)
library(ggthemes)
library(gridExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
### MCMC chains from Verity et al ####
IFR_chains<-tbl_df(read.csv("Data/IFR_age_chains.csv"))


### JOINT POSTERIOR MEDIANS FROM VERITY ET AL  #####
baseline<-data.frame("ages"=seq(5,85,by=10),
                     "IFR"=c(1.61324E-05,
                             6.95067E-05,
                             0.000308933,
                             0.000843828,
                             0.001607901,
                             0.00595244,
                             0.019261444,
                             0.042835686,
                             0.077998135
                     ),
                     "PIH"=c(0,
                             0.000407553,
                             0.010439694,
                             0.034330332,
                             0.042521914,
                             0.081608636,
                             0.117750227,
                             0.165820086,
                             0.184054661
                     )
)


### (do we need to actually simulate this here - also need to check..?)
##VERITY ET AL ESTIMATES WHICH ASSUMED CONSTANT ATTACK RATES WITH AGE 
##SO HERE WE UPDATE TO ASSUME ATTACK RATES ARE PROPORTIONAL TO UNMITIGATED EPIDEMIC FROM AGE-DEPENDENT SIR MODEL R0=3 WITH CHINESE DEMOGRAPHY AND MATRIX
China_AR<-c(0.485046824,0.820064569,0.921758997,0.943028327,0.890452815,0.863618104,0.904659973,0.896650206,
            0.889337465,0.870199419,0.806136728,0.829015942,0.709811137,0.736056837,0.699378916,0.686382172,0.686382172)


#### Baseline proportion of 80+ infected hospitalised 
prop_80p_hosp<-0.18

### Baseline proportion of 80+ infected requiring critical care
prop_80p_crit<-0.1277


### FUNCTIONS  ##############

extend_adjust_baseline<-function(draw_df,std_AR,prop_80p_H,prop_80p_C){
  
  ###FLATTEN UNDER 20 HOSPITALISATION RATES WHERE POSTERIOR UNINFORMED DUE TO ABSENCE OF DATA AND CAN LEAD TO MORE DEATHS THEN HOSPITALISATIONS
  prop_h_die<-draw_df$IFR/draw_df$PIH
  prop_h_die[1:4]<-prop_h_die[5]
  draw_df$PIH_adj<-draw_df$IFR/prop_h_die
  
  ## EXTEND IFR AND PIH TO 5 YEAR CATS USING LOG TRANSFORM
  more_ages<-data.frame("ages"=c(2.5,5,7.5,12.5,15,17.5,22.5,25, 27.5,32.5,35,37.5,42.5,45,47.5,52.5,55,
                                 57.5,62.5,65,67.5,72.5,75,77.5, 85),
                        "IFR"=0,"PIH"=0)
  
  more_ages$IFR[which(more_ages$ages%in%draw_df$ages)]<-draw_df$IFR
  more_ages$IFR[1]=more_ages$IFR[2]
  more_ages$IFR[seq(3,21,by=3)]<-10^(((more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(3,21,by=3)])*log10(more_ages$IFR[seq(2,20,by=3)])+
                                        (more_ages$ages[seq(3,21,by=3)]-more_ages$ages[seq(2,20,by=3)])*log10(more_ages$IFR[seq(5,23,by=3)])
  )/(more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(2,20,by=3)]))
  more_ages$IFR[seq(4,22,by=3)]<-10^(((more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(4,22,by=3)])*log10(more_ages$IFR[seq(2,20,by=3)])+
                                        (more_ages$ages[seq(4,22,by=3)]-more_ages$ages[seq(2,20,by=3)])*log10(more_ages$IFR[seq(5,23,by=3)])
  )/(more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(2,20,by=3)]))
  
  more_ages$IFR[24]<-10^(((more_ages$ages[25]-more_ages$ages[24])*log10(more_ages$IFR[23])+
                            (more_ages$ages[24]-more_ages$ages[23])*log10(more_ages$IFR[25])
  )/(more_ages$ages[25]-more_ages$ages[23]))
  
  more_ages$PIH[which(more_ages$ages%in%draw_df$ages)]<-draw_df$PIH_adj
  more_ages$PIH[1]=more_ages$PIH[2]
  more_ages$PIH[seq(3,21,by=3)]<-10^(((more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(3,21,by=3)])*log10(more_ages$PIH[seq(2,20,by=3)])+
                                        (more_ages$ages[seq(3,21,by=3)]-more_ages$ages[seq(2,20,by=3)])*log10(more_ages$PIH[seq(5,23,by=3)])
  )/(more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(2,20,by=3)]))
  more_ages$PIH[seq(4,22,by=3)]<-10^(((more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(4,22,by=3)])*log10(more_ages$PIH[seq(2,20,by=3)])+
                                        (more_ages$ages[seq(4,22,by=3)]-more_ages$ages[seq(2,20,by=3)])*log10(more_ages$PIH[seq(5,23,by=3)])
  )/(more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(2,20,by=3)]))
  
  more_ages$PIH[24]<-10^(((more_ages$ages[25]-more_ages$ages[24])*log10(more_ages$PIH[23])+
                            (more_ages$ages[24]-more_ages$ages[23])*log10(more_ages$PIH[25])
  )/(more_ages$ages[25]-more_ages$ages[23]))
  
  more_ages<-more_ages[-c(which(more_ages$ages%in%draw_df$ages[-length(draw_df$ages)])),]
  
  ## ADJUST FOR NEW ESTIMATED ATTACK RATES IN CHINA AND STANDARDISE AGE CURVE
  std_AR_rel_55<-std_AR/std_AR[12]
  more_ages$Adj_IFR<-more_ages$IFR/std_AR_rel_55
  more_ages$Adj_PIH<-more_ages$PIH/std_AR_rel_55
  more_ages$Adj_IFR_rel_80<-more_ages$Adj_IFR/more_ages$Adj_IFR[length(more_ages$Adj_IFR)]
  more_ages$Adj_PIH_rel_80<-more_ages$Adj_PIH/more_ages$Adj_PIH[length(more_ages$Adj_PIH)]
  
  ## BASELINE PROBABILTITY OF HOSPITALISATION AND CRITICAL CARE BY AGE
  more_ages$prob_hosp<-more_ages$Adj_PIH_rel_80*prop_80p_H
  more_ages$prob_severe<-more_ages$Adj_IFR_rel_80*prop_80p_C
  
  return(more_ages)
  
}

extend_adjust_draw<-function(draw_df,std_AR,prop_80p_H,prop_80p_C){
  
  ## EXTEND IFR AND PIH TO 5 YEAR CATS USING LOG TRANSFORM
  more_ages<-data.frame("ages"=c(2.5,5,7.5,12.5,15,17.5,22.5,25, 27.5,32.5,35,37.5,42.5,45,47.5,52.5,55,
                                 57.5,62.5,65,67.5,72.5,75,77.5, 85),
                        "IFR"=0)
  
  more_ages$IFR[which(more_ages$ages%in%draw_df$ages)]<-draw_df$IFR
  more_ages$IFR[1]=more_ages$IFR[2]
  more_ages$IFR[seq(3,21,by=3)]<-10^(((more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(3,21,by=3)])*log10(more_ages$IFR[seq(2,20,by=3)])+
                                        (more_ages$ages[seq(3,21,by=3)]-more_ages$ages[seq(2,20,by=3)])*log10(more_ages$IFR[seq(5,23,by=3)])
  )/(more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(2,20,by=3)]))
  more_ages$IFR[seq(4,22,by=3)]<-10^(((more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(4,22,by=3)])*log10(more_ages$IFR[seq(2,20,by=3)])+
                                        (more_ages$ages[seq(4,22,by=3)]-more_ages$ages[seq(2,20,by=3)])*log10(more_ages$IFR[seq(5,23,by=3)])
  )/(more_ages$ages[seq(5,23,by=3)]-more_ages$ages[seq(2,20,by=3)]))
  
  more_ages$IFR[24]<-10^(((more_ages$ages[25]-more_ages$ages[24])*log10(more_ages$IFR[23])+
                            (more_ages$ages[24]-more_ages$ages[23])*log10(more_ages$IFR[25])
  )/(more_ages$ages[25]-more_ages$ages[23]))
  
  more_ages<-more_ages[-c(which(more_ages$ages%in%draw_df$ages[-length(draw_df$ages)])),]
  ## ADJUST FOR NEW ESTIMATED ATTACK RATES IN CHINA AND STANDARDISE AGE CURVE
  std_AR_rel_55<-std_AR/std_AR[12]
  more_ages$Adj_IFR<-more_ages$IFR/std_AR_rel_55
  return(more_ages)
  
}

add_mortality<-function(adjusted_draw,RR_hosp_CC,RR_CC_IFR,RR_hosp_IFR,CC_mort){
  ### ADD PROB OF HOSPITALISATION NOT REQUIRING CRITICAL CARE AND CRITICAL CARE MAINTANING RISK OF FATALITY GIVEN EITHER
  adjusted_draw$prob_hosp<-adjusted_draw$Adj_IFR*RR_hosp_IFR
  adjusted_draw$prob_severe<-adjusted_draw$Adj_IFR*RR_CC_IFR
  ### CACLCULATE ALL HOSPITALISATION PROBABILITY
  prop_all_hosps_severe<-adjusted_draw$prob_severe/(adjusted_draw$prob_hosp)
  ### CACLCULATE RISK OF DEATH OUTSIDE AND INSIDE CRITICAL CARE
  adjusted_draw$prob_non_severe_death_treatment<-(adjusted_draw$Adj_IFR-CC_mort*adjusted_draw$prob_severe)/(adjusted_draw$prob_hosp-adjusted_draw$prob_severe)
  adjusted_draw$prob_severe_death_treatment<-CC_mort
  return(adjusted_draw[c(1,3:7)])
}

##### END OF FUNCTIONS ###########################

### CALCULATE UNCERTAINTY DRAWS ##################################################


## GET ADJUSTED IFR AND PIH FOR BASELINE POSTERIOR MEAN
baseline_adjust<-extend_adjust_baseline(baseline,China_AR,prop_80p_hosp,prop_80p_crit)

#baseline mortality in critical care
CC_mort=0.5
#baseline age-specific RR of mortality given hospitalisation (held constant across uncertainty)
baseline_hosp_IFR_RR<-baseline_adjust$prob_hosp/baseline_adjust$Adj_IFR
#baseline age-specific RR of mortality given require critical care (held constant across uncertainty)
baseline_CC_IFR_RR<-baseline_adjust$prob_severe/baseline_adjust$Adj_IFR




## generate uncertainty in lack of treatment
prob_no_treat_severe<-rtriangle(1000,0.85,0.95)
prob_non_severe_death_no_treatment<-rtriangle(1000,0.5,0.7)
prob_non_severe_death_treatment_lmic<-rtriangle(1000,0.2,0.3)



severity_params<-list()
severity_params_lmic<-list()
for(i in 1:1000){
  ### pick an IFR draw
draw_df<-data.frame("ages"=seq(5,85,by=10),"IFR"=(IFR_chains %>% filter(draw==i))$IFR)

### extend age-range of draw
extend_draw_df<-extend_adjust_draw(draw_df,China_AR,prop_80p_hosp,prop_80p_crit)

### get other age_specific severity parameters
severity_params[[i]]<-add_mortality(extend_draw_df,baseline_hosp_CC_RR,baseline_CC_IFR_RR,baseline_hosp_IFR_RR,CC_mort)
severity_params[[i]]$prob_severe_death_no_treatment<-rep(prob_no_treat_severe[i],17)
severity_params[[i]]$prob_non_severe_death_no_treatment<-rep(prob_non_severe_death_no_treatment[i],17)
severity_params_lmic[[i]]<-severity_params[[i]]
severity_params_lmic[[i]]$prob_non_severe_death_treatment<-c(rep(prob_non_severe_death_treatment_lmic[i],16),severity_params[[i]]$prob_non_severe_death_treatment[17])
}

saveRDS(severity_params,"severity_param_sets.rds")
saveRDS(severity_params_lmic,"severity_param_sets_lmic.rds")

### END OF CALCULATE UNCERTAINTY DRAWS ##################################################



################# PLOT AND COMPARE UNCERTAINTY WITH LITERATURE  ###########################

uncertainty_df<-data.frame("age"=character(),"type"=character(),"draw"=numeric(),
                           "value"<-numeric()
)
age_cats<-c(paste(seq(0,75,by=5),"-",seq(5,80,by=5),sep=""),"80+")
for(i in 1:1000){
  IFR_draw_df<-data.frame("age"=age_cats,"type"="Fatality risk (good access to care)","draw"=i,"value"=severity_params[[i]]$Adj_IFR)
  PIH_draw_df<-data.frame("age"=age_cats,"type"="Requiring hospitalisation","draw"=i,"value"=severity_params[[i]]$prob_hosp)
  PIC_draw_df<-data.frame("age"=age_cats,"type"="Requiring critical care","draw"=i,"value"=severity_params[[i]]$prob_severe*severity_params[[i]]$prob_hosp)
  uncertainty_df<-rbind(uncertainty_df,IFR_draw_df,PIH_draw_df,PIC_draw_df)
}
uncertainty_df$age<-factor(uncertainty_df$age,levels=age_cats)
verity_et_al_ages=c(seq(2.5,77.5,by=5),85)

## ESTIMATES OF PROBABILITY OF HOSPITALISATION ESTIMATED IN FRANCE BY SALJE ET AL https://science.sciencemag.org/content/early/2020/05/12/science.abc3517
french<-tbl_df(read.csv("Data/French PIH Estimates.csv",fileEncoding="UTF-8-BOM"))

## ESTIMATES OF PROBABILITY OF HOSPITALISATION ESTIMATED IN CHINA BY BI ET AL https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30287-5/fulltext
chinese<-tbl_df(read.table("Data/lancet_prob_sev_prep.txt",header=T))

other_data_ages=c(10,seq(25,85,by=10))

plot_compare<-data.frame("age"=numeric(),"estimate"=character(),"mid"=numeric(),"low"=numeric(),"high"=numeric())

chinese_est<-data.frame("age"=other_data_ages,
                    "estimate"="Bi et al.",
                    "mid"=chinese$mid,
                    "low"=chinese$low,
                    "high"=chinese$high
)

verity_est<-data.frame("age"=verity_et_al_ages,
                       "estimate"="Verity et al.",
                       "mid"=sapply(1:17,function(z){quantile(uncertainty_df$value[uncertainty_df$age==unique(uncertainty_df$age)[z]&uncertainty_df$type =="Requiring hospitalisation"],c(0.5))})*100,
                       "low"=sapply(1:17,function(z){quantile(uncertainty_df$value[uncertainty_df$age==unique(uncertainty_df$age)[z]&uncertainty_df$type =="Requiring hospitalisation"],c(0.0255))}*100),
                       "high"=sapply(1:17,function(z){quantile(uncertainty_df$value[uncertainty_df$age==unique(uncertainty_df$age)[z]&uncertainty_df$type =="Requiring hospitalisation"],c(0.975))}*100)
)
french_est<-data.frame("age"=other_data_ages,
                       "estimate"="Salje et al.",
                       "mid"=french$mean*100,
                       "low"=french$lower*100,
                       "high"=french$upper*100
)


plot_compare<-rbind(verity_est,french_est,chinese_est)

compare_plot<-ggplot(plot_compare, aes(x=age, y=mid, group=estimate, color=estimate)) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=.1) +
  theme_pubr() +
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle=65, vjust=0.65),legend.title = element_blank(),
        legend.position = c(.3, .85),legend.text = element_text(size = 15))+
  labs(x = "Age", y = "Proportion of infections hospitalised") 

uncert_plot <- ggplot(uncertainty_df, aes(age, value))+
  geom_boxplot(aes(fill=factor(type))) +
  theme_pubr() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=65, vjust=0.6),
        legend.position = c(.45, .85),legend.text = element_text(size = 15))+
  labs(x = "Age category", y = "Proportion of infections") 

tiff("compare_uncertainty.tif",res=200,height=5,width=10,unit="in")
gA <- ggplotGrob(uncert_plot)
gB <- ggplotGrob(compare_plot)
grid::grid.newpage()
grid::grid.draw(cbind(gA, gB))
dev.off()


################# END OF PLOT AND COMPARE UNCERTAINTY WITH LITERATURE  ###########################