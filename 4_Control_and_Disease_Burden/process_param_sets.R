rm(list=ls())
install.packages("triangle")

library(dplyr)
library(triangle)

setwd("C:/Users/pgw06/Imperial College London/ncov - Documents/2019-nCoV/LMIC/LMIC Parameters")
IFR_chains<-tbl_df(read.csv("IFR_age_chains.csv"))
hosp_chains<-tbl_df(read.csv("Hospitilisation_age_chains.csv"))



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

China_AR<-c(0.485046824,0.820064569,0.921758997,0.943028327,0.890452815,0.863618104,0.904659973,0.896650206,
            0.889337465,0.870199419,0.806136728,0.829015942,0.709811137,0.736056837,0.699378916,0.686382172,0.686382172)

UK_infs<-c(2649210.87,3349651.937,3291649.757,3009849.258,3042901.537,3376927.545,3663474.241,3624700.936,3293144.833,
               3355756.909,3370413.825,3091246.504,2507289.178,1793588.509,1782326.526,1282973.875,1835663.221)



prop_80p_hosp<-0.18

### value of eldest risk of % critical care that gives 30% of hospitlisations for UK
prop_80p_crit<-0.1277



## GET ADJUSTED IFR AND PIH FOR BASELINE POSTERIOR MEAN
baseline_adjust<-extend_adjust_IFR_PIH(baseline,China_AR,prop_80p_hosp,prop_80p_crit)

#(assuming baseline mortality in critical care of 50% in UK from IGNARC REPORT
CC_mort=0.5
#Fix age-specific RR of mortality given hospitalisation
baseline_hosp_IFR_RR<-baseline_adjust$prop_hosp/baseline_adjust$Adj_IFR
#Fix age-specific RR  mortality given need critical care
baseline_CC_IFR_RR<-baseline_adjust$prop_severe/baseline_adjust$Adj_IFR


## generate uncertainty in lack of treatment
prob_no_treat_severe<-rtriangle(1000,0.90,1)
prob_non_severe_death_no_treatment<-rtriangle(1000,0.5,0.7)
prob_non_severe_death_treatment_lmic<-rtriangle(1000,0.2,0.3)

rep(prob_no_treat_severe[1],17)

severity_params<-list()
severity_params_lmic<-list()
for(i in 1:1000){
### pick an IFR draw
draw_df<-data.frame("ages"=seq(5,85,by=10),"IFR"=(IFR_chains %>% filter(draw==i))$IFR,"PIH"=(hosp_chains %>% filter(draw==1))$hosp)

### extend age-range of draw
extend_draw_df<-extend_adjust_IFR_PIH(draw_df,China_AR,prop_80p_hosp,prop_80p_crit)


### get other age_specific severity parameters
severity_params[[i]]<-add_mortality(extend_draw_df,baseline_hosp_CC_RR,baseline_CC_IFR_RR,baseline_hosp_IFR_RR,CC_mort)
severity_params[[i]]$prob_severe_death_no_treatment<-rep(prob_no_treat_severe[i],17)
severity_params[[i]]$prob_non_severe_death_no_treatment<-rep(prob_non_severe_death_no_treatment[i],17)
severity_params_lmic[[i]]<-severity_params[[i]]
severity_params_lmic[[i]]$prob_non_severe_death_treatment<-c(rep(prob_non_severe_death_treatment_lmic[i],16),severity_params[[i]]$prob_non_severe_death_treatment[17])
}

severity_params_lmic

#severity_params[[i]]$
#severity_params_lmic[[i]]$
severity_params[[i]]

#$prob_non_severe_death_treatment

severity_params
  severity_params[[i]]
  
severity_params_lmic

saveRDS(severity_params,"severity_param_sets.rds")
saveRDS(severity_params_lmic,"severity_param_sets_lmic.rds")

add_mortality<-function(adjusted_draw,RR_hosp_CC,RR_CC_IFR,RR_hosp_IFR,CC_mort){
  adjusted_draw$prob_hosp<-adjusted_draw$Adj_IFR*RR_hosp_IFR
  adjusted_draw$prob_severe<-adjusted_draw$Adj_IFR*RR_CC_IFR
  prop_all_hosps_severe<-adjusted_draw$prob_severe/(adjusted_draw$prob_hosp)
  adjusted_draw$prob_non_severe_death_treatment<-(adjusted_draw$Adj_IFR-CC_mort*adjusted_draw$prob_severe)/(adjusted_draw$prob_hosp-adjusted_draw$prob_severe)
  adjusted_draw$prob_severe_death_treatment<-CC_mort
  return(adjusted_draw[c(1,4,8:11)])
  }



extend_adjust_IFR_PIH<-function(draw_df,std_AR,prop_80p_H,prop_80p_C){
  
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
 
   ## ADJUST FOR [INSERT PROP_H JUSTIFICATION] AND [INSERT PROP_C JUSTIFICATION]
  more_ages$prob_hosp<-more_ages$Adj_PIH_rel_80*prop_80p_H
  more_ages$prob_severe<-more_ages$Adj_IFR_rel_80*prop_80p_C
    
  return(more_ages)
  
}





