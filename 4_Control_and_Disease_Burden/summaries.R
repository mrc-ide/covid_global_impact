
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

HC_stretch_uncertainty<-readRDS("Outputs/HC_stretch_uncertainty.rds")

quantile(HC_stretch_uncertainty$multiple[HC_stretch_uncertainty$scenario=="mitigated"&HC_stretch_uncertainty$setting=="LIC"],c(0.025,0.5,0.975))
quantile(HC_stretch_uncertainty$multiple[HC_stretch_uncertainty$scenario=="mitigated"&HC_stretch_uncertainty$setting=="LMIC"],c(0.025,0.5,0.975))
quantile(HC_stretch_uncertainty$multiple[HC_stretch_uncertainty$scenario=="mitigated"&HC_stretch_uncertainty$setting=="UMIC"],c(0.025,0.5,0.975))
quantile(HC_stretch_uncertainty$multiple[HC_stretch_uncertainty$scenario=="mitigated"&HC_stretch_uncertainty$setting=="HIC"],c(0.025,0.5,0.975))

HC_stretch_uncertainty

overall_mort<-readRDS("Outputs/Mort_uncertainty.rds")

##UNLIMITED
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[1]&overall_mort$setting=="LIC"],c(0.025,0.5,0.975))
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[1]&overall_mort$setting=="LMIC"],c(0.025,0.5,0.975))

##TRUE
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[2]&overall_mort$setting=="LIC"],c(0.025,0.5,0.975))
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[2]&overall_mort$setting=="LMIC"],c(0.025,0.5,0.975))

##POOR OUTCOMES
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[3]&overall_mort$setting=="LIC"],c(0.025,0.5,0.975))
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[3]&overall_mort$setting=="LMIC"],c(0.025,0.5,0.975))

##Differences /EXCESS IMPACT
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[3]&overall_mort$setting=="LIC"]-overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[1]&overall_mort$setting=="LIC"],c(0.025,0.5,0.975))
quantile(overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[3]&overall_mort$setting=="LMIC"]-overall_mort$mort[overall_mort$scenario==unique(overall_mort$scenario)[1]&overall_mort$setting=="LMIC"],c(0.025,0.5,0.975))

