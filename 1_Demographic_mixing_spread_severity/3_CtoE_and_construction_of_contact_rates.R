rm(list=ls())

library(tidyverse); library(readxl); library(socialmixr);library(dfoptim);library(viridis)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### MATRICIES FROM SOCIALMIXR #####
Belgium_matrix<-contact_matrix(polymod, countries = "Belgium", age.limits = seq(0, 75,by=5))$matrix
Finland_matrix<-contact_matrix(polymod, countries = "Finland", age.limits = seq(0, 75,by=5))$matrix
Germany_matrix<-contact_matrix(polymod, countries = "Germany", age.limits = seq(0, 75,by=5))$matrix
Italy_matrix<-contact_matrix(polymod, countries = "Italy", age.limits = seq(0, 75,by=5))$matrix
Luxembourg_matrix<-contact_matrix(polymod, countries = "Luxembourg", age.limits = seq(0, 75,by=5))$matrix
Netherlands_matrix<-contact_matrix(polymod, countries = "Netherlands", age.limits = seq(0, 75,by=5))$matrix
Poland_matrix<-contact_matrix(polymod, countries = "Poland", age.limits = seq(0, 75,by=5))$matrix
UK_matrix<-contact_matrix(polymod, countries = "United Kingdom", age.limits = seq(0, 75,by=5))$matrix
France_matrix <- contact_matrix(clean(get_survey("https://doi.org/10.5281/zenodo.1157918")), age.limits = seq(0, 75,by=5))$matrix
HK_matrix <- contact_matrix(clean(get_survey("https://doi.org/10.5281/zenodo.1165561")), age.limits = seq(0, 75,by=5))$matrix
poly_ages<-seq(0,80,by=5)


Peru_matrix<-contact_matrix(clean(get_survey("https://doi.org/10.5281/zenodo.1095664")),age.limits = c(seq(0, 65,by=5)))$matrix
Peru_ages<-c(seq(0,65,by=5),80)

##### GET INDIA CONTACT MATRIX (https://pdfs.semanticscholar.org/844f/1baaaa84a7c1bd9fe26536bca068911784d2.pdf)#####
India_tib<-tbl_df(read.csv("Data/IndiaContactMixing_deidentified.csv"))
India_tib$India_respondent_cat<-floor(India_tib$respondent_age/5)+1
India_tib$India_respondent_cat[India_tib$India_respondent_cat>16]=16
India_tib$India_contact_cat<-floor(India_tib$d4age/5)+1
India_tib$India_contact_cat[India_tib$India_contact_cat>16]=16

India_no_na<-India_tib[!is.na(India_tib$India_respondent_cat),]
resp_Age<-array(dim=length(unique(India_no_na$pid)))
for(i in 1:length(resp_Age)){
  resp_Age[i]=India_no_na$India_respondent_cat[which(India_no_na$pid==unique(India_no_na$pid)[i])][1]
}

contact_mat=array(dim=c(16,16))
tot_age_cat<-array(dim=c(16))

for(i in 1:16){
  tot_age_cat[i]=length(resp_Age[resp_Age==i])
  for(j in 1:16){
    contact_mat[i,j]<-length(India_tib$India_contact_cat[India_tib$India_respondent_cat==i&India_tib$India_contact_cat==j])
  }
}

India_matrix<-contact_mat
for(i in 1:16){
  India_matrix[,i]<-contact_mat[,i]/tot_age_cat
}

India_ages<-seq(0,80,by=5)

####GET MATRICES FROM LITERATURE #####
China_matrix<-read_xlsx("Data/contact_matrix_data.xlsx",sheet="China_matrix",col_names=F)
China_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="China_ages",col_names=F))


Kenya_matrix<-read_xlsx("Data/contact_matrix_data.xlsx",sheet="Kenya_matrix",col_names=F)
Kenya_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="Kenya_ages",col_names=F))

Russia_matrix<-read_xlsx("Data/contact_matrix_data.xlsx",sheet="Russia_matrix",col_names=F)
Russia_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="Russia_ages",col_names=F))

South_Africa_matrix<-read_xlsx("Data/contact_matrix_data.xlsx",sheet="South_Africa_matrix",col_names=F)
SA_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="South_Africa_ages",col_names=F))

Uganda_matrix<-read_xlsx("Data/contact_matrix_data.xlsx",sheet="Uganda_matrix",col_names=F)
Uganda_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="Uganda_ages",col_names=F))

Zimbabwe_matrix<-read_xlsx("Data/contact_matrix_data.xlsx",sheet="Zimbabwe_matrix",col_names=F)
Zimbabwe_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="Zimbabwe_ages",col_names=F))



################################################  FIGURE 1C ############################################
### functions needed to calculate final sizes - see relevant Rfile for details###
output_set<-function(inf_pops,rates,mixing,demog,inits){
  return(sapply(seq_along(inf_pops),function(x){
    ((demog[x]-inits[x])/inf_pops[x]*(1-exp(-sum(rates[x]*mixing[x,]*(inf_pops-inits)/demog)))-1)
  }))
}
sum_output_set<-function(inf_pops,rates,mixing,demog,inits){
  if(min(inf_pops<=0)){return(rep(1000,length(inf_pops)))}
  outset=output_set(inf_pops,rates,mixing,demog,inits)
  return(sum(outset*outset))
}
single_set<-function(inf_pop,rate,pop,init){
  diff=(pop-init)/inf_pop*(1-exp(-rate*(inf_pop-init)/pop))-1
  return(diff*diff)
}
get_ARs<-function(country_name,contact_mat,demog,rvals,init,start_guess,reiterates,iterates,restarts,tol){
  MIJ<-t(sapply(seq(demog),function(x){
    contact_mat[x,]*demog[x]
  }))
  
  adjust_mat<-(MIJ+t(MIJ))/2
  
  
  new_mix_mat<-t(sapply(seq(demog),function(x){
    adjust_mat[x,]/demog[x]
  }))
  
  
  c_mat<-t(sapply(seq(demog),function(x){
    new_mix_mat[x,]/sum(new_mix_mat[x,])
  }
  ))
  
  ai=rowSums(new_mix_mat)
  ng_eigen=Re(eigen(new_mix_mat)$val[1])
  
  
  single_init=init
  tot_pop<-sum(demog)
  init_demog=single_init*demog/tot_pop
  
  final_vals=array(0,dim=c(length(rvals)))
  no_age_vals=array(0,dim=c(length(rvals)))
  final_err=array(0,dim=c(length(rvals)))
  age_dists<-array(0,dim=c(length(rvals),length(demog)))
  for(i in seq_along(rvals)){
    guess_modifiers=start_guess
    fit_err=1
    R=rvals[i]
    guess_inf=guess_modifiers*sum(demog)
    rmod<-R/ng_eigen*ai
    
    fit_single<-optim(guess_inf,single_set,rate=R,pop=tot_pop,init=single_init,method="Brent",lower=0,upper=tot_pop,control=list(abstol=tol,maxit=iterates))
    no_age_vals[i]=fit_single$par
    while(fit_err>0.0000001){
      fit<-nmkb(fit_single$par*demog/sum(demog)*guess_modifiers, sum_output_set, lower=0, upper=demog,rates=rmod,mixing=c_mat,demog=demog,inits=init_demog,control=list(maxfeval=iterates,tol=tol,restarts.max=restarts))
      for(f in 1:reiterates){
        fit<-nmkb(fit$par, sum_output_set, lower=0, upper=demog,rates=rmod,mixing=c_mat,demog=demog,inits=init_demog,control=list(maxfeval=iterates,tol=tol,restarts.max=restarts))
      }
      fit_err=fit$value
      guess_modifiers=guess_modifiers+0.1
    }
    final_vals[i]<-sum(fit$par)
    age_dists[i,]<-fit$par
    final_err[i]<-fit$value
  }
  return(list("country"=country_name,"demog"=demog,"rvals"=rvals,"rates"=ai,"homo_sizes"=no_age_vals,"homo_AR"=no_age_vals/tot_pop,"final_size"=final_vals,age_ARs=age_dists/demog,"age_final_sizes"=age_dists,"errors"=final_err))
}

### GET RELEVANT DEMOGRAPHY INPUTS   ###
country_demogs<-read.csv("Data/country_inputs.csv",header=T)
Belgium_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Belgium",7:21],sum(country_demogs[country_demogs$Country_or_region=="Belgium",22:27])))
Finland_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Finland",7:21],sum(country_demogs[country_demogs$Country_or_region=="Finland",22:27])))
Germany_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Germany",7:21],sum(country_demogs[country_demogs$Country_or_region=="Germany",22:27])))
Italy_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Italy",7:21],sum(country_demogs[country_demogs$Country_or_region=="Italy",22:27])))
Luxembourg_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Luxembourg",7:21],sum(country_demogs[country_demogs$Country_or_region=="Luxembourg",22:27])))
Netherlands_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Netherlands",7:21],sum(country_demogs[country_demogs$Country_or_region=="Netherlands",22:27])))
Poland_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Poland",7:21],sum(country_demogs[country_demogs$Country_or_region=="Poland",22:27])))
UK_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="United Kingdom",7:21],sum(country_demogs[country_demogs$Country_or_region=="United Kingdom",22:27])))
France_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="France",7:21],sum(country_demogs[country_demogs$Country_or_region=="France",22:27])))
HK_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Hong Kong SAR, China",7:21],sum(country_demogs[country_demogs$Country_or_region=="Hong Kong SAR, China",22:27])))

China_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="China",7:21],sum(country_demogs[country_demogs$Country_or_region=="China",22:27])))
Peru_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Peru",7:19],sum(country_demogs[country_demogs$Country_or_region=="Peru",20:27])))
Russia_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Russian Federation",7:17],sum(country_demogs[country_demogs$Country_or_region=="Russian Federation",18:27])))
SA_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="South Africa",7:15],sum(country_demogs[country_demogs$Country_or_region=="South Africa",16:27])))

Kenya_demog_fine<-unlist(c(country_demogs[country_demogs$Country_or_region=="Kenya",7:21],sum(country_demogs[country_demogs$Country_or_region=="Kenya",22:27])))
Kenya_demog<-c(Kenya_demog_fine[1],sum(Kenya_demog_fine[2:3]),Kenya_demog_fine[4],sum(Kenya_demog_fine[5:11]),sum(Kenya_demog_fine[12:16]))
India_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="India",7:21],sum(country_demogs[country_demogs$Country_or_region=="India",22:27])))
Uganda_demog_fine<-unlist(c(country_demogs[country_demogs$Country_or_region=="Uganda",7:21],sum(country_demogs[country_demogs$Country_or_region=="Uganda",22:27])))
Uganda_demog<-c(Uganda_demog_fine[1:3],sum(Uganda_demog_fine[4:5]),sum(Uganda_demog_fine[6:7]),sum(Uganda_demog_fine[8:9]),sum(Uganda_demog_fine[10:11]),sum(Uganda_demog_fine[12:13]),sum(Uganda_demog_fine[14:16]))
Zimbabwe_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Zimbabwe",7:21],sum(country_demogs[country_demogs$Country_or_region=="Zimbabwe",22:27])))

#### sIMULATIONS   ######
R=3
Belgium_sim<-get_ARs("Belgium",data.matrix((Belgium_matrix)),Belgium_demog,R,1,0.6,5,100000,10,0.00000001)
Finland_sim<-get_ARs("Finland",data.matrix((Finland_matrix)),Finland_demog,R,1,0.6,5,100000,10,0.00000001)
Germany_sim<-get_ARs("Germany",data.matrix((Germany_matrix)),Germany_demog,R,1,0.6,5,100000,10,0.00000001)
Italy_sim<-get_ARs("Italy",data.matrix((Italy_matrix)),Italy_demog,R,1,0.6,5,100000,10,0.00000001)
Luxembourg_sim<-get_ARs("Italy",data.matrix((Luxembourg_matrix)),Luxembourg_demog,R,1,0.6,5,100000,10,0.00000001)
Netherlands_sim<-get_ARs("Netherlands",data.matrix((Netherlands_matrix)),Netherlands_demog,R,1,0.6,5,100000,10,0.00000001)
Poland_sim<-get_ARs("Poland",data.matrix((Poland_matrix)),Poland_demog,R,1,0.6,5,100000,10,0.00000001)
France_sim<-get_ARs("France",data.matrix((France_matrix)),France_demog,R,1,0.6,5,100000,10,0.00000001)
UK_sim<-get_ARs("UK",data.matrix((UK_matrix)),UK_demog,R,1,0.6,5,100000,10,0.00000001)
HK_sim<-get_ARs("HK",data.matrix((HK_matrix)),HK_demog,R,1,0.6,5,100000,10,0.00000001)


China_sim<-get_ARs("China",data.matrix((China_matrix)),China_demog,R,1,0.6,5,100000,10,0.00000001)
Peru_sim<-get_ARs("Italy",data.matrix((Peru_matrix)),Peru_demog,R,1,0.6,5,100000,10,0.00000001)
Russia_sim<-get_ARs("Russia",data.matrix((Russia_matrix)),Russia_demog,R,1,0.6,5,100000,10,0.00000001)
SA_sim<-get_ARs("SA",data.matrix((South_Africa_matrix)),SA_demog,R,1,0.6,5,100000,10,0.00000001)

Kenya_sim<-get_ARs("Kenya",data.matrix((Kenya_matrix)),Kenya_demog,R,1,0.6,5,100000,10,0.00000001)
India_sim<-get_ARs("India",data.matrix((India_matrix)),India_demog,R,1,0.6,5,100000,10,0.00000001)
Uganda_sim<-get_ARs("Uganda",data.matrix((Uganda_matrix)),Uganda_demog,R,1,0.6,5,100000,10,0.00000001)
Zimbabwe_sim<-get_ARs("Zimbabwe",data.matrix((Zimbabwe_matrix)),Zimbabwe_demog,R,1,0.6,5,100000,10,0.00000001)


Belgium_AR<-Belgium_sim$age_final_sizes/Belgium_sim$demog
Germany_AR<-Germany_sim$age_final_sizes/Germany_sim$demog
Italy_AR<-Italy_sim$age_final_sizes/Italy_sim$demog
Luxembourg_AR<-Luxembourg_sim$age_final_sizes/Luxembourg_sim$demog
Netherlands_AR<-Netherlands_sim$age_final_sizes/Netherlands_sim$demog
Poland_AR<-Poland_sim$age_final_sizes/Poland_sim$demog
France_AR<-France_sim$age_final_sizes/France_sim$demog
Finland_AR<-Finland_sim$age_final_sizes/Finland_sim$demog
UK_AR<-UK_sim$age_final_sizes/UK_sim$demog
HK_AR<-HK_sim$age_final_sizes/HK_sim$demog

China_AR<-China_sim$age_final_sizes/China_sim$demog
Peru_AR<-Peru_sim$age_final_sizes/Peru_sim$demog
Russia_AR<-Russia_sim$age_final_sizes/Russia_sim$demog
SA_AR<-SA_sim$age_final_sizes/SA_sim$demog

Kenya_AR<-Kenya_sim$age_final_sizes/Kenya_sim$demog
India_AR<-India_sim$age_final_sizes/India_sim$demog
Uganda_AR<-Uganda_sim$age_final_sizes/Uganda_sim$demog
Zimbabwe_AR<-Zimbabwe_sim$age_final_sizes/Zimbabwe_sim$demog

China_AR<-China_sim$age_final_sizes/China_sim$demog

Zimbabwe_AR<-Zimbabwe_sim$age_final_sizes/Zimbabwe_sim$demog

lmic_cols<-rev(viridis(4))
hic_cols<-rev(viridis(10))
###### PLOT THE FIGURE   ####
tiff("Figure1C_to_E_Final_attack_rates_by_age.tif",height=5,width=15,res=300,unit="in")
par(mar=c(5,5,2,2),mfrow=c(1,3))
plot(0,0,ylim=c(0,1),xlim=c(0,80),col="white",xlab="Age",ylab="Final proportion of population infected",cex.lab=2,las=1,cex.axis=1.5)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Finland_AR,each=2),col=hic_cols[10],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Finland_AR,each=2),col=hic_cols[10],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Poland_AR,each=2),col=hic_cols[9],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Poland_AR,each=2),col=hic_cols[9],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Italy_AR,each=2),col=hic_cols[8],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Italy_AR,each=2),col=hic_cols[8],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Luxembourg_AR,each=2),col=hic_cols[7],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Luxembourg_AR,each=2),col=hic_cols[7],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_AR,each=2),col=hic_cols[6],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_AR,each=2),col=hic_cols[6],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Belgium_AR,each=2),col=hic_cols[5],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Belgium_AR,each=2),col=hic_cols[5],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Germany_AR,each=2),col=hic_cols[4],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Germany_AR,each=2),col=hic_cols[4],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(UK_AR,each=2),col=hic_cols[3],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(UK_AR,each=2),col=hic_cols[3],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(France_AR,each=2),col=hic_cols[2],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(France_AR,each=2),col=hic_cols[2],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(HK_AR,each=2),col=hic_cols[1],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(HK_AR,each=2),col=hic_cols[1],pch=15)
legend(10,0.7,c("Hong Kong","France","UK","Germany","Belgum",
                "Netherlands", "Luxembourg","Italy","Poland","Finland"),lty=1,pch=15,bty="n",col=(hic_cols),cex=1.9)


plot(0,0,ylim=c(0,1),xlim=c(0,80),col="white",xlab="Age",ylab="Final proportion of population infected",cex.lab=2,las=1,cex.axis=1.5)
lines(SA_ages[c(1,rep(2:(length(SA_ages)-1),each=2),length(SA_ages))],rep(SA_AR,each=2),col=lmic_cols[4],lwd=3.5)
points(SA_ages[c(1,rep(2:(length(SA_ages)-1),each=2),length(SA_ages))],rep(SA_AR,each=2),col=lmic_cols[4],pch=15)
lines(Russia_ages[c(1,rep(2:(length(Russia_ages)-1),each=2),length(Russia_ages))],rep(Russia_AR,each=2),col=lmic_cols[3],lwd=3.5)
points(Russia_ages[c(1,rep(2:(length(Russia_ages)-1),each=2),length(Russia_ages))],rep(Russia_AR,each=2),col=lmic_cols[3],pch=15)
lines(Peru_ages[c(1,rep(2:(length(Peru_ages)-1),each=2),length(Peru_ages))],rep(Peru_AR,each=2),col=lmic_cols[2],lwd=3.5)
points(Peru_ages[c(1,rep(2:(length(Peru_ages)-1),each=2),length(Peru_ages))],rep(Peru_AR,each=2),col=lmic_cols[2],pch=15)
lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_AR,each=2),col=lmic_cols[1],lwd=3.5)
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_AR,each=2),col=lmic_cols[1],pch=15)
legend(10,0.7,c("China","Peru","Russia","South Africa"),lty=1,pch=15,bty="n",col=(lmic_cols),cex=1.9)


plot(0,0,ylim=c(0,1),xlim=c(0,80),col="white",xlab="Age",ylab="Final proportion of population infected",cex.lab=2,las=1,cex.axis=1.5)
lines(India_ages[c(1,rep(2:(length(India_ages)-1),each=2),length(India_ages))],rep(India_AR,each=2),col=lmic_cols[4],lwd=3.5)
points(India_ages[c(1,rep(2:(length(India_ages)-1),each=2),length(India_ages))],rep(India_AR,each=2),col=lmic_cols[4],pch=15)
lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_AR,each=2),col=lmic_cols[3],lwd=3.5)
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_AR,each=2),col=lmic_cols[3],pch=15)
lines(Kenya_ages[c(1,rep(2:(length(Kenya_ages)-1),each=2),length(Kenya_ages))],rep(Kenya_AR,each=2),col=lmic_cols[2],lwd=3.5)
points(Kenya_ages[c(1,rep(2:(length(Kenya_ages)-1),each=2),length(Kenya_ages))],rep(Kenya_AR,each=2),col=lmic_cols[2],pch=15)
lines(Uganda_ages[c(1,rep(2:(length(Uganda_ages)-1),each=2),length(Uganda_ages))],rep(Uganda_AR,each=2),col=lmic_cols[1],lwd=3.5)
points(Uganda_ages[c(1,rep(2:(length(Uganda_ages)-1),each=2),length(Uganda_ages))],rep(Uganda_AR,each=2),col=lmic_cols[1],pch=15)
legend(10,0.7,c("Uganda","Kenya","Zimbabwe","India"),lty=1,pch=15,bty="n",col=lmic_cols,cex=1.9)
dev.off()
#### END FIGURE PLOT ################



###### PLOT THE CONTACT RATES FIGURE   ####
tiff("FigureS5_contact_rates_by_age.tif",height=5,width=15,res=300,unit="in")
par(mar=c(5,5,2,2),mfrow=c(1,3))


plot(0,0,ylim=c(0,40),xlim=c(0,80),col="white",xlab="Age",ylab="Per-capita contacts per day",cex.lab=2,las=1,cex.axis=1.5)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Finland_matrix),each=2),col=hic_cols[10],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Finland_matrix),each=2),col=hic_cols[10],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Poland_matrix),each=2),col=hic_cols[9],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Poland_matrix),each=2),col=hic_cols[9],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Italy_matrix),each=2),col=hic_cols[8],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Italy_matrix),each=2),col=hic_cols[8],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Luxembourg_matrix),each=2),col=hic_cols[7],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Luxembourg_matrix),each=2),col=hic_cols[7],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Netherlands_matrix),each=2),col=hic_cols[6],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Netherlands_matrix),each=2),col=hic_cols[6],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Belgium_matrix),each=2),col=hic_cols[5],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Belgium_matrix),each=2),col=hic_cols[5],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Germany_matrix),each=2),col=hic_cols[4],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(Germany_matrix),each=2),col=hic_cols[4],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(UK_matrix),each=2),col=hic_cols[3],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(UK_matrix),each=2),col=hic_cols[3],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(France_matrix),each=2),col=hic_cols[2],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(France_matrix),each=2),col=hic_cols[2],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(HK_matrix),each=2),col=hic_cols[1],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(rowSums(HK_matrix),each=2),col=hic_cols[1],pch=15)
legend(0,40,c("Hong Kong","France","UK","Germany","Belgum",
              "Netherlands", "Luxembourg","Italy","Poland","Finland"),lty=1,pch=15,bty="n",col=(hic_cols),cex=1.3,ncol=3)


plot(0,0,ylim=c(0,40),xlim=c(0,80),col="white",xlab="Age",ylab="Per-capita contacts per day",cex.lab=2,las=1,cex.axis=1.5)
lines(SA_ages[c(1,rep(2:(length(SA_ages)-1),each=2),length(SA_ages))],rep(rowSums(South_Africa_matrix),each=2),col=lmic_cols[4],lwd=3.5)
points(SA_ages[c(1,rep(2:(length(SA_ages)-1),each=2),length(SA_ages))],rep(rowSums(South_Africa_matrix),each=2),col=lmic_cols[4],pch=15)
lines(Russia_ages[c(1,rep(2:(length(Russia_ages)-1),each=2),length(Russia_ages))],rep(rowSums(Russia_matrix),each=2),col=lmic_cols[3],lwd=3.5)
points(Russia_ages[c(1,rep(2:(length(Russia_ages)-1),each=2),length(Russia_ages))],rep(rowSums(Russia_matrix),each=2),col=lmic_cols[3],pch=15)
lines(Peru_ages[c(1,rep(2:(length(Peru_ages)-1),each=2),length(Peru_ages))],rep(rowSums(Peru_matrix),each=2),col=lmic_cols[2],lwd=3.5)
points(Peru_ages[c(1,rep(2:(length(Peru_ages)-1),each=2),length(Peru_ages))],rep(rowSums(Peru_matrix),each=2),col=lmic_cols[2],pch=15)
lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(rowSums(China_matrix),each=2),col=lmic_cols[1],lwd=3.5)
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(rowSums(China_matrix),each=2),col=lmic_cols[1],pch=15)
legend(30,40,c("China","Peru","Russia","South Africa"),lty=1,pch=15,bty="n",col=(lmic_cols),cex=1.3)


plot(0,0,ylim=c(0,40),xlim=c(0,80),col="white",xlab="Age",ylab="Per-capita contacts per day",cex.lab=2,las=1,cex.axis=1.5)
lines(India_ages[c(1,rep(2:(length(India_ages)-1),each=2),length(India_ages))],rep(rowSums(India_matrix),each=2),col=lmic_cols[4],lwd=3.5)
points(India_ages[c(1,rep(2:(length(India_ages)-1),each=2),length(India_ages))],rep(rowSums(India_matrix),each=2),col=lmic_cols[4],pch=15)
lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(rowSums(Zimbabwe_matrix),each=2),col=lmic_cols[3],lwd=3.5)
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(rowSums(Zimbabwe_matrix),each=2),col=lmic_cols[3],pch=15)
lines(Kenya_ages[c(1,rep(2:(length(Kenya_ages)-1),each=2),length(Kenya_ages))],rep(rowSums(Kenya_matrix),each=2),col=lmic_cols[2],lwd=3.5)
points(Kenya_ages[c(1,rep(2:(length(Kenya_ages)-1),each=2),length(Kenya_ages))],rep(rowSums(Kenya_matrix),each=2),col=lmic_cols[2],pch=15)
lines(Uganda_ages[c(1,rep(2:(length(Uganda_ages)-1),each=2),length(Uganda_ages))],rep(rowSums(Uganda_matrix),each=2),col=lmic_cols[1],lwd=3.5)
points(Uganda_ages[c(1,rep(2:(length(Uganda_ages)-1),each=2),length(Uganda_ages))],rep(rowSums(Uganda_matrix),each=2),col=lmic_cols[1],pch=15)
legend(30,40,c("Uganda","Kenya","Zimbabwe","India"),lty=1,pch=15,bty="n",col=lmic_cols,cex=1.3)
dev.off()
#### END FIGURE PLOT ################
##### Save matricies that have sufficient age granularity for severity modelling

##First adjust peru data to account for no respondants aged 70-75 (though data on contacts in these ages) by extrapolating patterns in 65+
adjustperu<-contact_matrix(clean(get_survey("https://doi.org/10.5281/zenodo.1095664")),age.limits = c(seq(0, 75,by=5)))$matrix
country_demogs<-read.csv("country_inputs.csv",header=T)
demog_peru<-unlist(c(country_demogs[country_demogs$Country_or_region=="Peru",7:21],sum(country_demogs[country_demogs$Country_or_region=="Peru",22:27])))
sum_over_65<-(adjustperu[14,]*demog_peru[14]+adjustperu[15,]*demog_peru[15]+adjustperu[16,]*demog_peru[16])/sum(demog_peru[14:16])
adjustperu[14,]<-sum_over_65
adjustperu[15,]<-sum_over_65
adjustperu[16,]<-sum_over_65


contact_matrices=list()
contact_matrices[[1]]<-UK_matrix
contact_matrices[[2]]<-Belgium_matrix
contact_matrices[[3]]<-Germany_matrix
contact_matrices[[4]]<-Finland_matrix
contact_matrices[[5]]<-Poland_matrix
contact_matrices[[6]]<-Luxembourg_matrix
contact_matrices[[7]]<-Netherlands_matrix
contact_matrices[[8]]<-Italy_matrix
contact_matrices[[9]]<-France_matrix
contact_matrices[[10]]<-HK_matrix
contact_matrices[[11]]<-Russia_matrix
contact_matrices[[12]]<-China_matrix
contact_matrices[[13]]<-adjustperu
contact_matrices[[14]]<-India_matrix
contact_matrices[[15]]<-Zimbabwe_matrix

saveRDS(contact_matrices,"Outputs/contact_matrices.rds")
