rm(list=ls())

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

library(tidyverse); library(readxl); library(socialmixr);library(dfoptim);library(viridis)

contact_matrices<-readRDS("contact_matrices.rds")

China_matrix<-contact_matrices[[12]]
Netherlands_matrix<-contact_matrices[[7]]
Zimbabwe_matrix<-contact_matrices[[15]]


country_demogs<-read.csv("Data/country_inputs.csv",header=T)
China_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="China",7:21],sum(country_demogs[country_demogs$Country_or_region=="China",22:27])))
Netherlands_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Netherlands",7:21],sum(country_demogs[country_demogs$Country_or_region=="Netherlands",22:27])))
Zimbabwe_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Zimbabwe",7:21],sum(country_demogs[country_demogs$Country_or_region=="Zimbabwe",22:27])))

China_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="China_ages",col_names=F))
poly_ages<-seq(0,80,by=5)
Zimbabwe_ages<-unlist(read_xlsx("Data/contact_matrix_data.xlsx",sheet="Zimbabwe_ages",col_names=F))



#######  PLOT ATTACK RATES R0 FIGURE ####

R=3
China_sim<-get_ARs("China",data.matrix((China_matrix)),China_demog,R,1,0.6,5,100000,10,0.00000001)
Netherlands_sim<-get_ARs("Netherlands",data.matrix((Netherlands_matrix)),Netherlands_demog,R,1,0.6,5,100000,10,0.00000001)
Zimbabwe_sim<-get_ARs("Zimbabwe",data.matrix((Zimbabwe_matrix)),Zimbabwe_demog,R,1,0.6,5,100000,10,0.00000001)

China_2.3_sim<-get_ARs("China",data.matrix((China_matrix)),China_demog,2.3,1,0.6,5,100000,10,0.00000001)
Zimbabwe_2.3_sim<-get_ARs("Zimbabwe",data.matrix((Zimbabwe_matrix)),Zimbabwe_demog,2.3,1,0.6,5,100000,10,0.00000001)
Netherlands_2.3_sim<-get_ARs("Netherlands",data.matrix((Netherlands_matrix)),Netherlands_demog,2.3,1,0.6,5,100000,10,0.00000001)

China_3.5_sim<-get_ARs("China",data.matrix((China_matrix)),China_demog,3.5,1,0.6,5,100000,10,0.00000001)
Zimbabwe_3.5_sim<-get_ARs("Zimbabwe",data.matrix((Zimbabwe_matrix)),Zimbabwe_demog,3.5,1,0.6,5,100000,10,0.00000001)
Netherlands_3.5_sim<-get_ARs("Netherlands",data.matrix((Netherlands_matrix)),Netherlands_demog,3.5,1,0.6,5,100000,10,0.00000001)

China_2.3_AR<-China_2.3_sim$age_final_sizes/China_2.3_sim$demog
Zimbabwe_2.3_AR<-Zimbabwe_2.3_sim$age_final_sizes/Zimbabwe_2.3_sim$demog
China_3.5_AR<-China_3.5_sim$age_final_sizes/China_3.5_sim$demog
Zimbabwe_3.5_AR<-Zimbabwe_3.5_sim$age_final_sizes/Zimbabwe_3.5_sim$demog

Netherlands_3.5_AR<-Netherlands_3.5_sim$age_final_sizes/Netherlands_3.5_sim$demog
Netherlands_2.3_AR<-Netherlands_2.3_sim$age_final_sizes/Netherlands_2.3_sim$demog

China_AR<-China_sim$age_final_sizes/China_sim$demog
Netherlands_AR<-Netherlands_sim$age_final_sizes/Netherlands_sim$demog
Zimbabwe_AR<-Zimbabwe_sim$age_final_sizes/Zimbabwe_sim$demog



#### NEED TO RELATE TO SIMULATED OUTPUT
Mitigation_strats<-read.csv("Global_mitigation_strategies.csv")

zim_sd_all<-1-Mitigation_strats$Reductions[Mitigation_strats$Strategy==unique(Mitigation_strats$Strategy)[2]&Mitigation_strats$R0==3.0&Mitigation_strats$country=="Zimbabwe"]
china_sd_all<-1-Mitigation_strats$Reductions[Mitigation_strats$Strategy==unique(Mitigation_strats$Strategy)[2]&Mitigation_strats$R0==3.0&Mitigation_strats$country=="China"]
NL_sd_all<-1-Mitigation_strats$Reductions[Mitigation_strats$Strategy==unique(Mitigation_strats$Strategy)[2]&Mitigation_strats$R0==3.0&Mitigation_strats$country=="Netherlands"]

zim_sd_young<-1-Mitigation_strats$Reductions[Mitigation_strats$Strategy==unique(Mitigation_strats$Strategy)[3]&Mitigation_strats$R0==3.0&Mitigation_strats$country=="Zimbabwe"]
china_sd_young<-1-Mitigation_strats$Reductions[Mitigation_strats$Strategy==unique(Mitigation_strats$Strategy)[3]&Mitigation_strats$R0==3.0&Mitigation_strats$country=="China"]
NL_sd_young<-1-Mitigation_strats$Reductions[Mitigation_strats$Strategy==unique(Mitigation_strats$Strategy)[3]&Mitigation_strats$R0==3.0&Mitigation_strats$country=="Netherlands"]



China_mit<-get_ARs("China",data.matrix((China_matrix)),China_demog,R*china_sd_all,1,0.6,5,100000,10,0.00000001)
China_mit_AR<-China_mit$age_final_sizes/China_mit$demog
china_eld_mat<-China_matrix
china_eld_mat[14:16,]=china_eld_mat[14:16,]*0.4
China_mit_eld<-get_ARs("China",data.matrix((china_eld_mat)),China_demog,R*china_sd_young,1,0.6,5,100000,10,0.00000001)
China_mit_eld_AR<-China_mit_eld$age_final_sizes/China_mit_eld$demog

Zimbabwe_mit<-get_ARs("Zimbabwe",data.matrix((Zimbabwe_matrix)),Zimbabwe_demog,R*zim_sd_all,1,0.6,5,100000,10,0.00000001)
Zimbabwe_mit_AR<-Zimbabwe_mit$age_final_sizes/Zimbabwe_mit$demog
Zimbabwe_eld_mat<-Zimbabwe_matrix
Zimbabwe_eld_mat[14:16,]=Zimbabwe_eld_mat[14:16,]*0.4
Zimbabwe_mit_eld<-get_ARs("Zimbabwe",data.matrix((Zimbabwe_eld_mat)),Zimbabwe_demog,R*zim_sd_young,1,0.6,5,100000,10,0.00000001)
Zimbabwe_mit_eld_AR<-Zimbabwe_mit_eld$age_final_sizes/Zimbabwe_mit_eld$demog

Netherlands_mit<-get_ARs("Netherlands",data.matrix((Netherlands_matrix)),Netherlands_demog,R*NL_sd_all,1,0.6,5,100000,10,0.00000001)
Netherlands_mit_AR<-Netherlands_mit$age_final_sizes/Netherlands_mit$demog
Netherlands_eld_mat<-Netherlands_matrix
Netherlands_eld_mat[14:16,]=Netherlands_eld_mat[14:16,]*0.4
Netherlands_mit_eld<-get_ARs("Netherlands",data.matrix((Netherlands_eld_mat)),Netherlands_demog,R*NL_sd_young,1,0.6,5,100000,10,0.00000001)
Netherlands_mit_eld_AR<-Netherlands_mit_eld$age_final_sizes/Netherlands_mit_eld$demog


lmic_cols<-rev(viridis(4))

tiff("FigureS6_age_ARs_by_strategy_R0.tif",height=10,width=10,res=300,unit="in")
par(mar=c(5,5,2,2),mfrow=c(2,2))

plot(0,0,ylim=c(0,1),xlim=c(0,80),col="white",xlab="Age",ylab="Final proportion of population infected",cex.lab=1.5,las=1,cex.axis=1.2)
lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_AR,each=2),col=lmic_cols[1],lwd=3.5)
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_AR,each=2),col=lmic_cols[1],pch=15)

lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_2.3_AR,each=2),col=alpha(lmic_cols[1],0.5))
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_2.3_AR,each=2),col=alpha(lmic_cols[1],0.5),pch=15)

lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_3.5_AR,each=2),col=alpha(lmic_cols[1],0.5))
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_3.5_AR,each=2),col=alpha(lmic_cols[1],0.5),pch=15)

lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_AR,each=2),col=lmic_cols[2],lwd=3.5)
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_AR,each=2),col=lmic_cols[2],pch=15)

lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_2.3_AR,each=2),col=alpha(lmic_cols[2],0.5))
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_2.3_AR,each=2),col=alpha(lmic_cols[2],0.5),pch=15)
lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_3.5_AR,each=2),col=alpha(lmic_cols[2],0.5))
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_3.5_AR,each=2),col=alpha(lmic_cols[2],0.5),pch=15)

lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_AR,each=2),col=lmic_cols[3],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_AR,each=2),col=lmic_cols[3],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_3.5_AR,each=2),col=alpha(lmic_cols[3],0.5))
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_3.5_AR,each=2),col=alpha(lmic_cols[3],0.5),pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_2.3_AR,each=2),col=alpha(lmic_cols[3],0.5))
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_2.3_AR,each=2),col=alpha(lmic_cols[3],0.5),pch=15)
legend(0,0.2,c("Zimbabwe","China","Netherlands"),lty=1,pch=15,bty="n",col=lmic_cols[1:3],cex=1.3)

plot(0,0,ylim=c(0,1),xlim=c(0,80),col="white",xlab="Age",ylab="Final proportion of population infected",cex.lab=1.5,las=1,cex.axis=1.2)
lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_AR,each=2),col=lmic_cols[1],lwd=3.5)
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_AR,each=2),col=lmic_cols[1],pch=15)
lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_mit_AR,each=2),col=lmic_cols[2],lwd=3.5)
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_mit_AR,each=2),col=lmic_cols[2],pch=15)
lines(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_mit_eld_AR,each=2),col=lmic_cols[3],lwd=3.5)
points(Zimbabwe_ages[c(1,rep(2:(length(Zimbabwe_ages)-1),each=2),length(Zimbabwe_ages))],rep(Zimbabwe_mit_eld_AR,each=2),col=lmic_cols[3],pch=15)
legend(0,0.2,c("Zimbabwe unmitigated","Zimbabwe plus social distancing","Zimbabwe plus elderly protection"),lty=1,pch=15,bty="n",col=lmic_cols[1:3],cex=1.3)

plot(0,0,ylim=c(0,1),xlim=c(0,80),col="white",xlab="Age",ylab="Final proportion of population infected",cex.lab=1.5,las=1,cex.axis=1.2)
lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_AR,each=2),col=lmic_cols[1],lwd=3.5)
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_AR,each=2),col=lmic_cols[1],pch=15)
lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_mit_AR,each=2),col=lmic_cols[2],lwd=3.5)
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_mit_AR,each=2),col=lmic_cols[2],pch=15)
lines(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_mit_eld_AR,each=2),col=lmic_cols[3],lwd=3.5)
points(China_ages[c(1,rep(2:(length(China_ages)-1),each=2),length(China_ages))],rep(China_mit_eld_AR,each=2),col=lmic_cols[3],pch=15)
legend(0,0.2,c("China unmitigated","China plus social distancing","China plus elderly protection"),lty=1,pch=15,bty="n",col=lmic_cols[1:3],cex=1.3)



plot(0,0,ylim=c(0,1),xlim=c(0,80),col="white",xlab="Age",ylab="Final proportion of population infected",cex.lab=1.5,las=1,cex.axis=1.2)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_AR,each=2),col=lmic_cols[1],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_AR,each=2),col=lmic_cols[1],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_mit_AR,each=2),col=lmic_cols[2],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_mit_AR,each=2),col=lmic_cols[2],pch=15)
lines(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_mit_eld_AR,each=2),col=lmic_cols[3],lwd=3.5)
points(poly_ages[c(1,rep(2:(length(poly_ages)-1),each=2),length(poly_ages))],rep(Netherlands_mit_eld_AR,each=2),col=lmic_cols[3],pch=15)
legend(0,0.2,c("Netherlands unmitigated","Netherlands plus social distancing","Netherlands plus elderly protection"),lty=1,pch=15,bty="n",col=lmic_cols[1:3],cex=1.3)



dev.off()
