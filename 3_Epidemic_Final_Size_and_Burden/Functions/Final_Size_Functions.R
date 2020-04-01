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