# Load in Required Libraries 
library(readxl); library(dfoptim)

### set to directory
setwd("3_Epidemic_Final_Size_and_Burden/")
source("Functions/Final_Size_Functions.R")

### get WPP demography
demog_WPP<-read.csv("Data/country_inputs.csv")
pop_columns<-grep("pop",names(demog_WPP))

### get contact matrices
contact_mat_list<-readRDS("Outputs/processed_contact_matrices.rds")

### get severity inputs
severity_inputs<-read.csv("Data/severity_inputs.csv")
severity_inputs
##set R0
R=3.5
##set a reduction in elderly
elderly_sd=0.75
### initial size
init=1

## countries to calculate final size and mitigation parameters for 
countries<-unique(demog_WPP$Country_or_region)

### guess homogenous attack rate
guess_hom=0.85

## guess a lower bound for extent age-specific matrix alters average attack rate
guess_modifier=0.7

### guess level of SD required for optimal mitigation
guess_mitigation=0.65

### optimisation parameters
reiterates=3
iterates=100000
restarts=10
tol=0.0000001

country_sims<-list()
for(i in seq_along(countries)){
 pick=i
 print(demog_WPP$Country_or_region[pick])
 country_sims[[i]]<-get_ARs_IFR(country_name = demog_WPP$Country_or_region[pick],contact_mat=data.matrix((contact_mat_list[[demog_WPP$Matrix[pick]]])),demog_all=unlist(demog_WPP[pick,pop_columns]),elderly_sd = elderly_sd,IFRs = severity_inputs$IFR_adj,
                                hosp_probs =  severity_inputs$hosp_prob,crit_probs = severity_inputs$crit_care,R=R,init=init,
                                guess_hom =  guess_hom,guess_modifier= guess_modifier, guess_mitigation=guess_mitigation,
                                reiterates = reiterates,iterates = iterates,restarts = restarts,tol=tol)
}
  