# The Impact of COVID-19 and Strategies for Mitigation and Suppression in Low- and Middle-Income Countries

This repository contains the code used to generate the analyses contained within the paper "The Impact of COVID-19 and Strategies for Mitigation and Suppression in Low- and Middle-Income Countries", - Walker et al, 2020.

It is divided into 7 subfolders, each containing all the relevant data and R scripts required to generate a set of analyses contained in the paper. 

- 1_Demographic_mixing_spread_severity - contains the code used to generate *Figure 1*, *Supplementary Figures 5 & 6*
- 2_Determining_Healthcare_Capacity - contains the code used to generate *Figure 2*.
- 3_Comorbidity_Exploration - contains the code used to generate *Figure 3*.
- 4_Control_and_Disease_Burden - contains the code used to generate *Figure 4*, *Supplementary Figure 4*. 
- 5_Suppression_Triggering_and_Exploration - contains the code used to generate *Figure 5*.
- Table_1_Government_Measures - contains the code used to generate *Table 1*.
- S1_optimal_mitigation_example - contains the code used to generate *Supplementary Figure 1*.

In addition to this code, running these analyses also requires downloading and installing an R package developed specifically for these analyses and that allows simulation of COVID-19 epidemics using an age-structured SEIR model (with explicit healthcare representation). The package is called squire and is available here: https://github.com/mrc-ide/squire. For the paper concerned, squire version 0.4.14 was used and can be installed with:

```
devtools::install_github("mrc-ide/squire@v0.4.14")
```

For all other packages used in this repository, the version can be found within 'installed_packages.rds'
