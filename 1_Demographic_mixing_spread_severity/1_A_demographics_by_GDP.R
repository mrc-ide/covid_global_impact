rm(list=ls())

library(dplyr)
library(rgdal)
library(RColorBrewer)
library(digest)
library(scales)
library(readxl)
library(tidyverse)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(gtools)

hrbrthemes::import_roboto_condensed()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

demog<-read_xlsx("Data/GDP_WPP_link.xlsx",sheet="Linked")

GDP_demog<-demog %>%
  filter(GDP2018>0)
GDP_demog$quant<-quantcut(GDP_demog$GDP2018,10)

quant_demog<-GDP_demog %>% 
  group_by(quant)%>% 
    summarise_at(names(GDP_demog)[13:33],sum,na.rm=TRUE)
total_pops<-rowSums(quant_demog[,2:22])

ages=seq(0,100,by=5)
demog_df<-data.frame("quant"=character(),"age"=numeric(),"proportion"=numeric())
for(i in seq_along(ages)){
  age_data<-data.frame(1:10,ages[i],as.vector(quant_demog[,i+1]/total_pops))
  names(age_data)<-c("quant","age","proportion")
  demog_df<-rbind(demog_df,age_data)
  
}

demog_df$rev_quant=-demog_df$quant+11
demog_df$percent=rep(c("Lowest 10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","Highest 10%"),21)
vals<-c("Lowest 10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","Highest 10%")

tiff("Denom_plot.tif",height=6,width=6,unit="in",res=300)
  demog_df%>%
    mutate(percent = fct_reorder(percent, rev_quant)) %>%
  ggplot( aes(x = age, y = percent, height=proportion,fill = stat(x))) +
    geom_ridgeline_gradient(scale=10) +
    scale_fill_viridis_c(name = "", option = "C") +
    coord_cartesian(clip = "off") +
    theme_ridges(font_size = 16, grid = TRUE,center_axis_labels = TRUE) +
    xlab("Age")+
    ylab("Percentile of GDP")+
    theme(
     legend.position="none",
    )
  dev.off()


