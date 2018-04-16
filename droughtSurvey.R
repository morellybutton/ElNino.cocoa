#clean and assess drought survey

library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#load files
plot_data <-read_csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
yield_data <-read_csv(paste0(getwd(),"/Yield/Monthly_HarvestEstimates.csv"))
#go through each worksheet and compare to census
num<-plot_data[grep("FP",plot_data$name3,invert=T),] %>% pull(name3)

out_put<-c()
for(i in 1:length(num)){
  trans<-str_split_fixed(num[i]," ",3)[,1]
  drght <- read_xlsx(path=paste0(getwd(),"/Drought/Drought data_cleaned.xlsx"),sheet=num[i],
                     col_types=c("date","text","text","text","numeric","text","text","text","numeric","text","text","text","text"))
  colnames(drght)<-c("Date",as.character(drght[2,2:4]),"Tag",as.character(drght[2,6:8]),"Diameter",as.character(drght[2,10:13]))
  drght <- drght %>% filter(!is.na(Date))
  #find maximum value of "vigour" for "condition of tree"
  drght <- drght %>% rename(Condition="Condition of Tree",Mortality="No. in Mortality") %>% group_by(Tag) %>% 
    mutate(vigour1=str_split_fixed(Condition,",",3)[,1],vigour2=str_split_fixed(Condition,",",3)[,2],vigour=str_split_fixed(Condition,",",3)[,3]) %>%
    mutate(vigour=replace(vigour,vigour=="",vigour2[vigour==""])) %>%  mutate(vigour=replace(vigour,vigour=="",vigour1[vigour==""])) %>% 
    select(-vigour1,-vigour2) %>% mutate(Mortality=replace(Mortality,vigour==1,1)) 
  
  #census
  census_all<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",num[i]),"_LS.csv"), na=c("NA", "NaN", "")) 
  census_all <- census_all %>% mutate(Tag=as.numeric(Tag)) %>% filter(!is.na(Tag))
  #calculate basal area per tree
  census_all <- census_all %>% mutate(basal_area=pi*(DBH/2000)^2)
  
  #add vigour measure to census
  census_all <- left_join(census_all,drght %>% select(Tag,Mortality,vigour),by="Tag")
  #census_all <- census_all %>% 
    #mutate(vigour=replace(vigour,is.na(vigour)&Flag1!=0,8)) %>% 
    #filter(!is.na(vigour))
  #take proportional "vigour"
  out <- data_frame(plot=gsub(" ","",num[i]),age=as.numeric(plot_data[plot_data$name3==num[i],"age"]),distance=as.numeric(plot_data[plot_data$name3==num[i],"distance"]),vigour=census_all %>% filter(!is.na(vigour)) %>% pull(vigour) %>% as.numeric %>% mean,
                    no.trees=(census_all %>% filter(NSpecies=="Theobroma cacao"&Flag1!=0) %>% nrow),total.ba=(census_all %>% filter(NSpecies=="Theobroma cacao"&Flag1!=0&!is.na(basal_area)) %>% pull(basal_area) %>% sum),
                    no.meas=(census_all %>% filter(!is.na(vigour)&vigour!=1) %>% nrow),meas.ba=(census_all %>% filter(!is.na(vigour)&vigour!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum),
                    mortality=(census_all %>% filter(!is.na(Mortality)) %>% pull(Mortality) %>% as.numeric %>% sum))
  out_put<-bind_rows(out_put,out)
}

#write_csv(out_put,paste0(getwd(),"/Drought/Summary_plot_drought_measures.csv"))
out_put <- read_csv(paste0(getwd(),"/Drought/Summary_plot_drought_measures.csv"))

met_data<-read_csv(paste0(getwd(),"/MetData/Monthly_anomalies_enso.csv"))
met_data <- met_data %>% mutate(plot=gsub(" ","",name3))
enso_plot <- met_data %>% group_by(plot) %>% summarise(ah=mean(anom_ah,na.rm=T),tavg=mean(anom_tavg,na.rm=T),
                                                      tmin=mean(anom_tmin,na.rm=T),tmax=mean(anom_tmin,na.rm=T),
                                                      vpd=mean(anom_vpd,na.rm=T)) %>% gather(key="category",value="anomaly",-plot)
enso_plot <- left_join(enso_plot,out_put,by="plot")  
enso_plot <- enso_plot %>% mutate(perc_ba=meas.ba/total.ba,perc_tree=no.meas/no.trees)

g1<-enso_plot %>% filter(category=="ah"&!is.na(distance)) %>% ggplot(aes(anomaly,vigour)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Absolute Humidity [anomaly]") + ylab("Vigour Measure [1-7]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g2<-enso_plot %>% filter(category=="tavg"&!is.na(distance)) %>% ggplot(aes(anomaly,vigour)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Average Temperature [anomaly]") + ylab("Vigour Measure [1-7]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g3<-enso_plot %>% filter(category=="tmin"&!is.na(distance)) %>% ggplot(aes(anomaly,vigour)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Minimum Temperature [anomaly]") + ylab("Vigour Measure [1-7]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g4<-enso_plot %>% filter(category=="tmax"&!is.na(distance)) %>% ggplot(aes(anomaly,vigour)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Maximum Temperature [anomaly]") + ylab("Vigour Measure [1-7]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g5<-enso_plot %>% filter(category=="vpd"&!is.na(distance)) %>% ggplot(aes(anomaly,vigour)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Maximum Vapur Pressure Deficit [anomaly]") + ylab("Vigour Measure [1-7]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

ggarrange(g1,g2,g4,g5,common.legend = TRUE, legend = "bottom",nrow = 2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ElNino/Vigour.vs.anomalies.pdf"),height=8,width=8)


g1<-enso_plot %>% filter(category=="ah"&!is.na(distance)) %>% ggplot(aes(anomaly,mortality)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Absolute Humidity [anomaly]") + ylab("Mortality [trees]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g2<-enso_plot %>% filter(category=="tavg"&!is.na(distance)) %>% ggplot(aes(anomaly,mortality)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Average Temperature [anomaly]") + ylab("Mortality [trees]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

#g3<-enso_plot %>% filter(category=="tmin"&!is.na(distance)) %>% ggplot(aes(anomaly,vigour)) + geom_point(aes(color=factor(distance))) + theme_classic() +
#  xlab("Minimum Temperature [anomaly]") + ylab("Vigour Measure [1-7]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g4<-enso_plot %>% filter(category=="tmax"&!is.na(distance)) %>% ggplot(aes(anomaly,mortality)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Maximum Temperature [anomaly]") + ylab("Mortality [trees]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g5<-enso_plot %>% filter(category=="vpd"&!is.na(distance)) %>% ggplot(aes(anomaly,mortality)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Maximum Vapur Pressure Deficit [anomaly]") + ylab("Mortality [trees]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

ggarrange(g1,g2,g4,g5,common.legend = TRUE, legend = "bottom",nrow = 2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ElNino/Mortality.vs.anomalies.pdf"),height=8,width=8)

g1<-enso_plot %>% filter(category=="ah"&!is.na(distance)) %>% ggplot(aes(anomaly,perc_tree)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Absolute Humidity [anomaly]") + ylab("Proportion of Trees Affected [0/1]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g2<-enso_plot %>% filter(category=="tavg"&!is.na(distance)) %>% ggplot(aes(anomaly,perc_tree)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Average Temperature [anomaly]") + ylab("Proportion of Trees Affected [0/1]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

#g3<-enso_plot %>% filter(category=="tmin"&!is.na(distance)) %>% ggplot(aes(anomaly,vigour)) + geom_point(aes(color=factor(distance))) + theme_classic() +
#  xlab("Minimum Temperature [anomaly]") + ylab("Vigour Measure [1-7]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g4<-enso_plot %>% filter(category=="tmax"&!is.na(distance)) %>% ggplot(aes(anomaly,perc_tree)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Maximum Temperature [anomaly]") + ylab("Proportion of Trees Affected [0/1]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

g5<-enso_plot %>% filter(category=="vpd"&!is.na(distance)) %>% ggplot(aes(anomaly,perc_tree)) + geom_point(aes(color=factor(distance))) + theme_classic() +
  xlab("Maximum Vapur Pressure Deficit [anomaly]") + ylab("Proportion of Trees Affected [0/1]") + stat_smooth(method="lm") + scale_color_discrete(name="Distance from Forest")

ggarrange(g1,g2,g4,g5,common.legend = TRUE, legend = "bottom",nrow = 2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ElNino/PercTrees.vs.anomalies.pdf"),height=8,width=8)

#compare with yields
strong = data.frame(x1=c(as.Date("2015-06-01"),as.Date("2016-02-01")), x2=c(as.Date("2015-08-01"),as.Date("2016-03-01")), y1=c(-Inf,-Inf), y2=c(Inf,Inf))
v_strong = data.frame(x1=as.Date("2015-08-01"), x2=as.Date("2016-02-01"), y1=-Inf, y2=Inf)
wet_start = data.frame(x1=c(as.Date("2014-09-01"),as.Date("2015-03-01"),as.Date("2015-09-01"),as.Date("2016-03-01"),as.Date("2016-09-01"),as.Date("2017-03-01")))
wet_end = data.frame(x2=c(as.Date("2014-06-01"),as.Date("2014-11-01"),as.Date("2015-06-01"),as.Date("2015-11-01"),as.Date("2016-06-01"),as.Date("2016-11-01"),as.Date("2017-06-01")))

yield_data %>% ggplot() +
  geom_line(aes(Month,Monthly.harvest.tree,group=Plot,color=factor(distance))) + facet_wrap(~transect,ncol=1)+
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="Distance from Forest")+
  theme_classic() + xlab("") + ylab("Monthly Pod Harvests [kg/tree]")+theme(legend.position = "bottom")
ggsave(paste0(getwd(),"/Analysis/ElNino/MonthlyHarvest.withINO.pdf"),height=8,width=12)



library(car)
#do glm analysis to see what is driving drought response
qqp(enso_plot$vigour, "norm")

