#create figures comparing metdata measures and soil moisture measures before, during and after el nino

library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#load metdata
met_data <- read_csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))
plot_data <-read_csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")

plot_data <- plot_data %>% mutate(Plot=gsub(" ","",name3))
met_data <- left_join(met_data,plot_data %>% select(Plot,age,transect,distance,distance_1,Gap_Jan15,Gap_Jun15,PlotCode))

#remove noisy data from HMFP
hmfp <- met_data %>% filter(Plot=="HMFP"&month<"2017-01-01")
met_data <- met_data %>% filter(Plot!="HMFP")
met_data<-bind_rows(met_data,hmfp)

met_summ <- met_data %>% group_by(distance,month) %>% summarise(maxT=mean(maxT,na.rm=T),minT=mean(minT,na.rm=T),meanT=mean(meanT,na.rm=T),maxVPD=mean(maxVPD,na.rm=T),meanRH=mean(meanRH,na.rm=T),stress.mm=mean(stress.mm,na.rm=T),stress.mm.se=sd(stress.mm,na.rm=T)/sqrt(length(stress.mm))) 
#%>% mutate(dec_date=decimal_date(month))

#include box around ENSO index > 1.5 (to signify "Strong") and > 2.0 (to signify "Very Strong")
# > 1.5 is 2015-JJA,JAS, 2016-FMA, > 2.0 is 2015-ASO,SON,OND, 2016-NDJ,DJF,JFM
strong = data.frame(x1=c(as.Date("2015-06-01"),as.Date("2016-02-01")), x2=c(as.Date("2015-08-01"),as.Date("2016-03-01")), y1=c(-Inf,-Inf), y2=c(Inf,Inf))
v_strong = data.frame(x1=as.Date("2015-08-01"), x2=as.Date("2016-02-01"), y1=-Inf, y2=Inf)
wet_start = data.frame(x1=c(as.Date("2014-09-01"),as.Date("2015-03-01"),as.Date("2015-09-01"),as.Date("2016-03-01"),as.Date("2016-09-01"),as.Date("2017-03-01")))
wet_end = data.frame(x2=c(as.Date("2014-06-01"),as.Date("2014-11-01"),as.Date("2015-06-01"),as.Date("2015-11-01"),as.Date("2016-06-01"),as.Date("2016-11-01"),as.Date("2017-06-01")))
#water stress
g1<-met_summ %>% ggplot() + 
  #geom_point(aes(month,stress.mm,color=factor(distance))) + 
  geom_line(aes(month,stress.mm,color=factor(distance))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  geom_text(aes(x=min(wet_start$x1),label="Wet Season Start\n",y=50),color="blue",angle=90,text=element_text(size=11))+
  geom_text(aes(x=min(wet_end$x2),label="Wet Season End\n",y=50),color="black",angle=90,text=element_text(size=11))+
  scale_color_discrete(name="Distance from\nForest")+
  theme_classic() + xlab("") + ylab("Water Stress [mm]")+theme(legend.position="none")

g2<-met_summ %>% ggplot() + 
  #geom_point(aes(month,maxVPD,color=factor(distance))) + 
  geom_line(aes(month,maxVPD,color=factor(distance))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + scale_color_discrete(name="Distance from\nForest")+
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Maximum VPD [hPa]")+theme(legend.position="none")

g3<-met_summ %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,maxT,color=factor(distance))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + scale_color_discrete(name="Distance from\nForest")+
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("Date") + ylab("Maximum Temperature [C]")+theme(legend.position="bottom")

g4<-grid.arrange(g1,g2,g3,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.MicroClimate.Measures.pdf"),g4,height=9,width=6)

#create figure of ppt
met_ppt<-read_csv(paste0(getwd(),"/MetData/LargeMetstation_ppt.csv"))
harmat<-data.frame(x1=as.Date("2015-01-01"), x2=as.Date("2016-01-01"), x3=as.Date("2017-01-01"))
g4<-met_ppt %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,Tppt)) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Monthly Precipitation [mm]") +
  geom_text(aes(x=harmat$x1,y=250,label="Harmattan"),color="grey",size=3.5)+geom_text(aes(x=harmat$x2,y=250,label="Harmattan"),color="grey",size=3.5)+
  geom_text(aes(x=harmat$x3,y=250,label="Harmattan"),color="grey",size=3.5)+
  geom_text(aes(x=wet_start$x1[2],y=200,label="\nFirst Wet Season"),color="blue",angle=90,size=3)+
  geom_text(aes(x=wet_start$x1[1],y=200,label="\nShort Wet Season"),color="blue",angle=90,size=3) 

g1<-met_summ %>% ggplot() + 
  #geom_point(aes(month,stress.mm,color=factor(distance))) + 
  geom_line(aes(month,stress.mm,color=factor(distance))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  #geom_text(aes(x=min(wet_start$x1),label="Wet Season Start\n",y=50),color="blue",angle=90,text=element_text(size=11))+
  #geom_text(aes(x=min(wet_end$x2),label="Wet Season End\n",y=50),color="black",angle=90,text=element_text(size=11))+
  scale_color_discrete(name="Distance from\nForest")+
  theme_classic() + xlab("") + ylab("Water Stress [mm]")+theme(legend.position="none")

g5<-grid.arrange(g4,g1,g2,g3,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.MicroClimate.Measures.wppt.pdf"),g5,height=10,width=8)

met_month<-read_csv(paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))

#look at soil measures
met_ssumm <- met_month %>% group_by(dist,month) %>% summarise(ah=mean(ah,na.rm=T),vwc=mean(vwc,na.rm=T),stavg=mean(stavg,na.rm=T),stmax=mean(stmax,na.rm=T),stmin=mean(stmin,na.rm=T)) 
#remove -Inf values
met_ssumm <- met_ssumm %>% filter(stmax!="-Inf")

g4<-met_ppt %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,Tppt)) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Monthly Precipitation\n[mm]") +
  geom_text(aes(x=harmat$x1,y=250,label="Harmattan"),color="grey",size=3.5)+geom_text(aes(x=harmat$x2,y=250,label="Harmattan"),color="grey",size=3.5)+
  geom_text(aes(x=harmat$x3,y=250,label="Harmattan"),color="grey",size=3.5)+
  geom_text(aes(x=wet_start$x1[2],y=200,label="\nFirst Wet Season"),color="blue",angle=90,size=3)+
  geom_text(aes(x=wet_start$x1[1],y=200,label="\nShort Wet Season"),color="blue",angle=90,size=3) 

g6<-met_ssumm %>% ggplot() +
  geom_line(aes(month,ah,color=factor(dist))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="Distance from Forest")+
  theme_classic() + xlab("") + ylab("Absolute Humidity\n[kg/m3]")+theme(legend.position = "none")

g7<-met_ssumm %>% ggplot() +
  geom_line(aes(month,vwc,color=factor(dist))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="Distance from Forest")+
  theme_classic() + xlab("") + ylab("Volumetric Water\nContent")+theme(legend.position = "none")

g8<-met_ssumm %>% ggplot() +
  geom_line(aes(month,stmax,color=factor(dist))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="Distance from Forest")+
  theme_classic() + xlab("") + ylab("Maximum Soil\nTemperature [C]")+theme(legend.position = "bottom")

g9<-grid.arrange(g4,g6,g7,g8,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.Soil.Measures.wppt.pdf"),g9,height=10,width=8)

g2<-met_summ %>% ggplot() + 
  #geom_point(aes(month,maxVPD,color=factor(distance))) + 
  geom_line(aes(month,maxVPD,color=factor(distance))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + scale_color_discrete(name="Distance from Forest")+
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Maximum VPD\n[hPa]")+theme(legend.position="none")

g7<-met_ssumm %>% ggplot() +
  geom_line(aes(month,vwc,color=factor(dist))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="Distance from Forest")+
  theme_classic() + xlab("") + ylab("Soil Volumetric\nWater Content [0/1]")+theme(legend.position = "bottom")

ggarrange(g4,g6,g2,g7,common.legend = TRUE, legend = "bottom",nrow = 4)

ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.Measures.wppt.final.pdf"),height=10,width=8)


#calculate anomalies from monthly met data
met_month<-read_csv(paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))
met_month<-met_month %>% mutate(g_month=month(month))

#take mean outside of enso months
mean_data <- met_month %>% filter(month<v_strong$x1|month>v_strong$x2) %>% group_by(g_month) %>% summarise(m_ah=mean(ah,na.rm=T),m_tavg=mean(tavg,na.rm=T),m_tmin=mean(tmin,na.rm=T),m_tmax=mean(tmax,na.rm=T),m_mvpd=mean(mvpd,na.rm=T))

met_month <- left_join(met_month,mean_data,by="g_month")
met_month <- met_month %>% mutate(anom_ah=ah-m_ah,anom_tavg=tavg-m_tavg,anom_tmin=tmin-m_tmin,anom_tmax=tmax-m_tmin,anom_vpd=mvpd-m_mvpd)

met_elnino <- met_month %>% filter(month>strong$x1[1]&month<strong$x2[2]) %>% group_by(dist,month) 
#%>% summarise(anom_ah=mean(anom_ah,na.rm=T),anom_tavg=mean(anom_tavg,na.rm=T),anom_tmin=mean(anom_tmin,na.rm=T),
#            anom_tmax=mean(anom_tmax,na.rm=T),anom_vpd=mean(anom_vpd,na.rm=T))
plot_data<-plot_data %>% rename(plotcode=PlotCode)
#add other facets of plots
met_elnino <- left_join(met_elnino,plot_data %>% select(plotcode,name3,distance,age,distance_1,Gap_Jan15),by="plotcode")

write_csv(met_elnino,paste0(getwd(),"/MetData/Monthly_anomalies_enso.csv"))
