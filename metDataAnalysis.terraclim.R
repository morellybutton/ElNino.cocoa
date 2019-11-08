#explore influence of patch area and elevation on micro-climate for each month.
#calculated using field micro-climate data

library(tidyverse)
library(lubridate)
library(gridExtra)
#library(ggpubr)
#library(lme4)


setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#open terra climate data
#extract name of all .csvs
#terraclim data
terra_clim<-read_csv(paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies.csv"))

#plot met data for each metstation location - useless
ggplot(terra_clim,aes(Date,vpd)) + geom_line() + theme_classic()

ggplot(terra_clim,aes(Date,ppt)) + geom_line() + theme_classic()# + facet_wrap(~plot,ncol=1)

#ggplot(terra_clim,aes(Date,min_temp)) + geom_line() + theme_classic() + facet_wrap(~plot,ncol=1)

ggplot(terra_clim,aes(Date,tmax)) + geom_line(aes()) + theme_classic()# + facet_wrap(~plot,ncol=1)

ggplot(terra_clim,aes(Date,pet)) + geom_line(aes()) + theme_classic()

#anomalies around year of study, with harvesting dates
#harvest<-data_frame(c("2014-10-01","2015-10-01","2016-10-01"))
#colnames(harvest)<-"harvest.date"
g1<-ggplot(terra_clim %>% filter(year>=2014&year<2017),aes(Date,precip_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Precipitation Anomaly\n[mm]") + xlab("Date") + theme(text=element_text(size=16)) + geom_vline(aes(xintercept=as.Date("2015-10-01")),linetype="dashed",color="red") +
  geom_vline(aes(xintercept=as.Date("2016-10-01")),linetype="dashed",color="red")

g2<-ggplot(terra_clim %>% filter(year>=2014&year<2017),aes(Date,vpd_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Vapour Pressure Deficit\nAnomaly [kPa]") + xlab("Date") + theme(text=element_text(size=16))+ geom_vline(aes(xintercept=as.Date("2015-10-01")),linetype="dashed",color="red") +
  geom_vline(aes(xintercept=as.Date("2016-10-01")),linetype="dashed",color="red")

g3<-ggplot(terra_clim %>% filter(year>=2014&year<2017),aes(Date,tmax_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Maximum Temperature\nAnomaly [C]") + xlab("Date") + theme(text=element_text(size=16))+ geom_vline(aes(xintercept=as.Date("2015-10-01")),linetype="dashed",color="red") +
  geom_vline(aes(xintercept=as.Date("2016-10-01")),linetype="dashed",color="red")
ggpubr::ggarrange(g1,g2,g3,ncol=1,nrow=3,align="hv")
#ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.Anom.Comparison.pdf"))

ggpubr::ggarrange(g1,g2,g3,ncol=1,nrow=3,align="hv")
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Conferences/Agroforestry/TerraClim.Anom.Comparison.pdf",height=6,width=8)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/TerraClim.Anom.Comparison.pdf",height=9,width=8)



#include box around ENSO index > 1.5 (to signify "Strong") and > 2.0 (to signify "Very Strong")
# > 1.5 is 2015-JJA,JAS, 2016-FMA, > 2.0 is 2015-ASO,SON,OND, 2016-NDJ,DJF,JFM
strong2 = data.frame(x1=c(as.Date("1965-07-01"),as.Date("1972-08-01"),as.Date("1982-08-01"),as.Date("1987-06-01"),as.Date("1991-10-01"),as.Date("1997-06-01"),as.Date("2009-11-01"),as.Date("2015-06-01")), 
                     x2=c(as.Date("1965-12-01"),as.Date("1973-01-01"),as.Date("1983-03-01"),as.Date("1987-09-01"),as.Date("1992-03-01"),as.Date("1998-03-01"),as.Date("2010-01-01"),as.Date("2016-03-01")), y1=-Inf, y2=Inf)
v_strong2 = data.frame(x1=c(as.Date("1965-09-01"),as.Date("1972-11-01"),as.Date("1982-09-01"),as.Date("1997-08-01"),as.Date("2015-08-01")), 
                       x2=c(as.Date("1965-11-01"),as.Date("1972-12-01"),as.Date("1983-02-01"),as.Date("1998-01-01"),as.Date("2016-02-01")), y1=-Inf, y2=Inf)
g2<-ggplot(sat_anom,aes(Date,vpd_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(data=strong2,inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong2, inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) +
  xlab("") + ylab("Vapour Pressure Deficity Anomaly [kPa]")
g1<-ggplot(sat_anom,aes(Date,vpd)) + geom_line() + theme_classic() + geom_line(data=met_comp,aes(month,vpd/10),color="green") +
  xlab("") + ylab("VPD [kPa]")
ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3),align="h")
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.VPDComparison.pdf"))

g1<-ggplot(sat_anom,aes(Date,precip)) + geom_line() + geom_line(data=met_ppt,aes(month,Tppt),color="green") +
  xlab("") + ylab("") + theme_classic()

g2<-ggplot(sat_anom,aes(Date,ppt_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(data=strong2,inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong2, inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) +
  xlab("") + ylab("Monthly Precipitation Anomaly [mm]")

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.PrecipComparison.pdf"))

g1<-ggplot(sat_anom,aes(Date,max_temp)) + geom_line() + geom_line(data=met_comp,aes(month,max_temp),color="green") +
  xlab("") + ylab("") + theme_classic()

g2<-ggplot(sat_anom,aes(Date,max_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(data=strong2,inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong2, inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) +
  xlab("") + ylab("Monthly Maximum Temperature Anomaly [C]")

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.MaxTComparison.pdf"))

#combining satellite and ground measurements for comparison

met_comp<-met_comp %>% rename(Date=month,g.max_temp=max_temp,g.min_temp=min_temp,g.vpd=vpd)
met_comp<-left_join(met_comp,sat_anom %>% select(Date,vpd,tmax),by="Date")
met_ppt<-met_ppt %>% rename(Date=month)
met_ppt<-left_join(met_ppt,sat_anom %>% select(Date,precip),by="Date")


#plot the measurements
lm_eqn<-lm(max_temp~g.max_temp,data=met_comp)
g1<-met_comp %>% ggplot() + geom_point(aes(g.max_temp,max_temp)) + theme_classic() + ylab("TerraClim Max T [C]") +
  xlab("Measured Max T [C]") + geom_smooth(aes(g.max_temp,max_temp),method="lm") + 
  annotate("text",x=23,y=30,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn)$adj.r.squared,2)),parse=T)
       
  
lm_eqn2<-lm(min_temp~g.min_temp,data=met_comp)
g2<-met_comp %>% ggplot() + geom_point(aes(g.min_temp,min_temp)) + theme_classic() + ylab("TerraClim Min T [C]") +
  xlab("Measured Min T [C]") + geom_smooth(aes(g.min_temp,min_temp),method="lm") + 
  annotate("text",x=15,y=17,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn2)$adj.r.squared,2)),parse=T)


lm_eqn3<-lm(vpd~g.vpd/10,data=met_comp)
g3<-met_comp %>% ggplot() + geom_point(aes(g.vpd/10,vpd)) + theme_classic() + ylab("TerraClim VPD [kPa]") +
  xlab("Measured VPD [kPa]") + geom_smooth(aes(g.vpd/10,vpd),method="lm") + 
  annotate("text",x=1.0,y=2.0,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn3)$adj.r.squared,2)),parse=T)

lm_eqn4<-lm(precip~Tppt,data=met_ppt)
g4<-met_ppt %>% ggplot() + geom_point(aes(Tppt,precip)) + theme_classic() + ylab("TerraClim Precipitation [mm]") +
  xlab("Measured Precipitation [mm]") + geom_smooth(aes(Tppt,precip),method="lm") +
  annotate("text",x=50,y=350,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn4)$adj.r.squared,2)),parse=T)

ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClimvsGroundMeasures.pdf"))

