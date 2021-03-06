#Comparative figures between 2014/15, 2015/16 and 2016/17 for main ES factors

#library(ggplot2)
#library(reshape)
library(gridExtra)
library(tidyverse)
library(ggpubr)

folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/Kakum"
#pubs folder
ptemp<-"/Publications/2021/YieldResistance/"

setwd(paste0(folder_names,dtemp))

#create dataframe for figures (harvest, flower buds, mean T, water stress, soil moisture, disease incidence, cherelle set)
d.F.plot<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.2014.csv"))
d.F<- d.F.plot %>% filter(tree_size=="all") %>% select(Plot,Distance,HeavyCrop,Chset,FBuds,Flowers,Mist,PropCPB,PropBP,soil.moist,Tmax,maxVPD,stress.mm) %>% 
  rename(CPB=PropCPB,BP=PropBP,SoilMoist=soil.moist,Stress.mm=stress.mm)

d.F<-d.F %>% group_by(Plot) %>% mutate(Flowers=sum(0.95*FBuds,Flowers)) %>% ungroup()

df<- d.F %>% gather(key="variable",value="i2014",c(-Plot,-Distance))

d.F.plot<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.2015.csv"))
d.F<- d.F.plot %>% filter(tree_size=="all") %>% select(Plot,Distance,HeavyCrop,Chset,FBuds,Flowers,Mist,PropCPB,PropBP,soil.moist,Tmax,maxVPD,stress.mm) %>% 
  rename(CPB=PropCPB,BP=PropBP,SoilMoist=soil.moist,Stress.mm=stress.mm)

d.F<-d.F %>% group_by(Plot) %>%   mutate(Flowers=sum(0.95*FBuds,Flowers)) %>% ungroup()
d.F<-d.F %>% gather(key="variable",value="i2015",c(-Plot,-Distance))

df <-left_join(df,d.F %>% select(Plot,variable,i2015),by=c("Plot","variable"))

d.F.plot<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.2016.csv"))
d.F<- d.F.plot %>% filter(tree_size=="all") %>% select(Plot,Distance,HeavyCrop,Chset,FBuds,Flowers,Mist,PropCPB,PropBP,soil.moist,Tmax,maxVPD,stress.mm) %>% 
  rename(CPB=PropCPB,BP=PropBP,SoilMoist=soil.moist,Stress.mm=stress.mm)

d.F<-d.F %>% group_by(Plot) %>% mutate(Flowers=sum(0.95*FBuds,Flowers)) %>% ungroup()
d.F<-d.F %>% gather(key="variable",value="i2016",c(-Plot,-Distance))

df <-left_join(df,d.F %>% select(Plot,variable,i2016),by=c("Plot","variable"))
df$Distance<-factor(df$Distance)

df2<-df %>% filter(variable!="FBuds",variable!="maxVPD")
df2$variable<-factor(df2$variable,labels = c("Heavy Crop [kg tree-1]","Cherelle Set [%]","Flowers","Mistletoe [%]","Capsid Incidence [%]",
                                             "Black Pod Incidence [%]","Soil Moisture [%]","Max Temperature [C]","Water Stress [mm]"))

#plot comparisons between 2014 and 2015
#plot comparisons, yield
g1<-ggplot(df[df$variable=="HeavyCrop",],aes(i2014,i2015,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,4)+xlim(0,4)+xlab("Heavy Crop 2014 [kg tree-1]")+ylab("Heavy Crop 2015 [kg tree-1]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top")
  
g2<-ggplot(df[df$variable=="CPB",],aes(i2014*100,i2015*100,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Capsid Incidence 2014 [%]")+ylab("Capsid Incidence 2015 [%]")+ylim(0,20)+xlim(0,20)+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  )

g3<-ggplot(df[df$variable=="BP",],aes(i2014*100,i2015*100,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Black Pod Incidence 2014 [%]")+ylab("Black Pod Incidence 2015 [%]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  )+ ylim(0,6)+xlim(0,6)

g4<-ggplot(df[df$variable=="SoilMoist",],aes(i2014,i2015,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Soil Moisture 2014")+ylab("Soil Moisture 2015")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(10,25)+xlim(10,25)

g5<-ggplot(df[df$variable=="Tmax",],aes(i2014,i2015,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Maximum Temperature 2014 [C]")+ylab("Maximum Temperature 2015 [C]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(28,41)+xlim(28,41)

g6<-ggplot(df[df$variable=="Stress.mm",],aes(i2014,i2015,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Water Stress 2014 [mm]")+ylab("Water Stress 2015 [mm]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(-50,26)+xlim(-50,26)

ggarrange(g1,g2,g3,g4,g5,g6,nrow=2,ncol=3,common.legend=T)
ggsave(paste0(folder_names,ptemp,"/Cocoa_ES.Factor.Compare.2014_2015.tiff"),height=7,width=10)

#plot comparisons between 2015 and 2016
#plot comparisons, yield
g1<-ggplot(df[df$variable=="HeavyCrop",],aes(i2015,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,6)+xlim(0,6)+xlab("Heavy Crop 2015 [kg tree-1]")+ylab("Heavy Crop 2016 [kg tree-1]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top")

g2<-ggplot(df[df$variable=="CPB",],aes(i2015*100,i2016*100,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Capsid Incidence 2015 [%]")+ylab("Capsid Incidence 2016 [%]")+ylim(0,20)+xlim(0,20)+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  )

g3<-ggplot(df[df$variable=="BP",],aes(i2015*100,i2016*100,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Black Pod Incidence 2015 [%]")+ylab("Black Pod Incidence 2016 [%]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  )+ ylim(0,6)+xlim(0,6)

g4<-ggplot(df[df$variable=="SoilMoist",],aes(i2015,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Soil Moisture 2015")+ylab("Soil Moisture 2016")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(10,25)+xlim(10,25)

g5<-ggplot(df[df$variable=="Tmax",],aes(i2015,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Maximum Temperature 2015 [C]")+ylab("Maximum Temperature 2016 [C]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(28,41)+xlim(28,41)

g6<-ggplot(df[df$variable=="Stress.mm",],aes(i2015,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Water Stress 2015 [mm]")+ylab("Water Stress 2016 [mm]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(-50,26)+xlim(-50,26)

ggarrange(g1,g2,g3,g4,g5,g6,nrow=2,ncol=3,common.legend = T)
ggsave(paste0(folder_names,ptemp,"/Cocoa_ES.Factor.Compare.2015_2016.tiff"),height=7,widt=10)

#plot comparisons between 2014 and 2016
#plot comparisons, yield
g1<-ggplot(df[df$variable=="HeavyCrop",],aes(i2014,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,6)+xlim(0,6)+xlab("Heavy Crop 2014 [kg tree-1]")+ylab("Heavy Crop 2016 [kg tree-1]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top")

g2<-ggplot(df[df$variable=="CPB",],aes(i2014*100,i2016*100,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Capsid Incidence 2014 [%]")+ylab("Capsid Incidence 2016 [%]")+ylim(0,20)+xlim(0,20)+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  )

g3<-ggplot(df[df$variable=="BP",],aes(i2014*100,i2016*100,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Black Pod Incidence 2014 [%]")+ylab("Black Pod Incidence 2016 [%]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  )+ ylim(0,6)+xlim(0,6)

g4<-ggplot(df[df$variable=="SoilMoist",],aes(i2014,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Soil Moisture 2014")+ylab("Soil Moisture 2016")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(10,25)+xlim(10,25)

g5<-ggplot(df[df$variable=="Tmax",],aes(i2014,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Maximum Temperature 2014 [C]")+ylab("Maximum Temperature 2016 [C]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(28,41)+xlim(28,41)

g6<-ggplot(df[df$variable=="Stress.mm",],aes(i2014,i2016,group=Distance))+geom_point(aes(color=Distance))+geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Water Stress 2014 [mm]")+ylab("Water Stress 2016 [mm]")+theme(
    panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),
    legend.key = element_rect(colour = "white", fill = NA),legend.position="top"
  ) + ylim(-50,26)+xlim(-50,26)

ggarrange(g1,g2,g3,g4,g5,g6,nrow=2,ncol=3,common.legend=T)
ggsave(paste0(getwd(),"/Analysis/ElNino/ES.Factor.Compare.2014_2016.pdf"),height=10,widt=10)
