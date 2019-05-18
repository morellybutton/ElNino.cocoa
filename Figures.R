#create figures for cocoa el nino analysis

library(tidyverse)
library(ggpubr)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_anomalies.csv"))

#histogram of yields
g1<-ggplot(dF,aes(HeavyCrop)) + geom_freqpoly(stat="bin",bins=25,aes(color=season)) + theme_classic() + xlab("Heavy Crop [kg/tree]")
g2<-ggplot(dF,aes(LightCrop)) + geom_freqpoly(stat="bin",bins=25,aes(color=season)) + theme_classic() + xlab("Light Crop [kg/tree]")
g3<-ggplot(dF,aes(PropCPB)) + geom_freqpoly(stat="bin",bins=15,aes(color=season)) + theme_classic() + xlab("Capsid Incidence [Proportion]")
g4<-ggplot(dF,aes(PropBP)) + geom_freqpoly(stat="bin",bins=15,aes(color=season)) + theme_classic() + xlab("Black Pod Incidence [Proportion]")
ggarrange(g1,g2,g3,g4,ncol=2,nrow=2,common.legend = T)
ggsave("/Users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/SeasonalMeasures_histograms.pdf")

ggplot(dF,aes(HeavyCrop)) + geom_freqpoly(stat="bin",bins=25,aes(color=season),size=1) + theme_classic() + xlab("Heavy Crop [kg/tree]") +
  theme(legend.position="bottom",text=element_text(size=16))
ggsave("/Users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Conferences/Agroforestry/YieldMeasures_histogram.pdf",width=5,height=4)

#create yield model figures, 2014 and yield diff for 2015 & 2016
#cocoa yield model
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC1415_delta6.median.csv"))
tmp<-tmp[!is.na(tmp$full),]

tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Distance\nFrom Forest","Capsid Incidence","Canopy Gap","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#add significance column
tmp$sig<-0
tmp<-tmp %>% mutate(sig=replace(sig,Comparison=="Canopy Gap",1))

g1<-ggplot(tmp[!is.na(tmp$Importance),], aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Tree Yield\n(2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 12),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/Yieldmodel_2014.pdf",height=5,width=5)

#for 2015/16
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld15.delta2.confint.csv"))
tmp.15<-tmp.15[!is.na(tmp.15$full),]

#for delta 6
tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$Importance,decreasing=F),"Comparison"],
                          labels=c("Cocoa Density","Water Stress\n[mm]","Capsid\nIncidence","Soil Phosphorus","Soil\nMoisture","(Intercept)"))

#order by importance
tmp.15<-tmp.15[!is.na(tmp.15$Importance),]

#add significance column
tmp.15$sig<-1
tmp.15<-tmp.15 %>% mutate(sig=replace(sig,Comparison=="Soil\nMoisture"|Comparison=="Soil Phosphorus",0))

g1<-ggplot(tmp.15, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield Difference\n(2015)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 12),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/Yielddiffmodel_2015.pdf",height=5,width=5)

#for 2016/17
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld16.delta2.confint.csv"))
tmp.16<-tmp.16[!is.na(tmp.16$full),]

#for delta 6
tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$Importance,decreasing=F),"Comparison"],
                          labels=c("Age of\nCocoa","Soil\nMoisture","Water Stress\n[mm]","Distance from\nForest","Soil\nPotassium","Cocoa Density","Canopy Gap","Soil Phosphorus","Black Pod\nIncidence","(Intercept)"))

#order by importance
tmp.16<-tmp.16[!is.na(tmp.16$Importance),]

#add significance column
tmp.16$sig<-0
tmp.16<-tmp.16 %>% mutate(sig=replace(sig,Comparison=="Water Stress\n[mm]"|Comparison=="Soil\nMoisture"|Comparison=="Age of\nCocoa",1))

g1<-ggplot(tmp.16, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield Difference\n(2016)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 12),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/Yielddiffmodel_2016.pdf",height=5,width=5)

#positive relationship between soil P and fertiliser application?
dF.all<-dF %>% filter(tree_size=="all") 
#dF.small<- dF %>% filter(tree_size=="small")
#dF.large<- dF %>% filter(tree_size=="large")

tot.p<-lm(Tot.P~No.applications.yr*Biomass+Shannoni+Shade.density,data=dF.all)
summary(tot.p)
tot.p.z<-arm::standardize(tot.p)
summary(tot.p.z)

