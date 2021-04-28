#Code for analyzing cocoa yield anomalies during the El Nino
library(tidyverse)
#library(ggpubr)


folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/Kakum"
#pubs folder
ptemp<-"/Publications/2021/YieldResistance/"

setwd(paste0(folder_names,dtemp))

#look at lag effects of microclimate on yield
dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_anomalies.csv"))
dF.14<-dF %>% filter(season=="2014/15"&tree_size=="all") %>% rename(HeavyCrop14=HeavyCrop,LightCrop14=LightCrop,Tmax14=Tmax,maxVPD14=maxVPD)
dF <- left_join(dF,dF.14 %>% select(plot,HeavyCrop14,LightCrop14,Tmax14,maxVPD14))
#dF <- dF %>% distinct(plot,tree_size,season,.keep_all = T)

dF.15<-dF %>% filter(season=="2015/16"&tree_size=="all") %>% rename(Tmax15=Tmax,maxVPD15=maxVPD,HeavyCrop15=HeavyCrop,LightCrop15=LightCrop)
dF <- left_join(dF,dF.15 %>% select(plot,Tmax15,maxVPD15,HeavyCrop15,LightCrop15))

dF.16<-dF %>% filter(season=="2016/17"&tree_size=="all") %>% rename(HeavyCrop16=HeavyCrop,LightCrop16=LightCrop)
dF <- left_join(dF,dF.16 %>% select(plot,HeavyCrop16,LightCrop16))



#calculate resistance [and resilience] values using Isbell et al (2015) Nature
#resistance = Yn/abs(Ye-Yn)
#where Yn = mean productivity during normal year and Ye is yield during climate shock
#resilience = abs(Ye-Yn)/abs(Y[e+1]-Yn)
#where Y[e+1] = productivity during the year after a climate event.

dF <- dF %>% distinct(plot,tree_size,season,.keep_all = T) %>% 
  mutate(anom_heavycrop14=HeavyCrop-HeavyCrop14,HC_log=log(HeavyCrop/HeavyCrop14),LC_log=log(LightCrop/LightCrop14)) %>% 
  na_if(-Inf) %>% na_if(Inf) 

dF2 <- dF %>% filter(tree_size=="all") %>% mutate(resist.15=HeavyCrop14/abs(HeavyCrop15-HeavyCrop14),resist.16=HeavyCrop14/abs(HeavyCrop16-HeavyCrop14))


#plot yield of farms over three years
yld_summ <- dF2 %>% group_by(season) %>% summarise(yld.kg=mean(HeavyCrop),sd=sd(HeavyCrop))
sm<-aov(HeavyCrop~factor(season),data=dF2)
summary(sm)
TukeyHSD(sm)

g1<-ggplot() + geom_point(data=dF2,aes(factor(season),HeavyCrop),color="light grey") + 
  geom_line(data=dF2,aes(factor(season),HeavyCrop,group=plot),color="light grey") +
  theme(legend.position="none") + ylab("Tree Yield [kg]") + xlab("Season")  +
  theme_classic() + geom_point(data=yld_summ,aes(x=factor(season),y=yld.kg),size=3) +
  geom_errorbar(data=yld_summ,aes(x=factor(season),ymin=yld.kg-sd,ymax=yld.kg+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))

#clim_lag<-bind_rows(dF.14 %>% rename(Tmax_lag=Tmax14,VPD_lag=maxVPD14) %>% select(plot,season,Tmax_lag,VPD_lag),
#                    dF.15 %>% rename(Tmax_lag=Tmax15,VPD_lag=maxVPD15) %>%  select(plot,season,Tmax_lag,VPD_lag))

#clim_lag<-clim_lag %>% mutate(season=replace(season,season=="2015/16","2016/17"),season=replace(season,season=="2014/15","2015/16"))
  
#dF<-left_join(dF,clim_lag,by=c("plot","season"))

#plot resistance of farms over three years
dF3<-bind_rows(dF2 %>% filter(season=="2015/16") %>% select(-resist.16) %>% rename(resist=resist.15),
               dF2 %>% filter(season=="2016/17") %>% select(-resist.15) %>% rename(resist=resist.16))
yld_summ1 <- dF3 %>% group_by(season) %>% summarise(resist=mean(resist))
yld_summ1.sd <- dF3 %>% group_by(season) %>% summarise(sd=sd(resist))
yld_summ1<-left_join(yld_summ1,yld_summ1.sd,by="season")
sm1<-aov(resist~factor(season),data=dF3)
summary(sm1)
TukeyHSD(sm1)

g2<-ggplot() + geom_point(data=dF3,aes(factor(season),resist),color="light grey") + 
  geom_line(data=dF3,aes(factor(season),resist,group=plot),color="light grey") +
  theme(legend.position="none") + ylab("Resistance") + xlab("Season")  +
  theme_classic() + geom_point(data=yld_summ1,aes(x=factor(season),y=resist),size=3) +
  geom_errorbar(data=yld_summ1,aes(x=factor(season),ymin=resist-sd,ymax=resist+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))

ggpubr::ggarrange(g1,g2,ncol=2,nrow=1)
ggsave(paste0(folder_names,ptemp,"/YieldResistanceComparisons.pdf"), height=5, width=10)


#histogram of anomalies
#g1<-ggplot(dF,aes(anom_heavycrop)) + geom_histogram(stat="bin",bins=50,aes(fill=season)) + theme_classic() + xlab("Heavy Crop Anomaly")
#g2<-ggplot(dF,aes(anom_lightcrop)) + geom_histogram(stat="bin",bins=50,aes(fill=season)) + theme_classic() + xlab("Light Crop Anomaly")
#g3<-ggplot(dF,aes(anom_cpb)) + geom_histogram(stat="bin",bins=50,aes(fill=season)) + theme_classic() + xlab("Capsid Loss Anomaly")
#g4<-ggplot(dF,aes(anom_bp)) + geom_histogram(stat="bin",bins=50,aes(fill=season)) + theme_classic() + xlab("Black Pod Loss Anomaly")
#ggpubr::ggarrange(g1,g2,g3,g4,ncol=2,nrow=2,common.legend = T)
#ggsave("/Users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/Anomaly_histograms.pdf")

#histogram of yields
#g1<-ggplot(dF,aes(HeavyCrop)) + geom_freqpoly(stat="bin",bins=25,aes(color=season)) + theme_classic() + xlab("Heavy Crop [kg/tree]")
#g2<-ggplot(dF,aes(LightCrop)) + geom_freqpoly(stat="bin",bins=25,aes(color=season)) + theme_classic() + xlab("Light Crop [kg/tree]")
#g3<-ggplot(dF,aes(PropCPB)) + geom_freqpoly(stat="bin",bins=15,aes(color=season)) + theme_classic() + xlab("Capsid Incidence [Proportion]")
#g4<-ggplot(dF,aes(PropBP)) + geom_freqpoly(stat="bin",bins=15,aes(color=season)) + theme_classic() + xlab("Black Pod Incidence [Proportion]")
#ggpubr::ggarrange(g1,g2,g3,g4,ncol=2,nrow=2,common.legend = T)
#ggsave("/Users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/SeasonalMeasures_histograms.pdf")

#identify farms with consistently low yields
#find bottom 50% of yields for each season
#yield.1 <- dF %>% filter(season=="2014/15"&tree_size=="all") 
#yield.2 <- dF %>% filter(season=="2015/16"&tree_size=="all") 
#q.yield.1<-quantile(yield.1$HeavyCrop)
#q.yield.2<-quantile(yield.2$HeavyCrop)
#yield.3 <- dF %>% filter(season=="2016/17"&tree_size=="all") 
#q.yield.3<-quantile(yield.3$HeavyCrop)

#dF <- dF %>% filter(tree_size=="all") %>% mutate(low.yield=0) %>% mutate(low.yield=replace(low.yield,season=="2014/15"&HeavyCrop<q.yield.1[3],1),
#                                              low.yield=replace(low.yield,season=="2015/16"&HeavyCrop<q.yield.2[3],1),
#                                              low.yield=replace(low.yield,season=="2016/17"&HeavyCrop<q.yield.3[3],1))
#var.yield <- dF %>% group_by(plot) %>% summarise(no=sum(as.numeric(low.yield))) %>% mutate(low.plot=0) %>% mutate(low.plot=replace(low.plot,no==3,1))
#dF <- left_join(dF,var.yield %>% dplyr::select(plot,low.plot))

corr_dF<-dF %>% select(-Transect,-tree_size,-season,-plot)

s<-cor(corr_dF,use="complete.obs")
s[is.na(s)]<-0

#library(MuMIn)
library(arm)
library(car)
#library(AICcmodavg)
library(lattice)

options(na.action = "na.omit")

corrplot(s,color=T)

#yield anomalies over 3 years, random effects, plot and consistently low-yielding plots, do logratio of yields from 2014
#remove first year
dF.normal <- dF %>% filter(season=="2014/15")
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld14_norm.pdf"),width=8,height=8)
qqp(dF.normal$HeavyCrop,"norm")
dev.off()

dF.enso15<- dF %>% filter(season=="2015/16")
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.enso15_norm.pdf"),width=8,height=8)
qqp(dF.enso15$HeavyCrop,"norm")
dev.off()

#pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logenso15_norm.pdf"),width=8,height=8)
#qqp(dF.enso15$HC_log,"norm")
#dev.off()

dF.enso16<- dF %>% filter(season=="2016/17")
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.enso16_norm.pdf"),width=8,height=8)
qqp(dF.enso16$HeavyCrop,"norm")
dev.off()

#pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logenso16_norm.pdf"),width=8,height=8)
#qqp(dF.enso16$HC_log,"norm")
#dev.off()

#Look at all years together
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.allyrs_norm.pdf"),width=8,height=8)
qqp(dF$HeavyCrop,"norm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ElNino/Plot.allyrs_lnorm.pdf"),width=8,height=8)
qqp(dF$HeavyCrop,"lnorm")
dev.off()


#full model of resistence in yield (2015/2016)
#for 2015 ###########
(gm01<-lm(HeavyCrop~rescale(Age.of.cocoa) + rescale(Cocoa.density)+rescale(Canopy.gap.dry)+rescale(CN.ratio)+
            rescale(Tot.P)+rescale(K.meq)+rescale(soil.moist)+rescale(stress.mm)+rescale(PropCPB)+rescale(PropBP)+
            rescale(Biomass)+rescale(distance.cont)+rescale(No.applications.yr)+rescale(Tmax)+rescale(maxVPD)+
            rescale(Shannoni),data=dF.enso15))
summary(gm01)
#gm01s<-standardize(gm01)
#summary(gm01s)

(gm02<-lm(HeavyCrop~rescale(Age.of.cocoa)+rescale(CN.ratio)+
            rescale(Tot.P)+
            rescale(distance.cont)+rescale(No.applications.yr)+
            rescale(Shannoni),data=dF.enso15))
summary(gm02)

#for 2016 ###########
(gm16<-lm(HeavyCrop~rescale(Age.of.cocoa) + rescale(Cocoa.density)+rescale(Canopy.gap.dry)+rescale(CN.ratio)+
            rescale(Tot.P)+rescale(K.meq)+rescale(soil.moist)+rescale(stress.mm)+rescale(PropCPB)+rescale(PropBP)+
            rescale(Biomass)+rescale(distance.cont)+rescale(No.applications.yr)+rescale(Tmax)+rescale(maxVPD)+
            rescale(Shannoni),data=dF.enso16))
summary(gm16)

(gm16b<-lm(HeavyCrop~rescale(Age.of.cocoa)+
             rescale(Tot.P)+
            rescale(No.applications.yr)+rescale(Tmax)+rescale(maxVPD),data=dF.enso16))
summary(gm16b)

#check heteroskedasticity
diagnos.enso15 <- data.frame(Resid = resid(gm02, type = "pearson"), Fitted = fitted(gm02),Variable = dF.enso15$plot )
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.resist15_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos.enso15)
dev.off()

pdf(paste0(getwd(),"/Analysis/ElNino/Plot.resist15_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos.enso15, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()


diagnos.enso16 <- data.frame(Resid = resid(gm16b, type = "pearson"), Fitted = fitted(gm16b),Variable = dF.enso16$plot )
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.resist16_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos.enso16)
dev.off()

pdf(paste0(getwd(),"/Analysis/ElNino/Plot.resist16_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos.enso16, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

options(na.action = "na.fail")

gm01d<-dredge(gm02)
dredg.m01<-subset(gm01d,delta<2)
write.csv(dredg.m01,paste0(getwd(),"/Analysis/ElNino/Resist15_dredged01.csv"))

gm16d<-dredge(gm16b)
dredg.m16<-subset(gm16d,delta<2)
write.csv(dredg.m16,paste0(getwd(),"/Analysis/ElNino/Resist16_dredged01.csv"))

#assign candidate set of models manually, removing redundant models using nesting rule
cand.set15<-list()
#delta 2 has five  models
cand.set15[[1]]<-lm(HeavyCrop~rescale(Age.of.cocoa)+rescale(CN.ratio)+
                      rescale(Tot.P)+rescale(No.applications.yr),data=dF.enso15)
cand.set15[[2]]<-lm(HeavyCrop~rescale(Age.of.cocoa)+rescale(CN.ratio)+
                      rescale(Tot.P)+rescale(No.applications.yr)+
                      rescale(Shannoni),data=dF.enso15)

cand.set16<-list()
#delta 2 has ten models
cand.set16[[1]]<-lm(HeavyCrop~rescale(Age.of.cocoa) +
                      rescale(Tot.P) +
                      rescale(No.applications.yr),data=dF.enso16)
cand.set16[[2]]<-lm(HeavyCrop~rescale(Age.of.cocoa)+
                      rescale(Tot.P),data=dF.enso16)

##create a vector of names to trace back models in set
#Modnames.enso <- paste("mod", 1:length(cand.set), sep = " ")
#Modnames <- paste("mod", 1:length(cand.set), sep = " ")
Modnames.15 <- paste("mod", 1:length(cand.set15), sep = " ")
Modnames.16 <- paste("mod", 1:length(cand.set16), sep = " ")

#Modnames.y15 <- paste("mod", 1:length(cand.sety15), sep = " ")
#Modnames.y16 <- paste("mod", 1:length(cand.sety16), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set15, modnames = Modnames.15, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_HC15.resist_delta2.median.csv"))

res.table <-aictab(cand.set = cand.set16, modnames = Modnames.16, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_HC16.resist_delta2.median.csv"))

#2015 
topmodels.avg<-model.avg(cand.set15) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_resist15.delta2.txt"))
summary(topmodels.avg)
sink() 

x.15<-as.data.frame(summary(topmodels.avg)$coefmat.full[,5])
x.15$Comparison<-rownames(x.15)
colnames(x.15)<-c("pvalue","Comparison")

#create figure of coefficients with confidence intervals
tmp.15<-as.data.frame(t(topmodels.avg[[2]]))
tmp.15$Comparison <- rownames(tmp.15)
tmp.15$Lower.CL<-as.numeric(confint(topmodels.avg, full = T)[,1])
tmp.15$Upper.CL<-as.numeric(confint(topmodels.avg, full = T)[,2])

#add importance
tmp.15$pvalue<-x.15[match(tmp.15$Comparison,x.15$Comparison),"pvalue"]
#write.csv(tmp.15,paste0(getwd(),"/Analysis/ES/Model.Average_resist15.delta2.confint.csv"))
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resist15.delta2.confint.csv"))
tmp.15<-tmp.15[!is.na(tmp.15$full),]

tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$pvalue,decreasing=T),"Comparison"],
                          labels=c("Shade Diversity","Age of\nCocoa","Inter-annual Fertiliser\nApplication","Soil C:N","Soil Phosphorus","(Intercept)"))

#order by importance
tmp.15<-tmp.15 %>% filter(Comparison!="(Intercept)")

#add significance column
tmp.15$sig<-1
tmp.15<-tmp.15 %>% mutate(sig=replace(sig,Comparison=="Age of\nCocoa"|Comparison=="Soil C:N"|Comparison=="Soil Phosphorus"|Comparison=="Inter-annual Fertiliser\nApplication",0))

g1<-ggplot(tmp.15, aes(x = Comparison, y = full, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Farm Resistance\n(2015)")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

p1<-g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/Model_averaged_resistance15.pdf",height=6,width=6)

#2016 
topmodels.avg<-model.avg(cand.set16) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_resist16.delta2.txt"))
summary(topmodels.avg)
sink() 

x.16<-as.data.frame(summary(topmodels.avg)$coefmat.full[,5])
x.16$Comparison<-rownames(x.16)
colnames(x.16)<-c("pvalue","Comparison")

#create figure of coefficients with confidence intervals
tmp.16<-as.data.frame(t(topmodels.avg[[2]]))
tmp.16$Comparison <- rownames(tmp.16)
tmp.16$Lower.CL<-as.numeric(confint(topmodels.avg, full = T)[,1])
tmp.16$Upper.CL<-as.numeric(confint(topmodels.avg, full = T)[,2])

#add importance
tmp.16$pvalue<-x.16[match(tmp.16$Comparison,x.16$Comparison),"pvalue"]
#write.csv(tmp.16,paste0(getwd(),"/Analysis/ES/Model.Average_resist16.delta2.confint.csv"))
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resist16.delta2.confint.csv"))
tmp.16<-tmp.16[!is.na(tmp.16$full),]

tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$pvalue,decreasing=T),"Comparison"],
                          labels=c("Inter-annual Fertiliser\nApplication","Age of\nCocoa","Soil Phosphorus","(Intercept)"))

#order by importance
tmp.16<-tmp.16 %>% filter(Comparison!="(Intercept)")

#add significance column
tmp.16$sig<-1
tmp.16<-tmp.16 %>% mutate(sig=replace(sig,Comparison=="Age of\nCocoa"|Comparison=="Soil Phosphorus",0))

g2<-ggplot(tmp.16, aes(x = Comparison, y = full, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Farm Resistance\n(2016)")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

p2<-g2+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/Model_averaged_resistance16.pdf",height=6,width=6)

#cocoa yield model
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC1415_delta6.median.csv"))
tmp<-tmp[!is.na(tmp$full),]

tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],
                       labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Distance\nFrom Forest",
                                "Capsid Incidence","Canopy Gap","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#add significance column
tmp$sig<-0
tmp<-tmp %>% mutate(sig=replace(sig,Comparison=="Canopy Gap",1))

g3<-ggplot(tmp[!is.na(tmp$Importance),], aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) +
  geom_point(shape=15,size=5,aes(color=factor(sig)))+ 
  theme_classic() + ggtitle(paste0("Tree Yield\n(2014"))+geom_hline(yintercept = 0,linetype="dashed") + 
  scale_color_grey()+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

p3<-g3 + coord_flip()

ggpubr::ggarrange(p3,p1,p2,ncol=3,nrow=1)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/ModelsofYield&Resistance.pdf",height=6,width=18)

