#Code for analyzing cocoa yield anomalies during the El Nino
library(tidyverse)
library(ggpubr)

folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/Kakum"
#pubs folder
ptemp<-"Africa/ECOLIMITS1/Pubs/ElNino/Cocoa"

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
g1
ggsave(paste0(folder_names,ptemp,"/YieldComparisons.png"), height=4, width=5)

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
#ggsave("/Users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/YieldResistanceComparisons.pdf", height=5, width=10)

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

library(MuMIn)
library(arm)
library(car)
library(AICcmodavg)
library(lattice)

options(na.action = "na.omit")

corrplot(s,color=T)

#yield anomalies over 3 years, random effects, plot and consistently low-yielding plots, do logratio of yields from 2014
#remove first year
dF.enso<- dF %>% filter(season!="2014/15"&tree_size=="all")
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logenso_norm.pdf"),width=8,height=8)
qqp(dF.enso$HC_log,"norm")
dev.off()

#dF.enso15<- dF %>% filter(season=="2015/16")
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.enso_norm.pdf"),width=8,height=8)
qqp(dF.enso$HeavyCrop,"norm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logenso_norm.pdf"),width=8,height=8)
qqp(dF.enso$HC_log,"norm")
dev.off()

#dF.enso16<- dF %>% filter(season=="2016/17")
#pdf(paste0(getwd(),"/Analysis/ElNino/Plot.enso16_norm.pdf"),width=8,height=8)
#qqp(dF.enso16$HeavyCrop,"norm")
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logenso16_norm.pdf"),width=8,height=8)
#qqp(dF.enso16$HC_log,"norm")
#dev.off()

#check if need mixed model
(fm001<-lmer(HeavyCrop~1+(1|plot/Transect),data=dF.enso,REML=F))
(fm001b<-lmer(HeavyCrop~1+(1|plot),data=dF.enso,REML=F))
(fm001c<-lmer(HeavyCrop~1+(1|Transect),data=dF.enso,REML=F))
#(fm001d<-lmer(HeavyCrop~1,data=dF.enso,REML=F))
(fm001e<-lmer(HeavyCrop~1+(1|Transect),data=dF.enso,REML=F))
(fm001f<-lmer(HeavyCrop~1+(1|plot/Transect),data=dF.enso,REML=F))
(fm001h<-lmer(HeavyCrop~1+(1|season) + (1|Transect),data=dF.enso,REML=F))
(fm001i<-lmer(HeavyCrop~1+(1|season) ,data=dF.enso,REML=F))
(fm001g<-glm(HeavyCrop~1,data=dF.enso))
anova(fm001,fm001b,fm001c,fm001e,fm001f,fm001g,fm001h,fm001i) #use transect and plot as random effects

#full model of log difference in yield (2015/2014 and 2016/2014)
(gm01<-glmer(HeavyCrop~rescale(Age.of.cocoa) + rescale(Cocoa.density) + rescale(Canopy.gap.dry) + rescale(CN.ratio) +
               rescale(Tot.P) + rescale(K.meq) + rescale(Tmax)*rescale(soil.moist) + rescale(Tmax)*rescale(stress.mm) +
               rescale(PropCPB) + rescale(PropBP) + rescale(Tmax)*rescale(Biomass) + rescale(Tmax)*rescale(distance.cont) +
               rescale(No.applications.yr) + rescale(Tmax)*rescale(Shannoni) + rescale(Tmax)*rescale(BALegume) + (1|plot),
             data=dF.enso,family="gaussian"(link='log')))
summary(gm01)
# gm01s<-standardize(gm01)
# summary(gm01s)

r.squaredGLMM(gm01)

#check multicollinearity
vif(gm01)


#order of variables removed 1) rescale(Tmax)*rescale(Biomass), 2) rescale(Tmax)*rescale(soil.moist), 3) rescale(Tmax)*rescale(distance.cont),
#4) rescale(CN.ratio), 5) rescale(PropBP), 6) rescale(K.meq), 7) rescale(distance.cont), 8) rescale(Tmax)*rescale(BALegume),
#9) rescale(BALegume), 10) rescale(Tmax)*rescale(stress.mm), 11) rescale(stress.mm), 12) rescale(Tmax)*rescale(Shannoni),
#13) rescale(Shannoni), 14) rescale(Canopy.gap.dry)

(gm02<-glmer(HeavyCrop~rescale(Age.of.cocoa) + rescale(Cocoa.density)  +
               rescale(Tot.P) + rescale(soil.moist)  + rescale(Tmax) +
               rescale(PropCPB) + rescale(Biomass) +
               rescale(No.applications.yr) + (1|plot),
             data=dF.enso,family="gaussian"(link='log')))
summary(gm02)

r.squaredGLMM(gm02)

#check multicollinearity
vif(gm02)


#check heteroskedasticity
diagnos.enso <- data.frame(Resid = resid(gm02, type = "pearson"), Fitted = fitted(gm02),Variable = dF.enso$plot )
pdf(paste0(getwd(),"/Analysis/ElNino/Plot.ensoyld_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos.enso)
dev.off()

pdf(paste0(getwd(),"/Analysis/ElNino/Plot.ensoyld_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos.enso, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#create figures of significant models
x.gauss<-as.data.frame(summary(gm02)$coefficients)
colnames(x.gauss)<-c("Coefficients","Std_error","t_value","p_value")
x.gauss$Comparison<-rownames(x.gauss)
rownames(x.gauss)<-1:nrow(x.gauss)

x.gauss <- x.gauss %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
  mutate(sig=1) %>% mutate(sig=replace(sig,p_value=="NS",0))
x.gauss$Labels <- c("Intercept","Age of\nCocoa","Cocoa\nDensity","Total\nPhosphorus","Soil\nMoisture",
                    "Maximum\nTemperature","Capside\nIncidence","Distance\nto Biomass","Avg Fertiliser\nApplications [yr-1]")

#order by size of effect and Importance factor
x.gauss <- x.gauss %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.gauss))

#write.csv(x.gauss,paste0(getwd(),"/Analysis/ElNino/Finalmodel_glmgaussloglink.csv"))
x.gauss<-read.csv(paste0(getwd(),"/Analysis/ElNino/Finalmodel_glmgaussloglink.csv"))

x.gauss$Labels<-factor(x.gauss$Labels,levels=x.gauss[order(x.gauss$Importance,decreasing=T),"Labels"])
#x.gauss$shades<-factor(x.gauss$shades,levels=c("Elevation","Patch Area","Landscape\nInteraction","Leguminous Trees","Shade Diversity",
#                                               "Shade Tree\nInteraction","Disease","Other"),ordered=T)

#remove intercept
x.gauss<-x.gauss %>% filter(Labels!="Intercept")

g1<-ggplot(x.gauss, aes(x = Labels, y = Coefficients, ymin = Coefficients-Std_error, ymax = Coefficients+Std_error)) + 
  geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(size=5,aes(color=factor(sig)),shape=15) + 
  scale_color_manual(values = c("gray","black")) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on El Nino Yield")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none",legend.title=element_blank()) 

#  annotate("text",x=1,y=0.75,label=paste("R2 = ",signif(t.gamma[1,1], 3)),angle=0,size=4)

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ElNino/Finalmodel_results_ensoyldglmgaussloglink.pdf"),height=8.5,width=7)
ggsave(paste0(folder_names,ptemp,"/Finalmodel_results_ensoyldglmgaussloglink.png"),height=8.5,width=7)


#for 2015 ###########
#(gm15<-lmer(HC_log~Age.of.cocoa + Cocoa.density+Canopy.gap.dry+CN.ratio+Tot.P+K.meq+soil.moist+stress.mm+PropCPB+PropBP+Biomass+Tmax+maxVPD+low.plot+(1|Transect),data=dF.enso15,REML=F))
#summary(gm15)
#gm15s<-standardize(gm15)
#summary(gm15s)

#(gm15<-lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+soil.moist+stress.mm+PropCPB+Tmax+maxVPD,data=dF.enso15))
#summary(gm15)
#gm15s<-standardize(gm15)
#summary(gm15s)

#(ym15<-lmer(HeavyCrop~Age.of.cocoa + Cocoa.density+Canopy.gap.dry+CN.ratio+Tot.P+K.meq+soil.moist+stress.mm+PropCPB+PropBP+Biomass+distance.cont+No.applications.yr+Tmax+maxVPD+(1|Transect),data=dF.enso15,REML=F))
#summary(ym15)
#ym15s<-standardize(ym15)
#summary(ym15s)

#(ym15<-lm(HeavyCrop~Age.of.cocoa+CN.ratio+Tot.P+soil.moist+stress.mm,data=dF.enso15))
#summary(ym15)
#ym15s<-standardize(ym15)
#summary(ym15s)

#(gm15l<-lmer(HeavyCrop~Age.of.cocoa + Cocoa.density+CN.ratio+Tot.P+K.meq+soil.moist+PropCPB+No.applications.yr+VPD_lag+(1|Transect),data=dF.enso15,REML=F))
#summary(gm15l)
#gm15ls<-standardize(gm15l)
#summary(gm15ls)

#(gm15<-lmer(HeavyCrop~Age.of.cocoa + Cocoa.density+CN.ratio+Tot.P+PropCPB+No.applications.yr+(1|Transect),data=dF.enso15,REML=F))
#summary(gm15)
#gm15s<-standardize(gm15)
#summary(gm15s)

#for 2016 ###########
#(gm16<-lmer(HC_log~Age.of.cocoa + Cocoa.density+Canopy.gap.dry+CN.ratio+Tot.P+K.meq+soil.moist+stress.mm+PropCPB+PropBP+Biomass+distance.cont+No.applications.yr+Tmax+maxVPD+low.plot+(1|Transect),data=dF.enso16,REML=F))
#gm16s<-standardize(gm16)
#summary(gm16s)

#(gm16<-lm(HC_log~Age.of.cocoa + Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+soil.moist+stress.mm+PropBP+distance.cont,data=dF.enso16))
#gm16s<-standardize(gm16)
#summary(gm16s)

#(ym16<-lm(HeavyCrop~Age.of.cocoa+Tot.P+PropBP+No.applications.yr+Tmax+maxVPD,data=dF.enso16))
#ym16s<-standardize(ym16)
#summary(ym16s)

#(ym16l<-lm(HeavyCrop~Age.of.cocoa+Canopy.gap.dry+Tot.P+K.meq+stress.mm+PropBP+No.applications.yr+Tmax_lag,data=dF.enso16))
#ym16ls<-standardize(ym16l)
#summary(ym16ls)

# 
# #diagnos.ensoy15 <- data.frame(Resid = resid(ym15s, type = "pearson"), Fitted = fitted(ym15s),Variable = dF.enso15$plot )
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld15_ResidualvFittedValues_all.pdf"),width=8,height=8)
# #xyplot(Resid ~ Fitted, data = diagnos.ensoy15)
# #dev.off()
# 
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld15_qqplotResiduals_all.pdf"),width=8,height=8)
# #qqmath(~Resid, data = diagnos.ensoy15, distribution = qnorm, prepanel = prepanel.qqmathline,
# #       panel = function(x, ...) {
# #         panel.qqmathline(x, ...)
# #         panel.qqmath(x, ...)
# #       })
# #dev.off()
# 
# #diagnos.enso15 <- data.frame(Resid = resid(gm15s, type = "pearson"), Fitted = fitted(gm15s),Variable = dF.enso15$plot )
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logyld15_ResidualvFittedValues_all.pdf"),width=8,height=8)
# #xyplot(Resid ~ Fitted, data = diagnos.enso15)
# #dev.off()
# 
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logyld15_qqplotResiduals_all.pdf"),width=8,height=8)
# #qqmath(~Resid, data = diagnos.enso15, distribution = qnorm, prepanel = prepanel.qqmathline,
# #       panel = function(x, ...) {
# #         panel.qqmathline(x, ...)
# #         panel.qqmath(x, ...)
# #       })
# #dev.off()
# 
# #for lag
# #diagnos.ensol15 <- data.frame(Resid = resid(gm15ls, type = "pearson"), Fitted = fitted(gm15ls),Variable = dF.enso15$plot[!is.na(dF.enso15$VPD_lag)] )
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld15.lag_ResidualvFittedValues_all.pdf"),width=8,height=8)
# #xyplot(Resid ~ Fitted, data = diagnos.ensol15)
# #dev.off()
# 
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld15.lag_qqplotResiduals_all.pdf"),width=8,height=8)
# #qqmath(~Resid, data = diagnos.ensol15, distribution = qnorm, prepanel = prepanel.qqmathline,
# #       panel = function(x, ...) {
# #         panel.qqmathline(x, ...)
# #         panel.qqmath(x, ...)
# #       })
# #dev.off()
# 
# #diagnos.ensoy16 <- data.frame(Resid = resid(ym16s, type = "pearson"), Fitted = fitted(ym16s),Variable = dF.enso16$plot )
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld16_ResidualvFittedValues_all.pdf"),width=8,height=8)
# #xyplot(Resid ~ Fitted, data = diagnos.ensoy16)
# #dev.off()
# 
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld16_qqplotResiduals_all.pdf"),width=8,height=8)
# #qqmath(~Resid, data = diagnos.ensoy16, distribution = qnorm, prepanel = prepanel.qqmathline,
# #       panel = function(x, ...) {
# #         panel.qqmathline(x, ...)
# #         panel.qqmath(x, ...)
# #       })
# #dev.off()
# 
# #diagnos.enso16 <- data.frame(Resid = resid(gm16s, type = "pearson"), Fitted = fitted(gm16s),Variable = dF.enso16$plot )
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logyld16_ResidualvFittedValues_all.pdf"),width=8,height=8)
# #xyplot(Resid ~ Fitted, data = diagnos.enso16)
# #dev.off()
# 
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.logyld16_qqplotResiduals_all.pdf"),width=8,height=8)
# #qqmath(~Resid, data = diagnos.enso16, distribution = qnorm, prepanel = prepanel.qqmathline,
# #       panel = function(x, ...) {
# #         panel.qqmathline(x, ...)
# #         panel.qqmath(x, ...)
# #       })
# #dev.off()
# 
# #for lag model
# #diagnos.ensol16 <- data.frame(Resid = resid(gm16ls, type = "pearson"), Fitted = fitted(gm16ls),Variable = dF.enso16$plot )
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld16.lag_ResidualvFittedValues_all.pdf"),width=8,height=8)
# #xyplot(Resid ~ Fitted, data = diagnos.ensol16)
# #dev.off()#
# 
# #pdf(paste0(getwd(),"/Analysis/ElNino/Plot.yld16.lag_qqplotResiduals_all.pdf"),width=8,height=8)
# #qqmath(~Resid, data = diagnos.ensol16, distribution = qnorm, prepanel = prepanel.qqmathline,
# #       panel = function(x, ...) {
# #         panel.qqmathline(x, ...)
# #         panel.qqmath(x, ...)
# #       })
# #dev.off()
# 
# #full model of anomaly in yield for all years (season and plot as random effect)
# #(gm03<-lm(anom_heavycrop~Cocoa.density+Canopy.gap.dry+CN.ratio+Tot.P+K.meq+soil.moist+PropCPB+PropBP+Biomass+distance.cont+No.applications.yr+Tmax+maxVPD,data=dF.enso))
# #summary(gm03)
# #gm03s<-standardize(gm03)
# #summary(gm03s)
# 
# #options(na.action = "na.fail")
# 
# #gm01d<-dredge(gm02s)
# #dredg.m01<-subset(gm01d,delta<2)
# #write.csv(dredg.m01,paste0(getwd(),"/Analysis/ElNino/Yld.logdiff_dredged01.csv"))
# 
# #ym15d<-dredge(ym15s)
# #dredg.my15<-subset(ym15d,delta<2)
# #write.csv(dredg.my15,paste0(getwd(),"/Analysis/ElNino/Yld.enso15_dredged01.csv"))
# 
# #gm15d<-dredge(gm15s)
# #dredg.m15<-subset(gm15d,delta<2)
# #write.csv(dredg.m15,paste0(getwd(),"/Analysis/ElNino/LogYld.enso15_dredged01.csv"))
# 
# #gm15ld<-dredge(gm15ls)
# #dredg.m15l<-subset(gm15ld,delta<6)
# #write.csv(dredg.m15,paste0(getwd(),"/Analysis/ElNino/Yld.enso15_dredged01.csv"))
# 
# #ym16d<-dredge(ym16s)
# #dredg.my16<-subset(ym16d,delta<2)
# #write.csv(dredg.my16,paste0(getwd(),"/Analysis/ElNino/Yld.enso16_dredged01.csv"))
# 
# #gm16d<-dredge(gm16s)
# #dredg.m16<-subset(gm16d,delta<2)
# #write.csv(dredg.m16,paste0(getwd(),"/Analysis/ElNino/LogYld.enso16_dredged01.csv"))
# 
# #gm16ld<-dredge(gm16ls)
# #dredg.m16l<-subset(gm16ld,delta<2)
# #write.csv(dredg.m16,paste0(getwd(),"/Analysis/ElNino/Yld.enso16_dredged01.csv"))
# 
# #gm02d<-dredge(gm03s)
# 
# #assign candidate set of models manually, removing redundant models using nesting rule
# #cand.set<-list()
# ##delta 2 has 27 models
# #cand.set[[1]]<-standardize(lmer(HC_log~Tot.P+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[2]]<-standardize(lmer(HC_log~Tot.P+(1|plot),data=dF.enso,REML=F))
# #cand.set[[3]]<-standardize(lmer(HC_log~Age.of.cocoa+Tot.P+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[4]]<-standardize(lmer(HC_log~Canopy.gap.dry+Tot.P+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[5]]<-standardize(lmer(HC_log~Age.of.cocoa+Cocoa.density+Canopy.gap.dry+Tot.P+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[6]]<-standardize(lmer(HC_log~Age.of.cocoa+Canopy.gap.dry+Tot.P+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[7]]<-standardize(lmer(HC_log~Age.of.cocoa+Cocoa.density+Canopy.gap.dry+Tot.P+(1|plot),data=dF.enso,REML=F))
# #cand.set[[8]]<-standardize(lmer(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[9]]<-standardize(lmer(HC_log~Tot.P+PropCPB+Tmax+(1|plot),data=dF.enso,REML=F))
# #cand.set[[10]]<-standardize(lmer(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+(1|plot),data=dF.enso,REML=F))
# #cand.set[[11]]<-standardize(lmer(HC_log~Age.of.cocoa+Tot.P+(1|plot),data=dF.enso,REML=F))
# #cand.set[[12]]<-standardize(lmer(HC_log~Tot.P+stress.mm+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[13]]<-standardize(lmer(HC_log~Tot.P+K.meq+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[14]]<-standardize(lmer(HC_log~Canopy.gap.dry+Tot.P+(1|plot),data=dF.enso,REML=F))
# #cand.set[[15]]<-standardize(lmer(HC_log~Cocoa.density+Tot.P+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[16]]<-standardize(lmer(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+(1|plot),data=dF.enso,REML=F))
# #cand.set[[17]]<-standardize(lmer(HC_log~Age.of.cocoa+Canopy.gap.dry+Tot.P+(1|plot),data=dF.enso,REML=F))
# #cand.set[[18]]<-standardize(lmer(HC_log~Age.of.cocoa + Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+(1|plot),data=dF.enso,REML=F))
# #cand.set[[19]]<-standardize(lmer(HC_log~Tot.P+Tmax+(1|plot),data=dF.enso,REML=F))
# #cand.set[[20]]<-standardize(lmer(HC_log~Canopy.gap.dry+Tot.P+K.meq+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[21]]<-standardize(lmer(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[22]]<-standardize(lmer(HC_log~Age.of.cocoa+Tot.P+stress.mm+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[23]]<-standardize(lmer(HC_log~Tot.P+stress.mm+(1|plot),data=dF.enso,REML=F))
# #cand.set[[24]]<-standardize(lmer(HC_log~Tot.P+K.meq+(1|plot),data=dF.enso,REML=F))
# #cand.set[[25]]<-standardize(lmer(HC_log~Tot.P+PropCPB+maxVPD+(1|plot),data=dF.enso,REML=F))
# #cand.set[[26]]<-standardize(lmer(HC_log~Age.of.cocoa+Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+PropCPB+(1|plot),data=dF.enso,REML=F))
# #cand.set[[27]]<-standardize(lmer(HC_log~Cocoa.density+Tot.P+(1|plot),data=dF.enso,REML=F))
# 
# #cand.sety15<-list()
# #delta 2 has three  models
# #cand.sety15[[1]]<-standardize(lm(HeavyCrop~Age.of.cocoa+CN.ratio+Tot.P,data=dF.enso15))
# #cand.sety15[[2]]<-standardize(lm(HeavyCrop~CN.ratio+Tot.P,data=dF.enso15))
# #cand.sety15[[3]]<-standardize(lm(HeavyCrop~Age.of.cocoa+CN.ratio+Tot.P+soil.moist,data=dF.enso15))
# 
# #cand.set15<-list()
# #delta 2 has five  models
# #cand.set15[[1]]<-standardize(lm(HC_log~Tot.P+soil.moist+PropCPB,data=dF.enso15))
# #cand.set15[[2]]<-standardize(lm(HC_log~Tot.P+soil.moist+stress.mm,data=dF.enso15))
# #cand.set15[[3]]<-standardize(lm(HC_log~Tot.P+soil.moist+stress.mm+PropCPB,data=dF.enso15))
# #cand.set15[[4]]<-standardize(lm(HC_log~Tot.P+soil.moist,data=dF.enso15))
# #cand.set15[[5]]<-standardize(lm(HC_log~Cocoa.density+Tot.P+soil.moist+PropCPB,data=dF.enso15))
# 
# 
# #cand.sety16<-list()
# #delta 2 has four models
# #cand.sety16[[1]]<-standardize(lm(HeavyCrop~Age.of.cocoa+Tot.P+No.applications.yr,data=dF.enso16))
# #cand.sety16[[2]]<-standardize(lm(HeavyCrop~Age.of.cocoa+Tot.P+PropBP+No.applications.yr,data=dF.enso16))
# #cand.sety16[[3]]<-standardize(lm(HeavyCrop~Age.of.cocoa+Tot.P+PropBP,data=dF.enso16))
# #cand.sety16[[4]]<-standardize(lm(HeavyCrop~Age.of.cocoa+Tot.P,data=dF.enso16))
# 
# #cand.set16<-list()
# #delta 2 has ten models
# #cand.set16[[1]]<-standardize(lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+PropBP+distance.cont,data=dF.enso16))
# #cand.set16[[2]]<-standardize(lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+stress.mm+PropBP+distance.cont,data=dF.enso16))
# #cand.set16[[3]]<-standardize(lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+soil.moist+stress.mm+PropBP+distance.cont,data=dF.enso16))
# #cand.set16[[4]]<-standardize(lm(HC_log~Age.of.cocoa+Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+stress.mm+PropBP+distance.cont,data=dF.enso16))
# #cand.set16[[5]]<-standardize(lm(HC_log~Age.of.cocoa+Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+PropBP+distance.cont,data=dF.enso16))
# #cand.set16[[6]]<-standardize(lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+soil.moist+PropBP+distance.cont,data=dF.enso16))
# #cand.set16[[7]]<-standardize(lm(HC_log~Age.of.cocoa+Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+soil.moist+stress.mm+PropBP+distance.cont,data=dF.enso16))
# #cand.set16[[8]]<-standardize(lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+K.meq+PropBP,data=dF.enso16))
# #cand.set16[[9]]<-standardize(lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+PropBP,data=dF.enso16))
# #cand.set16[[10]]<-standardize(lm(HC_log~Cocoa.density+Canopy.gap.dry+Tot.P+PropBP+distance.cont,data=dF.enso16))
# 
# #cand.set<-cand.set1
# 
# ##create a vector of names to trace back models in set
# #Modnames.enso <- paste("mod", 1:length(cand.set), sep = " ")
# #Modnames <- paste("mod", 1:length(cand.set), sep = " ")
# #Modnames.15 <- paste("mod", 1:length(cand.set15), sep = " ")
# #Modnames.16 <- paste("mod", 1:length(cand.set16), sep = " ")
# 
# #Modnames.y15 <- paste("mod", 1:length(cand.sety15), sep = " ")
# #Modnames.y16 <- paste("mod", 1:length(cand.sety16), sep = " ")
# 
# ##generate AICc table
# #res.table <-aictab(cand.set = cand.set, modnames = Modnames.enso, sort = TRUE)
# #write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_logdiff.enso_delta2.median.csv"))#
# 
# #res.table <-aictab(cand.set = cand.set15, modnames = Modnames.15, sort = TRUE)
# #write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_HC15.enso_delta2.median.csv"))
# 
# #res.table <-aictab(cand.set = cand.sety15, modnames = Modnames.y15, sort = TRUE)
# #write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_logdiff15.enso_delta2.median.csv"))
# 
# #res.table <-aictab(cand.set = cand.set16, modnames = Modnames.16, sort = TRUE)
# #write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_HC16.enso_delta2.median.csv"))
# 
# #res.table <-aictab(cand.set = cand.sety16, modnames = Modnames.y16, sort = TRUE)
# #write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_logdiff16.enso_delta2.median.csv"))

#both years
#topmodels.avg<-model.avg(cand.set) 
#sink(paste0(getwd(),"/Analysis/ES/Model.Average_logdiff.delta2.txt"))
#summary(topmodels.avg)
# #sink() 
# 
# x1<-as.data.frame(summary(topmodels.avg)$importance)
# x1$Comparison<-rownames(x1)
# colnames(x1)<-c("Importance","Comparison")
# 
# #calculate model average and confidence intervals
# vars<-list()
# #cand.set.14b<-list(cand.set.14[[1]],cand.set.14[[2]],cand.set.14[[3]])
# #lose interaction of BA.legume and Shannon index because BA.legume not commone enough across plots
# for(i in 1:nrow(x1)){
#   vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames.enso)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames =  Modnames.enso)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames =  Modnames.enso)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames =  Modnames.enso)$Upper.CL),stringsAsFactors = F)
# }
# vars.enso<-do.call(rbind.data.frame,vars)
# colnames(vars.enso)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
# vars.enso[nrow(vars.enso)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames.enso)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames.enso)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames.enso)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames.enso)$Upper.CL),stringsAsFactors = F)
# 
# vars.enso[,2:5]<-sapply(vars.enso[,2:5],as.numeric)
# 
# #create figure of coefficients with confidence intervals
# tmp.enso<-as.data.frame(t(topmodels.avg[[2]]))
# tmp.enso$Comparison <- rownames(tmp.enso)
# tmp.enso[,4:7]<-vars.enso[match(tmp.enso$Comparison,vars.enso$Parameter),2:5]
# 
# #add importance
# tmp.enso$Importance<-x1[match(tmp.enso$Comparison,x1$Comparison),"Importance"]
# write.csv(tmp.enso,paste0(getwd(),"/Analysis/ES/Model.Average_logdiff.delta2.confint.csv"))
# 
# #tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))
# tmp.enso<-tmp.enso[!is.na(tmp.enso$full),]
# 
# #for delta 6
# tmp.enso$Comparison<-factor(tmp.enso$Comparison,levels=tmp.enso[order(tmp.enso$Importance,decreasing=F),"Comparison"],
#                           labels=c("Maximum\nVPD","Maximum\nTemperature","Water Stress\n[mm]","Soil\nPotassium","Age of\nCocoa","Cocoa Density","Canopy Gap","Capsid\nIncidence","Soil Phosphorus","(Intercept)"))
# 
# #order by importance
# tmp.enso<-tmp.enso[!is.na(tmp.enso$Importance),]
# 
# g1<-ggplot(tmp.enso, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
#   theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Log Difference of Yield\n(2015/16 & 2016/17)")+
#   xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
#   theme(text = element_text(size = 14)
#         ,axis.text.x=element_text(angle = 45,hjust=1))
# g1+coord_flip()
# ggsave(paste0(getwd(),"/Analysis/ElNino/Model_averaged_results_logdiff.pdf"),height=6,width=6)
# 
# 
# #2015/16
# topmodels.avg.15<-model.avg(cand.set15) 
# sink(paste0(getwd(),"/Analysis/ES/Model.Average_logyld15.delta2.txt"))
# summary(topmodels.avg.15)
# sink() 
# 
# x15<-as.data.frame(summary(topmodels.avg.15)$importance)
# x15$Comparison<-rownames(x15)
# colnames(x15)<-c("Importance","Comparison")
# 
# #calculate model average and confidence intervals
# vars<-list()
# #cand.set.14b<-list(cand.set.14[[1]],cand.set.14[[2]],cand.set.14[[3]])
# #lose interaction of BA.legume and Shannon index because BA.legume not commone enough across plots
# for(i in 1:nrow(x15)){
#   vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Uncond.SE,modavg(cand.set15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Lower.CL,modavg(cand.set15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Upper.CL),stringsAsFactors = F)
# }
# vars.15<-do.call(rbind.data.frame,vars)
# colnames(vars.15)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
# vars.15[nrow(vars.15)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Uncond.SE,modavg(cand.set15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Lower.CL,modavg(cand.set15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Upper.CL),stringsAsFactors = F)
# 
# vars.15[,2:5]<-sapply(vars.15[,2:5],as.numeric)
# 
# #create figure of coefficients with confidence intervals
# tmp.15<-as.data.frame(t(topmodels.avg.15[[2]]))
# tmp.15$Comparison <- rownames(tmp.15)
# tmp.15[,4:7]<-vars.15[match(tmp.15$Comparison,vars.15$Parameter),2:5]
# 
# #add importance
# tmp.15$Importance<-x15[match(tmp.15$Comparison,x15$Comparison),"Importance"]
# #write.csv(tmp.15,paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld15.delta2.confint.csv"))
# 
# tmp.15<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld15.delta2.confint.csv"))
# tmp.15<-tmp.15[!is.na(tmp.15$full),]
# 
# #for delta 6
# tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$Importance,decreasing=F),"Comparison"],
#                             labels=c("Cocoa Density","Water Stress\n[mm]","Capsid\nIncidence","Soil Phosphorus","Soil\nMoisture","(Intercept)"))
# 
# #order by importance
# tmp.15<-tmp.15[!is.na(tmp.15$Importance),]
# 
# g1<-ggplot(tmp.15, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
#   theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Log Difference of Yield\n(2015/16)/(2014/15)")+
#   xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
#   theme(text = element_text(size = 14)
#         ,axis.text.x=element_text(angle = 45,hjust=1))
# g1+coord_flip()
# ggsave(paste0(getwd(),"/Analysis/ElNino/Model_averaged_results_logyld15.pdf"),height=6,width=6)
# 
# #2016/17
# topmodels.avg.16<-model.avg(cand.set16) 
# sink(paste0(getwd(),"/Analysis/ES/Model.Average_logyld16.delta2.txt"))
# summary(topmodels.avg.16)
# sink() 
# 
# x16<-as.data.frame(summary(topmodels.avg.16)$importance)
# x16$Comparison<-rownames(x16)
# colnames(x16)<-c("Importance","Comparison")
# 
# #calculate model average and confidence intervals
# vars<-list()
# #cand.set.14b<-list(cand.set.14[[1]],cand.set.14[[2]],cand.set.14[[3]])
# #lose interaction of BA.legume and Shannon index because BA.legume not commone enough across plots
# for(i in 1:nrow(x16)){
#   vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Uncond.SE,modavg(cand.set16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Lower.CL,modavg(cand.set16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Upper.CL),stringsAsFactors = F)
# }
# vars.16<-do.call(rbind.data.frame,vars)
# colnames(vars.16)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
# vars.16[nrow(vars.16)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Uncond.SE,modavg(cand.set16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Lower.CL,modavg(cand.set16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Upper.CL),stringsAsFactors = F)
# 
# vars.16[,2:5]<-sapply(vars.16[,2:5],as.numeric)
# 
# #create figure of coefficients with confidence intervals
# tmp.16<-as.data.frame(t(topmodels.avg.16[[2]]))
# tmp.16$Comparison <- rownames(tmp.16)
# tmp.16[,4:7]<-vars.16[match(tmp.16$Comparison,vars.16$Parameter),2:5]
# 
# #add importance
# tmp.16$Importance<-x16[match(tmp.16$Comparison,x16$Comparison),"Importance"]
# #write.csv(tmp.16,paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld16.delta2.confint.csv"))
# 
# tmp.16<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld16.delta2.confint.csv"))
# tmp.16<-tmp.16[!is.na(tmp.16$full),]
# 
# #for delta 6
# tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$Importance,decreasing=F),"Comparison"],
#                           labels=c("Age of\nCocoa","Soil\nMoisture","Water Stress\n[mm]","Distance from\nForest","Soil\nPotassium","Cocoa Density","Canopy Gap","Soil Phosphorus","Black Pod\nIncidence","(Intercept)"))
# 
# #order by importance
# tmp.16<-tmp.16[!is.na(tmp.16$Importance),]
# 
# g1<-ggplot(tmp.16, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
#   theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Log Difference of Yield\n(2016/17)/(2014/15)")+
#   xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
#   theme(text = element_text(size = 14)
#         ,axis.text.x=element_text(angle = 45,hjust=1))
# g1+coord_flip()
# ggsave(paste0(getwd(),"/Analysis/ElNino/Model_averaged_results_logyld16.pdf"),height=6,width=6)
# 
# #Heavy Crop model
# #2015/16
# topmodels.avg.y15<-model.avg(cand.sety15) 
# sink(paste0(getwd(),"/Analysis/ES/Model.Average_yld15.delta2.txt"))
# summary(topmodels.avg.y15)
# sink() 
# 
# x15<-as.data.frame(summary(topmodels.avg.y15)$importance)
# x15$Comparison<-rownames(x15)
# colnames(x15)<-c("Importance","Comparison")
# 
# #calculate model average and confidence intervals
# vars<-list()
# #cand.set.14b<-list(cand.set.14[[1]],cand.set.14[[2]],cand.set.14[[3]])
# #lose interaction of BA.legume and Shannon index because BA.legume not commone enough across plots
# for(i in 1:nrow(x15)){
#   vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.sety15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.y15)$Mod.avg.beta,modavg(cand.sety15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.y15)$Uncond.SE,modavg(cand.sety15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.y15)$Lower.CL,modavg(cand.sety15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.y15)$Upper.CL),stringsAsFactors = F)
# }
# vars.15<-do.call(rbind.data.frame,vars)
# colnames(vars.15)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
# vars.15[nrow(vars.15)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.sety15,"(Intercept)",uncond.se = "revised",modnames = Modnames.y15)$Mod.avg.beta,modavg(cand.sety15,"(Intercept)",uncond.se = "revised",modnames = Modnames.y15)$Uncond.SE,modavg(cand.sety15,"(Intercept)",uncond.se = "revised",modnames = Modnames.y15)$Lower.CL,modavg(cand.sety15,"(Intercept)",uncond.se = "revised",modnames = Modnames.y15)$Upper.CL),stringsAsFactors = F)
# 
# vars.15[,2:5]<-sapply(vars.15[,2:5],as.numeric)
# 
# #create figure of coefficients with confidence intervals
# tmp.15<-as.data.frame(t(topmodels.avg.y15[[2]]))
# tmp.15$Comparison <- rownames(tmp.15)
# tmp.15[,4:7]<-vars.15[match(tmp.15$Comparison,vars.15$Parameter),2:5]
# 
# #add importance
# tmp.15$Importance<-x15[match(tmp.15$Comparison,x15$Comparison),"Importance"]
# #write.csv(tmp.15,paste0(getwd(),"/Analysis/ElNino/Model.Average_yld15.delta2.confint.csv"))
# 
# tmp.15<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_yld15.delta2.confint.csv"))
# tmp.15<-tmp.15[!is.na(tmp.15$full),]
# 
# #for delta 6
# tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$Importance,decreasing=F),"Comparison"],
#                           labels=c("Soil\nMoisture","Age of\nCocoa","Soil C:N","Soil Phosphorus","(Intercept)"))
# 
# #order by importance
# tmp.15<-tmp.15[!is.na(tmp.15$Importance),]
# 
# g1<-ggplot(tmp.15, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
#   theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Yield\n(2015/16)")+
#   xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
#   theme(text = element_text(size = 14)
#         ,axis.text.x=element_text(angle = 45,hjust=1))
# g1+coord_flip()
# ggsave(paste0(getwd(),"/Analysis/ElNino/Model_averaged_results_yld15.pdf"),height=6,width=6)
# 
# #2016/17
# topmodels.avg.y16<-model.avg(cand.sety16) 
# sink(paste0(getwd(),"/Analysis/ES/Model.Average_yld16.delta2.txt"))
# summary(topmodels.avg.y16)
# sink() 
# 
# x16<-as.data.frame(summary(topmodels.avg.y16)$importance)
# x16$Comparison<-rownames(x16)
# colnames(x16)<-c("Importance","Comparison")
# 
# #calculate model average and confidence intervals
# vars<-list()
# #cand.set.14b<-list(cand.set.14[[1]],cand.set.14[[2]],cand.set.14[[3]])
# #lose interaction of BA.legume and Shannon index because BA.legume not commone enough across plots
# for(i in 1:nrow(x16)){
#   vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.sety16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.y16)$Mod.avg.beta,modavg(cand.sety16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.y16)$Uncond.SE,modavg(cand.sety16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.y16)$Lower.CL,modavg(cand.sety16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.y16)$Upper.CL),stringsAsFactors = F)
# }
# vars.16<-do.call(rbind.data.frame,vars)
# colnames(vars.16)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
# vars.16[nrow(vars.16)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.sety16,"(Intercept)",uncond.se = "revised",modnames = Modnames.y16)$Mod.avg.beta,modavg(cand.sety16,"(Intercept)",uncond.se = "revised",modnames = Modnames.y16)$Uncond.SE,modavg(cand.sety16,"(Intercept)",uncond.se = "revised",modnames = Modnames.y16)$Lower.CL,modavg(cand.sety16,"(Intercept)",uncond.se = "revised",modnames = Modnames.y16)$Upper.CL),stringsAsFactors = F)
# 
# vars.16[,2:5]<-sapply(vars.16[,2:5],as.numeric)
# 
# #create figure of coefficients with confidence intervals
# tmp.16<-as.data.frame(t(topmodels.avg.y16[[2]]))
# tmp.16$Comparison <- rownames(tmp.16)
# tmp.16[,4:7]<-vars.16[match(tmp.16$Comparison,vars.16$Parameter),2:5]
# 
# #add importance
# tmp.16$Importance<-x16[match(tmp.16$Comparison,x16$Comparison),"Importance"]
# #write.csv(tmp.16,paste0(getwd(),"/Analysis/ElNino/Model.Average_yld16.delta2.confint.csv"))
# 
# tmp.16<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_yld16.delta2.confint.csv"))
# tmp.16<-tmp.16[!is.na(tmp.16$full),]
# 
# #for delta 6
# tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$Importance,decreasing=F),"Comparison"],
#                           labels=c("Black Pod\nIncidence","Inter-annual Fertiliser\nApplication","Age of\nCocoa","Soil Phosphorus","(Intercept)"))
# 
# #order by importance
# tmp.16<-tmp.16[!is.na(tmp.16$Importance),]
# 
# g1<-ggplot(tmp.16, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
#   theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Yield\n(2016/17)")+
#   xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
#   theme(text = element_text(size = 14)
#         ,axis.text.x=element_text(angle = 45,hjust=1))
# g1+coord_flip()
# ggsave(paste0(getwd(),"/Analysis/ElNino/Model_averaged_results_yld16.pdf"),height=6,width=6)
# 
# #test validity of the model logdiff 2015
# tmp.enso15<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld15.delta2.confint.csv"))
# 
# dF.enso15$z.Tot.P=rescale(dF.enso15$Tot.P)
# dF.enso15$z.PropCPB=rescale(dF.enso15$PropCPB)
# dF.enso15$z.Cocoa.density=rescale(dF.enso15$Cocoa.density)
# dF.enso15$z.stress.mm=rescale(dF.enso15$stress.mm)
# dF.enso15$z.soil.moist=rescale(dF.enso15$soil.moist)
# 
# 
# df<-dF.enso15 %>% group_by(plot) %>% 
#   mutate(HC_log.mod=tmp.enso15[tmp.enso15$Comparison=="(Intercept)","Estimate"]+tmp.enso15[tmp.enso15$Comparison=="z.Tot.P","Estimate"]*z.Tot.P+
#            tmp.enso15[tmp.enso15$Comparison=="z.soil.moist","Estimate"]*z.soil.moist+tmp.enso15[tmp.enso15$Comparison=="z.Cocoa.density","Estimate"]*z.Cocoa.density+
#            tmp.enso15[tmp.enso15$Comparison=="z.PropCPB","Estimate"]*z.PropCPB + tmp.enso15[tmp.enso15$Comparison=="z.stress.mm","Estimate"]*z.stress.mm)
#          
# 
# ggplot(df,aes(HC_log,HC_log.mod)) + geom_point(aes(color=factor(Distance))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
#   ylim(-0.5,1.5)+xlim(-0.5,1.5)+
#   xlab("Observed Log Ratio of Yield")+ylab("Modelled Log Ratio of Yield")+
#   ggtitle("2015/16")+theme_classic() + labs(color="Distance")+
#   theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
# ggsave(paste0(getwd(),"/Analysis/ElNino/Modelled.logyld15.norm.pdf"),width=6,height=6)
# 
# #test validity of the model logdiff 2016
# tmp.enso16<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_logyld16.delta2.confint.csv"))
# 
# dF.enso16$z.Tot.P=rescale(dF.enso16$Tot.P)
# dF.enso16$z.PropBP=rescale(dF.enso16$PropBP)
# dF.enso16$z.Cocoa.density=rescale(dF.enso16$Cocoa.density)
# dF.enso16$z.stress.mm=rescale(dF.enso16$stress.mm)
# dF.enso16$z.soil.moist=rescale(dF.enso16$soil.moist)
# dF.enso16$z.Canopy.gap.dry=rescale(dF.enso16$Canopy.gap.dry)
# dF.enso16$z.K.meq=rescale(dF.enso16$K.meq)
# dF.enso16$z.distance.cont=rescale(dF.enso16$distance.cont)
# dF.enso16$z.Age.of.cocoa=rescale(dF.enso16$Age.of.cocoa)
# 
# df<-dF.enso16 %>% group_by(plot) %>% 
#   mutate(HC_log.mod=tmp.enso16[tmp.enso16$Comparison=="(Intercept)","Estimate"]+tmp.enso16[tmp.enso16$Comparison=="z.Tot.P","Estimate"]*z.Tot.P+
#            tmp.enso16[tmp.enso16$Comparison=="z.soil.moist","Estimate"]*z.soil.moist+tmp.enso16[tmp.enso16$Comparison=="z.Cocoa.density","Estimate"]*z.Cocoa.density+
#            tmp.enso16[tmp.enso16$Comparison=="z.PropBP","Estimate"]*z.PropBP + tmp.enso16[tmp.enso16$Comparison=="z.stress.mm","Estimate"]*z.stress.mm +
#            tmp.enso16[tmp.enso16$Comparison=="z.K.meq","Estimate"]*z.K.meq + tmp.enso16[tmp.enso16$Comparison=="z.Age.of.cocoa","Estimate"]*z.Age.of.cocoa +
#            tmp.enso16[tmp.enso16$Comparison=="z.distance.cont","Estimate"]*z.distance.cont)
# 
# 
# ggplot(df,aes(HC_log,HC_log.mod)) + geom_point(aes(color=factor(Distance))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
#   ylim(-0.5,1.5)+xlim(-0.5,1.5)+
#   xlab("Observed Log Ratio of Yield")+ylab("Modelled Log Ratio of Yield")+
#   ggtitle("2016/17")+theme_classic() + labs(color="Distance")+
#   theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
# ggsave(paste0(getwd(),"/Analysis/ElNino/Modelled.logyld16.norm.pdf"),width=6,height=6)
# 
# #test validity of the model heavy crop 2015
# tmp.enso15<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_yld15.delta2.confint.csv"))
# 
# dF.enso15$z.Tot.P=rescale(dF.enso15$Tot.P)
# dF.enso15$z.Age.of.cocoa =rescale(dF.enso15$Age.of.cocoa)
# dF.enso15$z.CN.ratio=rescale(dF.enso15$CN.ratio)
# dF.enso15$z.soil.moist=rescale(dF.enso15$soil.moist)
# 
# 
# df<-dF.enso15 %>% group_by(plot) %>% 
#   mutate(HeavyCrop.mod=tmp.enso15[tmp.enso15$Comparison=="(Intercept)","Estimate"]+tmp.enso15[tmp.enso15$Comparison=="z.Tot.P","Estimate"]*z.Tot.P+
#            tmp.enso15[tmp.enso15$Comparison=="z.soil.moist","Estimate"]*z.soil.moist+tmp.enso15[tmp.enso15$Comparison=="z.Age.of.cocoa","Estimate"]*z.Age.of.cocoa+
#            tmp.enso15[tmp.enso15$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio)
# 
# 
# ggplot(df,aes(HeavyCrop,HeavyCrop.mod)) + geom_point(aes(color=factor(Distance))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
#   ylim(0,3)+xlim(0,3)+
#   xlab("Observed Heavy Crop Yield [kg/tree]")+ylab("Modelled Heavy Crop Yield [kg/tree]")+
#   ggtitle("2015/16")+theme_classic() + labs(color="Distance")+
#   theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
# ggsave(paste0(getwd(),"/Analysis/ElNino/Modelled.yld15.norm.pdf"),width=6,height=6)
# 
# #test validity of the model heavy crop 2016
# tmp.enso16<-read.csv(paste0(getwd(),"/Analysis/ElNino/Model.Average_yld16.delta2.confint.csv"))
# 
# dF.enso16$z.Tot.P=rescale(dF.enso16$Tot.P)
# dF.enso16$z.Age.of.cocoa =rescale(dF.enso16$Age.of.cocoa)
# dF.enso16$z.No.applications.yr=rescale(dF.enso16$No.applications.yr)
# dF.enso16$z.PropBP=rescale(dF.enso16$PropBP)
# 
# 
# df<-dF.enso16 %>% group_by(plot) %>% 
#   mutate(HeavyCrop.mod=tmp.enso16[tmp.enso16$Comparison=="(Intercept)","Estimate"]+tmp.enso16[tmp.enso16$Comparison=="z.Tot.P","Estimate"]*z.Tot.P+
#            tmp.enso16[tmp.enso16$Comparison=="z.PropBP","Estimate"]*z.PropBP+tmp.enso16[tmp.enso16$Comparison=="z.Age.of.cocoa","Estimate"]*z.Age.of.cocoa+
#            tmp.enso16[tmp.enso16$Comparison=="z.No.applications.yr","Estimate"]*z.No.applications.yr)
# 
# 
# ggplot(df,aes(HeavyCrop,HeavyCrop.mod)) + geom_point(aes(color=factor(Distance))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
#   ylim(0,3)+xlim(0,3)+
#   xlab("Observed Heavy Crop Yield [kg/tree]")+ylab("Modelled Heavy Crop Yield [kg/tree]")+
#   ggtitle("2016/17")+theme_classic() + labs(color="Distance")+
#   theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
# ggsave(paste0(getwd(),"/Analysis/ElNino/Modelled.yld16.norm.pdf"),width=6,height=6)
