#Exploring GAMS and HGAMS for cocoa yield modelling

library(mgcv)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")


#load per tree estimates
dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_anomalies.csv"))
dF.14.tree<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset_ptree.2014.csv"))
dF.15.tree<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset_ptree.2015.csv"))
dF.16.tree<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset_ptree.2016.csv"))

dF.14<-dF %>% filter(season=="2014/15"&tree_size=="all")

#check normality
car::qqp(dF.14$HeavyCrop,"norm")

#explore per yield drivers using basic GAM
yld14_mod1 <- gam(HeavyCrop ~ s(Cocoa.density, k = 5, bs = "tp") + s(Canopy.gap.dry, k = 5, bs = "tp") + s(Age.of.cocoa, k = 5, bs = "tp") + s(soil.moist, k = 5, bs = "tp") + 
                    s(PropCPB, k = 5, bs = "tp") + s(Biomass, k = 5, bs = "tp") + s(No.applications.yr, k = 5, bs = "tp") + s(distance.cont, k = 5, bs = "tp"),
                data=dF.14, method="REML", family="gaussian",select=TRUE)
summary(yld14_mod1)

yld14_mod2 <- gam(HeavyCrop ~ s(Cocoa.density, k = 5, bs = "tp") + s(Canopy.gap.dry, k = 5, bs = "tp") +
                    s(PropCPB, k = 5, bs = "tp") +  s(soil.moist, k = 5, bs = "tp") + s(Biomass, k = 5, bs = "tp") + s(No.applications.yr, k = 5, bs = "tp") + s(distance.cont, k = 5, bs = "tp"),
                  data=dF.14, method="REML", family="gaussian",select=TRUE)
summary(yld14_mod2)

pdf(paste0(getwd(),"/Analysis/ES/GAM.yield_factors.pdf"))
par(mfrow=c(3,3))
plot.gam(yld14_mod2,shade=T)
dev.off()

#run model checks
yld14.fit <- mgcViz::getViz(yld14_mod2)
mgcViz::qq(yld14.fit, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))

gam.check(yld14.fit,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

#save diagnostic plots
#https://stackoverflow.com/questions/22275610/how-to-get-only-the-plots-from-gam-check
b<-yld14_mod2
type <- "deviance"
resid <- residuals(b, type = type)
linpred <- napredict(b$na.action, b$linear.predictors)
observed.y <- napredict(b$na.action, b$y)

pdf(paste0(getwd(),"/Analysis/ES/GAM.yield_diagnostic.pdf"))
par(mfrow=c(2,2))

qq.gam(b, rep = 0, level = 0.9, type = type, rl.col = 2, 
       rep.col = "gray80")
hist(resid, xlab = "Residuals", main = "Histogram of residuals")
plot(linpred, resid, main = "Resids vs. linear pred.", 
         xlab = "linear predictor", ylab = "residuals")
plot(fitted(b), observed.y, xlab = "Fitted Values", 
     ylab = "Response", main = "Response vs. Fitted Values")
abline(a=0,b=1,lty=3)
dev.off()

b1<-plot.gam(yld14_mod2)


#compare age of cocoa to distance

tmp1<-summary(lm(Age.of.cocoa~distance.cont,data=dF.14))
g1<-ggplot(dF.14,aes(distance.cont,Age.of.cocoa)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=40,label = paste0("italic(R) ^ 2 == ",signif(tmp1$adj.r.squared,3)), parse = TRUE)+
  xlab("") + ylab("Age of Cocoa [years]")

tmp2<-summary(lm(Tot.P~distance.cont,data=dF.14))
g2<-ggplot(dF.14,aes(distance.cont,Tot.P)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=12,label = paste0("italic(R) ^ 2 == ",signif(tmp2$adj.r.squared,3)), parse = TRUE)+
  xlab("") + ylab("Available Phosphorus [ppm]")

tmp3<-summary(lm(CN.ratio~distance.cont,data=dF.14))
g3<-ggplot(dF.14,aes(distance.cont,CN.ratio)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=16,label = paste0("italic(R) ^ 2 == ",signif(tmp3$adj.r.squared,3)), parse = TRUE)+
  xlab("Distance from Forest [m]") + ylab("Soil C:N")

tmp4<-summary(lm(K.meq~distance.cont,data=dF.14))
g4<-ggplot(dF.14,aes(distance.cont,K.meq)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=3,label = paste0("italic(R) ^ 2 == ",signif(tmp4$adj.r.squared,3)), parse = TRUE)+
  xlab("Distance from Forest [m]") + ylab("Potassium [meq]")

ggpubr::ggarrange(g1,g2,g3,g4,ncol=2,nrow=2,labels="auto")
ggsave(paste0(getwd(),"/Analysis/ES/DistanceFromForestPredictors.pdf"),width=9,height = 8)
