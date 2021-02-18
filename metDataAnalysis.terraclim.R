#explore influence of patch area and elevation on micro-climate for each month.
#calculated using field micro-climate data

library(tidyverse)
library(lubridate)
library(gridExtra)
library(mgcv)
library(feather)
#library(ggpubr)
#library(lme4)


setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#open terra climate data
#extract name of all .csvs
#terraclim data
#terra_clim<-read_csv(paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies_1960.csv"))

#era5 data
df_era5 <- read_feather("/users/alex/Documents/Research/Africa/Ghana_ERA5/Alex_Ghana_ERA5_GEM_plots_2019-07-05.feather")

#open ONI values
oni<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ONI.csv")
#add "month" value
oni <- oni %>% mutate(Date=as.Date(paste0(PeriodNum,"-01"),format="%Y-%m-%d")) %>% rename(oni=NOAA)

terra_clim<-left_join(terra_clim,oni %>% select(Date,oni),by="Date")
#add in Time variable
terra_clim<-transform(terra_clim, Time = as.numeric(Date))


## AR(1)
m1 <- gamm(tmax_anom_sigma_3mo ~ s(Time),
           data = terra_clim, correlation = corARMA(form = ~ 1|year, p = 1))

## AR(2)
m2 <- gamm(tmax_anom_sigma_3mo ~s(Time),
           data = terra_clim, correlation = corARMA(form = ~ 1|year, p = 2))


anova(m1$lme, m2$lme)

summary(m2$gam)

ACF <- acf(resid(m2$lme, type = "normalized"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))

#Before drawing the fitted trend, I want to put a simultaneous confidence interval around the estimate. 
#mgcv makes this very easy to do via posterior simulation
tmpf <- tempfile()
curl::curl_download("https://gist.githubusercontent.com/gavinsimpson/d23ae67e653d5bfff652/raw/25fd719c3ab699e48927e286934045622d33b3bf/simulate.gamm.R", tmpf)
source(tmpf)

set.seed(10)
newd <- with(terra_clim, data.frame(Time = seq(min(Time), max(Time), length.out = 200)))
sims <- simulate(m2, nsim = 10000, newdata = newd)

ci <- apply(sims, 1L, quantile, probs = c(0.025, 0.975))
newd <- transform(newd,
                  fitted = predict(m2$gam, newdata = newd),
                  lower  = ci[1, ],
                  upper  = ci[2, ])

newd$Date<-as.Date(newd$Time,origin="1970-01-01")

#plot anomalies for each met data variable
r_squared1<-signif(summary(m2$gam)$r.sq,3)
mylabel = bquote(italic(R)^2 == .(format(r_squared1, digits = 3)))

mc1<-ggplot() + geom_bar(data=terra_clim,aes(Date,tmax_anom_sigma_3mo,fill=oni),stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Maximum Temperature Anomalies") + theme(text=element_text(size=18),legend.title.align=0.5 ) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
    geom_ribbon(data = newd, aes(ymin = lower, ymax = upper, x = Date, y = fitted),
                alpha = 0.2, fill = "grey") +
    geom_line(data = newd, aes(y = fitted, x = Date),size=1)  +
    annotate("text",x=as.Date("2013-12-01"),y=-2,label=mylabel,  size=6) 

#for precip

## AR(1)
m3 <- gamm(precip_anom_sigma_3mo ~ s(Time),
           data = terra_clim, correlation = corARMA(form = ~ 1|year, p = 1))

## AR(2)
m4 <- gamm(precip_anom_sigma_3mo ~s(Time),
           data = terra_clim, correlation = corARMA(form = ~ 1|year, p = 2))


anova(m3$lme, m4$lme)

summary(m4$gam)

ACF <- acf(resid(m4$lme, type = "normalized"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))

#Before drawing the fitted trend, I want to put a simultaneous confidence interval around the estimate. 
#mgcv makes this very easy to do via posterior simulation
tmpf <- tempfile()
curl::curl_download("https://gist.githubusercontent.com/gavinsimpson/d23ae67e653d5bfff652/raw/25fd719c3ab699e48927e286934045622d33b3bf/simulate.gamm.R", tmpf)
source(tmpf)

set.seed(10)
newd1 <- with(terra_clim, data.frame(Time = seq(min(Time), max(Time), length.out = 200)))
sims <- simulate(m4, nsim = 10000, newdata = newd1)

ci <- apply(sims, 1L, quantile, probs = c(0.025, 0.975))
newd1 <- transform(newd1,
                  fitted = predict(m4$gam, newdata = newd),
                  lower  = ci[1, ],
                  upper  = ci[2, ])

newd1$Date<-as.Date(newd1$Time,origin="1970-01-01")

#plot anomalies for each met data variable
r_squared<-signif(summary(m4$gam)$r.sq,3)
mylabel1 = bquote(italic(R)^2 == .(format(r_squared, digits = 3)))


mc2<-ggplot() + geom_bar(data=terra_clim,aes(Date,precip_anom_sigma_3mo,fill=oni),stat="identity")  + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Precipitation Anomalies") + theme(text=element_text(size=18),legend.title.align=0.5 ) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_ribbon(data = newd1, aes(ymin = lower, ymax = upper, x = Date, y = fitted),
              alpha = 0.2, fill = "grey") +
  geom_line(data = newd1, aes(y = fitted, x = Date),size=1)  +
  annotate("text",x=as.Date("2013-12-01"),y=-2,label=mylabel1,  size=6) 
#ggplot(terra_clim,aes(Date,min_temp)) + geom_line() + theme_classic() + facet_wrap(~plot,ncol=1)

ggplot(terra_clim,aes(Date,vpd_anom_sigma_3mo)) + geom_bar(stat="identity") + theme_classic()# + facet_wrap(~plot,ncol=1)

ggplot(terra_clim,aes(Date,pet_anom_sigma_3mo)) + geom_bar(stat="identity") + theme_classic()

#anomalies around year of study, with harvesting dates
#harvest<-data_frame(c("2014-10-01","2015-10-01","2016-10-01"))
#colnames(harvest)<-"harvest.date"
g1<-ggplot(terra_clim %>% filter(year>=2014&year<2017),aes(Date,precip_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Precipitation Anomaly\n[mm]") + xlab("Date") + theme(text=element_text(size=16)) + geom_vline(aes(xintercept=as.Date("2015-10-01")),linetype="dashed",color="red") +
  geom_vline(aes(xintercept=as.Date("2016-10-01")),linetype="dashed",color="red") + geom_vline(aes(xintercept=as.Date("2014-10-01")),linetype="dashed",color="red") +
  annotate("text",x=as.Date("2016-04-01"),y=200,label="Hot & Dry Year",size=8,fontface =2) +
  annotate("text",x=as.Date("2014-04-01"),y=200,label="Normal Year",size=8,fontface =2) +
  annotate("text",x=as.Date("2015-04-01"),y=200,label="Hot Year",size=8,fontface =2) 

g2<-ggplot(terra_clim %>% filter(year>=2014&year<2017),aes(Date,vpd_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Vapour Pressure Deficit\nAnomaly [kPa]") + xlab("Date") + theme(text=element_text(size=16))+ geom_vline(aes(xintercept=as.Date("2015-10-01")),linetype="dashed",color="red") +
  geom_vline(aes(xintercept=as.Date("2016-10-01")),linetype="dashed",color="red") + geom_vline(aes(xintercept=as.Date("2014-10-01")),linetype="dashed",color="red")

g3<-ggplot(terra_clim %>% filter(year>=2014&year<2017),aes(Date,tmax_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Maximum Temperature\nAnomaly [C]") + xlab("Date") + theme(text=element_text(size=16))+ geom_vline(aes(xintercept=as.Date("2015-10-01")),linetype="dashed",color="red") +
  geom_vline(aes(xintercept=as.Date("2016-10-01")),linetype="dashed",color="red")+ geom_vline(aes(xintercept=as.Date("2014-10-01")),linetype="dashed",color="red")

ggpubr::ggarrange(g1,g3,ncol=1,nrow=2,align="hv",labels="auto",heights=c(1.25,1))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/TerraClim.Anom.Comparison.pdf",height=8,width=12)


ggpubr::ggarrange(g1,g2,g3,ncol=1,nrow=3,align="hv")
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Conferences/Agroforestry/TerraClim.Anom.Comparison.pdf",height=6,width=8)


#combining satellite and ground measurements for comparison
#sat_anom<-read_csv(paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies.csv"))

met_ppt<-read_csv(paste0(getwd(),"/MetData/LargeMetstation_ppt.csv"))
met_summ<-read_csv(paste0(getwd(),"/MetData/ESPA_G11_summary.csv"))
met_summ$month <- as.Date(paste(year(met_summ$day),month(met_summ$day),"01",sep="-"),format="%Y-%m-%d")
met_comp <- met_summ %>% group_by(month) %>% summarise(max_temp=mean(Tmax,na.rm=T),min_temp=mean(Tmin,na.rm=T),vpd=mean(VPDmax,na.rm=T))

met_comp<-met_comp %>% rename(Date=month,g.max_temp=max_temp,g.min_temp=min_temp,g.vpd=vpd)
met_comp<-left_join(met_comp,terra_clim %>% select(Date,vpd,tmax),by="Date")
met_ppt<-met_ppt %>% rename(Date=month)
met_ppt<-left_join(met_ppt,terra_clim %>% select(Date,ppt),by="Date")

#plot the measurements
lm_eqn<-lm(tmax~g.max_temp,data=met_comp)
g1<-met_comp %>% ggplot() + geom_point(aes(g.max_temp,tmax)) + theme_classic() + ylab("TerraClim Max T [C]") +
  xlab("Measured Max T [C]") + geom_smooth(aes(g.max_temp,tmax),method="lm") + 
  annotate("text",x=30,y=34,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn)$adj.r.squared,2)),parse=T) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +xlim(27,36) + ylim(27,36)
       

lm_eqn4<-lm(ppt~Tppt,data=met_ppt)
g4<-met_ppt %>% ggplot() + geom_point(aes(Tppt,ppt)) + theme_classic() + ylab("TerraClim Precipitation [mm]") +
  xlab("Measured Precipitation [mm]") + geom_smooth(aes(Tppt,ppt),method="lm") +
  annotate("text",x=50,y=250,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn4)$adj.r.squared,2)),parse=T) +
  xlim(0,300) + ylim(0,300) + geom_abline(intercept=0,slope=1,linetype="dashed")

ggpubr::ggarrange(g1,g4,ncol=2,nrow=1,labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Cocoa/TerraClimvsGroundMeasures.pdf",height=5,width=10)


