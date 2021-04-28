library(lubridate); library(viridis); library(mgcv);
library(rstanarm); library(tidyverse); library(loo); 
options(mc.cores = parallel::detectCores()-1)
library(RcppRoll); library(visreg);
library(plantecophys)

setwd("/Users/AMOREL001/Google Drive/Research/Africa/ECOLIMITS1/ECOLIMITS2019/Kakum/")

# Functions ---------------------------------------------------------------
#taken from FAO (http://www.fao.org/3/X0490E/x0490e07.htm) and originally 
#Murray FW (1967) On the computation of saturation vapor pressure. J. Appl. Meteorol. 6: 203-204
#calc_vpd <- function(d2m_C, t2m_C){
#  e_a <- 0.6108*exp(17.27*d2m_C/(d2m_C+237.3))
#  e_s <- 0.6108*exp(17.27*t2m_C/(t2m_C+237.3))
#  vpd_kPa <- e_s - e_a
#  return(vpd_kPa)
#}

# --- Data prep ----------------------------------------------------------------------------------
clim <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_TMAX_KAKUM.csv"), guess_max = 1e5) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
  mutate(tmax=mean-273.15) %>% 
  #group_by(name) %>% 
  arrange(Date) %>% 
  mutate(mx_tmax = roll_maxr(tmax, n=12, fill=NA)) %>% select(-mean)
tmp <- clim %>% filter(year>=1980 & year <= 2010) %>% 
  group_by(month) %>% 
  summarize(u_tmax = mean(tmax, na.rm=T), 
            u_mx_tmax = mean(mx_tmax, na.rm=T))
clim <- left_join(clim, tmp, by=c("month"))
clim <- clim %>% mutate(tmax_anom = tmax-u_tmax, 
                        mx_tmax_anom = mx_tmax - u_mx_tmax) 

dwpt <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_DWPT_KAKUM.csv")) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
  mutate(dwpt=mean-273.15) %>% 
  #group_by(Plot) %>% 
  arrange(Date) %>% 
  mutate(mn_dwpt = roll_meanr(dwpt, n=12, fill=NA)) %>% select(-mean)

tmean <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_TMEAN_KAKUM.csv")) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
  mutate(tmean=mean-273.15) %>% 
  #group_by(Plot) %>% 
  arrange(Date) %>% 
  mutate(mn_tmean = roll_meanr(tmean, n=12, fill=NA)) %>% select(-mean)
tmp <- tmean %>% filter(year>=1980 & year <= 2010) %>% 
  group_by(month) %>% 
  summarize(u_tmean = mean(tmean, na.rm=T), 
            u_mx_tmean = mean(mn_tmean, na.rm=T))
tmean <- left_join(tmean, tmp, by=c("month"))
clim <- left_join(clim,tmean, by=c("Date","month","year"))

#kpa <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_PRES_KAKUM.csv")) %>%
 # mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
#  select(Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
 # mutate(kpa=mean/1000) %>% 
  #group_by(Plot) %>% 
#  arrange(Date) %>% 
#mutate(mn_kpa = roll_meanr(kpa, n=12, fill=NA)) %>% select(-mean)

#calculated using plantecophys package
#vpd <- tibble(DewtoVPD(Tdew=dwpt$dwpt,TdegC=tmean$tmean,Pa=kpa$kpa))
#colnames(vpd)<-"vpd"
#vpd<-bind_cols(dwpt %>% select(Plot,Date,year,month),vpd)

#tmp <- vpd %>% filter(year>=1980 & year <= 2017) %>% 
#  group_by(Plot,month) %>% 
#  summarize(u_vpd = mean(vpd, na.rm=T)) #to get kPa
#vpd <- left_join(vpd,tmp,by=c("Plot","month"))

#clim <- left_join(clim,vpd, by=c("Plot","Date","month","year"))

#pet <- read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_pet.csv")) %>% 
#  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
#  gather(key="site",value="pet",-Date,-year,-month) %>% mutate(pet=pet/10)
#tmp <- pet %>% filter(year>=1986 & year <= 2017) %>% 
#  group_by(site,month) %>% 
#  summarize(u_pet = mean(pet, na.rm=T))
#pet <- left_join(pet,tmp,by=c("site","month"))

#clim <- left_join(clim,pet, by=c("site","Date","month","year"))

ppt <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_PPT_KAKUM.csv")) %>% 
  select(Date,mean) %>% 
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  mutate(ppt=mean*1000) %>% mutate(wd=ppt-100) %>% mutate(wd=replace(wd,wd>0,0)) %>% 
  select(-mean)
  #gather(key="Plot",value="ppt",-Date,-year,-month)
tmp <- ppt %>% filter(year>=1980 & year <= 2017) %>% 
  group_by(month) %>% 
  summarize(u_precip = mean(ppt, na.rm=T),
            u_wd = mean(wd,na.rm=T))
ppt <- left_join(ppt,tmp,by=c("month"))

clim <- left_join(clim,ppt, by=c("Date","month","year"))
clim <- clim %>% mutate(tmean_anom = tmean-u_tmean,precip_anom = ppt-u_precip,wd_anom=wd-u_wd)

tmp <- clim %>% filter(year>=1980 & year <= 2016) %>% 
  group_by(month) %>% 
  summarize(tmean_sigma = sd(tmean, na.rm=T), 
            precip_sigma = sd(ppt, na.rm=T),
            mx_tmax_sigma = sd(mx_tmax, na.rm=T),
            tmax_sigma=sd(tmax,na.rm=T),
            wd_sigma=sd(wd,na.rm=T))
clim <- left_join(clim, tmp, by=c('month'))

clim <- clim %>% 
  #mutate(u_p_et = u_precip/pet) %>%
  #mutate(p_et = ppt/pet) %>%
  #mutate(u_season = ifelse(u_precip>pet, ("wet"), ("dry")),
  #       season = ifelse(ppt>pet, ("wet"), ("dry")), 
  #       p_et100 = ifelse(ppt>100,T,F), 
   #      p_et_bin = ifelse(ppt>pet, T, F), 
   #      u_p_et_bin = ifelse(u_precip >= pet, T, F)) %>% 
  mutate(tmax_anom_sigma = tmax_anom/tmax_sigma,
         precip_anom_sigma = precip_anom/precip_sigma,
         tmean_anom_sigma = tmean_anom/tmean_sigma,
         wd_anom_sigma = wd_anom/wd_sigma)
         #pet_anom_sigma = pet_anom/pet_sigma)
clim <- clim %>% arrange(Date) %>% 
  mutate(tmax_anom_sigma_3mo = roll_meanr(tmax_anom_sigma, n=3),
         precip_anom_sigma_3mo = roll_meanr(precip_anom_sigma, n=3),
         tmean_anom_sigma_3mo = roll_meanr(tmean_anom_sigma, n=3),
         wd_anom_sigma_3mo = roll_meanr(wd_anom_sigma, n=3)) %>% 
         #pet_anom_sigma_3mo = roll_meanr(pet_anom_sigma, n=3)) %>% 
  ungroup()

write_csv(clim,paste0(getwd(),"/Analysis/ElNino/era5_anomaliesii.csv"))

##################################################################
#Climate Figures
#graph of temperature anomaly and maximum water deficit
era5<-read.csv(paste0(getwd(),"/Analysis/ElNino/era5_anomaliesii.csv"))
oni<-read_csv(paste0(getwd(),"/Analysis/ElNino/ONI.csv"))

era5<-era5 %>% mutate(Date=as.Date(as.character(Date)))

#add "month" value
oni <- oni %>% mutate(Date=as.Date(paste0(PeriodNum,"-01"),format="%Y-%m-%d")) %>% rename(oni=NOAA)

era5<-left_join(era5,oni %>% select(Date,oni),by="Date")

g1<-ggplot(era5 %>% filter(year>=1981),aes(Date,wd_anom,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Anomalies [mm]") + ggtitle("Monthly Water Deficit") +
  ylim(-60,90)+
  theme(text=element_text(size=16),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  annotate("text",x=as.Date("2016-06-01"),y=85,label=paste0("Study\nPeriod"),size=5) +
  geom_vline(aes(xintercept=as.Date("2017-12-01")),linetype="dashed") 
#annotate("text",x=as.Date("2017-01-01"),y=90,label=paste0("Year 2"),angle=90)

g2<-ggplot(era5 %>% filter(year>=1981),aes(Date,tmax_anom,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Anomalies [C]") + ggtitle("Monthly Maximum Temperature") +
  ylim(-5,5)+
  theme(text=element_text(size=16),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  annotate("text",x=as.Date("2016-06-01"),y=4.5,label=paste0("Study\nPeriod"),size=5) +
  geom_vline(aes(xintercept=as.Date("2017-12-01")),linetype="dashed") 
# geom_segment(aes(x = as.Date("2015-01-01"), y = 2.15, xend = as.Date("2015-10-01"), yend = 2.15), size=0.5,arrow = arrow(length = unit(0.5, "cm")))

ggpubr::ggarrange(g1,g2,ncol=1,nrow=2,align="hv",heights=c(1.25,1),labels="auto", common.legend=T)


folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#pubs folder
ptemp<-"/Publications/2021/YieldResistance/"


ggsave(paste0(folder_names,ptemp,"/Era5.Anom.Comparison.tiff"),height=10,width=12)



