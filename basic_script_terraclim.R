library(lubridate); library(viridis); library(mgcv);
library(rstanarm); library(tidyverse); library(loo); 
options(mc.cores = parallel::detectCores()-1)
library(RcppRoll); library(visreg)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
# --- Data prep ----------------------------------------------------------------------------------
clim <- read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_tmax_1960.csv"), guess_max = 1e5) %>% 
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Date, mean,year,month) %>% rename(tmax=mean) %>% 
  mutate(tmax=tmax/10) %>% 
  arrange(Date) %>% 
  mutate(mx_tmax = roll_maxr(tmax, n=12, fill=NA))
tmp <- clim %>% filter(year>=1960 & year <= 2017) %>% 
  group_by(month) %>% 
  summarize(u_tmax = mean(tmax, na.rm=T), 
            u_mx_tmax = mean(mx_tmax, na.rm=T))
clim <- left_join(clim, tmp, by="month")
clim <- clim %>% mutate(tmax_anom = tmax-u_tmax, 
                        mx_tmax_anom = mx_tmax - u_mx_tmax) 

vpd <- read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_vpd_1960.csv")) %>% 
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Date, mean,year,month) %>% rename(vpd=mean) %>% mutate(vpd=vpd/100)

tmp <- vpd %>% filter(year>=1960 & year <= 2017) %>% 
  group_by(month) %>% 
  summarize(u_vpd = mean(vpd, na.rm=T)) #to get kPa
vpd <- left_join(vpd,tmp,by="month")

clim <- left_join(clim,vpd, by=c("Date","month","year"))

pet <- read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_pet_1960.csv")) %>% 
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Date, mean,year,month) %>% rename(pet=mean) %>% mutate(pet=pet/10)

tmp <- pet %>% filter(year>=1960 & year <= 2017) %>% 
  group_by(month) %>% 
  summarize(u_pet = mean(pet, na.rm=T))
pet <- left_join(pet,tmp,by="month")

clim <- left_join(clim,pet, by=c("Date","month","year"))

ppt <- read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_ppt_1960.csv")) %>% 
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Date, mean,year,month) %>% rename(ppt=mean) 
tmp <- ppt %>% filter(year>=1960 & year <= 2017) %>% 
  group_by(month) %>% 
  summarize(u_precip = mean(ppt, na.rm=T))
ppt <- left_join(ppt,tmp,by="month")

clim <- left_join(clim,ppt, by=c("Date","month","year"))
clim <- clim %>% mutate(vpd_anom = vpd-u_vpd,pet_anom = pet-u_pet,precip_anom = ppt-u_precip) 

tmp <- clim %>% filter(year>=1960 & year <= 2016) %>% 
  group_by(month) %>% 
  summarize(vpd_sigma = sd(vpd, na.rm=T),
            pet_sigma = sd(pet, na.rm=T), 
            precip_sigma = sd(ppt, na.rm=T),
            mx_tmax_sigma = sd(mx_tmax, na.rm=T),
            tmax_sigma=sd(tmax,na.rm=T))
clim <- left_join(clim, tmp, by='month')

clim <- clim %>% 
  mutate(u_p_et = u_precip/pet) %>%
  mutate(p_et = ppt/pet) %>%
  mutate(u_season = ifelse(u_precip>pet, ("wet"), ("dry")),
         season = ifelse(ppt>pet, ("wet"), ("dry")), 
         p_et100 = ifelse(ppt>100,T,F), 
         p_et_bin = ifelse(ppt>pet, T, F), 
         u_p_et_bin = ifelse(u_precip >= pet, T, F)) %>% 
  mutate(tmax_anom_sigma = tmax_anom/tmax_sigma,
         vpd_anom_sigma = vpd_anom/vpd_sigma, 
         precip_anom_sigma = precip_anom/precip_sigma,
         pet_anom_sigma = pet_anom/pet_sigma)
clim <- clim %>% arrange(Date) %>% 
  mutate(tmax_anom_sigma_3mo = roll_meanr(tmax_anom_sigma, n=3),
         vpd_anom_sigma_3mo = roll_meanr(vpd_anom_sigma, n=3),
         precip_anom_sigma_3mo = roll_meanr(precip_anom_sigma, n=3),
         pet_anom_sigma_3mo = roll_meanr(pet_anom_sigma, n=3)) %>% 
  ungroup()

write_csv(clim,paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies_1960.csv"))



#################
#extract relevant time periods for yield modelling, years 2014-2016
#flowering = Jan-March
#fruiting = July-Oct
yield.tc <- clim %>% filter(year>=2014,month<3|month>6&month<11) %>% 
  mutate(flowering=1,flowering=replace(flowering,month>3,0)) %>%
  mutate(fruiting=1,fruiting=replace(fruiting,month<7|month>10,0)) %>%
  group_by(year,site,fruiting,flowering) %>%
  summarise(tmax=mean(tmax,na.rm=T),vpd=mean(vpd,na.rm=T),
            tmax_anom=mean(tmax_anom,na.rm=T),vpd_anom=mean(vpd_anom,na.rm=T),
            p_et=mean(p_et,na.rm=T),u_p_et=mean(u_p_et,na.rm=T)) %>% ungroup()

yield.flower<-yield.tc %>% filter(flowering==1) %>% select(-fruiting,-flowering) %>%
  rename(tmax.flower=tmax,vpd.flower=vpd,tmax.anom.flower=tmax_anom,
         vpd_anom.flower=vpd_anom,p_et.flower=p_et,u_p_et.flower=u_p_et)

yield.fruit<-yield.tc %>% filter(fruiting==1) %>% select(-fruiting,-flowering) %>%
  rename(tmax.fruit=tmax,vpd.fruit=vpd,tmax.anom.fruit=tmax_anom,
         vpd_anom.fruit=vpd_anom,p_et.fruit=p_et,u_p_et.fruit=u_p_et)

yield.all<-left_join(yield.flower,yield.fruit, by=c("site","year"))
write_csv(yield.all,paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies4yieldmodel.csv"))

