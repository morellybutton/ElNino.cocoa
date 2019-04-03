#clean and assess drought survey

library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(readxl)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#load files
plot_data <-read_csv("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/plots.csv")
yield_data <-read_csv(paste0(getwd(),"/Yield/Monthly_HarvestEstimates.csv"))
#go through each worksheet and compare to census
num<-plot_data[grep("FP",plot_data$name3,invert=T),] %>% pull(name3)
#get mortality from census
census_dates<-read_csv(paste0(getwd(),"/AGB/ForestPlots/Census.dates.csv"))

out_put<-c()
for(i in 1:length(num)){
  trans<-str_split_fixed(num[i]," ",3)[,1]
  drght <- read_xlsx(path=paste0(getwd(),"/Drought/Drought data_cleaned.xlsx"),sheet=num[i],
                     col_types=c("date","text","text","text","numeric","text","text","text","numeric","text","text","text","text"))
  colnames(drght)<-c("Date",as.character(drght[2,2:4]),"Tag",as.character(drght[2,6:8]),"Diameter",as.character(drght[2,10:13]))
  drght <- drght %>% filter(!is.na(Date))
  #find maximum value of "vigour" for "condition of tree"
  drght <- drght %>% rename(Condition="Condition of Tree",Mortality="No. in Mortality") %>% group_by(Tag) %>% 
    mutate(vigour1=str_split_fixed(Condition,",",3)[,1],vigour2=str_split_fixed(Condition,",",3)[,2],vigour=str_split_fixed(Condition,",",3)[,3]) %>%
    mutate(vigour=replace(vigour,vigour=="",vigour2[vigour==""])) %>%  mutate(vigour=replace(vigour,vigour=="",vigour1[vigour==""])) %>% 
    select(-vigour1,-vigour2) %>% mutate(Mortality=replace(Mortality,vigour==1,1)) 
  
  #census
  census_all<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",num[i]),"_LS.csv"), na=c("NA", "NaN", "")) 
  census_all <- census_all %>% mutate(Tag=as.numeric(Tag)) %>% filter(!is.na(Tag))
  #calculate basal area per tree
  census_all <- census_all %>% mutate(basal_area=pi*(DBH/2000)^2)
  
  #add vigour measure to census
  census_all <- left_join(census_all,drght %>% select(Tag,Mortality,vigour),by="Tag")
  
  census_day <- census_dates %>% filter(plotname==num[i])
  #if Mortality is a "1" but not noted in census need to change Flag1
  census_all <- census_all %>% mutate(Flag1=replace(Flag1,Mortality=="1","0"))
  
  #remove new recruits "an"
  census_all <- census_all %>% filter(Flag1!="an")
  
  #include small tree survey as well
  census_small<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" ","",num[i]),"_SS.csv"), na=c("NA", "NaN", "")) 
  census_small <- census_small %>% mutate(Tag=as.numeric(Tag)) %>% filter(!is.na(Tag)&!is.na(Flag1))
  #calculate basal area per tree
  census_small <- census_small %>% mutate(basal_area=pi*(DBH/2000)^2)
  
  census_day <- census_dates %>% filter(plotname==num[i])
  #if Mortality is a "1" but not noted in census need to change Flag1
  census_all <- census_all %>% mutate(Flag1=replace(Flag1,Mortality=="1","0"))
  
  #remove new recruits "an"
  census_small <- census_small %>% filter(Flag1!="an")
  
  #identify "date" of mortality
  census_all$date_mort <- as.Date(NA)
  census_all <- census_all %>% mutate(date_mort=replace(date_mort,Flag1.1=="0",as.Date(census_day$date[2])))
  census_all <- census_all %>% mutate(date_mort=replace(date_mort,Flag1=="0"&Flag1.1!="0",as.Date(census_day$date[3])))
  census_all <- census_all %>% mutate(date_mort=replace(date_mort,Mortality=="1",as.Date(min(drght$Date,na.rm=T))))
  census_all <- census_all %>% mutate(Mortality=replace(Mortality,is.na(date_mort),0))
  census_all <- census_all %>% mutate(Mortality=replace(Mortality,is.na(Mortality),1))
  
  census_small$date_mort <- as.Date(NA)
  census_small <- census_small %>% mutate(date_mort=replace(date_mort,Flag1.1=="0",as.Date(census_day$date[2])))
  census_small <- census_small %>% mutate(date_mort=replace(date_mort,Flag1=="0"&Flag1.1!="0",as.Date(census_day$date[3])))
  census_small$Mortality <- 0
  census_small <- census_small %>% mutate(Mortality=replace(Mortality,!is.na(date_mort),1))
  
  out1 <- census_all %>% filter(NSpecies=="Theobroma cacao"&!is.na(date_mort))  %>% group_by(date_mort) %>% summarise(mortality=sum(as.numeric(Mortality),na.rm=T),ba.mort=sum(basal_area,na.rm=T))
  out2 <- census_small %>% filter(NSpecies=="Theobroma cacao") %>% group_by(date_mort) %>% summarise(s_mortality=sum(as.numeric(Mortality),na.rm=T),s_ba.mort=sum(basal_area,na.rm=T))
  out2[is.na(out2$date_mort),"s_mortality"]<-census_small %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1) %>% nrow
  out2[is.na(out2$date_mort),"s_ba.mort"]<-census_small %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum
  s_tot.trees=sum(out2$s_mortality,na.rm=T)
  s_tot.ba=sum(out2$s_ba.mort,na.rm=T)
  out2<-out2 %>% mutate(s_no.meas=s_mortality/s_tot.trees,s_meas.ba=s_ba.mort/s_tot.ba)
  if(nrow(out1)==0) out1<-data_frame(date_mort=c(census_day$date[2:3],as.Date(min(drght$Date,na.rm=T))),mortality=NA,ba.mort=NA)
  out1<-left_join(out1,out2,by="date_mort") 
  out1$plot<-gsub(" ","",num[i])
  
  out <- data_frame(plot=gsub(" ","",num[i]),date=as.Date(min(drght$Date,na.rm=T)),age=as.numeric(plot_data[plot_data$name3==num[i],"age"]),distance=as.numeric(plot_data[plot_data$name3==num[i],"distance"]),vigour=census_all %>% filter(!is.na(vigour)) %>% pull(vigour) %>% as.numeric %>% mean,
                    no.trees=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1) %>% nrow),total.ba=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum),
                    no.meas=(census_all %>% filter(!is.na(vigour)&vigour!=1) %>% nrow),meas.ba=(census_all %>% filter(!is.na(vigour)&vigour!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum))
  
  out1 <- out1 %>% rename(date=date_mort)
  #add in dates if no measures
  if(nrow(out1)<3) { out1 <- left_join(census_day %>% select(date) %>% filter(date>min(date)),out1,by="date");
  drght_survey <- data_frame(date=as.Date(min(drght$Date,na.rm=T))); drght_survey<-left_join(drght_survey,out1,by="date");
  out1<-bind_rows(out1,drght_survey)}
  out1$plot <- gsub(" ","",num[i])
  out1 <- out1[order(out1$date),]
  out1 <- out1 %>% mutate(perc_mortality=mortality/out$no.trees,perc_ba=ba.mort/out$total.ba) 
  out1 <- out1 %>% mutate(mortality=replace(mortality,is.na(mortality),0),ba.mort=replace(ba.mort,is.na(ba.mort),0),
                          s_mortality=replace(s_mortality,is.na(s_mortality),0),s_ba.mort=replace(s_ba.mort,is.na(s_ba.mort),0),
                          s_meas.ba=replace(s_meas.ba,is.na(s_meas.ba),0),perc_mortality=replace(perc_mortality,is.na(perc_mortality),0),
                          perc_ba=replace(perc_ba,is.na(perc_ba),0),s_no.meas=replace(s_no.meas,is.na(s_no.meas),0))
  out1$cum_perc_mort<-cumsum(out1$perc_mortality)
  out1$cum_perc_ba<-cumsum(out1$perc_ba)
  out1$cum_s_perc<-cumsum(out1$s_no.meas)
  out1$cum_s_ba<-cumsum(out1$s_meas.ba)
  
  out<-left_join(out1,out,by=c("plot","date"))
  #if(nrow(out)==1) out<-out %>% mutate(age=as.numeric(plot_data[plot_data$name3==num[i],"age"]),distance=as.numeric(plot_data[plot_data$name3==num[i],"distance"]),vigour=census_all %>% filter(!is.na(vigour)) %>% pull(vigour) %>% as.numeric %>% mean,
  #                                                   no.trees=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1) %>% nrow),total.ba=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum),
  #                                                   no.meas=(census_all %>% filter(!is.na(vigour)&vigour!=1) %>% nrow),meas.ba=(census_all %>% filter(!is.na(vigour)&vigour!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum))
  out_put<-bind_rows(out_put,out)
}

#write_csv(out_put,paste0(getwd(),"/Drought/Summary_plot_drought_measures.csv"))
out_put <- read_csv(paste0(getwd(),"/Drought/Summary_plot_drought_measures.csv"))

#do again for forest plots
forest<-plot_data[grep("FP",plot_data$name3),"name3"]

for(i in 1:length(forest)){
  #census
  census_all<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" FP","",forest[i,1]),"_forest.csv"), na=c("NA", "NaN", "")) 
  census_all <- census_all %>% mutate(Tag=as.numeric(Tag)) %>% filter(!is.na(Tag))
  #calculate basal area per tree
  census_all <- census_all %>% mutate(basal_area=pi*(DBH/2000)^2)
  
  census_day <- census_dates %>% filter(plotname==as.character(forest[i,1]))
  #if Mortality is a "1" but not noted in census need to change Flag1
  #census_all <- census_all %>% mutate(Flag1=replace(Flag1,Mortality=="1","0"))
  
  #remove new recruits "an"
  census_all <- census_all %>% filter(Flag1!="an")
  
  #include small tree survey as well
  census_small<-read_csv(paste0(getwd(),"/AGB/ForestPlots/",gsub(" FP","",forest[i,1]),"_forestSS.csv"), na=c("NA", "NaN", "")) 
  census_small <- census_small %>% mutate(Tag=as.numeric(Tag)) %>% filter(!is.na(Tag)&!is.na(Flag1))
  #calculate basal area per tree
  census_small <- census_small %>% mutate(basal_area=pi*(DBH/2000)^2)
  #remove new recruits "an"
  census_small <- census_small %>% filter(Flag1!="an")
  
  #identify "date" of mortality
  census_all$date_mort <- as.Date(NA)
  census_all <- census_all %>% mutate(date_mort=replace(date_mort,Flag1.1=="0",as.Date(census_day$date[2])))
  census_all <- census_all %>% mutate(date_mort=replace(date_mort,Flag1=="0"&Flag1.1!="0",as.Date(census_day$date[3])))
  #census_all <- census_all %>% mutate(date_mort=replace(date_mort,Mortality=="1",as.Date(min(drght$Date,na.rm=T))))
  census_all <- census_all %>% mutate(Mortality=0) %>% mutate(Mortality=replace(Mortality,!is.na(date_mort),1))
  
  census_small$date_mort <- as.Date(NA)
  census_small <- census_small %>% mutate(date_mort=replace(date_mort,Flag1.1=="0",as.Date(census_day$date[2])))
  census_small <- census_small %>% mutate(date_mort=replace(date_mort,Flag1=="0"&Flag1.1!="0",as.Date(census_day$date[3])))
  census_small$Mortality <- 0
  census_small <- census_small %>% mutate(Mortality=replace(Mortality,!is.na(date_mort),1))
  
  out1 <- census_all %>% filter(NSpecies=="Theobroma cacao"&!is.na(date_mort))  %>% group_by(date_mort) %>% summarise(mortality=sum(as.numeric(Mortality),na.rm=T),ba.mort=sum(basal_area,na.rm=T))
  out2 <- census_small %>% filter(NSpecies=="Theobroma cacao") %>% group_by(date_mort) %>% summarise(s_mortality=sum(as.numeric(Mortality),na.rm=T),s_ba.mort=sum(basal_area,na.rm=T))
  out2[is.na(out2$date_mort),"s_mortality"]<-census_small %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1) %>% nrow
  out2[is.na(out2$date_mort),"s_ba.mort"]<-census_small %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum
  s_tot.trees=sum(out2$s_mortality,na.rm=T)
  s_tot.ba=sum(out2$s_ba.mort,na.rm=T)
  out2<-out2 %>% mutate(s_no.meas=s_mortality/s_tot.trees,s_meas.ba=s_ba.mort/s_tot.ba)
  if(nrow(out1)==0) out1<-data_frame(date_mort=c(census_day$date[2:3],as.Date(min(drght$Date,na.rm=T))),mortality=NA,ba.mort=NA)
  out1<-left_join(out1,out2,by="date_mort") 
  out1$plot<-gsub(" ","",num[i])
  
  out <- data_frame(plot=gsub(" ","",num[i]),date=as.Date(min(drght$Date,na.rm=T)),age=as.numeric(plot_data[plot_data$name3==num[i],"age"]),distance=as.numeric(plot_data[plot_data$name3==num[i],"distance"]),vigour=census_all %>% filter(!is.na(vigour)) %>% pull(vigour) %>% as.numeric %>% mean,
                    no.trees=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1) %>% nrow),total.ba=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum),
                    no.meas=(census_all %>% filter(!is.na(vigour)&vigour!=1) %>% nrow),meas.ba=(census_all %>% filter(!is.na(vigour)&vigour!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum))
  
  out1 <- out1 %>% rename(date=date_mort)
  #add in dates if no measures
  if(nrow(out1)<3) { out1 <- left_join(census_day %>% select(date) %>% filter(date>min(date)),out1,by="date");
  drght_survey <- data_frame(date=as.Date(min(drght$Date,na.rm=T))); drght_survey<-left_join(drght_survey,out1,by="date");
  out1<-bind_rows(out1,drght_survey)}
  out1$plot <- gsub(" ","",num[i])
  out1 <- out1[order(out1$date),]
  out1 <- out1 %>% mutate(perc_mortality=mortality/out$no.trees,perc_ba=ba.mort/out$total.ba) 
  out1 <- out1 %>% mutate(mortality=replace(mortality,is.na(mortality),0),ba.mort=replace(ba.mort,is.na(ba.mort),0),
                          s_mortality=replace(s_mortality,is.na(s_mortality),0),s_ba.mort=replace(s_ba.mort,is.na(s_ba.mort),0),
                          s_meas.ba=replace(s_meas.ba,is.na(s_meas.ba),0),perc_mortality=replace(perc_mortality,is.na(perc_mortality),0),
                          perc_ba=replace(perc_ba,is.na(perc_ba),0),s_no.meas=replace(s_no.meas,is.na(s_no.meas),0))
  out1$cum_perc_mort<-cumsum(out1$perc_mortality)
  out1$cum_perc_ba<-cumsum(out1$perc_ba)
  out1$cum_s_perc<-cumsum(out1$s_no.meas)
  out1$cum_s_ba<-cumsum(out1$s_meas.ba)
  
  out<-left_join(out1,out,by=c("plot","date"))
  #if(nrow(out)==1) out<-out %>% mutate(age=as.numeric(plot_data[plot_data$name3==num[i],"age"]),distance=as.numeric(plot_data[plot_data$name3==num[i],"distance"]),vigour=census_all %>% filter(!is.na(vigour)) %>% pull(vigour) %>% as.numeric %>% mean,
  #                                                   no.trees=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1) %>% nrow),total.ba=(census_all %>% filter(NSpecies=="Theobroma cacao"&Mortality!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum),
  #                                                   no.meas=(census_all %>% filter(!is.na(vigour)&vigour!=1) %>% nrow),meas.ba=(census_all %>% filter(!is.na(vigour)&vigour!=1&!is.na(basal_area)) %>% pull(basal_area) %>% sum))
  out_put<-bind_rows(out_put,out)
}

#add month to out_put
out_put$month <- as.Date(paste(year(out_put$date),month(round_date(out_put$date,unit="month")),"01",sep="-"))
#add in mortality survey timing, 1,2 and 3
out_put$survey<-2
out_put <- out_put %>% mutate(survey=replace(survey,month<"2016-03-01",1),survey=replace(survey,month>"2016-03-01",3))

met_data<-read_csv(paste0(getwd(),"/MetData/Monthly_anomalies_enso.csv"))
met_data <- met_data %>% mutate(plot=gsub(" ","",name3))
enso_before <- met_data %>% filter(month<min(out_put$month)) %>% group_by(plot) %>% summarise(ah=mean(anom_ah,na.rm=T),tavg=mean(anom_tavg,na.rm=T),
                                                                                              tmin=mean(anom_tmin,na.rm=T),tmax=mean(anom_tmax,na.rm=T),
                                                                                              vpd=mean(anom_vpd,na.rm=T)) %>% gather(key="category",value="anomaly",-plot)
enso_plot <- met_data %>% filter(month>=min(out_put$month)&month<"2016-03-01") %>% group_by(plot) %>% summarise(ah=mean(anom_ah,na.rm=T),tavg=mean(anom_tavg,na.rm=T),
                                                                                              tmin=mean(anom_tmin,na.rm=T),tmax=mean(anom_tmax,na.rm=T),
                                                                                              vpd=mean(anom_vpd,na.rm=T)) %>% gather(key="category",value="anomaly",-plot)

enso_plot <- left_join(enso_plot,out_put %>% filter(month>"2015-12-01"),by="plot")  
enso_plot <- enso_plot %>% filter(!is.na(month))
#enso_plot <- enso_plot %>% mutate(perc_ba=meas.ba/total.ba,perc_tree=no.meas/no.trees)
enso_before <- left_join(enso_before,out_put %>% filter(month<"2016-01-01"),by="plot")
enso_before <- enso_before %>% filter(!is.na(month))

#combine rows for Tmax
enso_tmax <- bind_rows(enso_plot %>% filter(category=="tmax"),enso_before %>% filter(category=="tmax"))
####CUMULATIVE MORTALITY############
#Tmax vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_perc_mort~anomaly,data=enso_tmax %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_perc_mort~anomaly,data=enso_tmax %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=5,y=0.4,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g1<-ggplot(enso_tmax,aes(anomaly,cum_perc_mort)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Maximum Temperature [anomaly]") + ylab("Cumulative Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_s_perc~anomaly,data=enso_tmax %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_s_perc~anomaly,data=enso_tmax %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st_s<-do.call(rbind.data.frame,r)
colnames(st_s) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st_s<-bind_cols(st_s,p1)
data=data.frame(x=5,y=0.3,lab=c(paste0("R2 = ",signif(st_s[1,"r.adj"],2),";\np = ",signif(st_s[1,"p.value"],2)),
                                paste0("R2 = ",signif(st_s[2,"r.adj"],2),";\np = ",signif(st_s[2,"p.value"],2)),
                                paste0("R2 = ",signif(st_s[3,"r.adj"],2),";\np = ",signif(st_s[3,"p.value"],2))),
                survey=1:3)
#r3<-summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$adj.r.squared
#p3<-as.numeric(summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$coefficients[,"Pr(>|t|)"][2])
g2<-ggplot(enso_tmax,aes(anomaly,cum_s_perc)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Maximum Temperature [anomaly]") + ylab("Cumulative Small Tree Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)#+ geom_text(x=0,y=0.15,label=paste0("R2 = ",signif(r3,2),"; p =",signif(p3,2)))


ggarrange(g1,g2,nrow = 2)
ggsave(paste0(getwd(),"/Analysis/ElNino/CumMortality.vs.Tmax_anomalies.pdf"),height=8,width=12)

#combine rows for Tmin
enso_tmin <- bind_rows(enso_plot %>% filter(category=="tmin"),enso_before %>% filter(category=="tmin"))

#Tmin vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_perc_mort~anomaly,data=enso_tmin %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_perc_mort~anomaly,data=enso_tmin %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=0,y=0.4,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g1<-ggplot(enso_tmin,aes(anomaly,cum_perc_mort)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Minimum Temperature [anomaly]") + ylab("Cumulative Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_s_perc~anomaly,data=enso_tmin %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_s_perc~anomaly,data=enso_tmin %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st_s<-do.call(rbind.data.frame,r)
colnames(st_s) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st_s<-bind_cols(st_s,p1)
data=data.frame(x=0,y=0.3,lab=c(paste0("R2 = ",signif(st_s[1,"r.adj"],2),";\np = ",signif(st_s[1,"p.value"],2)),
                                paste0("R2 = ",signif(st_s[2,"r.adj"],2),";\np = ",signif(st_s[2,"p.value"],2)),
                                paste0("R2 = ",signif(st_s[3,"r.adj"],2),";\np = ",signif(st_s[3,"p.value"],2))),
                survey=1:3)
#r3<-summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$adj.r.squared
#p3<-as.numeric(summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$coefficients[,"Pr(>|t|)"][2])
g2<-ggplot(enso_tmin,aes(anomaly,cum_s_perc)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Minimum Temperature [anomaly]") + ylab("Cumulative Small Tree Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)#+ geom_text(x=0,y=0.15,label=paste0("R2 = ",signif(r3,2),"; p =",signif(p3,2)))


ggarrange(g1,g2,nrow = 2)
ggsave(paste0(getwd(),"/Analysis/ElNino/CumMortality.vs.Tmin_anomalies.pdf"),height=8,width=12)

#explore actual values rather than anomalies
met_before <- met_data %>% filter(month<min(out_put$month)) %>% group_by(plot) %>% summarise(ah=mean(ah,na.rm=T),tavg=mean(tavg,na.rm=T),
                                                                                              tmin=mean(tmin,na.rm=T),tmax=mean(tmax,na.rm=T),
                                                                                              vpd=mean(mvpd,na.rm=T)) %>% gather(key="category",value="mean",-plot)
met_plot <- met_data %>% filter(month>=min(out_put$month)&month<"2016-03-01") %>% group_by(plot) %>% summarise(ah=mean(ah,na.rm=T),tavg=mean(tavg,na.rm=T),
                                                                                                                tmin=mean(tmin,na.rm=T),tmax=mean(tmax,na.rm=T),
                                                                                                                vpd=mean(mvpd,na.rm=T)) %>% gather(key="category",value="mean",-plot)

met_plot <- left_join(met_plot,out_put %>% filter(month>"2015-12-01"),by="plot")  
met_plot <- met_plot %>% filter(!is.na(month))
#enso_plot <- enso_plot %>% mutate(perc_ba=meas.ba/total.ba,perc_tree=no.meas/no.trees)
met_before <- left_join(met_before,out_put %>% filter(month<"2016-01-01"),by="plot")
met_before <- met_before %>% filter(!is.na(month))

#combine rows for Tmax
met_tmax <- bind_rows(met_plot %>% filter(category=="tmax"),met_before %>% filter(category=="tmax"))

#Tmax vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_perc_mort~mean,data=met_tmax %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_perc_mort~mean,data=met_tmax %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=30,y=0.4,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g1<-ggplot(met_tmax,aes(mean,cum_perc_mort)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Maximum Temperature [C]") + ylab("Cumulative Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_s_perc~mean,data=met_tmax %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_s_perc~mean,data=met_tmax %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st_s<-do.call(rbind.data.frame,r)
colnames(st_s) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st_s<-bind_cols(st_s,p1)
data=data.frame(x=30,y=0.3,lab=c(paste0("R2 = ",signif(st_s[1,"r.adj"],2),";\np = ",signif(st_s[1,"p.value"],2)),
                                paste0("R2 = ",signif(st_s[2,"r.adj"],2),";\np = ",signif(st_s[2,"p.value"],2)),
                                paste0("R2 = ",signif(st_s[3,"r.adj"],2),";\np = ",signif(st_s[3,"p.value"],2))),
                survey=1:3)
#r3<-summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$adj.r.squared
#p3<-as.numeric(summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$coefficients[,"Pr(>|t|)"][2])
g2<-ggplot(met_tmax,aes(mean,cum_s_perc)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Maximum Temperature [C]") + ylab("Cumulative Small Tree Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)#+ geom_text(x=0,y=0.15,label=paste0("R2 = ",signif(r3,2),"; p =",signif(p3,2)))


ggarrange(g1,g2,nrow = 2)
ggsave(paste0(getwd(),"/Analysis/ElNino/CumMortality.vs.Tmax.pdf"),height=8,width=12)


#combine rows for Tmin
met_tmin <- bind_rows(met_plot %>% filter(category=="tmin"),met_before %>% filter(category=="tmin"))

#Tmin vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_perc_mort~mean,data=met_tmin %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_perc_mort~mean,data=met_tmin %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=23,y=0.4,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                 paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                 paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g1<-ggplot(met_tmin,aes(mean,cum_perc_mort)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Minimum Temperature [C]") + ylab("Cumulative Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(cum_s_perc~mean,data=met_tmin %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(cum_s_perc~mean,data=met_tmin %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st_s<-do.call(rbind.data.frame,r)
colnames(st_s) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st_s<-bind_cols(st_s,p1)
data=data.frame(x=23,y=0.3,lab=c(paste0("R2 = ",signif(st_s[1,"r.adj"],2),";\np = ",signif(st_s[1,"p.value"],2)),
                                 paste0("R2 = ",signif(st_s[2,"r.adj"],2),";\np = ",signif(st_s[2,"p.value"],2)),
                                 paste0("R2 = ",signif(st_s[3,"r.adj"],2),";\np = ",signif(st_s[3,"p.value"],2))),
                survey=1:3)
#r3<-summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$adj.r.squared
#p3<-as.numeric(summary(lm(anomaly~cum_s_perc,data=enso_plot %>% filter(category=="tmax"&!is.na(distance))))$coefficients[,"Pr(>|t|)"][2])
g2<-ggplot(met_tmin,aes(mean,cum_s_perc)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Maximum Temperature [C]") + ylab("Cumulative Small Tree Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)#+ geom_text(x=0,y=0.15,label=paste0("R2 = ",signif(r3,2),"; p =",signif(p3,2)))


ggarrange(g1,g2,nrow = 2)
ggsave(paste0(getwd(),"/Analysis/ElNino/CumMortality.vs.Tmin.pdf"),height=8,width=12)

###NOT CUMULATIVE#########
#Tmax vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(perc_mortality~anomaly,data=enso_tmax %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(perc_mortality~anomaly,data=enso_tmax %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=6,y=0.2,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g1<-ggplot(enso_tmax,aes(anomaly,perc_mortality)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Maximum Temperature [anomaly]") + ylab("Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

#Tmin vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(perc_mortality~anomaly,data=enso_tmin %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(perc_mortality~anomaly,data=enso_tmin %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=0,y=0.2,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g2<-ggplot(enso_tmin,aes(anomaly,perc_mortality)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Minimum Temperature [anomaly]") + ylab("Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

ggarrange(g1,g2,nrow = 2)
ggsave(paste0(getwd(),"/Analysis/ElNino/Mortality.vs.Temp_anomalies.pdf"),height=8,width=12)

#actual Tmax vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(perc_mortality~mean,data=met_tmax %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(perc_mortality~mean,data=met_tmax %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=30,y=0.2,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g1<-ggplot(met_tmax,aes(mean,perc_mortality)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Maximum Temperature [C]") + ylab("Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

#Tmin vs mortality
r<-list()
p<-list()
for(s in 1:3){
  r[[s]] <- summary(lm(perc_mortality~mean,data=met_tmin %>% filter(survey==s)))$adj.r.squared
  p[[s]] <- as.numeric(summary(lm(perc_mortality~mean,data=met_tmin %>% filter(survey==s)))$coefficients[,"Pr(>|t|)"][2])
}

st<-do.call(rbind.data.frame,r)
colnames(st) <- "r.adj"
p1<-do.call(rbind.data.frame,p)
colnames(p1)<-"p.value"
st<-bind_cols(st,p1)
data=data.frame(x=23,y=0.2,lab=c(paste0("R2 = ",signif(st[1,"r.adj"],2),";\np = ",signif(st[1,"p.value"],2)),
                                paste0("R2 = ",signif(st[2,"r.adj"],2),";\np = ",signif(st[2,"p.value"],2)),
                                paste0("R2 = ",signif(st[3,"r.adj"],2),";\np = ",signif(st[3,"p.value"],2))),
                survey=1:3)

g2<-ggplot(met_tmin,aes(mean,perc_mortality)) + geom_point() + facet_wrap(~factor(survey),ncol=3) + theme_classic()  +
  xlab("Minimum Temperature [C]") + ylab("Mortality [Prop Trees]") + stat_smooth(method="lm") + #scale_color_discrete(name="Distance from Forest") +
  geom_text(aes(x,y,label=lab),data=data)

ggarrange(g1,g2,nrow = 2)
ggsave(paste0(getwd(),"/Analysis/ElNino/Mortality.vs.Temp.pdf"),height=8,width=12)


#compare with yields
strong = data.frame(x1=c(as.Date("2015-06-01"),as.Date("2016-02-01")), x2=c(as.Date("2015-08-01"),as.Date("2016-03-01")), y1=c(-Inf,-Inf), y2=c(Inf,Inf))
v_strong = data.frame(x1=as.Date("2015-08-01"), x2=as.Date("2016-02-01"), y1=-Inf, y2=Inf)
wet_start = data.frame(x1=c(as.Date("2014-09-01"),as.Date("2015-03-01"),as.Date("2015-09-01"),as.Date("2016-03-01"),as.Date("2016-09-01"),as.Date("2017-03-01")))
wet_end = data.frame(x2=c(as.Date("2014-06-01"),as.Date("2014-11-01"),as.Date("2015-06-01"),as.Date("2015-11-01"),as.Date("2016-06-01"),as.Date("2016-11-01"),as.Date("2017-06-01")))

yield_data %>% ggplot() +
  geom_line(aes(Month,Monthly.harvest.tree,group=Plot,color=factor(distance))) + facet_wrap(~transect,ncol=1)+
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="Distance from Forest")+
  theme_classic() + xlab("") + ylab("Monthly Pod Harvests [kg/tree]")+theme(legend.position = "bottom")
ggsave(paste0(getwd(),"/Analysis/ElNino/MonthlyHarvest.withINO.pdf"),height=8,width=12)


library(car)
#remove forest plots and focus on max temp
enso_mort <- enso_plot %>% filter(category=="tmax"&!is.na(age))


#do analysis to see what is driving mortality during El Nino
qqp(enso_plot$mortality, "lnorm")

