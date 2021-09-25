#QAM PS3
rm(list = ls())
setwd("C:/Users/MXX/Desktop/Spring20/QAM/PS3")
library(data.table)
library(zoo)
library(moments)
library(readr)
library(tidyverse)
crsp <- as.data.table(read_csv("CRSP_monthly.csv", col_types = cols(DLRET = col_character(), 
                                          date = col_date(format = "%Y%m%d"))))

PS3_Q1 = function(crsp){
  
  #convert below returns to NA
  crsp[RET %in% c("B","C","-99","-88","-77","-66",""," "), RET:=NA]
  
  #convert below delisting returns to NA
  crsp[DLRET %in% c("A","P","S","T","-99","-88","-77","-66",""," "),DLRET := NA]
  
  #Convert return format to numeric
  crsp[, RET := as.numeric(as.character(RET))]
  crsp[, DLRET := as.numeric(as.character(DLRET))]
  
  #Create Year,Month and month numbers 
  crsp[,c("Month","Year") := .(month(date),year(date))]
  crsp[,yrmon := Year*12+Month]

  #add market cap and lag market cap
  crsp[,mktcap := abs(PRC) * abs(SHROUT)/1000]
  crsp[,lag_Mkt_Cap := shift(mktcap), by=PERMNO]
  
  #validate lags
  crsp[,pre_yrmon := shift(yrmon), by=PERMNO]
  crsp[,validlag := yrmon==(pre_yrmon+1)]
  
  #Select SHRCD and EXCHCD, after calculating lag market cap (some switched Exchanges but lag market cap 
  crsp <- crsp[SHRCD %in% c(10,11) & EXCHCD %in% c(1,2,3)]   #was still available)
  
  #check if there are enough lags
  crsp[,lags:= !is.na(shift(yrmon,12)),by=PERMNO] 
  
  #returns calculation
  crsp=crsp[!is.na(RET)|!is.na(DLRET),]  
  crsp[!is.na(DLRET)&!is.na(RET), RET:=(1+RET)*(1+DLRET)-1]  
  crsp[is.na(RET), RET:=DLRET]
  crsp=crsp[validlag==T & !is.na(RET) & !is.na(lag_Mkt_Cap)]
  
  #redo valid lags to account for the observations thrown out 
  crsp[,validlag := (shift(yrmon,type="lead")==(yrmon+1)), by=PERMNO]
  
  #Compute Ranking returns, and make sure there are at least 8 valid months in 11 of 13 observations
  crsp[,validmonths := rollapplyr(validlag, 11, sum, na.rm=T,fill=NA, partial=TRUE), by = PERMNO] # valid # of months
  crsp[,Ranking_Ret := rollapplyr(shift(log(1+RET),2), 11, sum, fill=NA, partial = TRUE), by=PERMNO]
  crsp[!lags | shift(validmonths,2) < 8, Ranking_Ret := NA]  #availability constraints
  
  setnames(crsp,"RET","Ret")
  crsp = crsp[, c("Year", "Month", "PERMNO",  "EXCHCD", "lag_Mkt_Cap", "Ret", "Ranking_Ret")]
  return(crsp)
}
q1=PS3_Q1(crsp)


PS3_Q2 <- function(Momentum){
  #Remove NAs 
  Momentum <- Momentum[!is.na(Ranking_Ret)]
  
  #DM deciles
  Momentum[,DM_decile := cut(Ranking_Ret, breaks = quantile(Ranking_Ret,probs=c(0:10)/10),
                            labels=F,right=F), by=list(Year,Month)]
  Momentum[is.na(DM_decile),DM_decile:=as.integer(10)]
  
  #FF deciles (based only on NYSE)
  temp=copy(Momentum[EXCHCD==1])
  temp[,KRF_decile := cut(Ranking_Ret, breaks = quantile(Ranking_Ret,probs=c(0:10)/10),
                          labels=F,right=F), by=list(Year,Month)]
  temp[is.na(KRF_decile) ,KRF_decile := as.integer(10)]
  Momentum=merge(Momentum,temp[,.(Year,Month,PERMNO, KRF_decile)],by=c("Year","Month","PERMNO"), all.x=T)
  
  #merge
  setorder(Momentum,Year,Month,Ranking_Ret)
  Momentum[,KRF_decile:=na.locf(KRF_decile, na.rm=F), by=.(Year,Month)]
  Momentum[,DM_decile:=as.integer(as.character(DM_decile))]
  Momentum[,KRF_decile:=as.integer(as.character(KRF_decile))]
  Momentum[is.na(KRF_decile), KRF_decile:=as.integer(1)]
  
  return(Momentum[,c("Year","Month","PERMNO","lag_Mkt_Cap","Ret","DM_decile","KRF_decile")])
}
q2=PS3_Q2(q1)


PS3_Q3 <- function(q2,ff){
  
  DM=q2[,list(DM_Ret = weighted.mean(Ret,lag_Mkt_Cap,na.rm=T)), by = list(Year,Month,DM_decile)]
  setnames(DM,"DM_decile","decile")
  setkey(DM,Year,Month,decile)
  
  KRF=q2[,list(KRF_Ret = weighted.mean(Ret,lag_Mkt_Cap,na.rm=T)), by = list(Year,Month,KRF_decile)]
  setnames(KRF,"KRF_decile","decile")
  setkey(KRF,Year,Month,decile)
  
  #Merge all together
  merged=DM[KRF]
  mom_ret=merge(merged,ff,by = c("Year","Month"))

  return(mom_ret[,c("Year","Month","decile","DM_Ret","KRF_Ret","Rf")])
}

ff=as.data.table(read.csv("F-F_Research_Data_Factors.csv"))
ff[,date:=as.Date(paste0(as.character(X), '01'), format='%Y%m%d')]
ff[,c("Year","Month"):=.(year(date),month(date))]
ff=ff[,-c(1,6)]
ff[,c(1,4)]=ff[,c(1,4)]/100
setnames(ff,c("Mkt.RF","RF"),c("Market_minus_Rf","Rf"))

q3=PS3_Q3(q2,ff)


PS3_Q4=function(momret){
  momret=momret[Year>=1927 & Year < 2013 | (Year == 2013 & Month <=3)]
  
  output = matrix(0,nrow = 4, ncol = 11)
  row.names(output) = c("r-rf", "SD", "SR","sk(m)")
  colnames(output) = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "WML")
  
  for (d in 1:10){
    output[1,d] = mean(momret[decile==d, ]$DM_Ret - momret[decile==d, ]$Rf)*12*100
    output[2,d] = sd(momret[decile==d, ]$DM_Ret - momret[decile==d, ]$Rf)*sqrt(12)*100
    output[3,d] = output[1,d]/output[2,d]
    output[4,d] = skewness(log(momret[decile==d, ]$DM_Ret +1))
  }
  output[1,11] = mean(momret[decile==10, ]$DM_Ret - momret[decile==1, ]$DM_Ret)*12*100
  output[2,11] = sd(momret[decile==10, ]$DM_Ret - momret[decile==1, ]$DM_Ret)*sqrt(12)*100
  output[3,11] = output[1,11]/output[2,11]
  output[4,11] = skewness(log(momret[decile==10, ]$DM_Ret - momret[decile==1, ]$DM_Ret + 1 + momret[decile==10, ]$Rf))
  return(round(output,2))
}
q4=PS3_Q4(q3)


PS3_Q5 <- function(me,DM,KRF){
  
  # DM data is till 2016.12, mutual time for all is 1927.1 to 2016.12
  my_ret = me[Year>=1927 & Year<=2016 ]
  dm_ret = DM[Year>=1927 & Year<=2016 ]
  krf_ret = KRF[Year>=1927 & Year<=2016 ]
  
  output = matrix(nrow=2,ncol=11)
  for(j in 1:10){
    my_dm = my_ret[decile == j,DM_Ret]
    dm = dm_ret[decile == j,DM_Ret]
    output[1,j] = cor(my_dm, dm)
    
    my_krf = my_ret[decile == j,KRF_Ret]
    krf = krf_ret[decile == j,KRF_Ret]
    output[2,j] = cor(my_krf, krf)
  }
  
  wl_my_dm = my_ret[decile==10,DM_Ret] - my_ret[decile==1,DM_Ret]
  wl_dm = dm_ret[decile == 10,DM_Ret] - dm_ret[decile == 1,DM_Ret]
  output[1,11] = cor(wl_my_dm,wl_dm)
  
  wl_my_krf = my_ret[decile==10,KRF_Ret] - my_ret[decile==1,KRF_Ret]
  wl_krf = krf_ret[decile == 10,KRF_Ret] - krf_ret[decile == 1,KRF_Ret]
  output[2,11] = cor(wl_my_krf,wl_krf)
  
  colnames(output) = c("Decile 1","Decile 2","Decile 3","Decile 4","Decile 5","Decile 6","Decile 7",
                       "Decile 8","Decile 9","Decile 10","WML")
  row.names(output) = c("DM Correlation","KRF Correlation")
  
  output <- round(output,4)
  
  return(output)
}

DM_returns = as.data.table(read.table("m_m_pt_tot.txt",header=FALSE, sep = "", dec=".")[,c(1,2,3)])
DM_returns[,V1 := as.Date(as.character(V1),format="%Y%m%d")]
DM_returns[, c("Year","Month"):= .(year(V1),month(V1))]   
DM_returns = DM_returns[,c(4,5,2,3)]
setnames(DM_returns,c("V2","V3"),c("decile","DM_Ret"))

KRF_returns = as.data.table(read.csv("10_Portfolios_Prior_12_2.csv",colClasses=c(rep("numeric",11))))
KRF_returns[, Year:= floor(X/100)]   
KRF_returns[, Month:= X %% Year]
KRF_returns[,2:11] =KRF_returns[,2:11]/100 
KRF_returns=KRF_returns[,-1]
colnames(KRF_returns)=c("1","2","3","4","5","6","7","8","9","10","Year","Month")
KRF_returns=as.data.table(gather(KRF_returns,decile, KRF_Ret,1:10))  #gather all decile columns

q5=PS3_Q5(q3,DM_returns,KRF_returns)


###### QUESTION 6 Let's review the sample from 2000 to now
library(ggplot2)
recent=q3[Year>=2000]
recent_result = PS3_Q4(recent)

recent[,date:=as.Date(paste0(Year,"-",Month,"-01"), "%Y-%m-%d")]
setorder(recent,decile,date)
recent[,cumret_dm := log(cumprod(DM_Ret+1)),by=decile]
recent[,cumret_krf := log(cumprod(KRF_Ret+1)),by=decile]

#plot DM cumulative returns from 2000 for each decile
dm = ggplot(recent, aes(x=date , y=cumret_dm)) + geom_line(aes(col=factor(decile))) + theme_bw()
dm
# KRF cumulative returns from 2000 for each decile
krf = ggplot(recent, aes(x=date , y=cumret_krf)) + geom_line(aes(col=factor(decile))) + theme_bw()
krf
#WML cum returns for DM and KRF
DM_WML = recent[decile=='10',]$DM_Ret - recent[decile=='1',]$DM_Ret
KRF_WML = recent[decile=='10',]$KRF_Ret - recent[decile=='1',]$KRF_Ret
DM_WML = log(DM_WML+1)
KRF_WML = log(KRF_WML+1)
WML = data.frame(date=unique(recent$date), cumsum(DM_WML), cumsum(KRF_WML))
dm_wml = ggplot()+geom_line(aes(x= WML$date, y=WML$cumsum.DM_WML.), colour='blue') 
dm_wml
krf_wml = ggplot()+geom_line(aes(x= WML$date, y=WML$cumsum.KRF_WML.), colour='red') 
krf_wml

