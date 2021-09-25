library(readr)
library(data.table)
library(lubridate)
library(zoo)
library(moments)
rm(list = ls())
setwd("C:/Users/MXX/Desktop/Spring20/QAM/PS2")

#Question 1
bond <- as.data.table(read_csv("bonds.csv", col_types = cols(KYCRSPID = col_character(), 
                                        KYTREASNO = col_skip(), TMTOTOUT = col_number())))

PS2_Q1=function(bond){
  stopifnot(is.data.table(bond))
  
  #Add month, year, and lag MV
  bond[,c("Year","Month"):=.(year(MCALDT),month(MCALDT))]
  bond[, lagMV:=shift(TMTOTOUT),by=KYCRSPID]  

  #Clean up, remove RET=-99 and NA's
  bond = bond[!which(TMRETNUA==-99)]
  bond = bond[!is.na(TMRETNUA)|!is.na(lagMV),] 
  
  #Output calculations
  output = bond[,list(Bond_lag_MV = sum(lagMV,na.rm = TRUE),
                    Bond_Ew_Ret = sum(TMRETNUA,na.rm = TRUE)/length(!is.na(TMRETNUA)),
                    Bond_Vw_Ret = sum(TMRETNUA*lagMV, na.rm = TRUE)/sum(lagMV,na.rm = TRUE)),
                  by = list(Year,Month)]
  
  return(output)  #should be from 01/1926 - 12/2019
}
Monthly_CRSP_Bonds=PS2_Q1(bond)


#Question 2
#from PS1
crsp=as.data.table(read.csv("C:/Users/MXX/Desktop/Spring20/QAM/PS1/CRSP_monthly.csv"))
crsp[,date:=as.Date(as.character(date),"%Y%m%d")]
PS1_Q1=function(crsp){
  
  #convert format 
  crsp[,DLRET:=as.numeric(as.character(DLRET))]
  crsp[,RET:=as.numeric(as.character(RET))]
  
  #omit rows with missing returns and price
  crsp = crsp[!is.na(RET)|!is.na(DLRET)|!is.na(PRC)]
  
  #add Market Value and lag_MV
  crsp[,MV:=abs(PRC)*SHROUT*1000]
  crsp[,lag_MV:=shift(MV), by=PERMNO]
  
  #select exchanges and stock type
  crsp = crsp[SHRCD %in% c(10,11)]
  crsp = crsp[EXCHCD %in% c(1,2,3)]
  
  #adjust return with delisting return
  crsp= crsp[!RET %in% c(-99,-88,-77,-66,-55,-44)]
  crsp=crsp[!is.na(RET)|!is.na(DLRET),]  #filter out first period with no RET or DLRET
  crsp[!is.na(DLRET)&!is.na(RET), RET:=(1+RET)*(1+DLRET)-1]  #if there is a RET and a DLRET
  crsp[is.na(RET), RET:=DLRET]      #if there is a DLRET but no RET
  crsp=crsp[!is.na(lag_MV),] #filter out remaining NA's (2 of them, missing previous price)
  
  #add year and month (use lubridate functions to add speed)
  crsp[,Year:=year(date)]
  crsp[,Month:=month(date)]
  
  #output
  EW=crsp[,list(Stock_Ew_Ret=mean(RET)), by=list(Year,Month)]
  VW=crsp[,list(Stock_Vw_Ret=weighted.mean(RET,lag_MV)),by=list(Year,Month)]
  lagmv=crsp[,list(Stock_lag_MV=sum(lag_MV)/1000000),by=list(Year,Month)]
  
  setkey(EW,Year,Month)
  setkey(VW,Year,Month)
  setkey(lagmv,Year,Month)
  lagmv[EW][VW]   #return merged output tables
}
Monthly_CRSP_Stocks=PS1_Q1(crsp)


Monthly_CRSP_Riskless <- as.data.table(read_csv("riskless.csv", col_types = cols(caldt = col_date(format = "%Y%m%d"))))

PS2_Q2 = function(stock, bond, riskless){
  stopifnot(is.data.table(bond)&is.data.table(stock)&is.data.table(riskless))
  
  riskless[,c("Year","Month"):=.(year(caldt), month(caldt))]
  #Merge data tables
  tot = merge(stock,bond,by=c("Year", "Month"))
  tot <- merge(tot,riskless,by=c("Year", "Month"))
  
  #compute stock and bond excess VW returns
  tot[,Stock_Excess_Vw_Ret:=Stock_Vw_Ret - t30ret]
  tot[,Bond_Excess_Vw_Ret:=Bond_Vw_Ret - t30ret]
  
  #output
  tot[,list(Year,Month,Stock_lag_MV, Stock_Excess_Vw_Ret, Bond_lag_MV, Bond_Excess_Vw_Ret)] 
}

Monthly_CRSP_Universe = PS2_Q2(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless)

#Question 3
PS2_Q3 = function(universe){
  stopifnot(is.data.table(universe))
  
  universe[, Excess_Vw_Ret := Stock_Excess_Vw_Ret*Stock_lag_MV/(Stock_lag_MV + Bond_lag_MV)
                              + Bond_Excess_Vw_Ret*Bond_lag_MV/(Stock_lag_MV + Bond_lag_MV)]
  universe[, Excess_60_40_Ret := 0.6*Stock_Excess_Vw_Ret+0.4*Bond_Excess_Vw_Ret] 
 
  #rolling sigmas, use 3-year rolling window on VW returns
  universe[,c("Stock_inverse_sigma_hat","Bond_inverse_sigma_hat"):=
            .( 1/rollapply(Stock_Excess_Vw_Ret,FUN=sd,width=list(seq(-36,-1,1)),fill=NA,align="right"),
               1/rollapply(Bond_Excess_Vw_Ret,FUN=sd,width=list(seq(-36,-1,1)),fill=NA,align="right"))]
  
  universe[,Unlevered_k := 1/(Stock_inverse_sigma_hat + Bond_inverse_sigma_hat)] 
  
  universe[,Excess_Unlevered_RP_Ret := Unlevered_k*Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret +
                                       Unlevered_k*Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret]
  
  #set levered k so that volatility match with that of the value weighted benchmark portfolio
  universe[, Levered_k := sd(Excess_Vw_Ret, na.rm=T)/sd(Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret +
                                                    Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret, na.rm=T)]
  
  universe[, Excess_Levered_RP_Ret:= Stock_inverse_sigma_hat*Levered_k*Stock_Excess_Vw_Ret +
                                     Bond_inverse_sigma_hat*Levered_k*Bond_Excess_Vw_Ret]
  universe[,c(1,2,4,6:14)]
}
Port_Rets = PS2_Q3(Monthly_CRSP_Universe)

#Question 4
PS2_Q4=function(ret){

  ret = ret[Year>=1930 & (Year<2010 | (Year==2010 & Month<=6))]
  #output
  out = matrix(0,6,6) 
  colnames(out) = c("Excess Return", "t-stat of Excess Return", "Volatility", "Sharpe Ratio", 
                       "Skewness", "Excess Kurtosis")
  row.names(out) = c("CRSP stocks", "CRSP bonds", "Value-weighted portfolio", "60/40 portfolio", 
                        "unlevered RP", "levered RP")
  out[1,1] = mean(ret$Stock_Excess_Vw_Ret)*12*100
  out[2,1] = mean(ret$Bond_Excess_Vw_Ret)*12*100
  out[3,1] = mean(ret$Excess_Vw_Ret)*12*100
  out[4,1] = mean(ret$Excess_60_40_Ret)*12*100
  out[5,1] = mean(ret$Excess_Unlevered_RP_Ret)*12*100
  out[6,1] = mean(ret$Excess_Levered_RP_Ret)*12*100
     
  out[1,2] = t.test(ret$Stock_Excess_Vw_Ret)$statistic
  out[2,2] = t.test(ret$Bond_Excess_Vw_Ret)$statistic
  out[3,2] = t.test(ret$Excess_Vw_Ret)$statistic
  out[4,2] = t.test(ret$Excess_60_40_Ret)$statistic
  out[5,2] = t.test(ret$Excess_Unlevered_RP_Ret)$statistic
  out[6,2] = t.test(ret$Excess_Levered_RP_Ret)$statistic
  
  out[1,3] = sd(ret$Stock_Excess_Vw_Ret)*sqrt(12)*100
  out[2,3] = sd(ret$Bond_Excess_Vw_Ret)*sqrt(12)*100
  out[3,3] = sd(ret$Excess_Vw_Ret)*sqrt(12)*100
  out[4,3] = sd(ret$Excess_60_40_Ret)*sqrt(12)*100
  out[5,3] = sd(ret$Excess_Unlevered_RP_Ret)*sqrt(12)*100
  out[6,3] = sd(ret$Excess_Levered_RP_Ret)*sqrt(12)*100
  
  out[1,4] = out[1,"Excess Return"] / out[1,"Volatility"]
  out[2,4] = out[2,"Excess Return"] / out[2,"Volatility"]
  out[3,4] = out[3,"Excess Return"] / out[3,"Volatility"]
  out[4,4] = out[4,"Excess Return"] / out[4,"Volatility"]
  out[5,4] = out[5,"Excess Return"] / out[5,"Volatility"]
  out[6,4] = out[6,"Excess Return"] / out[6,"Volatility"]
  
  out[1,5] = skewness(ret$Stock_Excess_Vw_Ret)
  out[2,5] = skewness(ret$Bond_Excess_Vw_Ret)
  out[3,5] = skewness(ret$Excess_Vw_Ret)
  out[4,5] = skewness(ret$Excess_60_40_Ret)
  out[5,5] = skewness(ret$Excess_Unlevered_RP_Ret)
  out[6,5] = skewness(ret$Excess_Levered_RP_Ret)
  
  out[1,6] = kurtosis(ret$Stock_Excess_Vw_Ret)-3
  out[2,6] = kurtosis(ret$Bond_Excess_Vw_Ret)-3
  out[3,6] = kurtosis(ret$Excess_Vw_Ret)-3
  out[4,6] = kurtosis(ret$Excess_60_40_Ret)-3
  out[5,6] = kurtosis(ret$Excess_Unlevered_RP_Ret)-3
  out[6,6] = kurtosis(ret$Excess_Levered_RP_Ret)-3
  
  round(out,digits = 2)
}
Q4output=PS2_Q4(Port_Rets)

