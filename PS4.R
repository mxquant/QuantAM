#QAM PS4
rm(list = ls())
setwd("C:/Users/MXX/Desktop/Spring20/QAM/PS4")
library(tidyverse)
library(dplyr)
require(data.table)
library(lubridate)
library(moments)
library(zoo)

#Q1

prba = as.data.table(read.csv("pension.csv")) 
cmpt = as.data.table(read.csv("compustat.csv")) 
crsp = as.data.table(read.csv("crsp.csv")) 
link = as.data.table(read.csv("link.csv"))

############################################## Clean CRSP ############################################
crsp.data=copy(crsp)

crsp.data = crsp.data[SHRCD %in% c(10,11)]
crsp.data = crsp.data[EXCHCD %in% c(1,2,3)]
crsp.data[RET %in% c("B","C","-99","-88","-77","-66",""," "), RET:=NA]
crsp.data[DLRET %in% c("A","P","S","T","-99","-88","-77","-66",""," "),DLRET := NA]

crsp.data[, RET := as.numeric(as.character(RET))]
crsp.data[, DLRET := as.numeric(as.character(DLRET))]
crsp.data[, date := as.Date(as.character(date),format="%Y%m%d")] 
crsp.data[,c("Year","Month") := .(year(date),month(date))]
crsp.data[,yrmon := (Year*12)+Month]
crsp.data[,validlag:=(shift(yrmon)==(yrmon-1)),by=PERMNO]

#fill missing price and calculate ME, lag ME
crsp.data=crsp.data%>% group_by(PERMNO) %>% fill(PRC,.direction = "down") %>% as.data.table
crsp.data[, ME := abs(PRC) * SHROUT/1000]
crsp.data[, MktCap := sum(ME,na.rm = T), .(PERMCO, Year, Month)]
crsp.data = crsp.data[ MktCap!=0]
#for first period with no lag ME, assume lag ME = ME/(1+average RET)
crsp.data[, Lagged_MktCap:=ifelse(!is.na(shift(MktCap)),shift(MktCap), MktCap/(1+sum(RET,na.rm=T)/(.N-1))),by=PERMNO]

crsp.data[!is.na(DLRET)&is.na(RET),RET := DLRET] 
crsp.data[!is.na(DLRET)&!is.na(RET),RET := (1+RET)*(1+DLRET) - 1] 
crsp.data=crsp.data[validlag==T]


################################ Clean Compustat and merge with Compustat Pension####################
cmpt.data=copy(cmpt)
prba.data=copy(prba)
cmpt.data = cmpt.data[curcd=="USD" & indfmt=="INDL"]
cmpt.data = merge(cmpt.data, prba.data, by=c("gvkey","datadate"),all.x=T)
cmpt.data[, datadate:= as.Date(as.character(datadate),"%Y%m%d")]
cmpt.data[, c("Year","Month"):= .(year(datadate),month(datadate))]
#calculate BE
cmpt.data[,BE:= coalesce(seq, ceq + pstk, at - lt -mib, at - lt) + coalesce(txditc, txdb + itcb, 0) - 
            coalesce(pstkrv, pstkl, pstk, 0) - coalesce(prba, 0) ]
          
cmpt.data = cmpt.data[,.(gvkey, datadate, fyear, Month, Year, BE)]


#################################### Merge CRSP with Compustat ######################################
link.data=copy(link)
merged <- merge(crsp.data, link.data, by.x='PERMCO', by.y = 'LPERMCO', allow.cartesian = T)

merged[,LINKDT := as.Date(as.character(LINKDT),"%Y%m%d")]
merged[LINKENDDT == 'E', LINKENDDT := NA]
merged[,LINKENDDT := as.Date(as.character(LINKENDDT),"%Y%m%d")]
merged = merged[(is.na(LINKDT) | date >= LINKDT) & (is.na(LINKENDDT) | date <= LINKENDDT)]
setorder(merged, gvkey, date)

#### code from TA session
# Multiple GVKEYs per PERMCO

### First, if LC not LC linktype, only keep LC
# identify Same PERMCO but different PERMNO
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LINKTYPE == 'LC'), by =.(PERMCO, date)]
merged = merged[!(prob == T & Good_match == T & LINKTYPE != 'LC')]

### Second, if P and not P linkprim, only keep p
merged[, prob := .N > 1, by= .(PERMCO, date)]
merged[, Good_match := sum(LINKPRIM == 'P'), by =.(PERMCO, date)]
merged <- merged[!(prob == T & Good_match == T & LINKPRIM != 'P')]

### Third, if 1 and not liid, only keep 1 
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LIID == 1), by =.(PERMCO,date)]
merged = merged[!(prob == T & Good_match == T & LIID != 1)]

### Fourth, use the link that's current
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(is.na(LINKENDDT)), by = .(PERMCO, date)]
merged = merged[!(prob==T & Good_match == T & !is.na(LINKENDDT))]

### Fifth, use the link that's been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := NULL]
merged[is.na(LINKENDDT), LINKENDDT := as.Date('2019-12-31', '%Y-%m-%d')]
merged[, Date_diff := as.integer(LINKENDDT - LINKDT)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged = merged[!(prob==T & Good_match != T)]

### Sixth, use the gvkey that has been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match :=NULL]
setorder(merged, gvkey, LINKDT)
merged[prob == T, start_Date := LINKDT[1], by = .(gvkey)]
setorder(merged, gvkey, LINKENDDT)
merged[prob == T, end_Date := LINKENDDT[.N], by = .(gvkey)]
merged[, Date_diff := as.integer(end_Date - start_Date)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged = merged[!(prob == T & Good_match != T)]

### Seventh, use the smaller gvkey
setorder(merged, PERMCO, date, gvkey)
merged = unique(merged, by = c('PERMCO', 'date'))

### Clean up extra variables and final check of match
merged1 = merged[, .(gvkey, date, EXCHCD, ME, PERMCO, PERMNO, RET , Year, Month,MktCap,Lagged_MktCap)]

################# Merge CRSP and Compustat 
crsp_cmpt = as.data.table(left_join(merged1,cmpt.data,by =c("gvkey"="gvkey","Year"="fyear")))
crsp_cmpt = crsp_cmpt[, .(gvkey, date, PERMNO, PERMCO, EXCHCD, RET, ME, BE,MktCap,Lagged_MktCap)] 

crsp_cmpt[, c("Year","Month"):= .(year(date), month(date))]
setorder(crsp_cmpt, Year, Month)


########################################## SMB #############################################
#take June of each year
june <- crsp_cmpt[Month == 6, .(PERMCO, Year, MktCap, EXCHCD)]
june[,sz_decile := findInterval(MktCap, quantile(.SD[EXCHCD==1,MktCap], seq(0.1,0.9,0.1)), left.open = T) + 1,by = .(Year)]

# june <- crsp_cmpt[Month == 6, .(PERMCO, Year, MktCap, EXCHCD)]  
# temp=copy(june[EXCHCD==1])
# temp[,sz_decile := cut(MktCap, breaks = quantile(MktCap,probs=c(0:10)/10),
#                         labels=F,right=F), by=Year]
# temp[is.na(sz_decile) ,sz_decile := as.integer(10)]
# 
# june=merge(june,temp,by=c("PERMCO","Year","MktCap","EXCHCD"), all.x=T)
# setorder(june,Year,MktCap)
# june[,sz_decile:=na.locf(sz_decile, na.rm=F), by=Year]
# june[,sz_decile:=as.integer(as.character(sz_decile))]
# june[is.na(sz_decile), sz_decile:=as.integer(1)]

june = june[,.(PERMCO, Year, sz_decile)]
setorder(crsp_cmpt, Year, Month)
setorder(june, Year)

size.decile = merge(crsp_cmpt, june, by = c("PERMCO","Year"), all.x = T)
size.decile[, Size_rank := .(shift(sz_decile, 6)), by=PERMNO]

size.decile = size.decile[!is.na(Size_rank) & !is.na(Lagged_MktCap)& Lagged_MktCap!=0]

size_port = size.decile[,.(Size_Ret = weighted.mean(RET, Lagged_MktCap, na.rm = TRUE)), .(Year, Month, Size_rank)]
setkey(size_port, Year, Month, Size_rank)
size_port=size_port[Year>=1973]

################################ HML Portfolio ###############################################
#historical BE
histBe=as.data.table(fread("DFF_BE_With_Nonindust.txt"))
hist_be_btm <- histBe[,c(-2,-3)]
colnames(hist_be_btm)=c("PERMNO",as.character(1926:2001))
hist_be_btm <- melt(hist_be_btm,id.vars = c("PERMNO"))
colnames(hist_be_btm) <- c("PERMNO","Year","HistBe")
hist_be_btm[, Year:= as.numeric(as.character(Year))]
#take December of each year
data.btm = crsp_cmpt[Month == 12,]
data.btm <- merge(data.btm,hist_be_btm,by= c("PERMNO","Year"), all.x=T)
data.btm <- data.btm[is.na(BE) & !is.na(HistBe) & HistBe!=-99.99, BE:=HistBe]  #fill in historical BE

data.btm[,BtM := BE/MktCap]
data.btm = data.btm[!is.na(BtM) & BtM>0]
# temp=copy(data.btm[EXCHCD==1])  
# temp[,bm_decile := cut(BtM, breaks = quantile(BtM,probs=c(0:10)/10, na.rm=T),
#                        labels=F,right=F), by=Year]
# temp[is.na(bm_decile) ,bm_decile := as.integer(10)]
# 
# data.btm=merge(data.btm,temp, by=colnames(data.btm)[1:13],all.x=T)
# setorder(data.btm,Year,BtM)
# data.btm[,bm_decile:=na.locf(bm_decile, na.rm=F), by=Year]
# data.btm[,bm_decile:=as.integer(as.character(bm_decile))]
# data.btm[is.na(bm_decile), bm_decile:=as.integer(1)]
data.btm[,bm_decile := findInterval(BtM, quantile(.SD[EXCHCD==1,BtM], seq(0.1,0.9,0.1)), left.open = T) + 1,by = .(Year, Month)]
data.btm[,lagged_BtM_decile := shift(bm_decile), by=PERMNO]

setorder(crsp_cmpt, Year, Month)
setorder(data.btm, Year)

# MERGE the BtM decile information back to crsp cleaned up data.
data.btm = data.btm[, .(PERMNO,PERMCO, Year, bm_decile, lagged_BtM_decile)]
btm.decile = merge(crsp_cmpt, data.btm, by = c("PERMNO","PERMCO","Year"), all.x = T)
btm.decile[, BtM_Rank := .(shift(lagged_BtM_decile, 6)), .(PERMNO)]

btm.decile = btm.decile[!is.na(BtM_Rank) & !is.na(Lagged_MktCap) & Lagged_MktCap!=0.0]
BtM_port = btm.decile[,.(BtM_Ret = weighted.mean(RET, Lagged_MktCap, na.rm = TRUE)), .(Year, Month, BtM_Rank)]
setkey(BtM_port, Year, Month, BtM_Rank)
BtM_port=BtM_port[Year>=1973]

########################################### HML+SMB Double Sort ###########################################
size.decile[,SMB:=ifelse(Size_rank<6,"S","B")]
btm.decile[,HML:=ifelse(BtM_Rank<4,"L",ifelse(BtM_Rank<8,"M","H"))]
BM = btm.decile[Year>=1973,.(PERMNO, PERMCO, Year, Month, RET, HML,Lagged_MktCap)]
size = size.decile[Year>=1973,.(PERMNO, PERMCO, Year, Month, RET, SMB,Lagged_MktCap)]

setkey(BM,PERMCO,PERMNO,Year,Month)
setkey(size,PERMCO,PERMNO,Year,Month)

SizeBtM =  merge(BM,size)

SMB_HML =  SizeBtM[,.(Ret = weighted.mean(RET.x, Lagged_MktCap.x, na.rm = T)),.(Year,Month, HML,SMB)]
setkey(SMB_HML, Year,Month)
setorder(SMB_HML, Year)

SMB = SMB_HML[,.(SMB_Ret = (.SD[SMB=="S" & HML=="L",Ret] + .SD[SMB=="S" & HML=="M",Ret]+ .SD[SMB=="S" & HML=="H",Ret] - .SD[SMB=="B" & HML=="L",Ret] - .SD[SMB=="B" & HML=="M",Ret] - .SD[SMB=="B" & HML=="H",Ret])/3), by =.(Year, Month)]
HML = SMB_HML[, .(HML_Ret =(.SD[SMB=="S" & HML=="H",Ret] + .SD[SMB=="B" & HML=="H",Ret] -.SD[SMB=="S" & HML=="L",Ret] - .SD[SMB=="B" & HML=="L",Ret])/2), by =.(Year, Month)]

######## Q1
q1 = full_join(size_port, BtM_port, by=c("Year", "Month"))
q1<- merge(q1,HML)
q1<- as.data.table(merge(q1,SMB))

#--------------------------------------------------------------------------------------------------

#Q2
##read and clean FF data
FF_Factors<- as.data.table(read.csv("F-F_Research_Data_Factors.csv",skip=3,nrow=1125))
smb_FF<- as.data.table(read.csv("Portfolios_Formed_on_ME.csv",skip=12,nrows=1125))

FF_Factors[, Year:= X%/%100]   
FF_Factors[, Month:= X - Year*100]
FF_Factors= FF_Factors[ X>=197301& X<=201912,]
FF_Factors= FF_Factors[,.(Year, Month, SMB,HML,RF)]
setorder(FF_Factors,Year,Month)

smb_FF = smb_FF[X>=197301& X<=201912,]
smb_FF = smb_FF[, c(1,11:20)]
colnames(smb_FF) = c("date", 1:10)
smb_FF = melt(smb_FF, id.vars = "date")
smb_FF[, Year:= date%/%100]   
smb_FF[, Month:= date - Year*100]
smb_FF = smb_FF[, .(Year, Month, Ret= value/100, Size_rank = variable)]
FF_Factors = FF_Factors[, .(Year, Month, SMB = SMB/100, HML= HML/100, RF= RF/100)]
FF_merged.size = merge(smb_FF, FF_Factors[, c(1,2,5)], by =c("Year","Month"))

sz_mat = matrix(nrow=5, ncol=11)
colnames(sz_mat)=c(1:10,"LongShort")
rownames(sz_mat)=c("Excess Return","Standard Deviation","Sharpe Ratio","Skewness","Correlation")
for(i in 1:10){
  sz_mat[1,i] = mean(size_port[Size_rank==i,Size_Ret] - FF_merged.size[Size_rank==i,RF])*12
  sz_mat[2,i] = sd(size_port[Size_rank==i,Size_Ret])*sqrt(12)
  sz_mat[3,i] = sz_mat[1,i]/sz_mat[2,i]
  sz_mat[4,i] = skewness(size_port[Size_rank==i,Size_Ret])
  sz_mat[5,i] = cor(size_port[Size_rank==i,Size_Ret], FF_merged.size[Size_rank==i,Ret])
}

wml = size_port[Size_rank==1,Size_Ret] - FF_merged.size[Size_rank==10,Ret]
sz_mat[1,11] <- mean(wml)*12
sz_mat[2,11] <- sd(wml)*sqrt(12)
sz_mat[3,11] <- sz_mat[1,11]/sz_mat[2,11]
sz_mat[4,11] <- skewness(wml)
sz_mat[5,11] <- cor(wml, (FF_merged.size[Size_rank==1,Ret] - FF_merged.size[Size_rank==10,Ret]))

#-------------------------------------------------------------------------------------------------

#Q3
BM_FF = as.data.table(read.csv("Portfolios_Formed_on_BE-ME.csv",skip=23,nrow=1125))
#clean FF data
BM_FF = BM_FF[X>=197301& X<=201912,]
BM_FF = BM_FF[, c(1,11:20)]
colnames(BM_FF) = c("date", 1:10)
BM_FF = melt(BM_FF, id.vars = "date")
BM_FF[, Year:= date%/%100]   
BM_FF[, Month:= date - Year*100]
BM_FF = BM_FF[, .(Year, Month, Ret= value/100, BtM_rank = variable)]
FF_merged.BtM = merge(BM_FF, FF_Factors[, c(1,2,5)], by =c("Year","Month"))


bm_mat = matrix(nrow=5, ncol=11)
colnames(bm_mat)=c(1:10,"LongShort")
rownames(bm_mat)=c("Excess Return","Standard Deviation","Sharpe Ratio","Skewness","Correlation")
for(i in 1:10){
  bm_mat[1,i] = mean(BtM_port[BtM_Rank==i,BtM_Ret] - FF_merged.BtM[BtM_rank==i,RF])*12
  bm_mat[2,i] = sd(BtM_port[BtM_Rank==i,BtM_Ret])*sqrt(12)
  bm_mat[3,i] = bm_mat[1,i]/bm_mat[2,i]
  bm_mat[4,i] = skewness(BtM_port[BtM_Rank==i,BtM_Ret])
  bm_mat[5,i] = cor(BtM_port[BtM_Rank==i,BtM_Ret], FF_merged.BtM[BtM_rank==i,Ret])
}


wml = BtM_port[BtM_Rank==10,BtM_Ret] - FF_merged.BtM[BtM_rank==1,Ret]
bm_mat[1,11] <- mean(wml)*12
bm_mat[2,11] <- sd(wml)*sqrt(12)
bm_mat[3,11] <- bm_mat[1,11]/bm_mat[2,11]
bm_mat[4,11] <- skewness(wml)
bm_mat[5,11] <- cor(wml, (FF_merged.BtM[BtM_rank==10,Ret] - FF_merged.BtM[BtM_rank==1,Ret]))

#------------------------------------------------------------------------------------------------

#Q4

test<- as.data.table(read.csv("F-F_Research_Data_Factors.csv",skip=3,nrow=1125))
test[,date:=as.yearmon(as.character(X),"%Y%m")]
test= test[X>=200501]

test[,mktcum:=cumprod(1+Mkt.RF/100)]
test[,smbcum:=cumprod(1+SMB/100)]
test[,hmlcum:=cumprod(1+HML/100)]
test=test[,.(date,mktcum,smbcum,hmlcum)]

matplot(test$date,test[,2:4],type="l", lwd = 2, lty = 1, main = "Recent Performance of SMB and HML",
        ylab = "Cumulative Returns", col = c(1:3))
legend("topleft", legend = c("Market","SMB","HML"), lwd = 2, col = c(1:3))



#--------------------------------------------------------------------------------------------------
#Q5
setkey(SMB,Year,Month)
setkey(FF_Factors,Year,Month)
SMB_mat <- matrix(nrow=5, ncol=1)
rownames(SMB_mat)=c("Excess Return","Standard Deviation","Sharpe Ratio","Skewness","Correlation")
colnames(SMB_mat)="SMB"
SMB_mat[1,1] <- mean(SMB$SMB_Ret - FF_Factors$RF)*12
SMB_mat[2,1] <- sd(SMB$SMB_Ret)*sqrt(12)
SMB_mat[3,1] <- SMB_mat[1,1]/SMB_mat[2,1]
SMB_mat[4,1] <- skewness(SMB$SMB_Ret)
SMB_mat[5,1] <- cor(SMB$SMB_Ret, FF_Factors$SMB)


setkey(HML,Year,Month)
HML_mat <- matrix(nrow=5, ncol=1)
rownames(HML_mat)=c("Excess Return","Standard Deviation","Sharpe Ratio","Skewness","Correlation")
colnames(HML_mat)="HML"
HML_mat[1,1] <- mean(HML$HML_Ret - FF_Factors$RF)*12
HML_mat[2,1] <- sd(HML$HML_Ret)*sqrt(12)
HML_mat[3,1] <- HML_mat[1,1]/HML_mat[2,1]
HML_mat[4,1] <- skewness(HML$HML_Ret)
HML_mat[5,1] <- cor(HML$HML_Ret, FF_Factors$HML)

