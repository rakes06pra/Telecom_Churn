setwd("C:\\Jig14676\\Telecom_Project")
getwd()
tele<-read.csv("telecomfinal.csv",stringsAsFactors = T)
options(scipen = 999)
library(dplyr)

names(tele)
str(tele)

summary(tele)


##---------Creating Data Quality Report(dqr)-------------##
#Extracting Variable names
VariableName <- names(tele)
dqr <- as.data.frame(VariableName)
rm(VariableName)

#Recording Data Type for each Variable
dqr$DataTypes <- sapply(tele,class)

#No. of Records for each Variables
dqr$No.ofRecords<-nrow(tele)

#counting No. of Unique Values for each variable
for(i in 1:ncol(tele)){
  dqr$UniqueRecords[i]<-length(unique(tele[,i]))
}

#No. of observations available for each variable and its percentage
dqr$DataAvailable<-colSums(!is.na(tele))
dqr$AvailablePercentage<-round(colMeans(!is.na(tele)),4)

#Total and Percentage of Missing Values for each Variable
dqr$Missing<-colSums(is.na(tele))
dqr$MissingPercentage<-round(colMeans(is.na(tele)),4)

#Minimum, Maximum, Mean, Quantile Values for each Variable
for(i in 1:ncol(tele)){
  dqr$Minimum[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",min(tele[,i],na.rm = T),0),2)
  dqr$Maximum[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",max(tele[,i],na.rm = T),0),2)
  dqr$Mean[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",mean(tele[,i],na.rm = T),0),2)
  dqr$fifthPercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.05,na.rm = T),0),2)
  dqr$tenthPercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.10,na.rm = T),0),2)
  dqr$twentyfifthPercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.25,na.rm = T),0),2)
  dqr$fiftythPercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.50,na.rm = T),0),2)
  dqr$seventyfifthPercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.75,na.rm = T),0),2)
  dqr$ninetythPercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.90,na.rm = T),0),2)
  dqr$ninetyfifthPercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.95,na.rm = T),0),2)
}

str(dqr)

#Exporting Data Quality Report
write.csv(dqr,"Data Quality Report.csv",row.names = F)


#Missing Value treatment for var "retdays" and Creating Dummy Variable
summary(tele$retdays)
sort(unique(tele$retdays),na.last = F)
tele$retdays_1<-ifelse(is.na(tele$retdays)==TRUE,0,1)
str(tele$retdays_1)
summary(tele$retdays_1)

#Omitting variables with more than 15% missing values and creating a new data set
tele1<-tele[,colMeans(is.na(tele))<=0.15]

#Variable drop_blk_Mean is created by adding vars BLCK_DAT_MEAN + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN
#So omitting variable blck_dat_Mean
names(tele1)
tele1<-tele1[,-50]

##***********Data Exploration => Profiling (dat-Continuous Variables, datC-Categorical Variables)**********************##

##---------Deciling Continuous Variables Basis Target Variable Churn------------##

names(tele1)
str(tele1)

#Variable-1 'mou_Mean'
summary(tele1$mou_Mean)
tele1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(tele1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-round(dat1$n/dat1$N,2)
dat1$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(tele1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))


#Variable-2 'totmrc_Mean'
summary(tele1$totmrc_Mean)
tele1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(tele1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-round(dat2$n/dat2$N,2)
dat2$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(tele1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))


#Variable-3 'rev_Range'
summary(tele1$rev_Range)
tele1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(tele1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-round(dat3$n/dat3$N,2)
dat3$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(tele1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))



#Variable-4 'mou_Range'
summary(tele1$mou_Range)
tele1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(tele1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(tele1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))



#Variable-5 'change_mou'
summary(tele1$change_mou)
tele1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(tele1%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(tele1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))



#Variable-6 'drop_blk_Mean'
summary(tele1$drop_blk_Mean)
tele1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(tele1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$LessThan<-unclass(tele1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6))



#Variable-7 'drop_vce_Range'
summary(tele1$drop_vce_Range)
tele1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(tele1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$LessThan<-unclass(tele1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7))


#Variable-8 'owylis_vce_Range'
summary(tele1$owylis_vce_Range)
tele1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(tele1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$LessThan<-unclass(tele1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))



#Variable-9 'mou_opkv_Range'
summary(tele1$mou_opkv_Range)
tele1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(tele1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$LessThan<-unclass(tele1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))



#Variable-10 'months'
summary(tele1$months)
tele1%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(tele1%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(tele1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))



#Variable-11 'totcalls'
summary(tele1$totcalls)
tele1%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(tele1%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$LessThan<-unclass(tele1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))



#Variable-12 'eqpdays'
summary(tele1$eqpdays)

#Missing value treatment - since only 1 missing observation(NA) will remove it.
index<-which(is.na(tele1$eqpdays))
tele1<-tele1[-index,]

tele1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(tele1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]][1:10]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]][1:10]
dat12$LessThan<-unclass(tele1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]][1:10]
dat12$varname<-rep("eqpdays",nrow(dat12))



#Variable-13 'custcare_Mean' ==> ***** Getting less than 4 deciles. Omit *******
summary(tele1$custcare_Mean)
tele1%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$varname<-rep("custcare_Mean",nrow(dat13))

plot(tele1$churn,tele1$custcare_Mean, col = "red")


#Variable-14 'callwait_Mean'
summary(tele1$callwait_Mean)
tele1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(tele1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$LessThan<-unclass(tele1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))



#Variable-15 'iwylis_vce_Mean'
summary(tele1$iwylis_vce_Mean)
tele1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(tele1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$LessThan<-unclass(tele1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))



#Variable-16 'callwait_Range' ==> ***** Getting less than 4 deciles. Omit *******
summary(tele1$callwait_Range)
tele1%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$varname<-rep("callwait_Range",nrow(dat16))

plot(tele1$churn,tele1$callwait_Range,col = "red")


#Variable-17 'ccrndmou_Range' ==> ***** Getting less than 4 deciles. Omit *******
summary(tele1$ccrndmou_Range)
tele1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))

plot(tele1$churn,tele1$ccrndmou_Range,col = "red")



#Variable-18 'adjqty'
summary(tele1$adjqty)
tele1%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(tele1%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$LessThan<-unclass(tele1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))



#Variable-19 'ovrrev_Mean'
summary(tele1$ovrrev_Mean)
tele1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(tele1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$LessThan<-unclass(tele1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))



#Variable-20 'rev_Mean'
summary(tele1$rev_Mean)
tele1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(tele1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$LessThan<-unclass(tele1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))



#Variable-21 'ovrmou_Mean'
summary(tele1$ovrmou_Mean)
tele1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(tele1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$LessThan<-unclass(tele1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))



#Variable-22 'comp_vce_Mean'  ==>**Data Transformation then delete****
summary(tele1$comp_vce_Mean)
tele1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(tele1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$LessThan<-unclass(tele1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))



#Variable-23 'plcd_vce_Mean'  ==>**Data Transformation then delete****
summary(tele1$plcd_vce_Mean)
tele1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(tele1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$LessThan<-unclass(tele1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))



#Variable-24 'avg3mou'
summary(tele1$avg3mou)
tele1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(tele1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$LessThan<-unclass(tele1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))



#Variable-25 'avgmou'
summary(tele1$avgmou)
tele1%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(tele1%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$LessThan<-unclass(tele1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))



#Variable-26 'avg3qty'
summary(tele1$avg3qty)
tele1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(tele1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$LessThan<-unclass(tele1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))



#Variable-27 'avgqty'
summary(tele1$avgqty)
tele1%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(tele1%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$LessThan<-unclass(tele1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))




#Variable-28 'avg6mou'
summary(tele1$avg6mou)
tele1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(tele1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$LessThan<-unclass(tele1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))



#Variable-29 'avg6qty'
summary(tele1$avg6qty)
tele1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(tele1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$LessThan<-unclass(tele1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))




#Variable-30 'age1'  ===> ***Use as Factor****
summary(tele1$age1)
tele1%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(tele1%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat30$LessThan<-unclass(tele1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat30$varname<-rep("age1",nrow(dat30))



#Variable-31 'age2'  ===> *****Getting less than 4 deciles. Use as Factor****
summary(tele1$age2)
tele1%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$varname<-rep("age2",nrow(dat31))

plot(tele1$churn,tele1$age2,col="red")



#Variable-32 'models'  ===> *****Getting less than 4 deciles. Use as Factor****
summary(tele1$models)
tele1%>%mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$varname<-rep("models",nrow(dat32))

plot(tele1$churn,tele1$models,col="red")



#Variable-33 'hnd_price'  ===> ***Use as Factor variable****
summary(tele1$hnd_price)
tele1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$N<-unclass(tele1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat33$LessThan<-unclass(tele1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat33$varname<-rep("hnd_price",nrow(dat33))



#Variable-34 'actvsubs'  ===> ***Factor variable****
summary(tele1$actvsubs)
tele1%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$varname<-rep("actvsubs",nrow(dat34))

plot(tele1$churn,tele1$actvsubs,col = "red")



#Variable-35 'uniqsubs'  ===> ***Factor variable****
summary(tele1$uniqsubs)
tele1%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$varname<-rep("uniqsubs",nrow(dat35))

plot(tele1$churn,tele1$uniqsubs,col = "red")



#Variable-36 'forgntvl'  ===> ***Factor variable****
summary(tele1$forgntvl)
tele1%>%mutate(dec=ntile(forgntvl,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$varname<-rep("forgntvl",nrow(dat36))

plot(tele1$churn,tele1$forgntvl,col = "red")



#Variable-37 'opk_dat_Mean'   ===> ***Omit****Getting less than 4 decile
summary(tele1$opk_dat_Mean)
tele1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$varname<-rep("opk_dat_Mean",nrow(dat37))



#Variable-38 'mtrcycle'   ===> ****Factor Variable*****
summary(tele1$mtrcycle)
tele1%>%mutate(dec=ntile(mtrcycle,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$varname<-rep("mtrcycle",nrow(dat38))



#Variable-39 'truck'   ===> ****Factor Variable*****
summary(tele1$truck)
tele1%>%mutate(dec=ntile(truck,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$varname<-rep("truck",nrow(dat39))


#Variable-40 'roam_Mean'   ===> ***Omit****Getting less than 4 decile
summary(tele1$roam_Mean)
tele1%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$varname<-rep("roam_Mean",nrow(dat40))


#Variable-41 'recv_sms_Mean'   ===> ***Omit****Getting less than 4 decile
summary(tele1$recv_sms_Mean)
tele1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$varname<-rep("recv_sms_Mean",nrow(dat41))



#Variable-42 'mou_pead_Mean'   ===> ***Omit****Getting less than 4 decile
summary(tele1$mou_pead_Mean)
tele1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$varname<-rep("mou_pead_Mean",nrow(dat42))



#Variable-43 'da_Mean'
summary(tele1$da_Mean)
tele1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(tele1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat43$LessThan<-unclass(tele1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat43$varname<-rep("da_Mean",nrow(dat43))



#Variable-44 'da_Range'
summary(tele1$da_Range)
tele1%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(tele1%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat44$LessThan<-unclass(tele1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat44$varname<-rep("da_Range",nrow(dat44))



#Variable-45 'datovr_Mean'   ===> ***Omit****Getting less than 4 decile
summary(tele1$datovr_Mean)
tele1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$varname<-rep("datovr_Mean",nrow(dat45))



#Variable-46 'datovr_Range'   ===> ***Omit****Getting less than 4 decile
summary(tele1$datovr_Range)
tele1%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$varname<-rep("datovr_Range",nrow(dat46))



#Variable-47 'drop_dat_Mean'   ===> ***Omit****Getting less than 4 decile
#mostly 95% of data has values 0.
summary(tele1$drop_dat_Mean)
tele1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$varname<-rep("drop_dat_Mean",nrow(dat47))



#Variable-48 'drop_vce_Mean'
summary(tele1$drop_vce_Mean)
tele1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$N<-unclass(tele1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat48$churn_perc<-dat48$n/dat48$N
dat48$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat48$LessThan<-unclass(tele1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat48$varname<-rep("drop_vce_Mean",nrow(dat48))



#Variable-49 'adjmou'
summary(tele1$adjmou)
tele1%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$N<-unclass(tele1%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat49$churn_perc<-dat49$n/dat49$N
dat49$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat49$LessThan<-unclass(tele1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat49$varname<-rep("adjmou",nrow(dat49))



#Variable-50 'totrev'
summary(tele1$totrev)
tele1%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$N<-unclass(tele1%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat50$churn_perc<-dat50$n/dat50$N
dat50$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat50$LessThan<-unclass(tele1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat50$varname<-rep("totrev",nrow(dat50))



#Variable-51 'adjrev'
summary(tele1$adjrev)
tele1%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$N<-unclass(tele1%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat51$churn_perc<-dat51$n/dat51$N
dat51$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat51$LessThan<-unclass(tele1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat51$varname<-rep("adjrev",nrow(dat51))



#Variable-52 'avgrev'
summary(tele1$avgrev)
tele1%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat52
dat52$N<-unclass(tele1%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat52$churn_perc<-dat52$n/dat52$N
dat52$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat52$LessThan<-unclass(tele1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat52$varname<-rep("avgrev",nrow(dat52))




#Variable-53 'comp_dat_Mean'   ===> **Getting less than 4 deciles***Omit***
summary(tele1$comp_dat_Mean)
tele1%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat53
dat53$varname<-rep("comp_dat_Mean",nrow(dat53))




#Variable-54 'plcd_dat_Mean'   ===> **Getting less than 4 deciles***Omit***
summary(tele1$plcd_dat_Mean)
tele1%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat54
dat54$varname<-rep("plcd_dat_Mean",nrow(dat54))


##*** Data Transforamtion. Creating Dummy Variables ****###

#Variable-55 Create Dummy Variable plcd_attempt_Mean and Deciling
tele1$plcd_attempt_Mean <- tele1$plcd_dat_Mean + tele1$plcd_vce_Mean

summary(tele1$plcd_attempt_Mean)
tele1%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat55
dat55$N<-unclass(tele1%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat55$churn_perc<-dat55$n/dat55$N
dat55$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_attempt_Mean)))[[2]]
dat55$LessThan<-unclass(tele1%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_attempt_Mean)))[[2]]
dat55$varname<-rep("plcd_attempt_Mean",nrow(dat55))


#Variable-56 Create Dummy Variable complete_Mean and Deciling
tele1$complete_Mean <- tele1$comp_dat_Mean + tele1$comp_vce_Mean

summary(tele1$complete_Mean)
tele1%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat56
dat56$N<-unclass(tele1%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat56$churn_perc<-dat56$n/dat56$N
dat56$GreaterThan<-unclass(tele1%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(min(complete_Mean)))[[2]]
dat56$LessThan<-unclass(tele1%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(max(complete_Mean)))[[2]]
dat56$varname<-rep("complete_Mean",nrow(dat56))


#Adding all appropriate dat1 to dat54 objects to create a dat object
dat<-rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10,
           dat11, dat12, dat14, dat15, dat18, dat19, dat20, dat21,
           dat22, dat23, dat24, dat25, dat26, dat27, dat28, dat29,
           dat30, dat33, dat43, dat44, dat48, dat49, dat50, dat51,
           dat52, dat55, dat56)

#Export Deciled Variables
write.csv(dat,"Deciled Usable Continuous Variables.csv",row.names = F)


#Removed variables that could not be deciled as will come insignificant in the model
#Also removing variables (plcd_dat_Mean, plcd_vce_Mean, comp_dat_Mean, comp_vce_Mean) already transformed.
names(tele1)
tele1<-tele1[,-c(13,16,17,22,23,45,48:50,56:58,65,66)]

names(tele1)


##-------------Categorical Variables---------##

#CateVariable-1 'crclscod'  ==> **some of the Levels show less than 5% churn rate. So omit these variables as it will come insignificant.***
summary(tele1$crclscod)
tele1%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC1
datC1$N<-unclass(tele1%>%filter(crclscod%in%datC1$levels)%>%count(crclscod))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("crclscod",nrow(datC1))


#CateVariable-2 'asl_flag'
summary(tele1$asl_flag)
tele1%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC2
datC2$N<-unclass(tele1%>%filter(asl_flag%in%datC2$levels)%>%count(asl_flag))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("asl_flag",nrow(datC2))


#CateVariable-3 'prizm_social_one'
summary(tele1$prizm_social_one)
tele1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC3
datC3$N<-unclass(tele1%>%filter(prizm_social_one%in%datC3$levels)%>%count(prizm_social_one))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("prizm_social_one",nrow(datC3))


#CateVariable-4 'area'
summary(tele1$area)
tele1%>%count(churn,levels=area)%>%filter(churn==1)->datC4
datC4$N<-unclass(tele1%>%filter(area%in%datC4$levels)%>%count(area))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("area",nrow(datC4))



#CateVariable-5 'refurb_new'
summary(tele1$refurb_new)
tele1%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC5
datC5$N<-unclass(tele1%>%filter(refurb_new%in%datC5$levels)%>%count(refurb_new))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("refurb_new",nrow(datC5))


#CateVariable-6 'hnd_webcap'
summary(tele1$hnd_webcap)
tele1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC6
datC6$N<-unclass(tele1%>%filter(hnd_webcap%in%datC6$levels)%>%count(hnd_webcap))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N
datC6$Var.Name<-rep("hnd_webcap",nrow(datC6))


#CateVariable-7 'marital'
summary(tele1$marital)
tele1%>%count(churn,levels=marital)%>%filter(churn==1)->datC7
datC7$N<-unclass(tele1%>%filter(marital%in%datC7$levels)%>%count(marital))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("marital",nrow(datC7))


#CateVariable-8 'ethnic'
summary(tele1$ethnic)
tele1%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC8
datC8$N<-unclass(tele1%>%filter(ethnic%in%datC8$levels)%>%count(ethnic))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("ethnic",nrow(datC8))


#CateVariable-9 'car_buy'
summary(tele1$car_buy)
tele1%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC9
datC9$N<-unclass(tele1%>%filter(car_buy%in%datC9$levels)%>%count(car_buy))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("car_buy",nrow(datC9))


#CateVariable-10 'csa'  ==> **some of the Levels show less than 5% churn rate. So omit these variables as it will come insignificant.***
summary(tele1$csa)
tele1%>%count(churn,levels=csa)%>%filter(churn==1)->datC10
datC10$N<-unclass(tele1%>%filter(csa%in%datC10$levels)%>%count(csa))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("csa",nrow(datC10))


#CateVariable-11 'retdays_1'
summary(tele1$retdays_1)
tele1%>%count(churn,levels=retdays_1)%>%filter(churn==1)->datC11
datC11$N<-unclass(tele1%>%filter(retdays_1%in%datC11$levels)%>%count(retdays_1))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("retdays_1",nrow(datC11))


#Use Variables as Factors => age2, models, actvsubs, uniqsubs, forgntvl, mtrcycle, truck

#CateVariable-12 'age2'
summary(tele1$age2)
tele1%>%count(churn,levels=age2)%>%filter(churn==1)->datC12
datC12$N<-unclass(tele1%>%filter(age2%in%datC12$levels)%>%count(age2))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("age2",nrow(datC12))


#CateVariable-13 'models'
summary(tele1$models)
tele1%>%count(churn,levels=models)%>%filter(churn==1)->datC13
datC13$N<-unclass(tele1%>%filter(models%in%datC13$levels)%>%count(models))[[2]]
datC13$ChurnPerc<-datC13$n/datC13$N
datC13$Var.Name<-rep("models",nrow(datC13))


#CateVariable-14 'actvsubs'
summary(tele1$actvsubs)
tele1%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC14
datC14$N<-unclass(tele1%>%filter(actvsubs%in%datC14$levels)%>%count(actvsubs))[[2]]
datC14$ChurnPerc<-datC14$n/datC14$N
datC14$Var.Name<-rep("actvsubs",nrow(datC14))


#CateVariable-15 'uniqsubs'
summary(tele1$uniqsubs)
tele1%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC15
datC15$N<-unclass(tele1%>%filter(uniqsubs%in%datC15$levels)%>%count(uniqsubs))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("uniqsubs",nrow(datC15))


#CateVariable-16 'forgntvl'
summary(tele1$forgntvl)
tele1%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC16
datC16$N<-unclass(tele1%>%filter(forgntvl%in%datC16$levels)%>%count(forgntvl))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("forgntvl",nrow(datC16))


#CateVariable-17 'mtrcycle'
summary(tele1$mtrcycle)
tele1%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC17
datC17$N<-unclass(tele1%>%filter(mtrcycle%in%datC17$levels)%>%count(mtrcycle))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("mtrcycle",nrow(datC17))


#CateVariable-18 'truck'
summary(tele1$truck)
tele1%>%count(churn,levels=truck)%>%filter(churn==1)->datC18
datC18$N<-unclass(tele1%>%filter(truck%in%datC18$levels)%>%count(truck))[[2]]
datC18$ChurnPerc<-datC18$n/datC18$N
datC18$Var.Name<-rep("truck",nrow(datC18))


#CateVariable-19 'hnd_price'
summary(tele1$hnd_price)
tele1%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC19
datC19$N<-unclass(tele1%>%filter(hnd_price%in%datC19$levels)%>%count(hnd_price))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("hnd_price",nrow(datC19))


#Adding datC1 to datC19 objects to create datC objects

datC_1 <- rbind(datC1, datC2, datC3, datC4, datC5, datC6, datC7, datC8, datC9, datC10, datC11)
datC_2 <- rbind(datC12, datC13, datC14, datC15, datC16, datC17, datC18, datC19)

#Export Deciled Categorical variables
write.csv(datC_1,"Deciled Categorical Variables1.csv",row.names = F)
write.csv(datC_2,"Deciled Categorical Variables2.csv",row.names = F)


#Removing Variables with levels less than 5% churn rate as will come insignificant
names(tele1)
tele1<-tele1[,-c(25,44)]
names(tele1)


#******Data Preparation*******#

#----------Outlier Treatment-----------#

#--------Continuous Variables--------#
#Box Plot Method

names(tele1)
summary(tele1)
str(tele1)

#Factor Vaiables: asl_flag,prizm_social_one,area,refurb_new,hnd_webcap,marital,ethnic,
#age1,age2,models,hnd_price,actvsubs,uniqsubs,forgntvl,mtrcycle,truck,churn,car_buy,Customer_ID,retdays_1

str(tele1)
list<-names(tele1)
list

#Remove Categorical Variables from the list
list<-list[-c(25:42,50,51)]
list

#Outliers Plot
par("mar")
par(mar=c(1,1,1,1))

par(mfrow=c(3,11))

for(i in 1:length(list)){
  boxplot(tele1[,list[i]],main=list[i])
}

for(i in 1:length(list)){
  plot(tele1[,list[i]],main=list[i])
}

#Outlier Treatment
for(i in 1:length(list)){
  x<-boxplot(tele1[,list[i]],main=list[i])
  out<-x$out
  index<-which(tele1[,list[i]]%in%x$out)
  tele1[index,list[i]]<-mean(tele1[,list[i]],na.rm=T)
  rm(x)
  rm(out)
}

par(mfrow=c(3,11))

#Checking after treatement
for(i in 1:length(list)){
  boxplot(tele1[,list[i]],main=list[i])
}

for(i in 1:length(list)){
  plot(tele1[,list[i]],main=list[i])
}

dev.off()    # to make plots bck to default


#Missing Value Treatment

summary(tele1)
names(tele1)
str(tele1)


#Factor Vaiables: asl_flag,prizm_social_one,area,refurb_new,hnd_webcap,marital,ethnic,
#age1,age2,models,hnd_price,actvsubs,uniqsubs,forgntvl,mtrcycle,truck,car_buy,retdays_1

index1<-which(is.na(tele1[,c(1:5)]))
tele1<-tele1[-index1,]
summary(tele1)

index2<-which(is.na(tele1$change_mou))
tele1<-tele1[-index2,]

summary(tele1)

index3<-which(is.na(tele1$area))
tele1<-tele1[-index3,]

index4<-which(is.na(tele1$marital))
tele1<-tele1[-index4,]

summary(tele1)

#Mean Imputation
tele1$avg6mou[is.na(tele1$avg6mou)]<-mean(tele1$avg6mou,na.rm = T)

tele1$avg6qty[is.na(tele1$avg6qty)]<-mean(tele1$avg6qty,na.rm = T)

tele1$hnd_price[is.na(tele1$hnd_price)]<-mean(tele1$hnd_price,na.rm = T)

summary(tele1)

#Creating separate category "Missing" for Factor Variable

#Variable "prizm_social_one"
tele1$prizm_social_one_1<-ifelse(is.na(tele1$prizm_social_one),"Missing",as.factor(tele1$prizm_social_one))
str(tele1$prizm_social_one_1)
tele1$prizm_social_one_1<-as.factor(tele1$prizm_social_one_1)
summary(tele1$prizm_social_one)
summary(tele1$prizm_social_one_1)
tele1$prizm_social_one_1<-factor(tele1$prizm_social_one_1,labels=c("C","R","S","T","U","Missing"))
summary(tele1$prizm_social_one_1)

names(tele1)
tele1<-tele1[,-26]
names(tele1)

#Variable "hnd_webcap"
tele1$hnd_webcap_1<-ifelse(is.na(tele1$hnd_webcap),"Missing",as.factor(tele1$hnd_webcap))
str(tele1$hnd_webcap_1)
tele1$hnd_webcap_1<-as.factor(tele1$hnd_webcap_1)
summary(tele1$hnd_webcap)
summary(tele1$hnd_webcap_1)
tele1$hnd_webcap_1<-factor(tele1$hnd_webcap_1,labels = c("UNKW","WC","WCMB","Missing"))
summary(tele1$hnd_webcap_1)

names(tele1)
tele1<-tele1[,-28]
names(tele1)

summary(tele1)

#Checking churn rate after Imputations
table(tele$churn)/nrow(tele)
table(tele1$churn)/nrow(tele1)

str(tele1)


#Convert to Factors and create dummy variables
#Variables: age1, age2, models, hnd_price, actvsubs, uniqsubs, forgntvl, mtrcycle, truck, Customer_ID, churn
str(tele1$age1)
tele1$age1_1<-ifelse(tele1$age1==0,"Default",ifelse(tele1$age1<=30,"Young",ifelse(tele1$age1>30 & tele1$age1<=55,"Mid Age","Old")))
str(tele1$age1_1)
tele1$age1_1<-as.factor(tele1$age1_1)
summary(tele1$age1_1)


str(tele1$age2)
tele1$age2_1<-ifelse(tele1$age2==0,"Default",ifelse(tele1$age2<=30,"Young",ifelse(tele1$age2>30 & tele1$age2<=55,"Mid Age","Old")))
str(tele1$age2_1)
tele1$age2_1<-as.factor(tele1$age2_1)
summary(tele1$age2_1)


names(tele1)
tele1<-tele1[,-30]  #remove age1 variable
names(tele1)

names(tele1)
tele1<-tele1[,-30]  #remove age2 variable
names(tele1)


str(tele1$models)
summary(tele1$models)
tele1$models<-as.factor(tele1$models)
summary(tele1$models)


str(tele1$hnd_price)
summary(tele1$hnd_price)
tele1$hnd_price<-as.factor(tele1$hnd_price)
summary(tele1$hnd_price)


str(tele1$actvsubs)
summary(tele1$actvsubs)
tele1$actvsubs<-as.factor(tele1$actvsubs)
summary(tele1$actvsubs)


str(tele1$uniqsubs)
summary(tele1$uniqsubs)
tele1$uniqsubs<-as.factor(tele1$uniqsubs)
summary(tele1$uniqsubs)


str(tele1$forgntvl)
summary(tele1$forgntvl)
tele1$forgntvl<-as.factor(tele1$forgntvl)
summary(tele1$forgntvl)


str(tele1$mtrcycle)
summary(tele1$mtrcycle)
tele1$mtrcycle<-as.factor(tele1$mtrcycle)
summary(tele1$mtrcycle)


str(tele1$truck)
summary(tele1$truck)
tele1$truck<-as.factor(tele1$truck)
summary(tele1$truck)


##******Logistic Regression Model Building******##

# Splitting into Test and Training Samples
set.seed(200)
index_sample<-sample(nrow(tele1),0.70*nrow(tele1),replace = F)
train<-tele1[index_sample,]
test<-tele1[-index_sample,]


#Checking Churn rate
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

names(train)


#Building Logistic Regression Model after excluding variable "Customer_ID"
mod<-glm(churn~.,data=train[,-46],family = "binomial")
summary(mod)


#Stepwise Regression Model
step(mod,direction = "both")  #This is taking alot of time to run.So doing manually.


##*****Creating Dummy Variables for Factor Variables with signiicant levels*****##

summary(tele1$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag == "Y",1,0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y",1,0)


summary(train$area)
train$area_CaliforniaNorth<-ifelse(train$area == "CALIFORNIA NORTH AREA",1,0)
test$area_CaliforniaNorth<-ifelse(test$area == "CALIFORNIA NORTH AREA",1,0)

train$area_CentralSouthTexas<-ifelse(train$area == "CENTRAL/SOUTH TEXAS AREA",1,0)
test$area_CentralSouthTexas<-ifelse(test$area == "CENTRAL/SOUTH TEXAS AREA",1,0)

train$area_NorthFlorida<-ifelse(train$area == "NORTH FLORIDA AREA",1,0)
test$area_NorthFlorida<-ifelse(test$area == "NORTH FLORIDA AREA",1,0)

train$area_NorthWestRockyMount<-ifelse(train$area == "NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$area_NorthWestRockyMount<-ifelse(test$area == "NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

train$area_SouthFlorida<-ifelse(train$area == "SOUTH FLORIDA AREA",1,0)
test$area_SouthFlorida<-ifelse(test$area == "SOUTH FLORIDA AREA",1,0)

train$area_SouthWest<-ifelse(train$area == "SOUTHWEST AREA",1,0)
test$area_SouthWest<-ifelse(test$area == "SOUTHWEST AREA",1,0)

train$area_Tennessee<-ifelse(train$area == "TENNESSEE AREA",1,0)
test$area_Tennessee<-ifelse(test$area == "TENNESSEE AREA",1,0)


summary(train$refurb_new)
train$refurb_new_R<-ifelse(train$refurb_new == "R",1,0)
test$refurb_new_R<-ifelse(test$refurb_new == "R",1,0)


summary(train$ethnic)
train$ethnic_C<-ifelse(train$ethnic == "C",1,0)
test$ethnic_C<-ifelse(test$ethnic == "C",1,0)

train$ethnic_N<-ifelse(train$ethnic == "N",1,0)
test$ethnic_N<-ifelse(test$ethnic == "N",1,0)

train$ethnic_O<-ifelse(train$ethnic == "O",1,0)
test$ethnic_O<-ifelse(test$ethnic == "O",1,0)

train$ethnic_S<-ifelse(train$ethnic == "S",1,0)
test$ethnic_S<-ifelse(test$ethnic == "S",1,0)

train$ethnic_U<-ifelse(train$ethnic == "U",1,0)
test$ethnic_U<-ifelse(test$ethnic == "U",1,0)

train$ethnic_Z<-ifelse(train$ethnic == "Z",1,0)
test$ethnic_Z<-ifelse(test$ethnic == "Z",1,0)


summary(train$hnd_price)
train$hnd_price_79.98<-ifelse(train$hnd_price=="79.98999023",1,0)
test$hnd_price_79.98<-ifelse(test$hnd_price=="79.98999023",1,0)

train$hnd_price_105.08<-ifelse(train$hnd_price=="105.083038078331",1,0)
test$hnd_price_105.08<-ifelse(test$hnd_price=="105.083038078331",1,0)

train$hnd_price_129.98<-ifelse(train$hnd_price=="129.9899902",1,0)
test$hnd_price_129.98<-ifelse(test$hnd_price=="129.9899902",1,0)

train$hnd_price_149.98<-ifelse(train$hnd_price=="149.9899902",1,0)
test$hnd_price_149.98<-ifelse(test$hnd_price=="149.9899902",1,0)

train$hnd_price_199.98<-ifelse(train$hnd_price=="199.9899902",1,0)
test$hnd_price_199.98<-ifelse(test$hnd_price=="199.9899902",1,0)

train$hnd_price_249.98<-ifelse(train$hnd_price=="249.9899902",1,0)
test$hnd_price_249.98<-ifelse(test$hnd_price=="249.9899902",1,0)


summary(train$uniqsubs)
train$uniqsubs_2<-ifelse(train$uniqsubs=="2",1,0)
test$uniqsubs_2<-ifelse(test$uniqsubs=="2",1,0)

train$uniqsubs_3<-ifelse(train$uniqsubs=="3",1,0)
test$uniqsubs_3<-ifelse(test$uniqsubs=="3",1,0)

train$uniqsubs_4<-ifelse(train$uniqsubs=="4",1,0)
test$uniqsubs_4<-ifelse(test$uniqsubs=="4",1,0)

train$uniqsubs_5<-ifelse(train$uniqsubs=="5",1,0)
test$uniqsubs_5<-ifelse(test$uniqsubs=="5",1,0)

train$uniqsubs_6<-ifelse(train$uniqsubs=="6",1,0)
test$uniqsubs_6<-ifelse(test$uniqsubs=="6",1,0)

train$uniqsubs_7<-ifelse(train$uniqsubs=="7",1,0)
test$uniqsubs_7<-ifelse(test$uniqsubs=="7",1,0)

train$uniqsubs_9<-ifelse(train$uniqsubs=="9",1,0)
test$uniqsubs_9<-ifelse(test$uniqsubs=="9",1,0)

summary(train$truck)
train$truck_1<-ifelse(train$truck=="1",1,0)
test$truck_1<-ifelse(test$truck=="1",1,0)


summary(train$prizm_social_one_1)
train$prizm_social_one_1_R<-ifelse(train$prizm_social_one_1=="R",1,0)
test$prizm_social_one_1_R<-ifelse(test$prizm_social_one_1=="R",1,0)

train$prizm_social_one_1_T<-ifelse(train$prizm_social_one_1=="T",1,0)
test$prizm_social_one_1_T<-ifelse(test$prizm_social_one_1=="T",1,0)


summary(train$age1_1)
train$age1_1_MidAge<-ifelse(train$age1_1=="Mid Age",1,0)
test$age1_1_MidAge<-ifelse(test$age1_1=="Mid Age",1,0)

train$age1_1_Old<-ifelse(train$age1_1=="Old",1,0)
test$age1_1_Old<-ifelse(test$age1_1=="Old",1,0)

train$age1_1_Young<-ifelse(train$age1_1=="Young",1,0)
test$age1_1_Young<-ifelse(test$age1_1=="Young",1,0)


summary(train$age2_1)
train$age2_1_Old<-ifelse(train$age2_1=="Old",1,0)
test$age2_1_Old<-ifelse(test$age2_1=="Old",1,0)


## ****Recurring Model with Significant Factor Variable ******##
names(train)
mod1<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + 
            drop_vce_Range + owylis_vce_Range + mou_opkv_Range + months + totcalls + eqpdays + 
            callwait_Mean + iwylis_vce_Mean + adjqty + ovrrev_Mean + rev_Mean + ovrmou_Mean + 
            avg3mou + avgmou + avg3qty + avgqty + avg6mou + avg6qty + asl_flag_Y + area_CaliforniaNorth + 
            area_CentralSouthTexas + area_NorthFlorida + area_NorthWestRockyMount + area_SouthFlorida + 
            area_SouthWest + area_Tennessee + refurb_new_R + ethnic_C + ethnic_N + ethnic_O +
            ethnic_S + ethnic_U + ethnic_Z +  hnd_price_79.98+ hnd_price_105.08 + hnd_price_129.98 +
            hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + 
            uniqsubs_4 + uniqsubs_5 + uniqsubs_6 + uniqsubs_7 + uniqsubs_9 + truck_1 + adjmou + totrev + 
            retdays_1 + complete_Mean + prizm_social_one_1_R + prizm_social_one_1_T + age1_1_MidAge + 
            age1_1_Old +age1_1_Young  + age2_1_Old, data=train,family = "binomial")

summary(mod1)


#Still some of the variables are insignificant. Removing all and rerunning with significant variables.

mod2<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + 
            drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean +
            avgmou + avg3qty + avgqty + avg6mou + asl_flag_Y + area_CaliforniaNorth + 
            area_CentralSouthTexas + area_NorthFlorida + area_NorthWestRockyMount + area_SouthFlorida + 
            area_SouthWest + area_Tennessee + refurb_new_R + ethnic_C + ethnic_N + ethnic_O +
            ethnic_S + ethnic_U + ethnic_Z +  hnd_price_79.98+ hnd_price_105.08 + hnd_price_129.98 +
            hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + 
            uniqsubs_4 + uniqsubs_7 + totrev + retdays_1 + complete_Mean + prizm_social_one_1_R +
            prizm_social_one_1_T + age1_1_MidAge + age1_1_Old + age1_1_Young + age2_1_Old,
            data=train,family = "binomial")

summary(mod2)


#still "age1_1_Young" getting p value greater than 0.05. So removing it and rerunning model again
mod2<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + 
            drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean +
            avgmou + avg3qty + avgqty + avg6mou + asl_flag_Y + area_CaliforniaNorth + 
            area_CentralSouthTexas + area_NorthFlorida + area_NorthWestRockyMount + area_SouthFlorida + 
            area_SouthWest + area_Tennessee + refurb_new_R + ethnic_C + ethnic_N + ethnic_O +
            ethnic_S + ethnic_U + ethnic_Z +  hnd_price_79.98+ hnd_price_105.08 + hnd_price_129.98 +
            hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + 
            uniqsubs_4 + uniqsubs_7 + totrev + retdays_1 + complete_Mean + prizm_social_one_1_R +
            prizm_social_one_1_T + age1_1_MidAge + age1_1_Old + age2_1_Old,
          data=train,family = "binomial")

summary(mod2)

#All the variables have come significant. With probability values less than 5%.
#So this model can be finalised after checking for absence of multicollinearty.



##*****Model Diagnostics****##

#Checking For MultiCollinearity
library(car)
vif(mod2)
#Variables => Ideally vif values should be < 5. Choosing vif cut-off value of 5.
#4 of the variabels have vif of > 5, showing Multicollinearity and should be removed from the model
#Vars to remove from model are "mou_Mean","avgmou","avg3qty","avg6mou"

#Rerunning model after removing "mou_Mean","avgmou","avg3qty","avg6mou" to remove problem of MultiCollinearity

#Removing one by one and rerunning model.. Removing "mou_Mean" first
mod3<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + 
            drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean +
            avgmou + avg3qty + avgqty + avg6mou + asl_flag_Y + area_CaliforniaNorth + 
            area_CentralSouthTexas + area_NorthFlorida + area_NorthWestRockyMount + area_SouthFlorida + 
            area_SouthWest + area_Tennessee + refurb_new_R + ethnic_C + ethnic_N + ethnic_O +
            ethnic_S + ethnic_U + ethnic_Z +  hnd_price_79.98+ hnd_price_105.08 + hnd_price_129.98 +
            hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + 
            uniqsubs_4 + uniqsubs_7 + totrev + retdays_1 + complete_Mean + prizm_social_one_1_R +
            prizm_social_one_1_T + age1_1_MidAge + age1_1_Old + age2_1_Old,
          data=train,family = "binomial")

summary(mod3)


#All variables are coming significant.

##*****Model Diagnostic******##
#Checking For MultiCollinearity
vif(mod3)

##Now "avgmou" and "avg3qty" having vif value > 5.
#Now remove "avgmou" and rerun the model again.

mod4<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + 
            drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean +
            avg3qty + avgqty + avg6mou + asl_flag_Y + area_CaliforniaNorth + 
            area_CentralSouthTexas + area_NorthFlorida + area_NorthWestRockyMount + area_SouthFlorida + 
            area_SouthWest + area_Tennessee + refurb_new_R + ethnic_C + ethnic_N + ethnic_O +
            ethnic_S + ethnic_U + ethnic_Z +  hnd_price_79.98+ hnd_price_105.08 + hnd_price_129.98 +
            hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + 
            uniqsubs_4 + uniqsubs_7 + totrev + retdays_1 + complete_Mean + prizm_social_one_1_R +
            prizm_social_one_1_T + age1_1_MidAge + age1_1_Old + age2_1_Old,
            data=train,family = "binomial")

summary(mod4)


#All variables are coming significant.

##*****Model Diagnostic******##
#Checking For MultiCollinearity
vif(mod4)


##Now "avg3qty" having vif value > 5.
#Now remove "avg3qty" and rerun the model again.

mod5<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + 
            drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean +
            avgqty + avg6mou + asl_flag_Y + area_CaliforniaNorth + 
            area_CentralSouthTexas + area_NorthFlorida + area_NorthWestRockyMount + area_SouthFlorida + 
            area_SouthWest + area_Tennessee + refurb_new_R + ethnic_C + ethnic_N + ethnic_O +
            ethnic_S + ethnic_U + ethnic_Z +  hnd_price_79.98+ hnd_price_105.08 + hnd_price_129.98 +
            hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + 
            uniqsubs_4 + uniqsubs_7 + totrev + retdays_1 + complete_Mean + prizm_social_one_1_R +
            prizm_social_one_1_T + age1_1_MidAge + age1_1_Old + age2_1_Old,
            data=train,family = "binomial")

summary(mod5)

#All variables are coming significant.

##*****Model Diagnostic******##
#Checking For MultiCollinearity
vif(mod5)

#All variables vif values are below 5. Thus there is no MultiCollinearity.
#So this model(mod5) is finalised.


#Checking Confidence Interval
confint(mod5)


##**** Model Testing ******##

#Predicted value ==> Predicting the Probability of a customer churning.
pred<-predict(mod5,type = "response", newdata = test)

head(pred)

#Assuming cut-off probability as per the churn rate in dataset
table(tele1$churn)/nrow(tele1)

#As maximum kappa is related to cutoff 0.2380871 we would go with this cutoff value

pred1<-ifelse(pred>=0.2380871,1,0)

table(pred1)

##****** Checking Prediction Quality ****##

#Kappa Matrix
library(irr)
kappa2(data.frame(test$churn,pred1))


#Confusion Matrix
library(caret)

confusionMatrix(pred1,test$churn,positive = "1")
table(test$churn)


#The Confusion Matrix shows:
# 8613 - churned and correct
# 6201 - churned and incorrect
# 1879 - Did not churn and incorrect
# 2723 - Did not churn and correct

#ROCR Curve
library(ROCR)
pred2 <- prediction(pred1,test$churn) #This function is to transform the input data (any format) into a standardized format.
perform <- performance(pred2,"tpr","fpr")   #All kind of prdection evaluations are performed using this function
plot(perform,col="red")
abline(0,1,lty=8,col="grey")  #This function adds one or more straight lines through the current plot
auc <- performance(pred2,"auc")
auc

#The auc is 0.5865544 which is more than 0.50
#Also the curve seems to be well above the grey line.
#So the model seems to be ok and is acceptable.

#Gains Chart
library(gains)
#Take the vector of actual response and a vector of predictions and construct a gains table
gains(test$churn,predict(mod5,type = "response",newdata = test),groups = 10)

#The Gains chart shows that top 40% of the probabilities contains 52.9% of customers that are likely to churn


test$prob <- predict(mod5,type = "response",newdata = test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

##Top 30% of the probabilities scores lie between 0.2744831 and 0.3040815
#Top 40% of the probabilities scores lie between 0.2508403 and 0.2744831
#We can use this probability to extract the data of customers who are highly likely to churn.


###**** Answering Business Questions *******###
### Top Line Questions of Interest to Senior Management:


# 1. What are the top five factors driving likelihood of churn at Mobicom?

# Look at the beta coefficients for all variables of mod5

head(sort(abs(mod5$coefficients),decreasing = TRUE),100)

summary(mod5)

#from summary of my final model "Mod5"
## The model results show that the top 5 factors affecting churn are:
## a. uniqsubs_7               with beta coefficient of 0.7269356719
## b. retdays_1                with beta coefficient of 0.6730047077
## c. ethnic_O                 with beta coefficient of 0.3172275090
## d. area_NorthWestRockyMount with beta coefficient of 0.2830132187
## e. area_SouthFlorida        with beta coefficient of 0.2741197309

# The 1st factor explains, with the unit increase in level 7 of the variable uniqsubs, there is 0.7269356719 unit increase in churn.
# The 2nd factor explains, with the unit increase in the variable retdays, there is 0.6730047077 unit increase in churn.
# The 3rd factor explains, with the unit increase in level O of the variable ethnic, there is 0.3172275090 unit increase in churn.
# The 4th factor explains, with the unit increase in level NorthWestRockyMount of the variable area, there is 0.2830132187 unit increase in churn.
# The 5th factor explains, with the unit increase in level SouthFlorida of the variable area, there is 0.2741197309 unit increase in churn.

#So special offer should be given to family or users with more than 7 unique subscribers.
#Special offers should be given to those users who make retention calls.
#Special offers should be provided to Asian ethnic users.
#special offer should be given to users in the North West Rocky Mountain areas and South Florida area.



# 2. Validation of survey findings. 

# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  

# The following variables explain "cost and billing" and "network and service quality":
# Variables totmrc_Mean i.e. "Monthly Recurring Charge is the base cost of the calling plan" represent cost to customer.
# Variable rev_range i.e.	"Range of revenue (charge amount)" represent billing amount.
# Variable ovrrev_mean = DATOVR_MEAN + VCEOVR_MEAN i.e. "Mean overage revenue is the sum of data and voice overage revenues". It is the sum of data and voice overage revenues representing the overage revenue earned from customers.
# Variable totrev i.e. "Total revenue" represent total revenue earned from customers. 

# Variable totmrc_Mean has beta coefficient value of -0.0054371663 meaning a unit increase in this variable causing decrease in churn by 0.0054371663/unit.
# Variable rev_range has beta coefficient value of 0.0020336982 meaning a unit increase in this variable causing increase in churn by 0.0020336982/unit.
# Variable ovrrev_mean has beta coefficient value of 0.0071179059 meaning a unit increase in this variable causing increase in churn by 0.0071179059/unit.
# Variable totrev has beta coefficient value of 0.0002544840 meaning a unit increase in this variable causing increase in churn by 0.0002544840/unit.

#From the above varaibles beta coefficient value, a unit increase in these variables having almost 0% impact. So cost and billing is not very important factor influencing the churn behaviour at Mobicom.


## The following variables explain "network and services quality":

#   Variables         Beta Coefficient

#   mou_Range         0.0003147123
#   change_mou       -0.0006505912
#   drop_blk_Mean     0.0078119383
#   drop_vce_Range    0.0186080514
#   mou_opkv_Range   -0.0010835846
#   iwylis_vce_Mean  -0.0149697172
#   avgqty            0.0011995858
#   avg6mou          -0.0002561289
#   retdays_1         0.6730047077
#   complete_Mean    -0.0017313764

#From the above statistics, data explains the following:

# For variable mou_Range, with unit increase in 'Range of number of minutes of use',there is increase in churn by 0.0003147123 unit.
# For variable change_mou, with unit increae in 'Percentage change in monthly minutes of use vs previous three month average', there is decrease in churn by 0.0006505912 unit.
# For variable drop_blk_Mean, with unit increase in 'Mean number of dropped or blocked calls',there is increase in churn by 0.0078119383 unit.
# For variable drop_vce_Range, with unit increase in 'Range of number of dropped (failed) voice calls',there is increase in churn by 0.0186080514 unit.
# For variable mou_opkv_Range, with unit increae in 'Range of unrounded minutes of use of off-peak voice calls', there is decrease in churn by 0.0010835846 unit.
# For variable iwylis_vce_Mean, with unit increae in 'Mean number of inbound wireless to wireless voice calls', there is decrease in churn by 0.0149697172 unit.
# For variable avgqty, with unit increase in 'Average monthly number of calls over the life of the customer',there is increase in churn by 0.0011995858 unit.
# For variable avg6mou, with unit increae in 'Average monthly minutes of use over the previous six months', there is decrease in churn by 0.0002561289 unit.
# For variable retdays_1, with unit increase in 'Number of days since last retention call',there is increase in churn by 0.6730047077 unit.
# For variable complete_Mean, with unit increae in 'Completed number of calls is equal to the sum of completed data calls and completed voice calls.', there is decrease in churn by 0.0017313764 unit.

#Among the above variables retdays_1 is important factor influencing the churn behaviour as beta cofficient is very high. That is with the increase in number of days since customer makes a retention call, customer's chance of churning is very high.
# These customersshould be provided with special offers and some good bundles.


# b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

# comp_dat_mean - Mean number of completed data calls
# plcd_dat_Mean - Mean number of attempted data calls placed
# opk_dat_Mean - Mean number of off-peak data calls
# blck_dat_Mean - Mean number of blocked (failed) data calls
# datovr_Mean - Mean revenue of data overage
# datovr_Range - Range of revenue of data overage
# drop_dat_Mean - Mean number of dropped (failed) data calls

# The above variables express data usage connectivity.

quantile(tele$plcd_dat_Mean,probs = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.86,0.88,0.90,1))

# The Data Qualtiy Report for all the above variables show that only 10% to 15% customers are actually making data calls or using internet.
# This could be a concern as according to survey report most of the customers who has churned are due to factors like interent or recommendation from friend and family.
# But according to the data quality report,less customers are using internet or data call may be due to the quality of the data.
# So it is good to provide the good quality of data network to the customers and make them to use more data or interent to reduce the churn rate.
# Since less customers are using internet so these variabels are not showing any influence on the churn behaviour at Mobicom.



# 3. Would you recommend rate plan migration as a proactive retention strategy?

# Variable ovrrev_Mean has beta coefficient of 0.0071179059.
# Variable ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue is the sum of data and voice overage revenues charged from customer after sum of both'.
# Since beta coefficient is 0.0071179059, this is not showing any good impact which can influence churn behaviour.
# But for some customers it might be a reason of churn. Also if new customers join then obviously rate plan can be a factor.
# But overall rate plan migration as a proactive retention strategy might not be much helpful at Mobicom.



# 4. What would be your recommendation on how to use this churn model for prioritisation of customers for a proactive retention campaigns in the future?

# Gains Chart
library(gains)
gains(test$churn,predict(mod5,type ="response",newdata = test),groups = 10)

#According to gains chart top 20% of the probabilities contains 29.4% customers who are highly likely to churn.
#Also if we consider top 30% of the probabilities, then  42.1% customers who are highly likely to churn.

# Selecting customers with high churn rate
test$prob <- predict(mod5,type = "response",newdata = test)
quantile(test$prob,prob = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.3040815 and 0.7431688

# Apllying cutoff value to predict customers who can churn
pred_20churn_cust <- predict(mod5,type = "response",newdata = test)
pred_20churn_cust<-ifelse(pred_20churn_cust>=0.3040815,1,0)
table(pred_20churn_cust,test$churn)

Targeted <- test[test$prob>0.3040815 & test$prob<=0.7431688 & test$churn == '1',"Customer_ID"]
Targeted <- as.data.frame(Targeted)
nrow(Targeted)

write.csv(Targeted,"Target_Churn_Customers.csv",row.names = F)

#So by using this model we can predict the list of customers who are high probability to churn and company can take some steps to stop them from churn.


# 5. What would be the target segments for proactive retention campaigns? 
# Falling ARPU forecast is also a concern and therefore, Mobicom would like 
# to save their high revenue customers besides managing churn. Given a budget 
# constraint of a contact list of 20% of the subscriber pool, which subscribers
# should prioritized if "revenue saves" is also a priority besides controlling churn. 
# In other words, controlling churn is the primary objective and revenue saves is the secondary objective.

pred_test <- predict(mod5,type = "response",newdata = test)
test$prob <- predict(mod5,type = "response",newdata = test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred_test1 <- ifelse(pred_test < 0.20,"Low_Score",ifelse(pred_test >0.20 & pred_test < 0.30,"Medium_Score","High_Score"))
table(pred_test1,test$churn)

str(test$totrev)
quantile(test$totrev,prob = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
revenue_levels <- ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev >= 670.660 &  test$totrev < 1034.281,"Medium_Revenue","High_Revenue"))
table(revenue_levels)

table(pred_test1,revenue_levels)

##Thus above table can be used to extract the levels of customers are to be targeted and the Target list can be extracted as follows:

test$prob_levels <- ifelse(pred_test < 0.20,"Low_score",ifelse(pred_test >= 0.20 & pred_test <0.30,"Medium_Score","High_Score"))
test$Revenue_levels <- ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev >= 670.660 &  test$totrev < 1034.281,"Medium_Revenue","High_Revenue"))

Targeted1 <- test[test$prob_levels == "High_Score" & test$Revenue_levels == "High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)
