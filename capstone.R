getwd()
telecom<-read.csv("C:/Data Science With R/Assignments/Graded Assignments/Topic 13 -  Final Case Study Course Wrap up/telecomcomfinal.csv", stringsAsFactors = T)
options(scipen = 999)
library(dplyr)

names(telecom)
str(telecom)
summary(telecom) 



##---------Creating Data Quality Report(dqr)-----------##

#Extracting Variable names
Variables<-names(telecom)
dqr<-as.data.frame(Variables)
rm(Variables)

#Recording Data Type for each Variable
dqr$DataType<-sapply(telecom,class)

#No. of Records for each Variable
dqr$No.ofRecords<-nrow(telecom)

#Counting No. of Unique Values for each variable
for(i in 1:ncol(telecom))
{
  dqr$UniqueRecords[i]<-length(unique(telecom[,i]))
}

#No.of observations available for each variable and its percentage
dqr$DataAvailable<-colSums(!is.na(telecom))
dqr$AvailablePercentage<-round(colMeans(!is.na(telecom)),4)

#Total and Percentage of Missing Values for each Variable
dqr$Missing<-colSums(is.na(telecom))
dqr$MissingPercentage<-round(colMeans(is.na(telecom)),4)

#Minimum, Maximum, Mean, Quantile Values for each Variable
dqr$Maximum<-sapply(telecom, max, na.rm=TRUE)
dqr$Minimum<-sapply(telecom,min, na.rm=TRUE)
dqr$Mean<-sapply(telecom,mean, na.rm = TRUE)

variables<-names(which(sapply(telecom,is.numeric)))
percent<-sapply(variables,function(y){
quantile(x= unlist(telecom[,y]),c(0.05,0.1,0.25,0.5,0.9,0.99),na.rm=TRUE)
}
)
view(percent)
percent<-as.data.frame(t(percent))
percent$variables<-c(variables)
dqr<-merge(dqr,percent, by="variables",all= TRUE)
colnames(dqr)[colnames(dqr) =="5%"]<="FifthPercentile"
colnames(dqr)[colnames(dqr)=="10%"]<-"TenthPercentile"
colnames(dqr)[colnames(dqr)=="25%"]<-"TwentyFifthPercentile"
colnames(dqr)[colnames(dqr)=="50%"]<-"FiftythPercentile"
colnames(dqr)[colnames(dqr)=="75%"]<-"SeventyFifthPercentile"
colnames(dqr)[colnames(dqr)=="90%"]<-"NinetythPercentile"
colnames(dqr)[colnames(dqr)=="99%"]<-"NinetyNinthPercentile"

view(dqr)
str(dqr)

#Exporting Data Quality Report
write.csv(dqr,"Data Quality Report.csv",row.names = T)





#Ommitting variables with more than 25% missing values and creating a new data set
telecom1<-telecom[,colMeans(is.na(telecom))<=0.25]



##**********Data Exploration => Profiling (dat-Continuous Variables , datC-Categorical Variables)**********## 


##-------- Deciling Continuous Variables Basis Target Variabe Churn---------##

names(telecom1)
str(telecom1)

# <1>Variable 'mou_Mean'
summary(telecom1$mou_Mean)
telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-round(dat1$n/dat1$N,2)
dat1$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))


# <2> Variable "totmrc_Mean" 
summary(telecom1$totmrc_Mean)
telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(telecom1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))


# <3> Variable "rev_Range" 
summary(telecom1$rev_Range)
telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))


# <4> Variable "mou_Range" 
summary(telecom1$mou_Range)
telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(telecom1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))                  
                  

# <5> Variable "change_mou" 
summary(telecom1$change_mou)
telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(telecom1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))    


# <6> Variable "drop_blk_Mean" 
telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$LessThan<-unclass(telecom1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6)) 


# <7> Variable "drop_vce_Range" 
summary(telecom1$drop_vce_Range)
telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$LessThan<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7)) 


# <8> Variable "owylis_vce_Range" 
summary(telecom1$owylis_vce_Range)
telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$LessThan<-unclass(telecom1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))


# <9> Variable "mou_opkv_Range" 
summary(telecom1$mou_opkv_Range)
telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$LessThan<-unclass(telecom1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))


# <10> Variable "months" 
summary(telecom1$months)
telecom1%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(telecom1%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(telecom1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))


# <11> Variable "totcalls" 
summary(telecom1$totcalls)
telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$LessThan<-unclass(telecom1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))


# <12> Variable "eqpdays"
summary(telecom1$eqpdays)

#Missing Value Treatment - Since there is just 1 missing observation, will remove the same.
miss<-which(is.na(telecom1$eqpdays))
telecom1<-telecom1[-miss,]

#Deciling basis Variable churn 
telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat12$LessThan<-unclass(telecom1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))


# <13> Variable "custcare_Mean"===>> ***Getting less than 4 deciles. Omit***
summary(telecom1$custcare_Mean)
telecom1%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$varname<-rep("custcare_Mean",nrow(dat13))

plot(telecom1$churn,telecom1$custcare_Mean, col="red")



# <14> Variable "callwait_Mean"
summary(telecom1$callwait_Mean)
telecom1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(telecom1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$LessThan<-unclass(telecom1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))


# <15> Variable "iwylis_vce_Mean"
summary(telecom1$iwylis_vce_Mean)
telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$LessThan<-unclass(telecom1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))


# <16> Variable "callwait_Range"===>> ***Getting less than 4 deciles. Omit***
summary(telecom1$callwait_Range)
telecom1%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$varname<-rep("callwait_Range",nrow(dat16))



# <17> Variable "ccrndmou_Range"===>> ***Getting less than 4 deciles. Omit***
summary(telecom1$ccrndmou_Range)
telecom1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))


# <18> Variable "adjqty"
summary(telecom1$adjqty)
telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$LessThan<-unclass(telecom1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))


# <19> Variable "ovrrev_Mean"
summary(telecom1$ovrrev_Mean)
telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$LessThan<-unclass(telecom1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))


# <20> Variable "rev_Mean"
summary(telecom1$rev_Mean)
telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$LessThan<-unclass(telecom1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))


# <21> Variable "ovrmou_Mean"
summary(telecom1$ovrmou_Mean)
telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$LessThan<-unclass(telecom1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))


# <22> Variable "comp_vce_Mean" ===>> **** Data Transformation then Delete ****
summary(telecom1$comp_vce_Mean)
telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$LessThan<-unclass(telecom1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))


# <23> Variable "plcd_vce_Mean" ===>> **** Data Transformation then Delete ****
summary(telecom1$plcd_vce_Mean)
telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$LessThan<-unclass(telecom1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))


# <24> Variable "avg3mou"
summary(telecom1$avg3mou)
telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))


# <25> Variable "avgmou"
summary(telecom1$avgmou)
telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))


# <26> Variable "avg3qty"
summary(telecom1$avg3qty)
telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))


# <27> Variable "avgqty"
summary(telecom1$avgqty)
telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))


# <28> Variable "avg6mou"
summary(telecom1$avg6mou)
telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))


# <29> Variable "avg6qty"
summary(telecom1$avg6qty)
telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))


# <30> Variable "age1" =====>>****Use As Factor****
summary(telecom1$age1)
telecom1%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(telecom1%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat30$LessThan<-unclass(telecom1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat30$varname<-rep("age1",nrow(dat30))


# <31> Variable "age2"===>> ***Getting less than 4 deciles. Use As Factor***
summary(telecom1$age2)
telecom1%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$varname<-rep("age2",nrow(dat31))


# <32> Variable "models" ===>> ***Getting less than 4 deciles. Factor Variable***
summary(telecom1$models)
telecom1%>%mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$varname<-rep("models",nrow(dat32))


# <33> Variable "hnd_price" =====>> ****** Use as Factor Variable*****
summary(telecom1$hnd_price)
telecom1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$N<-unclass(telecom1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat33$LessThan<-unclass(telecom1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat33$varname<-rep("hnd_price",nrow(dat33))


# <34> Variable "actvsubs" ===>> ***Factor Variable***
summary(telecom1$actvsubs)
telecom1%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$varname<-rep("actvsubs",nrow(dat34))


# <35> Variable "uniqsubs" ===>> ***Factor Variable***
summary(telecom1$uniqsubs)
telecom1%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$varname<-rep("uniqsubs",nrow(dat35))


# <36> Variable "forgntvl" ===>> ***Factor Variable***
summary(telecom1$forgntvl)
telecom1%>%mutate(dec=ntile(forgntvl,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$varname<-rep("forgntvl",nrow(dat36))


# <37> Variable "opk_dat_Mean" ===>> ***Omit***
summary(telecom1$opk_dat_Mean)
telecom1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$varname<-rep("opk_dat_Mean",nrow(dat37))


# <38> Variable "mtrcycle" ===>> ***Factor variable***
summary(telecom1$mtrcycle)
telecom1%>%mutate(dec=ntile(mtrcycle,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$varname<-rep("mtrcycle",nrow(dat38))


# <39> Variable "truck" ===>> ***Factor variable***
summary(telecom1$truck)
telecom1%>%mutate(dec=ntile(truck,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$varname<-rep("truck",nrow(dat39))


# <40> Variable "roam_Mean" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$roam_Mean)
telecom1%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$varname<-rep("roam_Mean",nrow(dat40))


# <41> Variable "recv_sms_Mean" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$recv_sms_Mean)
telecom1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$varname<-rep("recv_sms_Mean",nrow(dat41))


# <42> Variable "mou_pead_Mean" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$mou_pead_Mean)
telecom1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$varname<-rep("mou_pead_Mean",nrow(dat42))


# <43> Variable "da_Mean"
summary(telecom1$da_Mean)
telecom1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(telecom1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat43$LessThan<-unclass(telecom1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat43$varname<-rep("da_Mean",nrow(dat43))


# <44> Variable "da_Range"
summary(telecom1$da_Range)
telecom1%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(telecom1%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat44$LessThan<-unclass(telecom1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat44$varname<-rep("da_Range",nrow(dat44))


# <45> Variable "datovr_Mean" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$datovr_Mean)
telecom1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$varname<-rep("datovr_Mean",nrow(dat45))


# <46> Variable "datovr_Range" ===>> ***Getting less than 4 deciles. So Omit***
summary(telecom1$datovr_Range)
telecom1%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$varname<-rep("datovr_Range",nrow(dat46))


# <47> Variable "drop_dat_Mean" ===>> ***Getting less than 4 deciles. 
        #And almost 95% data has values 0. So Omit***
summary(telecom1$drop_dat_Mean)
telecom1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$varname<-rep("drop_dat_Mean",nrow(dat47))


# <48> Variable "drop_vce_Mean" 
summary(telecom1$drop_vce_Mean)
telecom1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$N<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat48$churn_perc<-dat48$n/dat48$N
dat48$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat48$LessThan<-unclass(telecom1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat48$varname<-rep("drop_vce_Mean",nrow(dat48))


# <49> Variable "adjmou" 
summary(telecom1$adjmou)
telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$N<-unclass(telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat49$churn_perc<-dat49$n/dat49$N
dat49$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat49$LessThan<-unclass(telecom1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat49$varname<-rep("adjmou",nrow(dat49))


# <50> Variable "totrev"
summary(telecom1$totrev)
telecom1%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$N<-unclass(telecom1%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat50$churn_perc<-dat50$n/dat50$N
dat50$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat50$LessThan<-unclass(telecom1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat50$varname<-rep("totrev",nrow(dat50))


# <51> Variable "adjrev" 
summary(telecom1$adjrev)
telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$N<-unclass(telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat51$churn_perc<-dat51$n/dat51$N
dat51$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat51$LessThan<-unclass(telecom1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat51$varname<-rep("adjrev",nrow(dat51))


# <52> Variable "avgrev" 
summary(telecom1$avgrev)
telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat52
dat52$N<-unclass(telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat52$churn_perc<-dat52$n/dat52$N
dat52$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat52$LessThan<-unclass(telecom1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat52$varname<-rep("avgrev",nrow(dat52))


# <53> Variable "comp_dat_Mean" ===>> ***Data Transformation then omit***
summary(telecom1$comp_dat_Mean)
telecom1%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat53
dat53$varname<-rep("comp_dat_Mean",nrow(dat53))


# <54> Variable "plcd_dat_Mean" ====>> ****Data Transformation then omit****
summary(telecom1$plcd_dat_Mean)
telecom1%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat54




## **** Data Transformation. Creating Dummy Variables **** ##

#<55> Create Dummy Variable plcd_Mean and Deciling
telecom1$plcd_Mean<-telecom1$plcd_vce_Mean+telecom1$plcd_dat_Mean

summary(telecom1$plcd_Mean)
telecom1%>%mutate(dec=ntile(plcd_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat55
dat55$N<-unclass(telecom1%>%mutate(dec=ntile(plcd_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat55$churn_perc<-dat55$n/dat55$N
dat55$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(plcd_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_Mean)))[[2]]
dat55$LessThan<-unclass(telecom1%>%mutate(dec=ntile(plcd_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_Mean)))[[2]]
dat55$varname<-rep("plcd_Mean",nrow(dat55))


#<56> Create Dummy Variable complete_Mean and Deciling
telecom1$complete_Mean<-telecom1$comp_vce_Mean+telecom1$comp_dat_Mean

summary(telecom1$complete_Mean)
telecom1%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat56
dat56$N<-unclass(telecom1%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat56$churn_perc<-dat56$n/dat56$N
dat56$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(min(complete_Mean)))[[2]]
dat56$LessThan<-unclass(telecom1%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(max(complete_Mean)))[[2]]
dat56$varname<-rep("complete_Mean",nrow(dat56))

#57 Create Dummy Variable comp_perc and deciling
telecom1$comp_perc<-telecom1$complete_Mean/telecom1$plcd_Mean

summary(telecom1$comp_perc)
telecom1%>%mutate(dec=ntile(comp_perc,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat57
dat57$N<-unclass(telecom1%>%mutate(dec=ntile(comp_perc,n=10))%>%count(dec)%>%unname())[[2]]
dat57$churn_perc<-dat57$n/dat57$N
dat57$GreaterThan<-unclass(telecom1%>%mutate(dec=ntile(comp_perc,n=10))%>%group_by(dec)%>%summarise(min(comp_perc)))[[2]]
dat57$LessThan<-unclass(telecom1%>%mutate(dec=ntile(comp_perc,n=10))%>%group_by(dec)%>%summarise(max(comp_perc)))[[2]]
dat57$varname<-rep("comp_perc",nrow(dat57))



#Adding all appropriate dat1 to dat54 objects to create a dat object
dat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12,dat14,dat15,dat18,dat19,
           dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,dat33,dat43,dat44,
           dat48,dat49,dat50,dat51,dat52,dat55,dat56,dat57)

#Exporting Deciled variables
write.csv(dat,"Deciled Usable Continuous variables.csv",row.names = F)


#Removing Variables that could not be deciled as will come insignificant in the model
#Also omitting transformed Vars "comp_vce_Mean" ,"comp_dat_Mean", "plcd_vce_Mean", plcd_dat_Mean


##-------- Categorical Variables---------##
df<-select_if(telecom1,is.character)
View(df)

##-----Event rate for each level in a categorical variable-----##

# <19> Variable "crclscod" =====>>> **** Some Levels show less than 5% churn rate. So Omit as will come insignificant ****  
summary(telecom1$crclscod)
telecom1%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC19
datC19$N<-unclass(telecom1%>%filter(crclscod%in%datC19$levels)%>%count(crclscod))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("crclscod",nrow(datC19))


# <20> Variable "asl_flag"  
summary(telecom1$asl_flag)
telecom1%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC20
datC20$N<-unclass(telecom1%>%filter(asl_flag%in%datC20$levels)%>%count(asl_flag))[[2]]
datC20$ChurnPerc<-datC20$n/datC20$N
datC20$Var.Name<-rep("asl_flag",nrow(datC20))


# <21> Variable "prizm_social_one"  
summary(telecom1$prizm_social_one)
telecom1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC21
datC21$N<-unclass(telecom1%>%filter(prizm_social_one%in%datC21$levels)%>%count(prizm_social_one))[[2]]
datC21$ChurnPerc<-datC21$n/datC21$N
datC21$Var.Name<-rep("prizm_social_one",nrow(datC21))


# <22> Variable "area"  
summary(telecom1$area)
telecom1%>%count(churn,levels=area)%>%filter(churn==1)->datC22
datC22$N<-unclass(telecom1%>%filter(area%in%datC22$levels)%>%count(area))[[2]]
datC22$ChurnPerc<-datC22$n/datC22$N
datC22$Var.Name<-rep("area",nrow(datC22))


# <23> Variable "refurb_new"  
summary(telecom1$refurb_new)
telecom1%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC23
datC23$N<-unclass(telecom1%>%filter(refurb_new%in%datC23$levels)%>%count(refurb_new))[[2]]
datC23$ChurnPerc<-datC23$n/datC23$N
datC23$Var.Name<-rep("refurb_new",nrow(datC23))


# <24> Variable "hnd_webcap"  
summary(telecom1$hnd_webcap)
telecom1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC24
datC24$N<-unclass(telecom1%>%filter(hnd_webcap%in%datC24$levels)%>%count(hnd_webcap))[[2]]
datC24$ChurnPerc<-datC24$n/datC24$N
datC24$Var.Name<-rep("hnd_webcap",nrow(datC24))


# <25> Variable "marital"  
summary(telecom1$marital)
telecom1%>%count(churn,levels=marital)%>%filter(churn==1)->datC25
datC25$N<-unclass(telecom1%>%filter(marital%in%datC25$levels)%>%count(marital))[[2]]
datC25$ChurnPerc<-datC25$n/datC25$N
datC25$Var.Name<-rep("marital",nrow(datC25))


# <26> Variable "ethnic"  
summary(telecom1$ethnic)
telecom1%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC26
datC26$N<-unclass(telecom1%>%filter(ethnic%in%datC26$levels)%>%count(ethnic))[[2]]
datC26$ChurnPerc<-datC26$n/datC26$N
datC26$Var.Name<-rep("ethnic",nrow(datC26))


# <27> Variable "car_buy"  
summary(telecom1$car_buy)
telecom1%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC27
datC27$N<-unclass(telecom1%>%filter(car_buy%in%datC27$levels)%>%count(car_buy))[[2]]
datC27$ChurnPerc<-datC27$n/datC27$N
datC27$Var.Name<-rep("car_buy",nrow(datC27))


# <28> Variable "csa" ===>>> **** Some Levels show less than 5% churn rate. So Omit as will come insignificant **** 
summary(telecom1$csa)
telecom1%>%count(churn,levels=csa)%>%filter(churn==1)->datC28
datC28$N<-unclass(telecom1%>%filter(csa%in%datC28$levels)%>%count(csa))[[2]]
datC28$ChurnPerc<-datC28$n/datC28$N
datC28$Var.Name<-rep("csa",nrow(datC28))




# Use VArs as Factor => age2, models, actvsubs, uniqsubs, forgntvl, mtrcycle, truck, 

# <31> Variable "age2"  
summary(telecom1$age2)
telecom1%>%count(churn,levels=age2)%>%filter(churn==1)->datC31
datC31$N<-unclass(telecom1%>%filter(age2%in%datC31$levels)%>%count(age2))[[2]]
datC31$ChurnPerc<-datC31$n/datC31$N
datC31$Var.Name<-rep("age2",nrow(datC31))


# <32> Variable "models"  
summary(telecom1$models)
telecom1%>%count(churn,levels=models)%>%filter(churn==1)->datC32
datC32$N<-unclass(telecom1%>%filter(models%in%datC32$levels)%>%count(models))[[2]]
datC32$ChurnPerc<-datC32$n/datC32$N
datC32$Var.Name<-rep("models",nrow(datC32))


# <34> Variable "actvsubs"  
summary(telecom1$actvsubs)
telecom1%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC34
datC34$N<-unclass(telecom1%>%filter(actvsubs%in%datC34$levels)%>%count(actvsubs))[[2]]
datC34$ChurnPerc<-datC34$n/datC34$N
datC34$Var.Name<-rep("actvsubs",nrow(datC34))


# <35> Variable "uniqsubs"  
summary(telecom1$uniqsubs)
telecom1%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC35
datC35$N<-unclass(telecom1%>%filter(uniqsubs%in%datC35$levels)%>%count(uniqsubs))[[2]]
datC35$ChurnPerc<-datC35$n/datC35$N
datC35$Var.Name<-rep("uniqsubs",nrow(datC35))


# <36> Variable "forgntvl"  
summary(telecom1$forgntvl)
telecom1%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC36
datC36$N<-unclass(telecom1%>%filter(forgntvl%in%datC36$levels)%>%count(forgntvl))[[2]]
datC36$ChurnPerc<-datC36$n/datC36$N
datC36$Var.Name<-rep("forgntvl",nrow(datC36))


# <37> Variable "mtrcycle"  
summary(telecom1$mtrcycle)
telecom1%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC37
datC37$N<-unclass(telecom1%>%filter(mtrcycle%in%datC37$levels)%>%count(mtrcycle))[[2]]
datC37$ChurnPerc<-datC37$n/datC37$N
datC37$Var.Name<-rep("mtrcycle",nrow(datC37))


# <38> Variable "Truck"  
summary(telecom1$truck)
telecom1%>%count(churn,levels=truck)%>%filter(churn==1)->datC38
datC38$N<-unclass(telecom1%>%filter(truck%in%datC38$levels)%>%count(truck))[[2]]
datC38$ChurnPerc<-datC38$n/datC38$N
datC38$Var.Name<-rep("Truck",nrow(datC38))


# <39> Variable "hnd_price"  
summary(telecom1$hnd_price)
telecom1%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC39
datC39$N<-unclass(telecom1%>%filter(hnd_price%in%datC39$levels)%>%count(hnd_price))[[2]]
datC39$ChurnPerc<-datC39$n/datC39$N
datC39$Var.Name<-rep("hnd_price",nrow(datC39))


#<40> Variable "income
summary(telecom1$income)
telecom1%>%count(churn,levels=income)%>%filter(churn==1)->datC40
datC40$N<-unclass(telecom1%>%filter(income%in%datC39$levels)%>%count(hnd_price))[[2]]
datC40$ChurnPerc<-datC40$n/datC40$N
datC40$Var.Name<-rep("income",nrow(datC40))

#Adding datC19 to datC38 objects to create a datC object
datC_1<-rbind(datC19,datC20,datC21,datC22,datC23,datC24,datC25,datC26,datC27,
            datC28)

datC_2<-rbind(datC31,datC32,datC34,datC35,datC36,datC37,datC38)


#Exporting Deciled variables
write.csv(datC_1,"Event Rate - Categorical variables1.csv",row.names = F)
write.csv(datC_2,"Event Rate - Categorical variables2.csv",row.names = F)



#*********Data Preparation*********#

#-----Outlier Treatment----#

#-----Continuous Variables------#
#Method <II> ----- Box Plot Method==>> Used
names(telecom1)
summary(telecom1)
str(telecom1)

#Factor Variables=> asl_flag, prizm_social_one, area, refurb_new, hnd_webcap, 
#marital, ethnic, age1, age2, models, hnd_price, actvsubs, "uniqsubs", 
#"forgntvl", "mtrcycle", "truck", churn, car_buy, Customer_ID

str(telecom1)
list<-names(telecom1)
list
# Removing Categorical Variables
list<-list[-c(25:42,50,51)]
list

# Outlier Plots
par(mfrow=c(3,11))
for(i in 1:length(list))
{
  boxplot(telecom1[,list[i]],main=list[i])
}

for(i in 1:length(list))
{
  plot(telecom1[,list[i]],main=list[i])
}


# Outlier Treatment
for(i in 1:length(list))
{
  x<-boxplot(telecom1[,list[i]],main=list[i])
  out<-x$out
  index<-which(telecom1[,list[i]]%in% x$out)
  telecom1[miss,list[i]]<-mean(telecom1[,list[i]],na.rm = T)
  rm(x)
  rm(out)
}


# Checking After Treatment

for(i in 1:length(list))
{
  boxplot(telecom1[,list[i]],main=list[i])
}

for(i in 1:length(list))
{
  plot(telecom1[,list[i]],main=list[i])
}

dev.off()



#-----Missing Value Treatment -------# 

summary(telecom1)
names(telecom1)

# Factor Variables=> crclscod, asl_flag, prizm_social_one, area, refurb_new, hnd_webcap, marital, ethnic, "age1", 
#    "age2", "models", "hnd_price","actvsubs", "uniqsubs", "forgntvl", "mtrcycle", "truck", car_buy, csa


# Deleting Missing Values
miss1<-which(is.na(telecom1[,c(1:5)]))
telecom1<-telecom1[-miss1,]
summary(telecom1)

#Missing value Imputation
miss2<-which(is.na(telecom1$avg6mou))
telecom1$avg6mou[miss2]<-1344

miss3<-which(is.na(telecom1$avg6qty))
telecom1$qvg6qty[miss3]<-1347

miss4<-which(is.na(telecom1$marital))
telecom1$marital[miss4]<-"S"

miss5<-which(is.na(telecom1$age1))
telecom1$age1[miss5]<-36

miss6<-which(is.na(telecom1$age2))
telecom1$age2[miss6]<-48

miss7<-which(is.na(telecom1$hnd_price))
telecom1$hnd_price[miss7]<-299.9899902

miss8<-which(is.na(telecom1$hnd_webcap))
telecom1$hnd_webcap[miss8]<-"WC"

miss9<-which(is.na(telecom1$area))
telecom1$area[miss9]<-"OHIO AREA"

miss10<-which(is.na(telecom1$prizm_social_one))
telecom1$prizm_social_one[miss10]<-"T"

miss11<-which(is.na(telecom1$ethnic))
telecom1$ethnic[miss11]<-"M"

miss12<-which(is.na(telecom1$forgntvl))
telecom1$forgntvl[miss12]<-1

miss13<-which(is.na(telecom1$income))
telecom1$income[miss13]<-7



#Checking Churn Rate in the data after Imputations
table(telecom$churn)/nrow(telecom)
table(telecom1$churn)/nrow(telecom1)



# Convert to Factor and Create Dummy Variables => 
      #age1, age2, income, models, hnd_price, actvsubs,uniqsubs, forgntvl, mtrcycle, truck, Customer ID, Churn


str(telecom1$age1)
telecom1$age1_1<-ifelse(telecom1$age1==0,"Default",ifelse(telecom1$age1<=30,"Young",
                                                    ifelse(telecom1$age1>30 & telecom1$age1<=55,"Middle Age","Old")))
str(telecom1$age1_1)
telecom1$age1_1<-as.factor(telecom1$age1_1)
summary(telecom1$age1_1)



str(telecom1$age2)
telecom1$age2_1<-ifelse(telecom1$age2==0,"Default",ifelse(telecom1$age2<=30,"Young",
                                                    ifelse(telecom1$age2>30 & telecom1$age2<=55,"Middle Age","Old")))
str(telecom1$age2_1)
telecom1$age2_1<-as.factor(telecom1$age2_1)
summary(telecom1$age2_1)

str(telecom1$income)
telecom1$income_1<-ifelse(telecom1$income==0,"default",ifelse(telecom1$income<4,"poor",ifelse(telecom1$income>=4 & telecom1$income<=7,"middle class","rich")))
telecom1$income_1<-as.factor(telecom1$income_1)
summary(telecom1$income_1)



str(telecom1$models)
summary(telecom1$models)
telecom1$models<-as.factor(telecom1$models)
summary(telecom1$models)


str(telecom1$hnd_price)
summary(telecom1$hnd_price)
telecom1$hnd_price<-as.factor(telecom1$hnd_price)
summary(telecom1$hnd_price)


str(telecom1$actvsubs)
summary(telecom1$actvsubs)
telecom1$actvsubs<-as.factor(telecom1$actvsubs)
summary(telecom1$actvsubs)


str(telecom1$uniqsubs)
summary(telecom1$uniqsubs)
telecom1$uniqsubs<-as.factor(telecom1$uniqsubs)
summary(telecom1$uniqsubs)


str(telecom1$forgntvl)
summary(telecom1$forgntvl)
telecom1$forgntvl<-as.factor(telecom1$forgntvl)
summary(telecom1$forgntvl)


str(telecom1$mtrcycle)
summary(telecom1$mtrcycle)
telecom1$mtrcycle<-as.factor(telecom1$mtrcycle)
summary(telecom1$mtrcycle)


str(telecom1$truck)
summary(telecom1$truck)
telecom1$truck<-as.factor(telecom1$truck)
summary(telecom1$truck)


str(telecom1$hnd_webcap)
summary(telecom1$hnd_webcap)
telecom1$hnd_webcap<-as.factor(telecom1$hnd_webcap)


names(telecom1)
df1<-names(telecom1)
df1<-as.data.frame(df1)
View(df1)
telecom1ctelecom1[,-c(12,14,17,18,22,23,31,39,40,67,55,66)]
names(telecom1)


### ********** Logistic Regression Model Building ********** ###

# Splitting into Test and Training Samples
set.seed(100)
index<-sample(nrow(telecom1),nrow(telecom1)*0.70,replace=F)
train<-telecom1[index,]
test<-telecom1[-index,]


#Checking Churn Rate 
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)



# Building Logistic Regression Model 
mod<-glm(churn~.,data=train,family=binomial(link = "logit"))
summary(mod)




## ***** Creating Dummy Vars for Factor Vars with significant levels ***** ##

summary(telecom1$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag == "Y", 1, 0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y", 1, 0)


summary(train$area)
train$Cal_Nrth<-ifelse(train$area == "CALIFORNIA NORTH AREA", 1, 0)
test$Cal_Nrth<-ifelse(test$area == "CALIFORNIA NORTH AREA", 1, 0)

train$ohio<-ifelse(train$area=="OHIO AREA",1,0)
test$ohio<-ifelse(test$area=="OHIO AREA",1,0)

summary(train$refurb_new)
train$refurb_R<-ifelse(train$refurb_new == "R", 1, 0)
test$refurb_R<-ifelse(test$refurb_new == "R", 1, 0)

train$refurb_N<-ifelse(train$refurb_new == "N", 1, 0)
test$refurb_N<-ifelse(test$refurb_new == "N", 1, 0)


summary(train$ethnic)
train$ethnic_C<-ifelse(train$ethnic == "C", 1, 0)
test$ethnic_C<-ifelse(test$ethnic == "C", 1, 0)

train$ethnic_N<-ifelse(train$ethnic == "N", 1, 0)
test$ethnic_N<-ifelse(test$ethnic == "N", 1, 0)


train$ethnic_S<-ifelse(train$ethnic == "S", 1, 0)
test$ethnic_S<-ifelse(test$ethnic == "S", 1, 0)


summary(train$hnd_price)
train$hnd_price_79.98<-ifelse(train$hnd_price == "79.98999023", 1, 0)
test$hnd_price_79.98<-ifelse(test$hnd_price == "79.98999023", 1, 0)

train$hnd_price_129.98<-ifelse(train$hnd_price == "129.9899902", 1, 0)
test$hnd_price_129.98<-ifelse(test$hnd_price == "129.9899902", 1, 0)


summary(train$uniqsubs)
train$unq_5<-ifelse(train$uniqsubs == "5", 1, 0) 
test$unq_5<-ifelse(test$uniqsubs == "5", 1, 0)

train$unq_7<-ifelse(train$uniqsubs == "7", 1, 0)
test$unq_7<-ifelse(test$uniqsubs == "7", 1, 0)


summary(train$prizm_social_one)
train$przm_social_R<-ifelse(train$prizm_social_one == "R", 1, 0)
test$przm_social_R<-ifelse(test$prizm_social_one == "R", 1, 0)

train$przm_social_T<-ifelse(train$prizm_social_one == "T", 1, 0)
test$przm_social_T<-ifelse(test$prizm_social_one == "T", 1, 0)


summary(train$age1_1)
train$Young1<-ifelse(train$age1_1 == "Young", 1, 0)
test$Young1<-ifelse(test$age1_1 == "Young", 1, 0)

train$Middle1<-ifelse(train$age1_1 == "Middle", 1, 0)
test$Middle1<-ifelse(test$age1_1 == "Middle", 1, 0)

train$Old1<-ifelse(train$age1_1 == "Old", 1, 0)
test$Old1<-ifelse(test$age1_1 == "Old", 1, 0)


summary(train$age2_1)
train$Young2<-ifelse(train$age2_1 == "Young", 1, 0)
test$Young2<-ifelse(test$age2_1 == "Young", 1, 0)

train$Middle2<-ifelse(train$age1_1 == "Middle", 1, 0)
test$Middle2<-ifelse(test$age1_1 == "Middle", 1, 0)

train$Old2<-ifelse(train$age2_1 == "Old", 1, 0)
test$Old2<-ifelse(test$age2_1 == "Old", 1, 0)


summary(train$income_1)
train$poor<-ifelse(train$income_1 == "poor",1,0)
test$poor<-ifelse(train$income_1 == "poor",1,0)

train$rich<-ifelse(train$income_1 == "rich",1,0)
test$rich<-ifelse(test$income_1 == "rich",1,0)

summary(train$actvsubs)
train$actvsubs_4<-ifelse(train$actvsubs == "4",1,0)
test$actvsubs_4<-ifelse(test$actvsubs == "4",1,0)

train$actvsubs_5<-ifelse(train$actvsubs == "5",1,0)
test$actvsubs_5<-ifelse(test$actvsubs == "5",1,0)



## **** Rerunning Model with Significant Variables ***** ##
mod2<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range + rev_Mean +
            mou_opkv_Range + months + totcalls + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgmou + avg3qty + avgqty + avg3mou +
            avg6mou + avgrev + hnd_webcap + models + custcare_Mean + callwait_Mean +plcd_Mean + comp_perc + avg6qty +
            + asl_flag_Y + Cal_Nrth + ohio + refurb_R + ethnic_C + ethnic_N + ethnic_S + hnd_price_79.98  + hnd_price_129.98 + 
            hnd_price_129.98 + unq_5 + unq_7 + actvsubs_4+ actvsubs_5+ poor+ rich + adjmou + totrev + da_Mean+ datovr_Mean + complete_Mean + 
            przm_social_R + przm_social_T + Middle1 + Old1+ Young1+Young2 + old2 + Middle2,data =train, family = binomial(link = "logit"))

summary(mod2)

## **** Further Rerunning Model with Significant Variables ***** ##


## ***** Model Diagnostics ***** ##

# Checking For Multicollinearity
library(car)
vif(mod2)



# Re-running Model with above vars omited to remove problem of Multicollinearity.

mod3<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range + 
            mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + avgqty + asl_flag_Y + models + comp_perc +
            Cal_Nrth +ohio + refurb_new_R + ethnic_C + ethnic_N + ethnic_S + hnd_price_79.98 + hnd_price_129.98 +
            poor + rich + actvsubs_4 +actvsubs_5 + unq_5 + unq_7 + adjmou + adjrev + da_Mean+ datovr_Mean + 
            przm_social_R + przm_social_T + Middle1 + Young1+Old1+Middle2+Young2+Old2, data=train, family=binomial(link = "logit"))
summary(mod3)



## ***** Model Testing ***** ##

#Predicted Values ==> Predicting the probability of a customer churning.
pred<-predict(mod3, type="response", newdata=test)
head(pred)


#Assuming cut-off probablity as per the churn rate in data set
table(telecom1$churn)/nrow(telecom1)

#choosing cutoff value according to kappa value
s<-seq(0.2,0.5,0.01)
n<-1 
a<-as.vector(length(s))
for (i in s ) {
  print(i)
  test$result<-ifelse(test$pred>i,1,0)
  a[n]<-confusionMatrix(as.factor(test$result),as.factor(test$churn),positive = "1")$overall[2]
  
  print(n)
  n=n+1
}
max(a)
#As maximum kappa is related to cutoff 0.23 we would go with this cutoff value


pred1<-ifelse(pred>=0.237,1,0)
table(pred1)

# After several itteration in cut-off values, the model is predicting the best at the above Cut-off level.


## **** Checking Prediction Quality **** ##

#Kappa Matrix
library(irr)
kappa2(data.frame(test$churn,pred1))

#Confusion Matrix
library(caret)
confusionMatrix(pred1,test$churn,positive = "1")
table(test$churn)


#ROCR Curve
library(ROCR)
pred2<-prediction(pred1,test$churn)
pref<-performance(pred2,"tpr","fpr")
plot(pref,col="green")
abline(0,1,lty=8,col="blue")
auc<-performance(pred2,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

# The auc is 0.5886 
# The curve seems to be well above the blue line.


#Gains Chart
library(gains)
gains(test$churn,predict(mod3,type="response",newdata=test),groups = 10)

#the Gains Chart shows that the top 30% of the probabilities contain 53% customers that are likely to churn.


test$prob<-predict(mod3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 40% of the probability scores lie between 0.2477838 and 0.2298172
#We can use this probablity to extract the data of customers who are highly likely to churn.





### ********** Answering Business Questions ********** ###
### Top Line Questions of Interest to Senior Management: 

#  1.  What are the top five factors driving likelihood of churn at Mobicom?


head(sort(abs(mod3$coefficients),decreasing = T),10)
summary(mod3)

# from the summary of my final model, "mod3".

## The model results show that the top 5 factors affecting churn are:
### a. model           
### b. comp_perc       
### c. unqisubs         
### d. asl_flag     
### e. refurb_new  


#   2.  Validation of survey findings. 
# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  

# The following variables explain "cost and billing" and "network and service quality"

# Variables totmrc_Mean i.e. 'base plan charge' representing cost to customer, 
# var rev_Range i.e. 'Range of Revenue(charge amount)' representing billing amount,
# var ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue' (It is the sum of data and voice 
# overage revenues) representing the overage revenue earned from customers after billing the same to them.   
# and var  totrev i.e. 'Total revenue' representing total revenue earned from customers.

# the beta values have a unit increase in them is having almost 0% impact 
# on churn. SO it seems cost and billing is not very important factors here influencing churn behaviour at Mobicom.



# The following variables explain "network and service quality" 

# VARIABLE         

# mou_Range         
# change_mou       
# drop_blk_Mean     
# drop_vce_Range     
# mou_opkv_Range   
# iwylis_vce_Mean  
# avgqty             
# avg6mou          
# adjmou            
# complete_Mean    


# Network issues increases chances of churn rate.So, more concentratuon should be given on network and service quality improvement in oreder to reduce churn.


 

#  2b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

#   comp_dat_Mean - Mean no. of completed data calls. 
#   plcd_dat_Mean - Mean number of attempted data calls placed
#   opk_dat_Mean - Mean number of off-peak data calls
#   blck_dat_Mean - Mean no. of blocked / failed data calls
#   datovr_Mean - Mean revenue of data overage. 
#   datovr_Range - Range of revenue of data overage
#   drop_dat_Mean - Mean no. of dropped / failed data calls

#   The above variables express data usage connectivity. 
quantile(telecom1$plcd_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(telecom1$comp_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(telecom1$opk_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(telecom1$blck_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(telecom1$datovr_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(telecom1$datovr_Range,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(telecom1$drop_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#   The Data Quality Report for all the above variables show that only 10% to 15% customers are actualy 
#   making data calls or using the internet. 
 
#   In this case it seems customers are not really using the internet. So it would be good to work 
#   towards attaining more customers to use data and also towards proving quality network connectivity
#   and service to provide maximum customer satisfaction and reduce Churn.
#   Since there is not enough usable data for the above variables they are not showing any influence 
#   on the Churn Behaviour at Mobicom.


#   3. Would you recommend rate plan migration as a proactive retention strategy?

#   var ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue' 
#   It is the sum of data and voice overage revenues representing the overage revenue earned 
#   from customers after billing the same to them. 
#   Yes, rate plan migration is recommended as a proactive retention plan.

#   4. What would be your recommendation on how to use this churn model for prioritisation
#   of customers for a proactive retention campaigns in the future?

# Solution:
#Gains Chart
library(gains)
gains(test$churn,predict(mod3,type="response",newdata=test),groups = 10)
#the Gains Chart shows that the top 20% of the probabilities contain 29.5% customers that are highly likely to churn.


# Selecting Customers with high churn rate
test$prob<-predict(mod3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.2935543 and 0.8342993

# Applying cutoff value to predict customers who Will Churn
pred4<-predict(mod3, type="response", newdata=test)
pred4<-ifelse(pred4>=0.2935543 , 1, 0)
table(pred4,test$churn)

Targeted<-test[test$prob>0.2935543 & test$prob<=0.8342993 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)

write.csv(Targeted,"Target_Customers.csv",row.names = F)

#   Thus Using the model can be used to predict customers with high probability of Churn and extract the 
#   target list using their "Customer ID". 



# 5. What would be the target segments for proactive retention campaigns? 
# Falling ARPU forecast is also a concern and therefore, Mobicom would like to save their high revenue 
# customers besides managing churn. Given a budget constraint of a contact list of 20% of the subscriber pool, 
# which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
# In other words, controlling churn is the primary objective and revenue saves is the secondary objective.

# Solution:
pred5<-predict(mod3, type="response", newdata=test)
test$prob<-predict(mod3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
table(pred6,test$churn)

str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<665.672,"Low_Revenue",ifelse(test$totrev>=665.672 & 
                                                                  test$totrev<1132.350,"Medium_Revenue","High_Revenue"))

table(Revenue_Levels)

table(pred6,Revenue_Levels)

##  Thus this table can be used to select the levels of customers are to be targeted
##  and the Target list can be extracted as follows:

test$prob_levels<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<665.672,"Low_Revenue",ifelse(test$totrev>=665.672 & 
                                                                  test$totrev<1132.350,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)
