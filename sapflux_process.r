##########################################################
########Decagon logger data organization       ###########
########Heather Kropp started June 2018        ###########
##########################################################
##########################################################
##########################################################
### This script calculates sapflow from dynagauge      ###
### sensors installed at the bewkdes field site        ###
##########################################################
##########################################################
##########################################################
##########################################################
### Input files from sensor output from campbell_sf.r  ###
### A_mv.csv,B_mv.csv,C_mV.csv, DG_dT.csv,H_V.csv      ###
##########################################################
### Output files: sapflow in g/s: sapflowF             ###
###              sensor data:   datS                   ###
##########################################################
##########################################################
library(plyr)

########################################
### set plotting directory           ###
########################################
plotDI <- "z:\\data_repo\\field_data\\bewkes\\sapflow\\diag_plot"

########################################
### set flag to use ksh default      ###
### or kshapp                        ###
### 1= use kshapp 0=default          ###
########################################
#flag indicates whether a default value 
#should be used or if accounting for 
#daily drift in Ksh apparent should be used
flag.kshapp <- 1


########################################
### set flag to indicate whether     ###
### should be made. 1=make plot      ###
### 0= no plots                      ###
########################################
flag.plot <- 0

########################################
### read in data                     ###
########################################
if(os.flag==1){
datA <- read.csv("z:\\data_repo\\field_data\\bewkes\\sensor\\campbell\\A_mV.csv")
datB <- read.csv("z:\\data_repo\\field_data\\bewkes\\sensor\\campbell\\B_mV.csv")
datC <- read.csv("z:\\data_repo\\field_data\\bewkes\\sensor\\campbell\\C_mV.csv")
datH <- read.csv("z:\\data_repo\\field_data\\bewkes\\sensor\\campbell\\H_V.csv")
datDT <- read.csv("z:\\data_repo\\field_data\\bewkes\\sensor\\campbell\\DG_dT.csv")
datS <- read.csv("z:\\data_repo\\field_data\\bewkes\\sensor\\campbell\\sensorInfo.csv")
}else{
datA <- read.csv("smb://geography~storage01.colgate.edu/data/data_repo/field_data/bewkes/sensor/campbell/A_mV.csv")
datB <- read.csv("smb://geography~storage01.colgate.edu/data//data_repo/field_data/bewkes/sensor/campbell/B_mV.csv")
datC <- read.csv("smb://geography~storage01.colgate.edu/data//data_repo/field_data/bewkes/sensor/campbell/C_mV.csv")
datH <- read.csv("smb://geography~storage01.colgate.edu/data//data_repo/field_data/bewkes/sensor/campbell/H_V.csv")
datDT <- read.csv("smb://geography~storage01.colgate.edu/data//data_repo/field_data/bewkes/sensor/campbell/DG_dT.csv")
datS <- read.csv("smb://geography~storage01.colgate.edu/data//data_repo/field_data/bewkes/sensor/campbell/sensorInfo.csv")

}
########################################
### Energy balance equations         ###
########################################

#calculate Pin
#V^2 / heater resistance
Pin <- data.frame(datH[,1:3])
for(i in 1:dim(datS)[1]){
	Pin[,i+3] <- (datH[,i+3]*datH[,i+3])/datS$sensor.resist[i]
	colnames(Pin)[i+3] <- paste0("Pin",i)
}

#calculate stem area in m2
SA <- pi*(((datS$diameter.mm/1000)/2)^2)

#thermocouple distance is 4mm for all of our sensors
#units need to be m
dX <- 4/1000

#calculate Qv
#thermal conductivity of woody stems
#0.42 W m-1 k-1
Kst <- 0.42

#calculate  vertical Q
#Kst(i) * SA(i) * (B_mv(i) - A_mv(i)) / (dx(i)* 10 * 0.04)
#0.04 mV C-1 is a conversion
Qv <- data.frame(datH[,1:3])
for(i in 1:dim(datS)[1]){
	Qv[,i+3] <- (Kst * SA[i] * (datB[,i+3]-datA[,i+3]))/(dX*0.04)
	colnames(Qv)[i+3] <- paste0("Qv",i)
}


#default ksh is assumed to be 0.8
Ksh <- 0.8
#calculate ksh apparent
#(DG_Pin(i) - DG_Qv(i)) / C_mv(i)

KshappA <-  data.frame(datH[,1:3])
for(i in 1:dim(datS)[1]){
	KshappA[,i+3] <- (Pin[,i+3]-Qv[,i+3])/datC[,i+3]
	colnames(KshappA)[i+3] <- paste0("KshappA",i)
}

#get predawn average ksh
KshappPD <- KshappA[KshappA$hour>=3&KshappA$hour<=4.5,]
#get average kshapp value during this time

KshappL <-  list()
for(i in 1:dim(datS)[1]){
	KshappL[[i]] <- aggregate(KshappPD[,i+3],by=list(KshappPD[,1]), FUN="mean")
	colnames(KshappL[[i]]) <- c("doy",paste0("Kshapp",i))
}
Kshapp <- join_all(KshappL, by="doy", type="full")

#calculate Qr
#C_mv(i) * DG_Ksh(i)
#match daily Kshapp to dimensions
CAll <- join(datC,Kshapp, by="doy",type="left")
#use default value if missing
for(i in 1:dim(datS)[1]){
	CAll[,i+3+dim(datS)[1]] <- ifelse(is.na(CAll[,i+3+dim(datS)[1]]),Ksh,CAll[,i+3+dim(datS)[1]])
}

Qr<-  data.frame(datH[,1:3])
if(flag.kshapp==1){
	for(i in 1:dim(datS)[1]){
		Qr[,i+3] <- CAll[,i+3]*CAll[,i+3+dim(datS)[1]]
		colnames(Qr)[i+3] <- paste0("Qr",i)
	}
}else{
	for(i in 1:dim(datS)[1]){
		Qr[,i+3] <- datC[,i+3]*Ksh
		colnames(Qr)[i+3] <- paste0("Qr",i)
	}	
}

#calculate Qf
#Pin - Qv - Qr
Qf<-  data.frame(datH[,1:3])
for(i in 1:dim(datS)[1]){
	Qf[,i+3] <- Pin[,i+3]-Qv[,i+3]-Qr[,i+3]
	colnames(Qf)[i+3] <- paste0("Qf",i)
	}

#calculate sapflow
#in g/s
#Qf(i)* 3600/(dT * 4.186)

sapflowUF <-  data.frame(datH[,1:3])
for(i in 1:dim(datS)[1]){
	sapflowUF[,i+3] <- (Qf[,i+3]*3600)/(datDT[,i+3]*4.186)
	colnames(sapflowUF)[i+3] <- paste0("sapflowUF",i)
}

#sapflow with low flow filter
#filter if Qf less than 20% of Pin and below minimum dT threshold
#or if Qf less than 0

sapflowF <-  data.frame(datH[,1:3])
for(i in 1:dim(datS)[1]){
	sapflowF[,i+3] <- ifelse(Qf[,i+3]<(0.2*Pin[,i+3])&datDT[,i+3]<0.75,0, 
					ifelse(Qf[,i+3]<0,0,sapflowUF[,i+3]))				
	colnames(sapflowF)[i+3] <- paste0("sapflowF",i)
}


########################################
### diagnostic plots                 ###
########################################
if(flag.plot==1){
	### make a plot of Qf with Pin for comp ###
	for(i in 1:dim(datS)[1]){
	jpeg(paste0(plotDI,"\\Qf\\Qf_sensor",i,".jpg"), width=750,height=500,quality=100)
		plot(Qf$doy+(Qf$hour/24),Qf[,i+3], type="l", xlab="doy",ylab="Qf", main=paste("sensor",i), ylim=c(-.1,.1),
				lwd=3)
		points(Pin$doy+(Pin$hour/24),Pin[,i+3]*.2,type="l",col="tomato", lwd=2, lty=3)
		points(Pin$doy+(Pin$hour/24),Pin[,i+3]*.8,type="l",col="royalblue", lwd=2, lty=3)
		legend(Qf$doy[1]+(Qf$hour[1]/24),-0.02,c("Qf","20% Pin","80%Pin"), lwd=c(3,2,2),lty=c(1,3,3),
				col=c("black","tomato","royalblue"), bty="n")
	dev.off()
	}
	### make a plot of sapflowF for comp ###
	for(i in 1:dim(datS)[1]){
	jpeg(paste0(plotDI,"\\sapflow\\sapflow_sensor",i,".jpg"), width=750,height=500,quality=100)	
		plot(sapflowF$doy+(sapflowF$hour/24),sapflowF[,i+3], type="l", xlab="doy",ylab="sapflow (g/s)", main=paste("sensor",i), ylim=c(-.1,30),
				lwd=3)
	dev.off()
	}
	### make a plot of dT for comp ###
	for(i in 1:dim(datS)[1]){
	jpeg(paste0(plotDI,"\\dT\\dT_sensor",i,".jpg"), width=750,height=500,quality=100)	
		plot(datDT$doy+(datDT$hour/24),datDT[,i+3], type="l", xlab="doy",ylab="dT (C)", main=paste("sensor",i), ylim=c(-.2,2),
				lwd=3)
		abline(h=0.75, col="tomato", lwd=3,lty=3)
	dev.off()
	}
	
}


########################################
### prepare output                   ###
########################################
rm(list=setdiff(ls(), c("sapflowF","datS")))

