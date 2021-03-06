##########################################################
########Campbell logger data organization      ###########
########Heather Kropp started June 2018        ###########
##########################################################
##########################################################
##########################################################
### This script organizes sapflow data output by       ###
### the campbell logger  at the Bewkes field site      ###
##########################################################
##########################################################
##########################################################
##########################################################
library(lubridate)
library(plyr)


#data directory
datDI <- "c:\\Users\\hkropp\\Google Drive\\bewkes_logger\\data\\campbell\\raw\\DGL"
#sensor infor
sensI <- read.csv("c:\\Users\\hkropp\\Google Drive\\bewkes_logger\\data\\campbell\\sensor\\sensorInfo.csv")
#output dir
outDI <-  c("c:\\Users\\hkropp\\Google Drive\\bewkes_logger\\sensor\\campbell",
			"z:\\data_repo\\field_data\\bewkes\\sensor\\campbell")
#list the files in the datalogger
sF <-  list.files(paste0(datDI), ".dat")

########################################
### extract date from data file name ###
########################################

#get date of download
cDF <- gsub(".dat","",gsub("bewkes_TableDGL","",sF))

#now pull out dates
monthF <- numeric()
dayF <- numeric()
yearF <- numeric()
for(i in 1:length(sF)){
	monthF[i] <- as.numeric(strsplit(cDF[i],"_")[[1]][2])
	dayF[i] <- as.numeric(strsplit(cDF[i],"_")[[1]][3])
	yearF[i] <- as.numeric(strsplit(cDF[i],"_")[[1]][4])
}

dateF <- as.Date(paste0(monthF,"/",dayF,"/",yearF), "%m/%d/%y")
#calculate decimal date
ddF <- year(dateF)+(yday(dateF)/365)

#df 
dfFiles <- data.frame(filename=sF,date=ddF)
#order files by date
dfFiles <- dfFiles[order(dfFiles$date),]



########################################
### read in data files               ###
########################################
#read in header data
headerF <- read.csv(paste0(datDI,"\\",dfFiles$filename[1]), skip=1,nrows=3, na.strings="NAN",header=FALSE,stringsAsFactors=FALSE)
#get header info
headL <- gsub("\\(*\\d*\\)","",as.character(headerF[1,]))
sensL <- as.numeric(gsub("\\D","",as.character(headerF[1,])))
#read in sapflow data files
sapflowL <- list()
endL <- numeric(0)
for(i in 1:dim(dfFiles)[1]){

	sapflowL[[i]] <- read.csv(paste0(datDI,"\\",dfFiles$filename[i]), skip=4, na.strings="NAN",header=FALSE)

	#pull out last record number at end of the file
	endL[i] <- sapflowL[[i]][dim(sapflowL[[i]])[1],2]
	
}

#generate starting values
startL <- c(0)
for(i in 1:dim(dfFiles)[1]-1){
	startL <-append(startL, endL[i]+1) 
}

#subset sapflow to only be in the time period of data collection

subE <- numeric(0)
subS <- numeric(0)
sapflowS <- list()
for(i in 1:dim(dfFiles)[1]){
	subS[i] <- which(sapflowL[[i]][,2]==startL[i])
	subE[i] <- which(sapflowL[[i]][,2]==endL[i])
	sapflowS[[i]] <- sapflowL[[i]][subS[i]:subE[i],]
}

sapflow <- ldply(sapflowS,data.frame)

colnames(sapflow) <- headL
#convert time
dateSF <- as.Date(sapflow$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
hoursD <- format(strptime(sapflow$TIMESTAMP, "%Y-%m-%d %H:%M:%S"),"%H")
minsD <- format(strptime(sapflow$TIMESTAMP, "%Y-%m-%d %H:%M:%S"),"%M")
hourD <- as.numeric(hoursD)+(as.numeric(minsD)/60)
#time df

timeDF <- data.frame(doy=yday(dateSF),year=year(dateSF),hour=hourD)

#variables of interest
varI <- c("C_mV","B_mV","A_mV","H_V","DG_dT")

#pull out variables of interest
colN <- list()

for(i in 1:length(varI)){
	colN[[i]] <- which(headL==varI[i])
}

#subset the data to only pull out data of interest
datSub <- list()
for(i in 1:length(varI)){
	datSub[[i]] <- data.frame(sapflow[,colN[[i]]])
	colnames(datSub[[i]]) <- paste0(headL[colN[[i]]],sensL[colN[[i]]])

}
#pull out sensors that are currently sampling

sensSub <- sensI[is.na(sensI$shrubID)==FALSE,]

datSub2 <- list()
for(i in 1:length(varI)){
	datSub2[[i]] <- datSub[[i]][,sensSub$sensorID]
	datSub2[[i]] <- data.frame(timeDF,datSub2[[i]])
}

#write tables
#output sensor info

for(j in 1:length(outDI)){
	write.table(sensSub, paste0(outDI[j],"\\sensorInfo.csv"), sep=",", row.names=FALSE)
	for(i in 1:length(varI)){
		write.table(datSub2[[i]], paste0(outDI[j],"\\",varI[i],".csv"), sep=",", row.names=FALSE)
	}	
}
