##########################################################
########Decagon logger data organization       ###########
########Heather Kropp started June 2018        ###########
##########################################################
##########################################################
##########################################################
### This script organizes data output by               ###
### the decagon logger  at the Bewkes field site       ###
##########################################################
##########################################################
##########################################################
##########################################################
library(lubridate)
library(plyr)

#data directory
datDI <- "c:\\Users\\hkropp\\Google Drive\\bewkes_logger\\data\\decagon\\csv"
sensDI <- "c:\\Users\\hkropp\\Google Drive\\bewkes_logger\\data\\decagon\\sensor"
#output directory
outDI <- c("c:\\Users\\hkropp\\Google Drive\\bewkes_logger\\sensor\\decagon",
			"z:\\data_repo\\field_data\\bewkes\\sensor\\decagon")

#list the files in the datalogger
decF <-  list.files(paste0(datDI),".csv")
########################################
### extract date from data file name ###
########################################

#pull out file date
Date <- character(0)

for(i in 1:length(decF)){
	Date[i]<- strsplit(strsplit(decF[i],"\\-")[[1]][2],"\\s")[[1]][2]
}

#convert to numerical date

DateF <- as.Date(Date, "%d%b%y")
#decimal  year
ddF <- (yday(DateF)/365)+year(DateF)

#find the file with the latest date
dfUse <- which(ddF==max(ddF))

#file read in df
files <- data.frame(path=decF,ddF=ddF)
#sort so reading in from lowest to highest date
files <- files[order(files$ddF),]
########################################
### read in data file                ###
########################################
#need to append data


dataF <- list()

for(i in 1:dim(files)[1]){
	dataF[[i]] <- read.csv(paste0(datDI,"\\",files$path[i]), na.strings="#N/A", skip=3,header=FALSE)
}

#turn into a dataframe
decDat <- ldply(dataF,data.frame)

#read in headers
headDat <- read.csv(paste0(datDI,"\\",decF[3]), nrows=3,stringsAsFactors=FALSE)
#read in sensor info
sensDat <- read.csv(paste0(sensDI,"\\sensor info.csv"))

#join header to proper name
headerN <- data.frame(name=as.character(headDat[2,]))
headerN <- join(headerN, sensDat, by="name", type="left")

########################################
### organize data                    ###
########################################

###pull out timestamp info###

dateDec <- decDat[,which(headerN$ID=="timestamp")]
#get date format
dateDecD <- as.Date(dateDec, "%m/%d/%Y %H:%M")

hoursD <- format(strptime(dateDec, "%m/%d/%Y %H:%M"),"%H")
minsD <- format(strptime(dateDec, "%m/%d/%Y %H:%M"),"%M")
hourD <- as.numeric(hoursD)+(as.numeric(minsD)/60)

timeDat <- data.frame(doy=yday(dateDecD),year=year(dateDecD), hour=hourD)


###organize by measurement type ###
#get the number of outputs to generate
types <- as.character(unique(headerN$output.group))
types <- types[types!="time"]
types <- types[types!="none"]

#create a list of the data
colsT <- list()
for(i in 1:length(types)){
	colsT[[i]] <- which(headerN$output.group==types[[i]])

}

#create a data frame with only the data of interest
dataT <-  list()
sensorT <- list()
for(i in 1:length(types)){
	dataT[[i]] <- data.frame(decDat[,colsT[[i]]])
	colnames(dataT[[i]]) <- headerN$ID[colsT[[i]]]
	dataT[[i]] <- data.frame(timeDat,dataT[[i]])
	sensorT[[i]] <- data.frame(headerN[colsT[[i]],2:5])
}

#output data
for(j in 1:length(outDI)){
	for(i in 1:length(types)){
		write.table(dataT[[i]],paste0(outDI[j],"\\met_",types[i],".csv"), sep=",", row.names=FALSE)
		write.table(sensorT[[i]],paste0(outDI[j],"\\sensorInfo_",types[i],".csv"), sep=",", row.names=FALSE)
	}
}
