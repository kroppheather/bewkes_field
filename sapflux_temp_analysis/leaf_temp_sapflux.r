##########################################################
########comparison of sapflux and leaf temp   ###########
########collected from the TIM camera          ###########
########for bewkes imagery                     ###########
########Heather Kropp started July 2018        ###########
##########################################################
##########################################################
##########################################################
### Input files: sapflow in g/s: sapflowF             ###
###              sensor data:   datS                   ###
##########################################################
##########################################################


#######################################
#####read in sapflux data         ##### 
#######################################
os.flag <- 1

source("c:\\Users\\hkropp\\Documents\\GitHub\\bewkes_field\\sapflux_process.r")

#######################################
#####libraries                    ##### 
#######################################
library(plyr)


#######################################
#####set up directories           ##### 
#######################################
plotDI <- "z:\\projects\\thermal_canopy\\bewkes\\sapflux_temp"

#######################################
#####read in other data           ##### 
#######################################

datSLA <- read.csv("z:\\data_repo\\field_data\\bewkes\\SLA\\SLA.csv")

datLT <- read.csv("z:\\projects\\thermal_canopy\\bewkes\\leaf_temp\\subset_out\\leaf_temp_ir.csv")

datM <- read.csv("z:\\data_repo\\field_data\\bewkes\\sensor\\decagon\\met_air.csv")

#######################################
#####calculate half hour leaf temp##### 
#######################################


#first aggregate the leaf temp so that they are averaged 
#in the half hour of measurement
datLT$hours <- round_any(datLT$time, 0.5, floor)

sensorLT <- aggregate(datLT$average, by=list(datLT$sensorID, datLT$hours,datLT$doy), FUN="mean")
colnames(sensorLT) <- c("sensorID","hour","doy","leafT")

#join air temperature
sensorLT <- join(sensorLT, datM, by=c("hour","doy"), type="left")


Ndays <- unique(sensorLT$doy)
#calculate difference leaf - air
sensorLT$leafD <- sensorLT$leafT-sensorLT$temp

#######################################
#####look at plots of sapflux and #####
#####leaf temp vs air temp        #####
#####to visualize raw data        ##### 
#######################################
for(i in 1:length(Ndays)){ 
	for(j in 1:5){
		jpeg(paste0(plotDI,"\\sapflux_temp_sensor",j,"_doy",Ndays[i],".jpg"), width=1000,
			height=700, quality=100)
			par(mfrow=c(1,3))
				par(mai=c(1,1,1,1))
				plot(c(0,1),c(0,1),xlim=c(4,20), ylim=c(0,45), xlab=" ",
					ylab=" ",axes=FALSE, yaxs="i", xaxs="i", type="n")
					
				points(sapflowF$hour[sapflowF$doy==Ndays[i]],
					sapflowF[sapflowF$doy==Ndays[i],j+3],
					pch=19, col="cornflowerblue", cex=2, type="b")
				axis(1,	seq(5,20, by=5), lwd.ticks=2,cex.axis=2)
				axis(2,	seq(0,45, by=5), lwd.ticks=2,cex.axis=2, las=2)
				mtext("hour",side=1, cex=2, line=4)
				mtext("sapflow (g/s)", side=2, cex=2, line=5)
				par(mai=c(1,1,1,1))
				plot(c(0,1),c(0,1)	,xlim=c(4,20), ylim=c(0,35), xlab=" ",
					ylab=" ",axes=FALSE, yaxs="i", xaxs="i", type="n")
				
				points(datM$hour[datM$doy==Ndays[i]],datM$temp[datM$doy==Ndays[i]],
						pch=19, col="coral3", cex=2, type="b")
				points(sensorLT$hour[sensorLT$doy==Ndays[i]&sensorLT$sensorID==j],
						sensorLT$leafT[sensorLT$doy==Ndays[i]&sensorLT$sensorID==j], 		
						pch=19, col="cornflowerblue", cex=2, type="b")
				axis(1,	seq(5,20, by=5), lwd.ticks=2,cex.axis=2)
				axis(2,	seq(0,35, by=5), lwd.ticks=2,cex.axis=2, las=2)
				mtext("hour",side=1, cex=2, line=4)
				mtext("temperature (C)", side=2, cex=2, line=5)
				legend("bottomleft", c("leaf","air"), pch=19, 
					col=c("cornflowerblue","coral3"),bty="n", cex=2)
				par(mai=c(1,1,1,1))
				plot(c(0,1),c(0,1)	,xlim=c(4,20), ylim=c(-10,10), xlab=" ",
					ylab=" ",axes=FALSE, yaxs="i", xaxs="i", type="n")
				points(sensorLT$hour[sensorLT$doy==Ndays[i]&sensorLT$sensorID==j],
						sensorLT$leafD[sensorLT$doy==Ndays[i]&sensorLT$sensorID==j], 		
						pch=19, col="cornflowerblue", cex=2, type="b")
				axis(1,	seq(5,20, by=5), lwd.ticks=2,cex.axis=2)	
				axis(2,	seq(-10,10, by=5), lwd.ticks=2,cex.axis=2, las=2)	
				mtext("hour",side=1, cex=2, line=4)
				mtext("temperature difference (leaf-air, C)", side=2, cex=2, line=5)				
		dev.off()				
	}					
}



#######################################
#####covert sapflux data          ##### 
#######################################

##convert transpiration units ###
#get the average area per leaf in cm2
leafArea <- mean(datSLA$leaf.area.cm2/datSLA$leaf.number)


#calculate sensor leaf area in m2
#from average leaf area estimate and leaf counts
datS$leafA.m2 <- (datS$leaf.count*leafArea)*(1/100)*(1/100)

#calculate sapflow on the per area leaf basis
#for transpiration
#units of Ecanopy g m-2 s-1
Ecanopy <- data.frame(sapflowF[,1:3])
for(i in 1:dim(datS)[1]){

	Ecanopy[,i+3] <- sapflowF[,i+3]/datS$leafA.m2[i]

}

#turn Ecanopy into a data frame
Edf <- data.frame(doy=rep(Ecanopy[,1],times=5),year=rep(Ecanopy[,2],times=5),hour=rep(Ecanopy[,3],times=5),
					E=as.vector(data.matrix(Ecanopy[,4:8])), sensorID=rep(seq(1,5),each=dim(Ecanopy)[1]))
Edf <- na.omit(Edf)
quantile(Edf$E,.99)

Edf<- Edf[Edf$E<=quantile(Edf$E,.99),]
#### water vapor calculations ####
#calculate vapor pressure deficit
  #making the functions
e.sat <- function(Temp) { 0.611*exp((17.502*Temp)/(Temp+240.97)) }
#rh in decimal form
vpd <- function(esat,RH) {esat-((RH)*esat)}

datM$e.sat <- e.sat(datM$temp)
datM$vpd <- vpd(datM$e.sat,datM$rh)

#Kg 
kg.func <- function(Temp) {115.8 + (0.423*Temp)}

datM$kg <- kg.func(datM$temp)

#conversion of transpiration to kg m-2 s-1 from g
Ecanopy.kg <- data.frame(Ecanopy[,1:3],Ecanopy[,4:(3+dim(datS)[1])]*(1/1000))


#stomatal conductance (gs)
gs.func<- function (Kg.coeff, Elkg, Vpd, P)
{((Kg.coeff*Elkg)/ Vpd)*P}

#join met to transpiration

EcanopyMet <- join(Ecanopy.kg, datM, by=c("doy","year","hour"), type="left")

#calculate gs
gs.raw <- data.frame(EcanopyMet[,1:3])
for(i in 1:dim(datS)[1]){
	gs.raw[,i+3] <- gs.func(EcanopyMet[,3+i], EcanopyMet$kg, EcanopyMet$vpd, EcanopyMet$press)

}
#convert to moles m-2 s-1

unit.conv<-function(gs,T,P){gs*.446*(273/(T+273))*(P/101.3)}

gs.mmol <- data.frame(EcanopyMet[,1:3])
for(i in 1:dim(datS)[1]){
	gs.mmol[,i+3] <- unit.conv(gs.raw[,i+3], EcanopyMet$temp, EcanopyMet$press)
}



#### pair gs with leaf temp difference
gsDF <- data.frame(doy=rep(gs.mmol[,1],times=5),year=rep(gs.mmol[,2],times=5),hour=rep(gs.mmol[,3],times=5),
					gs=as.vector(data.matrix(gs.mmol[,4:8])), sensorID=rep(seq(1,5),each=dim(gs.mmol)[1]))
gsDF <- na.omit(gsDF)					
quantile(gsDF$gs, .99)	
gsDF <- gsDF[gsDF$gs<=quantile(gsDF$gs, .99),]	
				
#join to leaf temp
leafAll <- join(sensorLT, gsDF, by=c("hour","doy","sensorID"), type="left")
leafAllm <- join(leafAll, datM, by=c("hour","doy"), type="left")
leafAllm2 <- join(leafAllm,Edf, by=c("hour","doy","sensorID"), type="left")

#######################################
#####initial plots                ##### 
#######################################

plot(leafAllm2$leafT[leafAllm2$E>0&leafAllm2$sensorID==1],leafAllm2$E[leafAllm2$E>0&leafAllm2$sensorID==1],pch=19)
plot(leafAllm2$leafD[leafAllm2$E>0&leafAllm2$sensorID==1],leafAllm2$E[leafAllm2$E>0&leafAllm2$sensorID==1],pch=19)
plot(leafAllm2$leafD[leafAllm2$E>0&leafAllm2$sensorID==2],leafAllm2$E[leafAllm2$E>0&leafAllm2$sensorID==2],pch=19)
plot(leafAllm2$leafD[leafAllm2$E>0&leafAllm2$sensorID==3],leafAllm2$E[leafAllm2$E>0&leafAllm2$sensorID==3],pch=19)
plot(leafAllm2$leafD[leafAllm2$E>0&leafAllm2$sensorID==4],leafAllm2$E[leafAllm2$E>0&leafAllm2$sensorID==4],pch=19)
plot(leafAllm2$leafD[leafAllm2$E>0&leafAllm2$sensorID==5],leafAllm2$E[leafAllm2$E>0&leafAllm2$sensorID==5],pch=19)

plot(leafAllm2$vpd,leafAllm2$leafD,pch=19)
plot(leafAllm2$vpd[leafAllm2$doy==199],leafAllm2$leafD[leafAllm2$doy==199],pch=19)
plot(leafAllm2$vpd[leafAllm2$doy==207],leafAllm2$leafD[leafAllm2$doy==207],pch=19)


plot(leafAllm$vpd,leafAllm$leafD,pch=19)
plot(leafAllm$vpd[leafAllm$doy==199],leafAllm$leafD[leafAllm$doy==199],pch=19)
plot(leafAllm$vpd[leafAllm$doy==207],leafAllm$leafD[leafAllm$doy==207],pch=19)
plot(leafAllm$gs[leafAllm$doy==199],leafAllm$leafD[leafAllm$doy==199],pch=19)
plot(leafAllm$gs[leafAllm$doy==207],leafAllm$leafD[leafAllm$doy==207],pch=19)

#join met data to gs and E

gsDFall <- join(gsDF, datM, by=c("hour","doy"), type="left")
Eall <- join(Edf, datM, by=c("hour","doy"), type="left")

plot(gsDFall$vpd[gsDFall$gs>0&gsDFall$doy==199],gsDFall$gs[gsDFall$gs>0&gsDFall$doy==199],pch=19)
plot(gsDFall$vpd[gsDFall$gs>0&gsDFall$doy==207],gsDFall$gs[gsDFall$gs>0&gsDFall$doy==207],pch=19)

#######################################
#####sensor diurnal plots         ##### 
#######################################


#make half hourly plots for each day
hd <- 20
wd <- 50
lwx <- 3
mwx <- 4
lnx <- 4
px <- 5
for(j in 1:length(Ndays)){
	for(i in 1:5){
	jpeg(paste0(plotDI,"\\met_hh\\metplot_sensor",i,"doy",Ndays[j],".jpg"), width=2000,height=3500, quality=100)
		layout(matrix(seq(1,5), ncol=1), width=rep(lcm(wd),5), height=rep(lcm(hd),5))
		par(mai=c(0,0,0,0))
		plot(datM$hour[datM$doy==Ndays[j]],datM$vpd[datM$doy==Ndays[j]], type="b", pch=19, xlim=c(0,24), 
			axes=FALSE,xaxs="i",yaxs="i" , cex=px,ylim=c(0,2))
		axis(2,seq(0,1.5, by=.5), rep(	"",length(seq(0,1.5, by=.5))), lwd.ticks=lwx)
		mtext(seq(0,1.5, by=.5),at=seq(0,1.5, by=.5), side=2,cex=mwx,line=lnx, las=2)
		box(which="plot")
		par(mai=c(0,0,0,0))
		plot(datM$hour[datM$doy==Ndays[j]],datM$temp[datM$doy==Ndays[j]], type="b", pch=19,  ylim=c(10,37),
		axes=FALSE,xaxs="i",yaxs="i",cex=px, xlim=c(0,24))
		points(sensorLT$hour[sensorLT$doy==Ndays[j]&sensorLT$sensorID==i],sensorLT$leafT[sensorLT$doy==Ndays[j]&sensorLT$sensorID==i],
				col="cornflowerblue", pch=19, type="b", cex=px)
		axis(2,seq(15,30, by=5), rep(	"",length(seq(15,30, by=5))), lwd.ticks=lwx)
		mtext(seq(15,30, by=5),at=seq(15,30, by=5), side=2,cex=mwx,line=lnx, las=2)	
		box(which="plot")
		par(mai=c(0,0,0,0))		
		plot(sensorLT$hour[sensorLT$doy==Ndays[j]&sensorLT$sensorID==i],sensorLT$leafD[sensorLT$doy==Ndays[j]&sensorLT$sensorID==i],
			col="cornflowerblue", pch=19, type="b",xlim=c(0,24),axes=FALSE,xaxs="i",yaxs="i" , cex=px,
			ylim=c(-5,10))
		axis(2,seq(-2,8, by=2), rep(	"",length(seq(-2,8, by=2))), lwd.ticks=lwx)
		mtext(seq(-2,8, by=2),at=seq(-2,8, by=2), side=2,cex=mwx,line=lnx, las=2)	
		box(which="plot")		
		par(mai=c(0,0,0,0))	
		plot(Edf$hour[Edf$doy==Ndays[j]&Edf$sensorID==i],Edf$E[Edf$doy==Ndays[j]&Edf$sensorID==i],col="cornflowerblue", pch=19, type="b", xlim=c(0,24),
				axes=FALSE,xaxs="i",yaxs="i" , cex=px,ylim=c(0,100))
		axis(2,seq(10,90, by=10), rep(	"",length(seq(10,90, by=10))), lwd.ticks=lwx)
		mtext(seq(10,90, by=10),at=seq(10,90, by=10), side=2,cex=mwx,line=lnx, las=2)
		box(which="plot")
		par(mai=c(0,0,0,0))
		plot(gsDF$hour[gsDF$doy==Ndays[j]&gsDF$sensorID==i],gsDF$gs[gsDF$doy==Ndays[j]&gsDF$sensorID==i],col="cornflowerblue", pch=19, 
		type="b", xlim=c(0,24),axes=FALSE,xaxs="i",yaxs="i" , cex=px,ylim=c(0,450))
		axis(2,seq(0,400, by=100), rep(	"",length(seq(0,400, by=100))), lwd.ticks=lwx)
		mtext(seq(0,400, by=100),at=seq(0,400, by=100), side=2,cex=mwx,line=lnx, las=2)
		axis(1,seq(0,24, by=1), rep(	"",length(seq(0,24, by=1))), lwd.ticks=lwx)
		mtext(seq(0,24, by=1),at=seq(0,24, by=1), side=1,cex=mwx,line=lnx)
		box(which="plot")
	dev.off()
	}
}


