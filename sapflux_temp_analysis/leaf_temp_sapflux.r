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


#make a df of gs and filter by quantile

gsDF <- data.frame(doy=rep(gs.mmol[,1],times=5),year=rep(gs.mmol[,2],times=5),hour=rep(gs.mmol[,3],times=5),
					gs=as.vector(data.matrix(gs.mmol[,4:8])), sensorID=rep(seq(1,5),each=dim(gs.mmol)[1]))
gsDF <- na.omit(gsDF)					
quantile(gsDF$gs, .99)	
gsDF <- gsDF[gsDF$gs<=quantile(gsDF$gs, .99),]	
				
#join to leaf temp
leafAll <- join(sensorLT, gsDF, by=c("hour","doy","sensorID"), type="inner")
leafAllm <- join(leafAll, datM, by=c("hour","doy"), type="inner")
leafAllm2 <- join(leafAllm,Edf, by=c("hour","doy","sensorID"), type="inner")

#join met data to gs and E

gsDFall <- join(gsDF, datM, by=c("hour","doy"), type="left")
Eall <- join(Edf, datM, by=c("hour","doy"), type="left")

#also look at averages across all sensors
gsLAve <- aggregate(leafAllm2$gs,by=list(leafAllm2$hour,leafAllm2$doy),FUN="mean")
colnames(gsLAve) <- c("hour","doy","gs")
eLAve <- aggregate(leafAllm2$E,by=list(leafAllm2$hour,leafAllm2$doy),FUN="mean")
colnames(eLAve) <- c("hour","doy","E")
ldLAve<- aggregate(leafAllm2$leafD,by=list(leafAllm2$hour,leafAllm2$doy),FUN="mean")
colnames(ldLAve) <- c("hour","doy","ld")

vLAve<- aggregate(leafAllm2$vpd,by=list(leafAllm2$hour,leafAllm2$doy),FUN="mean")
colnames(vLAve) <- c("hour","doy","vpd")


aveAll <- data.frame(gsLAve,E=eLAve$E,ld=ldLAve$ld,vpd=vLAve$vpd)
aveAll <- aveAll[aveAll$gs>0,]

plot(aveAll$vpd,aveAll$ld)
plot(aveAll$vpd,aveAll$gs)
plot(aveAll$ld,aveAll$gs)

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

#######################################
#####plot of all measurements     ##### 
#######################################
hd <- 40
wd <- 40

sensorA <- leafAllm2[leafAllm2$gs>0,]

colj <- c("cornflowerblue","coral2")
px <- 5
mx <- 3
lx <- 3
lnn <- 5 
llx <- 4
llnn <- 13

jpeg(paste0(plotDI,"\\sensor_comp\\comp_sensor.jpg"), width=4000,height=2000, quality=100)
		layout(matrix(seq(1,3), ncol=3), width=rep(lcm(wd),3), height=rep(lcm(hd),3))
		
		#plot of all leaf temp difference and D
		par(mai=c(2,2,2,2))
		plot(c(0,1),c(0,1), ylim=c(-5,10), xlim=c(0,2),type="n", axes=FALSE,
			xaxs="i",yaxs="i", xlab=" ",ylab=" ")
		for(j in 1:length(Ndays)){
		points(leafAllm2$vpd[leafAllm2$doy==Ndays[j]],leafAllm2$leafD[leafAllm2$doy==Ndays[j]], col=colj[j],
				cex=px, pch=19)
		}
		axis(1, seq(0,2, by=.5), rep("", length(seq(0,2, by=.5))), lwd.ticks=lx)
		mtext(seq(0,2, by=.5), at=seq(0,2, by=.5), side=1,line=lnn, cex=mx)
		axis(2, seq(-5,10, by=5), rep("", length(seq(-5,10, by=5))), lwd.ticks=lx)
		mtext(seq(-5,10, by=5), at=seq(-5,10, by=5), side=2,line=lnn, cex=mx,las=2)
		mtext("vapor pressure deficit",side=1,line=llnn,cex=llx)
		mtext("leaf-air temperature",side=2,line=llnn,cex=llx)
		#plot of all gs and D
		par(mai=c(2,2,2,2))
		plot(c(0,1),c(0,1), ylim=c(0,450), xlim=c(0,2),type="n", axes=FALSE,
			xaxs="i",yaxs="i", xlab=" ",ylab=" ")	
		for(j in 1:length(Ndays)){
			points(gsDFall$vpd[gsDFall$doy==Ndays[j]&gsDFall$gs>0],gsDFall$gs[gsDFall$doy==Ndays[j]&gsDFall$gs>0],
				col=colj[j],cex=px,pch=19)
		}
		axis(1, seq(0,2, by=.5), rep("", length(seq(0,2, by=.5))), lwd.ticks=lx)
		mtext(seq(0,2, by=.5), at=seq(0,2, by=.5), side=1,line=lnn, cex=mx)
		axis(2, seq(0,400, by=100), rep("", length(seq(0,400, by=100))), lwd.ticks=lx)
		mtext(seq(0,400, by=100), at=seq(0,400, by=100), side=2,line=lnn, cex=mx,las=2)		
		mtext("vapor pressure deficit",side=1,line=llnn,cex=llx)
		mtext("canopy stomatal conductance",side=2,line=llnn,cex=llx)
		#plot of all leaf temp difference and gs
		par(mai=c(2,2,2,2))
		plot(c(0,1),c(0,1), xlim=c(-5,10), ylim=c(0,500),type="n", axes=FALSE,
			xaxs="i",yaxs="i", xlab=" ",ylab=" ")	
		for(j in 1:length(Ndays)){
			points(sensorA$leafD[sensorA$doy==Ndays[j]],sensorA$gs[sensorA$doy==Ndays[j]],
			col=colj[j],cex=px,pch=19)
		}
		
		axis(2, seq(0,400, by=100), rep("", length(seq(0,400, by=100))), lwd.ticks=lx)
		mtext(seq(0,400, by=100), at=seq(0,400, by=100), side=2,line=lnn, cex=mx,las=2)			
		
		axis(1, seq(-5,10, by=5), rep("", length(seq(-5,10, by=5))), lwd.ticks=lx)
		mtext(seq(-5,10, by=5), at=seq(-5,10, by=5), side=1,line=lnn, cex=mx)
		mtext("leaf-air temperature",side=1,line=llnn,cex=llx)
		mtext("canopy stomatal conductance",side=2,line=llnn,cex=llx)		
dev.off()

summary(lm(sensorA$gs~sensorA$leafD))


#######################################
#####sensor mean plot             ##### 
#######################################

#exclude gs below 0.6 vpd
aveAllD <- aveAll[aveAll$vpd>=0.6,]


hd <- 40
wd <- 40
colj <- c("cornflowerblue","coral2")
px <- 5
mx <- 3
lx <- 3
lnn <- 5 
llx <- 4
llnn <- 13

jpeg(paste0(plotDI,"\\sensor_comp\\comp_average.jpg"), width=4000,height=2000, quality=100)
	layout(matrix(seq(1,3), ncol=3), width=rep(lcm(wd),3), height=rep(lcm(hd),3))
		#plot of all leaf temp difference and D
		par(mai=c(2,2,2,2))
		plot(c(0,1),c(0,1), ylim=c(-5,10), xlim=c(0,2),type="n", axes=FALSE,
			xaxs="i",yaxs="i", xlab=" ",ylab=" ")
		for(j in 1:length(Ndays)){
			points(aveAll$vpd[aveAll$doy==Ndays[j]],aveAll$ld[aveAll$doy==Ndays[j]],
			col=colj[j],cex=px,pch=19)
		}
		axis(1, seq(0,2, by=.5), rep("", length(seq(0,2, by=.5))), lwd.ticks=lx)
		mtext(seq(0,2, by=.5), at=seq(0,2, by=.5), side=1,line=lnn, cex=mx)
		axis(2, seq(-5,10, by=5), rep("", length(seq(-5,10, by=5))), lwd.ticks=lx)
		mtext(seq(-5,10, by=5), at=seq(-5,10, by=5), side=2,line=lnn, cex=mx,las=2)
		mtext("vapor pressure deficit",side=1,line=llnn,cex=llx)
		mtext("leaf-air temperature",side=2,line=llnn,cex=llx)
		
		#plot of all gs and D
		par(mai=c(2,2,2,2))
		plot(c(0,1),c(0,1), ylim=c(0,450), xlim=c(0,2),type="n", axes=FALSE,
			xaxs="i",yaxs="i", xlab=" ",ylab=" ")	
		for(j in 1:length(Ndays)){
			points(aveAllD$vpd[aveAll$doy==Ndays[j]],aveAllD$gs[aveAll$doy==Ndays[j]],
			col=colj[j],cex=px,pch=19)
		}	
		axis(1, seq(0,2, by=.5), rep("", length(seq(0,2, by=.5))), lwd.ticks=lx)
		mtext(seq(0,2, by=.5), at=seq(0,2, by=.5), side=1,line=lnn, cex=mx)
		axis(2, seq(0,400, by=100), rep("", length(seq(0,400, by=100))), lwd.ticks=lx)
		mtext(seq(0,400, by=100), at=seq(0,400, by=100), side=2,line=lnn, cex=mx,las=2)		
		mtext("vapor pressure deficit",side=1,line=llnn,cex=llx)
		mtext("canopy stomatal conductance",side=2,line=llnn,cex=llx)		
		#plot of all leaf temp difference and gs
		par(mai=c(2,2,2,2))
		plot(c(0,1),c(0,1), xlim=c(-5,10), ylim=c(0,500),type="n", axes=FALSE,
			xaxs="i",yaxs="i", xlab=" ",ylab=" ")		
		for(j in 1:length(Ndays)){
			points(aveAllD$ld[aveAll$doy==Ndays[j]],aveAllD$gs[aveAll$doy==Ndays[j]],
			col=colj[j],cex=px,pch=19)
		}
		axis(2, seq(0,400, by=100), rep("", length(seq(0,400, by=100))), lwd.ticks=lx)
		mtext(seq(0,400, by=100), at=seq(0,400, by=100), side=2,line=lnn, cex=mx,las=2)			
		
		axis(1, seq(-5,10, by=5), rep("", length(seq(-5,10, by=5))), lwd.ticks=lx)
		mtext(seq(-5,10, by=5), at=seq(-5,10, by=5), side=1,line=lnn, cex=mx)
		mtext("leaf-air temperature",side=1,line=llnn,cex=llx)
		mtext("canopy stomatal conductance",side=2,line=llnn,cex=llx)			
		legend("topleft", paste(Ndays), col=colj,pch=19,cex=4,bty="n")
dev.off()	

summary(lm(aveAllD$gs~aveAllD$ld))		