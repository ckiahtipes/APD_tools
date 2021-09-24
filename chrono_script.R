#Chronologies, etc. with rbacon.

library(rbacon)

###BLD400###

Bacon("BLD400",boundary=c(87,100,133),thick=2) #This does a better job of bounding the weird upper section of the core.
Bacon("BLD400",slump=c(140,180),boundary=100,thick=2,mem.strength=15,mem.mean=0.23) 

Bacon("BLD400",boundary=c(87,100),slump=c(132,180),thick=2,mem.strength=15,mem.mean=0.23) 

Bacon("BLD400",slump=c(132,180),thick=2,mem.strength=15,mem.mean=0.23) #Latest iteration - hiatus doesn't change overlap at all.

#Gathering age-depth model info and making useful plot for piecing together chronologies and sketches.

BLD400_adm=read.table("Cores/BLD400/BLD400_57_ages.txt",header=TRUE,sep="\t")

plot(BLD400_adm$min,BLD400_adm$depth*-1,type="l",ylim=c(-200,0),xlim=c(0,2700))
lines(BLD400_adm$max,BLD400_adm$depth*-1)
lines(BLD400_adm$median,BLD400_adm$depth*-1,lty=2,col="red")
lines(BLD400_adm$mean,BLD400_adm$depth*-1,lty=2,col="dark blue")

###IMB100###

Bacon("IMB100",coredir="Cores/") #This run worked - problems with patterning in iterations and uncertainty in lower depths.

Bacon("IMB100",thick=2,boundary=180,acc.mean=c(50,500),mem.mean=0.4) #100% of dates overlap with age-depth model!!! Say no when prompted...

#Gathering age-depth model info and making useful plot for piecing together chronologies and sketches.

IMB100_adm=read.table("Cores/IMB100/IMB100_46_ages.txt",header=TRUE,sep="\t")

plot(IMB100_adm$min,IMB100_adm$depth*-1,type="l",ylim=c(-250,0),xlim=c(40000,0),xlab="cal yr BP",ylab="depth cm")
lines(IMB100_adm$max,IMB100_adm$depth*-1)
lines(IMB100_adm$median,IMB100_adm$depth*-1,lty=2,col="red")
lines(IMB100_adm$mean,IMB100_adm$depth*-1,lty=2,col="dark blue")
title(main="IMB100 Age-Depth Model (v46)")

#building in sampling interval

samp_int=c(seq(14,234,2))*-1
line_boundaries=c(rep(40000,length(samp_int)))

for(i in 1:length(samp_int)){
  lines(c(40000,0),c(samp_int[i],samp_int[i]),lty=3)
}

calib.plot() #Get just the calibrated dates by depth.

###IYO100###

Bacon("IYO100",coredir="Cores/")

Bacon("IYO100",thick=2,mem.mean=0.5)

IYO100_adm=read.table("Cores/IYO100/IYO100_34_ages.txt",header=TRUE,sep="\t")

plot(IYO100_adm$min,IYO100_adm$depth*-1,type="l",ylim=c(-120,0),xlim=c(2000,0),xlab="cal yr BP",ylab="depth cm")
lines(IYO100_adm$max,IYO100_adm$depth*-1)
lines(IYO100_adm$median,IYO100_adm$depth*-1,lty=2,col="red")
lines(IYO100_adm$mean,IYO100_adm$depth*-1,lty=2,col="dark blue")
title(main="IYO100 Age-Depth Model (v34)")

samp_int=c(seq(0,120,2))*-1 #creating lines to show sampling interval, helps line it up with the core sketches/photos
for(i in 1:length(samp_int)){
  lines(c(2000,0),c(samp_int[i],samp_int[i]),lty=3)
}

calib.plot() #Get just the calibrated dates by depth.

###ING100###

#Bacon("ING100",coredir="Cores/")

Bacon("ING100",coredir="Cores/",slump=c(131,150)) #If we treat the bottom as a slump, we get better results. For now, we assume mixed but continuous deposition between 12 kyr bp 5,000 kyr bp

ING100_adm=read.table("Cores/ING100/ING100_27_ages.txt",header=TRUE,sep="\t")

plot(ING100_adm$min,ING100_adm$depth*-1,type="l",ylim=c(-150,0),xlim=c(18000,0),xlab="cal yr BP",ylab="depth cm")
lines(ING100_adm$max,ING100_adm$depth*-1)
lines(ING100_adm$median,ING100_adm$depth*-1,lty=2,col="red")
lines(ING100_adm$mean,ING100_adm$depth*-1,lty=2,col="dark blue")
title(main="ING100 Age-Depth Model (v34)")


###ING800###

#Bacon("ING800",coredir="Cores/")

Bacon("ING800",coredir="Cores/",hiatus.depths=72,acc.mean=100)

ING800_adm=read.table("Cores/ING800/ING800_42_ages.txt",header=TRUE,sep="\t")

plot(ING800_adm$min,ING800_adm$depth*-1,type="l",ylim=c(-250,0),xlim=c(30000,0),xlab="cal yr BP",ylab="depth cm")
lines(ING800_adm$max,ING800_adm$depth*-1)
lines(ING800_adm$median,ING800_adm$depth*-1,lty=2,col="red")
lines(ING800_adm$mean,ING800_adm$depth*-1,lty=2,col="dark blue")
title(main="ING800 Age-Depth Model (v34)")









