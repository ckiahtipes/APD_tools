#Mapping APD Surface samples.

#Necessary packages and fucntions.
library(raster) #Necessary package for working with worldclim data
library(maps) #Necessary package to get world map.
library(vegan)

zscore<-function(y1) {
  n1 <- mean(y1)
  n2 <- sd(y1)
  zscore <- (y1-n1)/n2
  zscore
}

economize=TRUE
save_pdf=FALSE

###A MAPPING DIVERSION
###This can be improved by pulling worldclim data, would basically fold extracting climatic variables into the mapping process.

select_locations=read.csv("APD_surface-pollen_sites.csv",header=TRUE,sep=",")
select_locations=as.data.frame(select_locations)

pol_pct=read.csv("APD_surface-pollen_pct.csv",header=TRUE,row.names="X")
pol_pct=as.data.frame(pol_pct)

sites_used=select_locations$Sample.name.APD

#Copying lat/long ranges and colors from previous.
LAT_RANGE=c(-10,15)
LON_RANGE=c(-5,30)
select_LAT=select_locations$LATITUDE
select_LON=select_locations$LONGITUDE
color_code=heat.colors(max(select_LON))
#lat/long ranges and colors

tavg.files=list.files("worldclim/wc2.1_30s_tavg/",".tif",full.names=TRUE)
tavg=stack(tavg.files)

prec.files=list.files("worldclim/wc2.1_30s_prec/",".tif",full.names=TRUE)
prec=stack(prec.files)

month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

names(tavg)=month
names(prec)=month

sample_latlong=data.frame(sites_used,select_LON,select_LAT,row.names="sites_used")

#Making average temp data frame

tavg.data=extract(tavg,sample_latlong)
tavg.data=as.data.frame(tavg.data)
row.names(tavg.data)=sites_used

#Making precip data frame

prec.data=extract(prec,sample_latlong)
prec.data=as.data.frame(prec.data)
row.names(prec.data)=sites_used

###MAPPING TIPS

tempcol=colorRampPalette(c("purple","blue","skyblue","green","lightgreen","yellow","orange","red","darkred")) #This is a cool means of constructing gradient colors

map_LON=c(-20,40) #Defining a different mapped area from the latitude/longitude selection of the taxa
map_LAT=c(-25,25) #Defining a different mapped area from the latitude/longitude selection of the taxa

plot_months=c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")

old.mar=par(mar=c(5, 4, 4, 2) + 0.1)
old.par=par(mfrow=c(1,1))

if(economize==FALSE){
  plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Longitude",ylab="Latitude",pch=NA) #Solving plotting problems by making an empty plot with defined boundaries
  plot(tavg$Jan,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE) #Draw map within predefined area up to the area limits.
  map("world",add=TRUE,xlim=map_LON) #Can also control the drawing here, tends to bleed over. 
  title(main="January temperatures, WorldClim 2.1")
  
  par(mar=c(5,4,4,5)+0.1) #Setting margins so legend doesn't get cut out in a fucked up way.
  
  plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Longitude",ylab="Latitude",pch=NA)
  plot(prec$Aug,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE)
  isohyets=rasterToContour(prec$Aug,maxpixels=1e6,nlevels=30)
  plot(isohyets,add=TRUE,xlim=map_LON,ylim=map_LAT)
  title(main="August Precipitation, WorldClim 2.1")
  
  #Overall, the above is approaching a satisfactory result. Margins are fucked up and the legend is plotting on top of the map.
  
  #These mapping tools are really cool - making isohyets has long been a dream and I had not fathomed how easy it might be. 
  #Need to do a better job of getting a consistent range of climate data, isohyet reconstructions, and map extent...
  
  project.area=extent(map_LON[1],map_LON[2],map_LAT[1],map_LAT[2])
  proj.prec=crop(prec,project.area)
  proj.tavg=crop(tavg,project.area)
  proj.isohyets=rasterToContour(proj.prec$Apr,maxpixels = 1e4, nlevels=20)
  
  
  #This makes a flawed, but exciting map. Still having issues with the legend getting stuck inside of the plot. Might have to toy with margins. 
  plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Longitude",ylab="Latitude",pch=NA)
  plot(proj.prec$Apr,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE,zlim=c(0,1000))
  plot(proj.isohyets,add=TRUE,lty=2)
  title(main="April Precipitation Scaled to tropics (20S-20N), WorldClim 2.1")
  points(sample_latlong$select_LON,sample_latlong$select_LAT,pch=19)
  map("world",add=TRUE,xlim=map_LON)
  #There's some sites without climate data and we should cull those. 
  
  exclude_na=is.na(prec.data$Jan) #We're also excluding sites without climate data. 
  
  sample_latlong=sample_latlong[exclude_na!=TRUE,]
  prec.data=prec.data[exclude_na!=TRUE,]
  tavg.data=tavg.data[exclude_na!=TRUE,]
  pol_pct=pol_pct[exclude_na!=TRUE,]
  
  #Now we can add sites to one of these maps just to get a look.
  proj.isohyets=rasterToContour(proj.prec$Nov,maxpixels = 1e4, nlevels=20)
  plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Longitude",ylab="Latitude",pch=NA)
  plot(proj.prec$Nov,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE,zlim=c(0,1000))
  plot(proj.isohyets,add=TRUE,lty=2)
  points(sample_latlong$select_LON,sample_latlong$select_LAT,pch=19)
  map("world",add=TRUE,xlim=map_LON)
  title(main="November Precipitation Scaled to tropics (20S-20N), WorldClim 2.1")
  
  #Now let's loop this around and make a year's worth of precip data to see how this unfolds.
  #Three columns, four rows.
  #Set month order
  
  plot_months=c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
  old.par=par(mfrow=c(1,1))
  par(mfrow=c(4,3))
  par(mar=c(3, 3, 2, 3) + 0.1)
  
  #Need to save a .gif version and a regular version - ADD LATER
  setEPS()
  png(filename=paste0(names(proj.prec[[plot_months[i]]]),'.png'),width=1000,height=800)
  for(i in 1:length(plot_months)){
    plot.iso=rasterToContour(proj.prec[[paste0(plot_months[i])]])
    plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Longitude",ylab="Latitude",pch=NA)
    plot(proj.prec[[paste0(plot_months[i])]],col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE,zlim=c(0,1000))
    plot(plot.iso,add=TRUE,lty=2)
    points(sample_latlong$select_LON,sample_latlong$select_LAT,pch=19)
    map("world",add=TRUE,xlim=map_LON)
    title(main=paste0(plot_months[i]))
    
  }
  dev.off()
  #Reset number of plots
  par(old.par)
  
  #Reset project margins and empty garbage
  par(old.mar)
  gc()
  
  ###Okay, all of this roughly works and plots something that is sensible. 
  
  seasonal=array(c(DJF.prec,MAM.prec,JJA.prec,SON.prec))
  #seasonal=as.raster(seasonal)
  names(seasonal)=c("DJF.prec","MAM.prec","JJA.prec","SON.prec")
  seas_names=c("DJF","MAM","JJA","SON")
  par(mfrow=c(2,2))
  par(mar=c(3, 3, 2, 3) + 0.1)
  
  for(i in 1:length(seas_names)){
    plot.iso=rasterToContour(seasonal[[i]])
    plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Lon",ylab="Lat",pch=NA)
    plot(seasonal[[i]],col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE,zlim=c(0,2500))
    plot(plot.iso,add=TRUE,lty=2)
    points(sample_latlong$select_LON,sample_latlong$select_LAT,pch=19)
    map("world",add=TRUE,xlim=map_LON)
    title(main=paste0(seas_names[i]))
  }
  
  #Reset number of plots
  par(old.par)
  
  #Reset project margins and empty garbage
  par(old.mar)
  gc()
  
  par(mar=c(5,4,4,5)+0.1)
  prec.annual=sum(proj.prec[[1:12]])
  ann.iso=rasterToContour(prec.annual)
  plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Lon",ylab="Lat",pch=NA)
  plot(prec.annual,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE)
  plot(ann.iso,add=TRUE,lty=2)
  points(sample_latlong$select_LON,sample_latlong$select_LAT,pch=19)
  map("world",add=TRUE,xlim=map_LON)
  title(main="Annual Precipitation, WorldClim 2.1")
  par(old.mar)
  
  gc()
  
  #393 Sites.
  prec.means=apply(prec.data,1,mean)
  
  for(i in 1:length(prec.means)){
    if(i==1){
      dry_mos=vector("numeric",length(prec.means))
      blw_avg_mos=vector("numeric",length(prec.means))
      sd=vector("numeric",length(prec.means))
    }
    x=prec.data[i,prec.data[i,]>prec.means[i]]
    y=prec.data[i,prec.data[i,]<1]
    z=sd(prec.data[i,])
    blw_avg_mos[i]=length(x)
    sd[i]=z
    if(length(y)>0){
      dry_mos[i]=length(y)
    }
  }
  
  #The above roughly works, but we need some more insights before making any concrete moves...
  
  #Set order
  
  by_lat=order(sample_latlong$select_LAT)
  by_lon=order(sample_latlong$select_LON)
  
  prec.DJF=apply(prec.data[,c(12,1,2)],1,sum)
  prec.MAM=apply(prec.data[,c(3,4,5)],1,sum)
  prec.JJA=apply(prec.data[,c(6,7,8)],1,sum)
  prec.SON=apply(prec.data[,c(9,10,11)],1,sum)
  prec.ANN=apply(prec.data,1,sum)
  
  prec.DATA=list(prec.ANN,prec.DJF,prec.MAM,prec.JJA,prec.SON)
  prec.DATA=as.array(prec.DATA)
  names(prec.DATA)=c("ANN",seas_names)
  
  plot(prec.ANN[by_lat],sample_latlong$select_LAT[by_lat],xlim=c(0,max(prec.ANN)),col="black",type="o")
  points(prec.DJF[by_lat],sample_latlong$select_LAT[by_lat],col="orange",pch=24,type="o")
  points(prec.MAM[by_lat],sample_latlong$select_LAT[by_lat],col="red",pch=2,type="o")
  points(prec.JJA[by_lat],sample_latlong$select_LAT[by_lat],col="blue",pch=3,type="o")
  points(prec.SON[by_lat],sample_latlong$select_LAT[by_lat],col="dark green",pch=4,type="o")
  title(main="WorldClim precip data DJF, MOM, JJA, SON, ANN")
  
  legend(2500,15,c("ANN","DJF","MAM","JJA","SON"),col=c("black","orange","red","blue","dark green"),pch=c(1,2,3,4,5))
  
  plot(prec.ANN[by_lat])
  
  #Now we can stack this on the previous plot
  
  plot(prec.ANN[by_lat]/max(prec.ANN),ylim=c(0,1),pch=19)
  points(prec.DJF[by_lat]/max(prec.ANN),col="orange",pch=19)
  points(prec.MAM[by_lat]/max(prec.ANN),col="red",pch=19)
  points(prec.JJA[by_lat]/max(prec.ANN),col="blue",pch=19)
  points(prec.SON[by_lat]/max(prec.ANN),col="dark green",pch=19)
  lines((pol_pct$Amaranthaceae.Chenopodiaceae_undiff.+pol_pct$Amaranthus.type+pol_pct$Poaceae_undiff.)/100)
  title(main="WorldClim precip data DJF, MOM, JJA, SON, ANN, and Amaranthaceae + Poaceae",cex=0.75)
  
  #Try plotting pollen pct/zscores by latitude and see how some major types fall out.
  #Look at the taxa list and try grouping by veg zone?
  
  #Exploratory plot - histograms and map. Color sheme isn't terribly precise and the number of breaks is too high in the first plot. 
  
  par(mar=c(3, 3, 2, 3) + 0.1)
  par(mfrow=c(2,2))
  
  for(i in 1:length(seas_names)){
    plot.iso=rasterToContour(seasonal[[i]])
    plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Lon",ylab="Lat",pch=NA)
    plot(seasonal[[i]],col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE,zlim=c(0,2000))
    plot(plot.iso,add=TRUE,lty=2)
    points(sample_latlong$select_LON,sample_latlong$select_LAT,pch=19)
    map("world",add=TRUE,xlim=map_LON)
    title(main=paste0(seas_names[i]))
    
    hist(prec.DATA[[i]],breaks=length(proj.isohyets$level),xlim=c(0,max(prec.DATA[[i]])),ylim=c(0,150),main=paste0(seas_names[i]),col=tempcol(length(proj.isohyets$level)))
  }
  par(old.par)
  par(old.mar)
  
  #Plotting a 4 square set of histograms to think about patterns in precipitation across major/minor dry and wet seasons.
  
  ###Fixed this. Now we can group positions by some more precise rainfall data.
  #Need to construct a permanent array with isohyets for annual precip and seasonal precip.
  
  ANN.iso=rasterToContour(prec.annual)
  DJF.iso=rasterToContour(seasonal$DJF.prec)
  MAM.iso=rasterToContour(seasonal$MAM.prec)
  JJA.iso=rasterToContour(seasonal$JJA.prec)
  SON.iso=rasterToContour(seasonal$SON.prec)
  
  iso.RANK=list(ANN.iso,DJF.iso,MAM.iso,JJA.iso,SON.iso)
  iso.RANK=as.array(iso.RANK)
  iso_cols=c("ANN","DJF","MAM","JJA","SON")
  
  names(iso.RANK)=iso_cols
  
  iso_assign=matrix(nrow=nrow(sample_latlong),ncol=length(iso_cols))
  iso_assign=as.data.frame(iso_assign)
  colnames(iso_assign)=iso_cols
  row.names(iso_assign)=row.names(sample_latlong)
  
  
  for(i in 1:length(iso_cols)){
    bins=iso.RANK[[i]][[1]]
    values=as.numeric(bins)
    for(ii in 1:length(bins)){
      if(ii==1){
        x=prec.DATA[[i]]<values[ii]
        iso_assign[prec.DATA[[i]]<values[ii],i]=ii
      } else {
        x=prec.DATA[[i]]>values[ii-1] & prec.DATA[[i]]<values[ii]
        iso_assign[prec.DATA[[i]]>values[ii-1] & prec.DATA[[i]]<values[ii],i]=ii
      }
    }
  }
  
  rank_totals=apply(iso_assign[,2:5],1,sum) #This is actually kind of badass - can make this the new "depth" in a pollen diagram?
  hist(rank_totals,breaks=10)
  
  #Here we want to take the decomposed precipitation data and use it to look at palynological results.
  #Testing shows that z-scores make a reasonably convincing plot.
  #Can we treat latitude like depth
  
  pol_zscores=apply(pol_pct,1,zscore) #Make z-scores
  pol_zscores=as.data.frame(t(pol_zscores)) #Fix dataframe so things plot properly
  
  plot(pol_zscores$Acacia[order(rank_totals)],sample_latlong$select_LAT[order(rank_totals)],xlim=c(-4,10),pch=21,col="black",bg=tempcol(length(rank_totals)))
  points(pol_zscores$Amaranthaceae.Chenopodiaceae_undiff.[order(rank_totals)]+4,sample_latlong$select_LAT[order(rank_totals)],pch=21,col="black",bg=tempcol(length(rank_totals)))
  
  
  #You could use arrows to show deviation from mean value...
  
  #plot(45,45,xlim=c(-4,20),ylim=c(-5,16),xlab="LAT",ylab="Z-score")
  
  x0=rep(0,length(rank_totals))
  y0=prec.ANN[order(rank_totals)]
  x1=pol_pct$Acacia[order(rank_totals)]/100
  y1=prec.ANN[order(rank_totals)]
  color_spec=tempcol(max(prec.ANN))
  point_colors=color_spec[prec.ANN[order(rank_totals)]]
  
  plot(45,45,xlim=c(min(x1),max(x1)),ylim=c(min(y1),max(y1)),xlab="LAT",ylab="Z-score")
  arrows(x0[x1>0],y0[x1>0],x1[x1>0],y1[x1>0],code=0)
  
  points(x1[x1>0],y1[x1>0],pch=21,col="black",bg=point_colors[x1>0])
  title(main="% Acacia pollen & WorldClim2.1 precipitation, colors reflect annual precip")
  
  #This bit of code creates a decent plot showing pcts by precipitation with colors corresponding to their isohyet.
  
  gc()
  
} #END OF ECONOMIZE SECTION

project.area=extent(map_LON[1],map_LON[2],map_LAT[1],map_LAT[2])
proj.prec=crop(prec,project.area)
proj.tavg=crop(tavg,project.area)
proj.isohyets=rasterToContour(proj.prec$Apr,maxpixels = 1e4, nlevels=20)

exclude_na=is.na(prec.data$Jan) #We're also excluding sites without climate data. 

sample_latlong=sample_latlong[exclude_na!=TRUE,]
prec.data=prec.data[exclude_na!=TRUE,]
tavg.data=tavg.data[exclude_na!=TRUE,]
pol_pct=pol_pct[exclude_na!=TRUE,]

###Can we make shading and isohyets that represent seasons?

DJF.prec=sum(proj.prec$Dec,proj.prec$Jan,proj.prec$Feb)
MAM.prec=sum(proj.prec$Mar,proj.prec$Apr,proj.prec$May)
JJA.prec=sum(proj.prec$Jun,proj.prec$Jul,proj.prec$Aug)
SON.prec=sum(proj.prec$Sep,proj.prec$Oct,proj.prec$Nov)

seasonal=array(c(DJF.prec,MAM.prec,JJA.prec,SON.prec))
#seasonal=as.raster(seasonal)
names(seasonal)=c("DJF.prec","MAM.prec","JJA.prec","SON.prec")
seas_names=c("DJF","MAM","JJA","SON")

###Annual precip.
par(mar=c(5,4,4,5)+0.1)
prec.annual=sum(proj.prec[[1:12]])
ann.iso=rasterToContour(prec.annual)
plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Lon",ylab="Lat",pch=NA)
plot(prec.annual,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE)
plot(ann.iso,add=TRUE,lty=2)
points(sample_latlong$select_LON,sample_latlong$select_LAT,pch=19)
map("world",add=TRUE,xlim=map_LON)
title(main="Annual Precipitation, WorldClim 2.1")
par(old.mar)

gc()

#393 Sites.
prec.means=apply(prec.data,1,mean)

for(i in 1:length(prec.means)){
  if(i==1){
    dry_mos=vector("numeric",length(prec.means))
    blw_avg_mos=vector("numeric",length(prec.means))
    sd=vector("numeric",length(prec.means))
  }
  x=prec.data[i,prec.data[i,]>prec.means[i]]
  y=prec.data[i,prec.data[i,]<1]
  z=sd(prec.data[i,])
  blw_avg_mos[i]=length(x)
  sd[i]=z
  if(length(y)>0){
    dry_mos[i]=length(y)
  }
}

#The above roughly works, but we need some more insights before making any concrete moves...

#Set order

by_lat=order(sample_latlong$select_LAT)
by_lon=order(sample_latlong$select_LON)

prec.DJF=apply(prec.data[,c(12,1,2)],1,sum)
prec.MAM=apply(prec.data[,c(3,4,5)],1,sum)
prec.JJA=apply(prec.data[,c(6,7,8)],1,sum)
prec.SON=apply(prec.data[,c(9,10,11)],1,sum)
prec.ANN=apply(prec.data,1,sum)

prec.DATA=list(prec.ANN,prec.DJF,prec.MAM,prec.JJA,prec.SON)
prec.DATA=as.array(prec.DATA)
names(prec.DATA)=c("ANN",seas_names)

plot(prec.ANN[by_lat],sample_latlong$select_LAT[by_lat],xlim=c(0,max(prec.ANN)),col="black",type="o")
points(prec.DJF[by_lat],sample_latlong$select_LAT[by_lat],col="orange",pch=24,type="o")
points(prec.MAM[by_lat],sample_latlong$select_LAT[by_lat],col="red",pch=2,type="o")
points(prec.JJA[by_lat],sample_latlong$select_LAT[by_lat],col="blue",pch=3,type="o")
points(prec.SON[by_lat],sample_latlong$select_LAT[by_lat],col="dark green",pch=4,type="o")
title(main="WorldClim precip data DJF, MOM, JJA, SON, ANN")

legend(2500,15,c("ANN","DJF","MAM","JJA","SON"),col=c("black","orange","red","blue","dark green"),pch=c(1,2,3,4,5))

plot(prec.ANN[by_lat])

ANN.iso=rasterToContour(prec.annual)
DJF.iso=rasterToContour(seasonal$DJF.prec)
MAM.iso=rasterToContour(seasonal$MAM.prec)
JJA.iso=rasterToContour(seasonal$JJA.prec)
SON.iso=rasterToContour(seasonal$SON.prec)

iso.RANK=list(ANN.iso,DJF.iso,MAM.iso,JJA.iso,SON.iso)
iso.RANK=as.array(iso.RANK)
iso_cols=c("ANN","DJF","MAM","JJA","SON")

names(iso.RANK)=iso_cols

iso_assign=matrix(nrow=nrow(sample_latlong),ncol=length(iso_cols))
iso_assign=as.data.frame(iso_assign)
colnames(iso_assign)=iso_cols
row.names(iso_assign)=row.names(sample_latlong)

for(i in 1:length(iso_cols)){
  bins=iso.RANK[[i]][[1]]
  values=as.numeric(bins)
  for(ii in 1:length(bins)){
    if(ii==1){
      x=prec.DATA[[i]]<values[ii]
      iso_assign[prec.DATA[[i]]<values[ii],i]=ii
    } else {
      x=prec.DATA[[i]]>values[ii-1] & prec.DATA[[i]]<values[ii]
      iso_assign[prec.DATA[[i]]>values[ii-1] & prec.DATA[[i]]<values[ii],i]=ii
    }
  }
}

rank_totals=apply(iso_assign[,2:5],1,sum) #This is actually kind of badass - can make this the new "depth" in a pollen diagram?
hist(rank_totals,breaks=10)

#Here we want to take the decomposed precipitation data and use it to look at palynological results.


