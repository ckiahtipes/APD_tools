###Summarizing Pollen data from West and Central Africa.

#Two jobs here: 
  #1: is a quick review of West African records by latitude for Jacopo.
  #2: review terrestrial and marine records from Central Africa.

#Necessary functions:

zscore<-function(y1) {
  n1 <- mean(y1)
  n2 <- sd(y1)
  zscore <- (y1-n1)/n2
  zscore
}

###Harmonizer will also be necessary, include here or save fancy stuff for R markdown?

pull_types=TRUE
append_original=TRUE
check_doubles=TRUE
repair_doubles=TRUE

reference=read.csv("AML_base_2019.csv",header=TRUE)

#We can start off by reading from a list of records that we want to query...

sites=read.csv("records/APDrecs.csv",header=TRUE)
msites=read.csv("records/mAPDrecs.csv",header=TRUE)
record_details=rbind(sites,msites)

rec_names=record_details[record_details$TYPE=="APD Chron",4] #List of marine and terrestrial records from which we're pulling pollen results.

rec_names=c("DANGBO","TILLA","KW31","NIGERDC2")

#Question is how to apply harmonizer (concatenate a list, or have a for loop run over it?).

harmonizer_results=lapply(1:length(rec_names),function(x){
  matrix()
})

for(i in 1:length(rec_names)){
  
  harmonizer(rec_names=paste0(rec_names[i]),taxa_file=TRUE,Tilia_format = FALSE)
  
  x=read.csv("harmonizeR_output.csv",header=TRUE)
  
  harmonizer_results[[i]]=x[,1:15] #Only need to keep the first 25 columns, probably less...

  
  mismatches=harmonizer_results[[i]][is.na(harmonizer_results[[i]][,6]),4]
  row_counter=c(1:nrow(harmonizer_results[[i]]))
  mismatch_rows=row_counter[is.na(harmonizer_results[[i]][,6])]
  print(mismatches)
  if(length(mismatches)>0){
    for(j in 1:length(mismatches)){
      if(mismatches[j]=="Sample age"){
        harmonizer_results[[i]][mismatch_rows[j],6]=paste0('CHRON_',j)
        print("repairing Sample age variables")
      } else { ###This is where we're running into trouble, need to double back and write in the suggested name
        ###Part of the problem is that the listings for some Original Taxa include special chars, so it returns NULL...
        extended_suggestions=harmonizer_results[[i]][mismatch_rows[j],6:ncol(harmonizer_results[[i]])]
        
        col_counts=c(6:ncol(harmonizer_results[[i]]))
        
        select=col_counts[is.na(extended_suggestions)==FALSE]
        
        harmonizer_results[[i]][mismatch_rows[j],6]=harmonizer_results[[i]][mismatch_rows[j],select]
        
      }
    }
  }
  
  if(pull_types==TRUE){
    P_HABIT=vector(mode="character",length=nrow(harmonizer_results[[i]]))
    FAMILY=vector(mode="character",length=nrow(harmonizer_results[[i]]))
    for(j in 1:length(P_HABIT)){
      x=grep(harmonizer_results[[i]][j,6],reference$PROPOSED.NAMES,fixed=TRUE)
      P_HABIT[j]=reference$POLLEN.HABIT[x[1]]
      FAMILY[j]=reference$FAMILIES[x[1]]
    }
    harmonizer_results[[i]]=cbind(harmonizer_results[[i]],P_HABIT,FAMILY)
  }
  
}

for(i in 1:length(rec_names)){
  if(append_original==TRUE){
    orig_data=read.csv(paste0('data/',rec_names[i],'_data.csv'),header=TRUE,row.names="X")
    x=t(orig_data)
    harmonizer_results[[i]]=cbind(harmonizer_results[[i]],x)
  }
  
  if(check_doubles==TRUE){
    unique_names=unique(harmonizer_results[[i]][,6])
    if(length(unique_names)<nrow(harmonizer_results[[i]])){
      print(paste0(rec_names[i]," has doubles!"))
      if(repair_doubles==TRUE){
        y=table(harmonizer_results[[i]][,6])
        z=y[y>1]
        dubnames=names(z)
        for(k in 1:length(dubnames)){
          d=grep(dubnames[k],harmonizer_results[[i]][,6])
          
          ncol_all=ncol(harmonizer_results[[i]])
          ncol_details=ncol_all-ncol(x)
          
          pull=x[d,]
          rev_data=apply(pull,2,sum)
          rev_data=t(as.data.frame(rev_data))
          rev_details=harmonizer_results[[i]][d[1],1:ncol_details]
          rev_entry=cbind(rev_details,rev_data)
          
          harmonizer_results[[i]]=rbind(harmonizer_results[[i]],rev_entry)
          
          harmonizer_results[[i]]=harmonizer_results[[i]][-c(d),]
          
          
        }
      }
    }
  }
}

###Now that all of this data is harmonized and doubles are removed, let's try organizing the results and plotting them.

plot_results=lapply(1:length(rec_names),function(x){
  lapply(1,function(y){
    array(dim=6)
    })
  })

plot_results=as.array(plot_results)
names(plot_results)=rec_names

#Writing all results into an array with separate matrices for taxa and data for each record. Naming conventions within the array aren't working, can't access TAXA/DATA with "$".

for(i in 1:length(rec_names)){
  TAXA=harmonizer_results[[i]][,c(2,17,6,16)]
  plot_results[[i]][[1]]=TAXA
  names(plot_results[[i]][1])="TAXA"
  DATA=harmonizer_results[[i]][,c(18:ncol(harmonizer_results[[i]]))]
  plot_results[[i]][[2]]=DATA
  names(plot_results[[i]][2])="DATA"
}

#Now we have an array with just the taxonomic info and the data in it. Now, lets standardize the processing to get the samples ready to plot.
  #First job is to isolate all pollen from spores, unknowns, inderminables, and chronological designations.

for(i in 1:length(rec_names)){
  row_tracker=c(1:nrow(plot_results[[i]][[1]]))
  chron_cut=row_tracker[is.na(plot_results[[i]][[1]][,4])]
  if(length(chron_cut)<1){
    fossil_taxa=plot_results[[i]][[1]]
    fossil_data=plot_results[[i]][[2]]
    
    plot_results[[i]][[1]]=fossil_taxa[order(fossil_taxa$P_HABIT),]
    plot_results[[i]][[2]]=fossil_data[order(fossil_taxa$P_HABIT),]
    
  } else {
    fossil_taxa=plot_results[[i]][[1]][-c(chron_cut),]
    fossil_data=plot_results[[i]][[2]][-c(chron_cut),]
    CHRON=plot_results[[i]][[2]][chron_cut,]
    
    plot_results[[i]][[1]]=fossil_taxa[order(fossil_taxa$P_HABIT),]
    plot_results[[i]][[2]]=fossil_data[order(fossil_taxa$P_HABIT),]
    plot_results[[i]][[3]]=CHRON
  }
  
  
  #The above separates the chronology, leaving only the fossil data, below we will sort it by pollen type (arboreal, herbaceous, etc.)
  
  #plot_results[[i]][[1]]=fossil_taxa[sort(fossil_taxa$P_HABIT),]
  #plot_results[[i]][[2]]=fossil_data[sort(fossil_taxa$P_HABIT),]
  #plot_results[[i]][[3]]=CHRON
  
  #With this in hand, we can organize results by pollen type and derive a pollen sum.
  
  all_pollen=fossil_data[fossil_taxa[,4]!="Sp" & fossil_taxa[,4]!="X" & fossil_taxa[,4]!="Other" & fossil_taxa[,4]!= "Ss" & fossil_taxa[,4]!="St" & fossil_taxa[,4]!= "Sb" & fossil_taxa[,4]!="S",]
  row.names(all_pollen)=fossil_taxa[fossil_taxa[,4]!="Sp" & fossil_taxa[,4]!="X" & fossil_taxa[,4]!="Other" & fossil_taxa[,4]!= "Ss" & fossil_taxa[,4]!="St" & fossil_taxa[,4]!= "Sb" & fossil_taxa[,4]!="S",3]
  
  herbaceous=fossil_data[fossil_taxa[,4]=="N" | fossil_taxa[,4]=="NL" | fossil_taxa[,4]=="Nq" | fossil_taxa[,4]=="NG",]
  row.names(herbaceous)=fossil_taxa[fossil_taxa[,4]=="N" | fossil_taxa[,4]=="NL" | fossil_taxa[,4]=="Nq" | fossil_taxa[,4]=="NG",3]
  
  arb_pollen=fossil_data[fossil_taxa[,4]=="A" | fossil_taxa[,4]=="AL" | fossil_taxa[,4]=="L",] #Including all Lianas
  row.names(arb_pollen)=fossil_taxa[fossil_taxa[,4]=="A" | fossil_taxa[,4]=="AL" | fossil_taxa[,4]=="L",3]
  
  plm_pollen=fossil_data[fossil_taxa[,4]=="PI" | fossil_taxa[,4]=="PA" | fossil_taxa[,4]=="PL",]
  row.names(plm_pollen)=fossil_taxa[fossil_taxa[,4]=="PI" | fossil_taxa[,4]=="PA" | fossil_taxa[,4]=="PL",3]
  
  ind_pollen=fossil_data[fossil_taxa[,4]=="I",] #Indeteminate growth form palms are with the palms, so this isn't perfectly consistent.
  row.names(ind_pollen)=fossil_taxa[fossil_taxa[,4]=="I",3]
  
  spores=fossil_data[fossil_taxa[,4]=="Sp" | fossil_taxa[,4]=="S" | fossil_taxa[,4]=="Ss" | fossil_taxa[,4]=="St" | fossil_taxa[,4]=="Sb" |fossil_taxa[,4]=="Stp",]
  row.names(spores)=fossil_taxa[fossil_taxa[,4]=="Sp" | fossil_taxa[,4]=="S" | fossil_taxa[,4]=="Ss" | fossil_taxa[,4]=="St" | fossil_taxa[,4]=="Sb" |fossil_taxa[,4]=="Stp",3]
  
  #NEED TO PULL AND EXCLUDE INDETERMINATE?
  
  plot_results[[i]][[4]]=plot_results[[i]][[1]][plot_results[[i]][[1]][,4]!="Sp" & plot_results[[i]][[1]][,4]!="X" & plot_results[[i]][[1]][,4]!="Other" & plot_results[[i]][[1]][,4]!= "Ss" & plot_results[[i]][[1]][,4]!="St" & plot_results[[i]][[1]][,4]!= "Sb" & plot_results[[i]][[1]][,4]!="S",]
  
  #This is where we might insert some code to exclude select arboreal taxa (Rhizophora, Syzygium, etc.).
  
  arb_sum=apply(arb_pollen,2,sum)
  #arb_sum[arb_sum==0]=10 #Temporary fix to make sure we don't get Inf of NaN's in the cases where there's no arboreal pollen (Tilla) ###This fix works, but yuck.
  hrb_sum=apply(herbaceous,2,sum)
  plm_sum=apply(plm_pollen,2,sum)
  ind_sum=apply(ind_pollen,2,sum)
  all_sum=apply(all_pollen,2,sum)
  
  ptype_summary=rbind(arb_sum,hrb_sum,plm_sum,ind_sum)
  ptype_summary=t(ptype_summary) #Need to transpose table to get the %s
  ptype_pct=(ptype_summary/all_sum)*100 #This works so far...
  ptype_pct=t(ptype_pct)
  
  depth=as.numeric(colnames(ptype_pct))*-1
  ptype_pct=ptype_pct[,sort(rev(depth))]
  barplot(ptype_pct[,ncol(ptype_pct):1],horiz=TRUE,beside=FALSE,xlim=c(0,100))
  title(main=paste0(rec_names[i]," Results Summary"))
  
  #Percent plotting specific taxa
  
  all_pct=matrix(nrow=nrow(all_pollen),ncol=ncol(fossil_data))
  all_pct=as.data.frame(all_pct)
  row.names(all_pct)=row.names(all_pollen)
  colnames(all_pct)=colnames(fossil_data)
  
  arb_pct=matrix(nrow=nrow(arb_pollen),ncol=ncol(fossil_data))
  arb_pct=as.data.frame(arb_pct)
  row.names(arb_pct)=row.names(arb_pollen)
  colnames(arb_pct)=colnames(fossil_data)
  
  hrb_pct=matrix(nrow=nrow(herbaceous),ncol=ncol(fossil_data))
  hrb_pct=as.data.frame(hrb_pct)
  row.names(hrb_pct)=row.names(herbaceous)
  colnames(hrb_pct)=colnames(fossil_data)
  
  plm_pct=matrix(nrow=nrow(plm_pollen),ncol=ncol(fossil_data))
  plm_pct=as.data.frame(plm_pct)
  row.names(plm_pct)=row.names(plm_pollen)
  colnames(plm_pct)=colnames(fossil_data)
  
  sp_pct=matrix(nrow=nrow(spores),ncol=ncol(fossil_data))
  sp_pct=as.data.frame(sp_pct)
  row.names(sp_pct)=row.names(spores)
  colnames(sp_pct)=colnames(fossil_data)
  
  for(j in 1:nrow(fossil_data)){
    x1=(all_pollen[j,]/arb_sum)*100
    all_pct[j,]=x1
    
    x2=(arb_pollen[j,]/arb_sum)*100
    arb_pct[j,]=x2
    
    x3=(herbaceous[j,]/arb_sum)*100
    hrb_pct[j,]=x3
    
    x4=(plm_pollen[j,]/arb_sum)*100
    plm_pct[j,]=x4
    
    x5=(spores[j,]/arb_sum)*100
    sp_pct[j,]=x5
  }
  
  all_sum[all_sum==0]=NA
  AP_NAP=arb_sum/(all_sum)
  NAP=hrb_sum/(all_sum)
  
  indices=cbind(AP_NAP,NAP)
  
  plot_results[[i]][[6]]=indices
  
  plot_results[[i]][[5]]=all_pct
  
}

#We are fucked here because R wants to work with columns, which you already knew you fucking twit.

#Take a list of records from a similar area and plot their results together?


plot(0,0,pch=NA,xlim=c(-2,12),ylim=c(-30000,0),main="Grass Z-Scores, Marine Records",xlab=NA,axes=FALSE,ylab="yr BP")
axis(1,at=seq(-2,12,2),labels = c(-2,0,2,4,-2,0,2,4))
axis(2,at=seq(-30000,0,1000))
marine_group=c(msites$CODE[msites$LAT>0])
for(i in 1:length(marine_group)){
  x=grep(marine_group[i],rec_names)
  p_pct=t(plot_results[[x]][[5]])
  p_pct=as.data.frame(p_pct)
  p_chr=plot_results[[x]][[3]][1,]*-1
  p_chr[p_chr==0]=NA
  z=grep("Poaceae undiff.",colnames(p_pct))
  if(length(z)>0){
    lines(zscore(p_pct$`Poaceae undiff.`),p_chr,pch=21,bg=i,type="o",lty=3)
  }
}

marine_group=c(msites$CODE[msites$LAT<0])
for(i in 1:length(marine_group)){
  x=grep(marine_group[i],rec_names)
  p_pct=t(plot_results[[x]][[5]])
  p_pct=as.data.frame(p_pct)
  p_chr=plot_results[[x]][[3]][1,]*-1
  p_chr[p_chr==0]=NA
  z=grep("Poaceae undiff.",colnames(p_pct))
  if(length(z)>0){
    lines(zscore(p_pct$`Poaceae undiff.`)+8,p_chr,pch=21,bg=i,type="o",lty=3)
  }
}

#Terrestrial Records...

plot(0,0,pch=NA,xlim=c(-2,12),ylim=c(-30000,0),main="Grass Z-Scores, Terrestrial Records",xlab=NA,axes=FALSE,ylab="yr BP")
axis(1,at=seq(-2,12,2),labels = c(-2,0,2,4,-2,0,2,4))
axis(2,at=seq(-30000,0,1000))

CB_group=c(sites$CODE[sites$TYPE=="APD Chron" & sites$LONG<14])
for(i in 1:length(CB_group)){
  x=grep(CB_group[i],rec_names)
  p_pct=t(plot_results[[x]][[5]])
  p_pct=as.data.frame(p_pct)
  p_chr=plot_results[[x]][[3]][1,]*-1
  #min_age=min(p_chr,na.rm=TRUE)
  p_chr[p_chr==0]=NA
  z=grep("Poaceae undiff.",colnames(p_pct))
  if(length(z)>0){ #Cut out  & min_age<(-10000)
    print("plotting")
    lines(zscore(p_pct$`Poaceae undiff.`),p_chr,pch=21,bg=i,type="o",lty=3)
  }
}

CB_group=c(sites$CODE[sites$TYPE=="APD Chron" & sites$LONG>14])
for(i in 1:length(CB_group)){
  x=grep(CB_group[i],rec_names)
  p_pct=t(plot_results[[x]][[5]])
  p_pct=as.data.frame(p_pct)
  p_chr=plot_results[[x]][[3]][1,]*-1
  #min_age=min(p_chr,na.rm=TRUE)
  p_chr[p_chr==0]=NA
  z=grep("Poaceae undiff.",colnames(p_pct))
  if(length(z)>0){ #Cut out  & min_age<(-10000)
    print("plotting")
    lines(zscore(p_pct$`Poaceae undiff.`)+8,p_chr,pch=21,bg=i,type="o",lty=3)
  }
}

###Let's try to plot some West African Data
WAF_group=c("DANGBO","TILLA","KW31","NIGERDC2")
plot(0,0,pch=NA,xlim=c(0,4),ylim=c(-30000,0),main="AP_NAP Ratio, West African Records",xlab=NA,axes=FALSE,ylab="yr BP")
axis(1,at=seq(0,3,1),labels = c(WAF_group))
axis(2,at=seq(-30000,0,1000))


for(i in 1:length(WAF_group)){
  x=grep(WAF_group[i],rec_names)
  p_pct=t(plot_results[[x]][[5]])
  p_pct=as.data.frame(p_pct)
  p_chr=plot_results[[x]][[3]][1,]*-1
  #min_age=min(p_chr,na.rm=TRUE)
  p_chr[p_chr==0]=NA
  z=grep("Poaceae undiff.",colnames(p_pct))
  #if(length(z)>0){ #Cut out  & min_age<(-10000)
  #  print("plotting")
  #  z_grass=zscore(p_pct$`Poaceae undiff.`)
  #  pct_grass=p_pct$`Poaceae undiff.`
  #  lines(pct_grass/(max(pct_grass,na.rm=TRUE))+(i-1),p_chr,pch=21,bg=i,type="o",lty=3)
  #}
  AP_NAP=plot_results[[x]][[6]]
  lines(AP_NAP[,1]/(max(AP_NAP[,1],na.rm=TRUE))+(i-1),p_chr,pch=21,bg=i,type="o",lty=3)
  
}












