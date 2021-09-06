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

sites=read.table("records/APDrecs.csv",header=TRUE,sep=",")
msites=read.table("records/mAPDrecs.csv",header=TRUE,sep=",")
rec_names=sites$CODE
mrec_names=msites$CODE

rec_names=c(rec_names,mrec_names) #List of marine and terrestrial records from which we're pulling pollen results.

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
    array(dim=2)
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
  
  fossil_taxa=plot_results[[i]][[1]][-c(chron_cut),]
  fossil_data=plot_results[[i]][[2]][-c(chron_cut),]
  CHRON=plot_results[[i]][[2]][chron_cut,]
  
  #The above separates the chronology, leaving only the fossil data, below we will sort it by pollen type (arboreal, herbaceous, etc.)
  
  plot_results[[i]][[1]]=fossil_taxa[sort(fossil_taxa$P_HABIT),]
  plot_results[[i]][[2]]=fossil_data[sort(fossil_taxa$P_HABIT),]
  plot_results[[i]][[3]]=CHRON
  
  #With this in hand, we can calculate percent of the pollen sum and get some summary pollen results.
  all_pollen=fossil_data[fossil_taxa[,4]!="Sp" | fossil_taxa[,4]!="X",]
  row.names(all_pollen)=fossil_taxa[fossil_taxa[,4]!="Sp" | fossil_taxa[,4]!="X",3]
  herbaceous=fossil_data[fossil_taxa[,4]=="N" | fossil_taxa[,4]=="NL" | fossil_taxa[,4]=="NQ",]
  arb_pollen=fossil_data[fossil_taxa[,4]=="A" | fossil_taxa[,4]=="AL",]
  plm_pollen=fossil_data[fossil_taxa[,4]=="PI" | fossil_taxa[,4]=="PA" | fossil_taxa[,4]=="PL",]
  spores=fossil_data[fossil_taxa[,4]=="Sp",]
  
}









