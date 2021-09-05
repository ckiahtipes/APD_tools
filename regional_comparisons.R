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
append_original=FALSE
check_doubles=TRUE
replace_doubles=TRUE

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
  
  if(pull_types==TRUE){
    P_HABIT=vector(mode="character",length=nrow(harmonizer_results[[i]]))
    for(j in 1:length(P_HABIT)){
      x=grep(harmonizer_results[[i]][j,6],reference$PROPOSED.NAMES,fixed=TRUE)
      P_HABIT[j]=reference$POLLEN.HABIT[x[1]]
    }
    harmonizer_results[[i]]=cbind(harmonizer_results[[i]],P_HABIT)
  }
  
  mismatches=harmonizer_results[[i]][is.na(harmonizer_results[[i]][,6]),4]
  row_counter=c(1:nrow(harmonizer_results[[i]]))
  mismatch_rows=row_counter[is.na(harmonizer_results[[i]][,6])]
  print(mismatches)
  for(j in 1:length(mismatches)){
    if(mismatches[j]=="Sample age"){
      harmonizer_results[[i]][mismatch_rows[j],6]=paste0('CHRON_',j)
      print("repairing Sample age variables")
    } else { ###This is where we're running into trouble, need to double back and write in the suggested name
             ###Part of the problem is that the listings for some Original Taxa include special chars, so it returns NULL...
      extended_suggestions=harmonizer_results[[i]][mismatch_rows[j],is.na(harmonizer_results[[i]][mismatch_rows[j],6:15])==FALSE]
      #harmonizer_results[[i]][mismatch_rows[j],6]=harmonizer_results
    }
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
    }
  }

  
}
