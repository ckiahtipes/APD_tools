#Revising Ngotto Data for APD upload and publication in Paleoecology of Africa.

#Analysis and plotting for Palaeoecology of Africa Volume

#Use core codes to grab data.

rec_names=c("FC000","FC300","FC400")


#Figure out how many taxa there are per record.
name_length=vector(mode="numeric",length=1)
fullname_length=vector(mode="numeric",length=1)

for(i in 1:length(rec_names)){
  pol_data=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_data.csv"),header=TRUE) ###Pulling "X" which requires fix below..
  pol_names=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_taxa.csv"),header=TRUE)
  fullnames=pol_names$FullNames
  cn=colnames(pol_data)
  cn=cn[-1] ###Here's the sloppy fix to the "X" problem.
  name_length[i]=length(cn)
  fullname_length[i]=length(fullnames)
}

#Use name_length to build out a matrix.
taxa_just=matrix(ncol=6,nrow=sum(name_length))
taxa_just=as.data.frame(taxa_just)
colnames(taxa_just)=c("REC_no","CODE_APD","ORIG_TAXON","FULL_TAXON","SUGG_TAXON","POLL_TYPE")

for(i in 1:length(rec_names)){
  print(rec_names[i])
  pol_data=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_data.csv"),header=TRUE)
  taxa_full=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_taxa.csv"),header=TRUE)
  cn=colnames(pol_data)
  cn=cn[-1] #removing "X"
  fullnames=taxa_full$FullNames
  
  if(i==1){ #Adding numbers to track records
    begin=1
    end=name_length[i]
    taxa_just[begin:end,1]=rep(i,name_length[i])
  } else {
    begin=sum(name_length[1:(i-1)])+1
    end=sum(name_length[1:i])
    taxa_just[begin:end,1]=rep(i,name_length[i])
  }
  
  if(i==1){ #Adding record codes to list
    begin=1
    end=name_length[i]
    taxa_just[begin:end,2]=rep(rec_names[i],name_length[i])
  } else {
    begin=sum(name_length[1:(i-1)])+1
    end=sum(name_length[1:i])
    taxa_just[begin:end,2]=rep(rec_names[i],name_length[i])
  }
  
  if(i==1){ #Adding shortened names to matrix
    begin=1
    end=name_length[i]
    taxa_just[begin:end,3]=c(cn)
  } else {
    begin=sum(name_length[1:(i-1)])+1
    end=sum(name_length[1:i])
    taxa_just[begin:end,3]=c(cn)
  }
  
  if(i==1){ #Adding full names to matrix
    begin=1
    end=name_length[i]
    taxa_just[begin:end,4]=c(fullnames)
  } else {
    begin=sum(name_length[1:(i-1)])+1
    end=sum(name_length[1:i])
    taxa_just[begin:end,4]=c(fullnames)
  }
}

#Okay, so the above gives me a giant spreadsheet with every taxon from every core.

#Now, the job is to take this list and compare it with AML's accepted taxonomy to pull the correct pollen type.

AML_full=read.csv("AML_base.csv",header=TRUE) ###THIS MIGHT DESERVE ITS OWN FOLDER

#This is a section of AML's list from 2007 for the APD. 5,0000 unique entries in original names.

#Revised names are 2,402. 

original_names=unique(AML_full$ORIGINAL.NAMES)
revised_names=unique(AML_full$PROPOSED.NAMES)

#Search by taxon and return matches. This yields a list of the length of the number of matches in AML's list. 

n_matches=vector("numeric",length=length(taxa_just$ORIG_TAXON)) #tracking how many matches there are for each taxon, pulling the max value to size the matrix.

for(i in 1:length(taxa_just$ORIG_TAXON)){ #Looking for matches
  #if(taxa_just$ORIG_TAXON[i]=="X"){
  #  y=NA
  #  n_matches[i]=y
  #} else {
  y=grep(taxa_just$FULL_TAXON[i],revised_names)
  n_matches[i]=length(y)
  #print(y)
  #}
} #end of loop searching by taxon

#Now we make a matrix to catch all of the different suggestions from AML's list and write the matches into each column.

match_matrix=matrix(nrow=length(n_matches),ncol=max(n_matches,na.rm=TRUE))
match_matrix=as.data.frame(match_matrix)

for(i in 1:length(taxa_just$ORIG_TAXON)){
  if(taxa_just$ORIG_TAXON[i]=="X"){
  } else {
    y=grep(taxa_just$FULL_TAXON[i],revised_names)
    if(length(y)==0){
      match_matrix[i,]=rep(NA,ncol(match_matrix))
    } else {
      for(j in 1:length(y)){
        match_matrix[i,j]=revised_names[y[j]]
      }
    }
  }
}

#This works. So we attach the original taxon names in the first column position.

match_matrix=cbind(taxa_just$FULL_TAXON,match_matrix)

#There's a number of taxa which aren't coming up in the search (because we're going directly to the revised names and old names may be missing).
#We need to find out if the missing values have matches in the "original names" column of AML's list and use those to resolve suggested names.

length_pull=c(1:length(match_matrix$V1)) #Needed a numeric list of the positions.
missing_positions=length_pull[is.na(match_matrix$V1)] #Using NAs to create list of positions.
missing_length=vector(mode="numeric",length=length(missing_positions)) #Make a vector in which to write the number of matches.
rev_missing_length=vector(mode="numeric",length=length(missing_positions)) #Make a vector to write the number of matches in the revised names column.

#Running through the list, searching for matches, and writing the number of matches to a vector.

for(i in 1:length(missing_positions)){
  
  y=grep(taxa_just$ORIG_TAXON[missing_positions[i]],original_names)
  missing_length[i]=length(y)
  
  #x=revised_names[y]
  #print(y)
  #print(x)
  
}

#General observation is that some of these have quite a few matches (up to 32 for "Composit").
#Need to take lists of matches, pull the revised columns, and then see how many unique values there are.

#So, we make another matrix for these values and fill it with the suggestions.

missing_matrix=matrix(nrow=length(missing_positions),ncol=max(missing_length,na.rm=TRUE)) #Tracking positions of the matches
missing_matrix=as.data.frame(missing_matrix)
miss_names_matrix=matrix(nrow=length(missing_positions),ncol=max(missing_length,na.rm=TRUE)) #Tracking names of matches
miss_names_matrix=as.data.frame(miss_names_matrix)
sugg_miss_names=matrix(nrow=length(missing_positions),ncol=max(missing_length,na.rm=TRUE))
sugg_miss_names=as.data.frame(sugg_miss_names)

#Now, we search for the missing names in the original column (code names!?) and write them in the matrix.

for(i in 1:length(missing_positions)){
  if(taxa_just$ORIG_TAXON[missing_positions[i]]=="X"){
    y=NA
    missing_matrix[i,]=rep(y,ncol(missing_matrix))
  } else {
    y=grep(taxa_just$FULL_TAXON[missing_positions[i]],AML_full$ORIGINAL.NAMES)
    if(length(y)==0){
      missing_matrix[i,]=rep(NA,ncol(missing_matrix))
      miss_names_matrix[i,]=rep(NA,ncol(miss_names_matrix))
      sugg_miss_names[i,]=rep(NA,ncol(sugg_miss_names))
    } else {
      for(j in 1:length(y)){
        missing_matrix[i,j]=y[j]
        miss_names_matrix[i,j]=AML_full$ORIGINAL.NAMES[y[j]]
        sugg_miss_names[i,j]=AML_full$PROPOSED.NAMES[y[j]]
      }
    }
  }
}

#This works and it creates three matrices: one with positions, one with names of the matched taxa, and one with suggested names. 
#Coverage still isn't perfect, still getting plenty of NAs.

replace_length=vector(length=length(missing_positions))

for(i in 1:nrow(sugg_miss_names)){
  names=unique(c(sugg_miss_names[i,]))
  replace_length[i]=length(names)
}

#Okay, now we know how many potential suggested names may exist for the names without direct matches in the revised names column.
#We also know that it will fit in the matrix we've already got going for the matches ###What if it doesn't? 

#Write the suggested missing names in the original NA positions.

for(i in 1:length(missing_positions)){
  y=unique(c(sugg_miss_names[i,]))
  for(j in 1:length(replace_length[i])){
    match_matrix[missing_positions[i],j+1]=y[j]
  }
}

#So far, so good.
#Revised the missing position code above to re-evaluate.

length_rev=c(1:length(match_matrix$V1)) #Needed a numeric list of the positions.
revmiss_positions=length_pull[is.na(match_matrix$V1)] #Using NAs to create list of positions.

#We've gone from 409 NAs to 148, so what's left?

mismatches=match_matrix[revmiss_positions,1]

#What's left is mostly "X"s, "Chron"s, taxa names tagged with numbers (e.g., "Legumino.1"), and misspelled taxa (e.g., "Syzgi", as opposed to "Syzygium").
#At this point it is possible to fix all of these manually, especially after we exclude the types we need to reject ("X","Chron","indet").

write.table(match_matrix,file="matched_taxa_AML-APD.csv",sep=",",col.names=TRUE)

#Noting here that I somehow ignored the full name listings available with every record.
#Searching by the full name brings the unmatched names down to 83 entries.
#I've also cut out the "X" entries while reading the files.

#Updated the list to include all of the marine taxa. Now, we can trace everything back and make corrections in the original files.
#Looking at the original tables, we've got a number of simple formatting errors. 

#My preference is not to spend a lot of time modifying the original data, so there aren't multiple versions of the original APD files running around.


repair_tracker=revmiss_positions

#Can we scan the shortened names and fill in some of these? It seems likely.

problem_entries=taxa_just[repair_tracker,]

#write.table(problem_entries,file="problem_entries_AML-APD.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Refining list using code names (they search a little easier than some full names)

for(i in 1:length(repair_tracker)){
  y=grep(taxa_just$ORIG_TAXON[repair_tracker[i]],revised_names)
  if(length(y)==0){
    y=grep(taxa_just$ORIG_TAXON[repair_tracker[i]],original_names)
    if(length(y)>0){
      for(j in 1:length(y)){
        z=grep(original_names[y[j]],AML_full$ORIGINAL.NAMES)
        match_matrix[repair_tracker[i],j+1]=AML_full$PROPOSED.NAMES[z[1]]
      }
    } else {}
  } else {
    for(j in 1:length(y)){
      match_matrix[repair_tracker[i],j+1]=revised_names[y[j]]
    }
  }
}

length_rev=c(1:length(match_matrix$V1)) #Needed a numeric list of the positions.
revmiss_positions=length_pull[is.na(match_matrix$V1)] #Using NAs to create list of positions.


match_colnames=vector(mode="character",length=1)

for(i in 1:(ncol(match_matrix))-1){
  x=paste0("sugg_taxon_",i)
  match_colnames[i]=x
}

match_colnames=c("Original Taxon",match_colnames)

colnames(match_matrix)=match_colnames

taxon_checklist=cbind(taxa_just,match_matrix)

write.table(taxon_checklist,file="Ngotto_taxon_checklist.csv",sep=",",col.names=TRUE) #Needs flexible file-naming

###Took the taxon_checklist written above and validated all of the taxa:
  #1) Verify presence in APD listing
  #2) Check taxa with the African Plants Database

#Now the goal is to make a singular taxon list for every record (facilitates comparison via PCA, etc.)
#We still need to make updated lists for each core, yes?

####THIS COULD BE A SEPARATE THING.

#validated_taxa=read.csv("pollen/Ngotto/Ngotto_alltaxa.csv",header=TRUE,sep=",")
#
##remove suggestions
#
#validated_taxa=validated_taxa[,1:10]
#
##Lets update the individual records first
#
#for(i in 1:length(rec_names)){
#  
#  orig_results=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_data.csv"),header=TRUE)
#  orig_taxa=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_taxa.csv"),header=TRUE)
#  sugg_taxa=validated_taxa[validated_taxa$CODE_APD==rec_names[i],]
#  new_taxa=cbind(orig_taxa[,1:2],sugg_taxa[,c(4,6)]) #Combined list showing revisions and original taxa
#  #new_data=rbind(c("depth_cm",new_results[,3]),orig_results) #Combined data with revisions and original taxa ###DEAD CODE!
#  
#  poll_habit=vector(mode="character",length=1)
#  
#  for(j in 1:nrow(new_taxa)){ #Grabbing pollen types from AML's list.
#    x=grep(paste0("^",new_taxa[j,4],"$"),AML_full$PROPOSED.NAMES,ignore.case=TRUE)
#    y=AML_full$POLLEN.HABIT[x[1]]
#    poll_habit[j]=y
#  }
#  
#  validated_taxa[validated_taxa$CODE_APD==rec_names[i],7]=orig_taxa$Family #writing family names into the spreadsheet
#  validated_taxa[validated_taxa$CODE_APD==rec_names[i],8]=poll_habit #writing pollen habit into the spreadsheet
#  
#  new_taxa$Pol_type=poll_habit
#  write.csv(new_taxa,file=paste0("pollen/Ngotto",rec_names[i],"taxa_rev.csv"),row.names=FALSE)
#  
#}
#
#write.table(validated_taxa,file="Ngotto_taxon_details.csv",sep=",",col.names=TRUE)


















