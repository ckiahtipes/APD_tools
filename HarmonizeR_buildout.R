###Buildout for Harmonizer function.

#The function is written in base R and has no dependencies.

#Requirements:
  #Character vector listing names of pollen records, must match file names for pollen data and taxa lists (i.e. FC000 = FC000_data.csv, FC000_taxa.csv). <------ Trying to imitate APD format here.
  #Master reference of accepted pollen types.




#Harmonizer function

harmonizer=function(rec_names,master="AML_2015"){ #Function needs names of pollen records and a master list. ###consider repairing "rec_names" to something more sensible.
  
  ###UNIVERSAL OBJECTS: MASTER POLLEN/MORPHOTYPE LIST
  
  AML_full=read.csv("AML_base.csv",header=TRUE) ###CAK_edits #master file: need systematic naming convention and use across document.
  original_names=unique(AML_full$ORIGINAL.NAMES) ###CAK_edits #master file: need systematic naming convention and use across document.
  revised_names=unique(AML_full$PROPOSED.NAMES) ###CAK_edits #master file: need systematic naming convention and use across document.
  
  ###COUNT TAXA AND PREPARE TABLES
  
  name_length=vector(mode="numeric",length=1) #vector to track number of short names per record
  fullname_length=vector(mode="numeric",length=1) #vector to track number of long names per record ###CAK_comment this validates that the _data and _taxa files are matched, keep it.
  
  for(i in 1:length(rec_names)){ #Read data and get taxa names from columns.
    pol_data=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_data.csv"),header=TRUE) ###Pulling "X" which requires fix below.. ###CAK_EDITS #read file: fix file reading, note in README.md
    pol_names=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_taxa.csv"),header=TRUE) 
    fullnames=pol_names$FullNames
    cn=colnames(pol_data)
    cn=cn[-1] ###Here's the sloppy fix to the "X" problem. ###CAK_EDITS #read file: remove when above problem is fixed.
    name_length[i]=length(cn)
    fullname_length[i]=length(fullnames)
    if(name_length[i]-fullname_length[i]!=0){
      print(paste0("Check names in ",rec_names[i]))
    } else {
      print(paste0("Taxa and Data files match, ",rec_names[i]))
    }
  } #Close for loop to track names per record.
  
  ###USE COUNTS TO CREATE MATRIX FOR PLACING DATA
    #This might be improved by imagining two discrete steps:
      #1) read tables, compare taxa with AML's list, and leave suggestions
      #2) read tables, collect user validated data, and write new tables.
  
  #Use name_length to build out a matrix.
  taxa_just=matrix(ncol=6,nrow=sum(name_length)) #Lumping everything together, right?
  taxa_just=as.data.frame(taxa_just)
  colnames(taxa_just)=c("REC_no","CODE_APD","ORIG_TAXON","FULL_TAXON","SUGG_TAXON","POLL_TYPE") ###CAK_comment: we're committing to the final table form early on - should this go in phases?
  ###CAK_EDITS #initial table: should include a "comment/suggestion" column where harmonizer leaves a note about what the problem may be.
  ###CAK_EDITS #initial table: we're not using SUGG_TAXON or POLL_TYPE in this table at all.
  
  for(i in 1:length(rec_names)){ #For loop reads each record and writes taxa names into it. 
    print(rec_names[i])
    pol_data=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_data.csv"),header=TRUE)
    taxa_full=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_taxa.csv"),header=TRUE)
    cn=colnames(pol_data)
    cn=cn[-1] #removing "X" ###CAK_EDITS #read file: remove when above problem is fixed.
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
  } #End for loop pulling names and writing into combined table.
  
  ###MATCHING LISTED TAXA WITH MASTER LIST: SETTING UP MATCHING MATRICES
  
  n_matches=vector("numeric",length=length(taxa_just$ORIG_TAXON)) #tracking how many matches there are for each taxon, will pull the max value to size the matrix into which results are written.
  
  for(i in 1:length(taxa_just$ORIG_TAXON)){ #Looking for matches
    ###CAK EDITS #Cleanup: looks like the code below was written to deal with "X" entries - check that this is resolved and delete.
    #if(taxa_just$ORIG_TAXON[i]=="X"){
    #  y=NA
    #  n_matches[i]=y
    #} else {
    y=grep(taxa_just$FULL_TAXON[i],revised_names) #Use grep to search taxon names in AML's list
    z=grep(taxa_just$FULL_TAXON[i],original_names) #Use grep to search taxon names in AML's list
    
    if(length(y)>length(z)){ #I want to take the larger of the two lists of matches...
      n_matches[i]=length(y) #...and write it into the n_matches vector.
    } else {
      n_matches[i]=length(z)
    }
    #print(y)
    #}
  } #end of loop searching by taxon
  
  ###SEARCHING TAXON NAMES AND WRITING MATCH MATRIX
  ###CAK_EDITS #Reading and matching efficiency: there's several improvements in the original script that haven't been collapsed...
  ###This is the first sweep, need to be specific about the order
    ###Alternatively, I could collapse the different searches into this one loop...
  
  match_matrix=matrix(nrow=length(n_matches),ncol=max(n_matches,na.rm=TRUE))
  match_matrix=as.data.frame(match_matrix)
  
  for(i in 1:length(taxa_just$ORIG_TAXON)){ #For loop searching for original taxa names, and matching them to revised names.
    if(taxa_just$ORIG_TAXON[i]=="X"){ #God damn it! Dealing with this "X" bullshit again.
    } else {
      y=grep(taxa_just$FULL_TAXON[i],revised_names)
      if(length(y)==0){ #If there's no matches, y is NA and has no length.
        match_matrix[i,]=rep(NA,ncol(match_matrix)) #Write in NAs....
      } else { #If there are matches y will be >0...
        for(j in 1:length(y)){ #For loop to handle multiple matches.
          match_matrix[i,j]=revised_names[y[j]] #And we want to write each of those matches to the matrix.
        } #End of for loop handling mutliple matches
      }
    }
  } #End of loop matching orignal and revised names
  
  ###BRIDGING CODE AND TESTING FOR ERRORS
    ###CAK_EDITS #Code cleanup: this code was used to help me figure out past errors, can it be incorporated into feedback or other code chunks?
  
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
  
    ###This is the end of a big section of code that needs to be tested & collapsed so that the function is smaller.
    ###Most important job is to use "sweeps" and settings in the function to determine how this is done.
  ###BRIDGING CODE AND TESTING FOR ERRORS
  
  write.table(taxon_checklist,file="Ngotto_taxon_checklist.csv",sep=",",col.names=TRUE) #Needs flexible file-naming
  
} #Closing out function.

#Test data and function runs.

rec_names=c("FC000","FC300","FC400") #consider repairing "rec_names" to something more sensible.