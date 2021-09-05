#Harmonizer function

harmonizer=function(rec_names,master_file="AML_base_2019.csv",taxa_file=FALSE,Tilia_format=TRUE,APD_format=TRUE){ #Function needs names of pollen records and a master list. ###consider repairing "rec_names" to something more sensible.
  path=getwd()
  ###UNIVERSAL OBJECTS: MASTER POLLEN/MORPHOTYPE LIST
  
  master=read.csv(paste0(master_file),header=TRUE) ###CAK_edits #master file: need systematic naming convention and use across document.#Update to read current list from AML
  if(APD_format==TRUE){
    original_names=unique(master$ORIGINAL.NAMES) ###CAK_edits #master file: need systematic naming convention and use across document.
    proposed_names=unique(master$PROPOSED.NAMES) ###CAK_edits #master file: need systematic naming convention and use across document.
  } else {
    proposed_names=unique(master$PROPOSED.NAMES) ###CAK_edits #master file: need systematic naming convention and use across document. #Other master documents NEED to have this title!
  }
  
  ###COUNT TAXA AND PREPARE TABLES
  
  name_length=vector(mode="numeric",length=1) #vector to track number of short names per record
  fullname_length=vector(mode="numeric",length=1) #vector to track number of long names per record 
  
  for(i in 1:length(rec_names)){ #Read data and get taxa names from columns.
    if(Tilia_format==TRUE){
      pol_data=read.csv(paste0(path,'/data/',rec_names[i],".csv"),header=TRUE,row.names="X")
      pol_data=t(pol_data)
      pol_data=as.data.frame(pol_data)
    } else {
      pol_data=read.csv(paste0(path,'/data/',rec_names[i],"_data.csv"),header=TRUE,row.names="X")
    }
    cn=colnames(pol_data)
    name_length[i]=length(cn)
    if(taxa_file==TRUE){
      pol_names=read.csv(paste0(path,'/data/',rec_names[i],"_taxa.csv"),header=TRUE) 
      fullnames=pol_names$FullNames
      fullname_length[i]=length(fullnames)
      if(name_length[i]-fullname_length[i]!=0){
        print(paste0("Check names in ",rec_names[i]))
      } else {
        print(paste0("Taxa and Data files match, ",rec_names[i]))
      }
    }
  } #Close for loop to track names per record.
  
  #USE COUNTS TO CREATE MATRIX FOR PLACING DATA
  
  #Use name_length to build out a matrix.
  taxa_just=matrix(ncol=5,nrow=sum(name_length)) 
  taxa_just=as.data.frame(taxa_just)
  colnames(taxa_just)=c("REC_no","CODE_APD","SHORT_TAXON","FULL_TAXON","COMMENT") 

  
  for(i in 1:length(rec_names)){ #For loop reads each record and writes taxa names into it. 
    print(rec_names[i])
    if(Tilia_format==TRUE){
      pol_data=read.csv(paste0(path,'/data/',rec_names[i],".csv"),header=TRUE,row.names="X")
      pol_data=t(pol_data)
      pol_data=as.data.frame(pol_data)
    } else {
      pol_data=read.csv(paste0(path,'/data/',rec_names[i],"_data.csv"),header=TRUE,row.names="X")
    }
    cn=colnames(pol_data)
    
    if(taxa_file==TRUE){
      taxa_full=read.csv(paste0(path,'/data/',rec_names[i],"_taxa.csv"),header=TRUE)
      fullnames=taxa_full$FullNames
    } else {}
    
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
      taxa_just[begin:end,2]=rep(rec_names[i],name_length[i]) ###Using APD Codes to name records!!!
    } else {
      begin=sum(name_length[1:(i-1)])+1
      end=sum(name_length[1:i])
      taxa_just[begin:end,2]=rep(rec_names[i],name_length[i])
    }
    ###Need to update this to get flexibility in data formats - Tilia vs. Rioja exports...
    if(taxa_file==TRUE){ #If we have a _taxa.csv file, write short names from the CodeNames entries
      if(i==1){ #Adding shortened names to matrix
        begin=1
        end=name_length[i]
        taxa_just[begin:end,3]=c(taxa_full$CodeNames)
      } else {
        begin=sum(name_length[1:(i-1)])+1
        end=sum(name_length[1:i])
        taxa_just[begin:end,3]=c(taxa_full$CodeNames)
      }
    } else { #If there's no _taxa.csv file, then we don't do anything now and derive short names later...
    }
    
    if(taxa_file==TRUE){ #We write the taxa_just matrix differently depending on what's available... ###CAK 7.6.2021 If we have the taxa file we write it from fullnames.
      if(i==1){ #Adding full names to matrix
        begin=1
        end=name_length[i]
        taxa_just[begin:end,4]=c(fullnames)
      } else {
        begin=sum(name_length[1:(i-1)])+1
        end=sum(name_length[1:i])
        taxa_just[begin:end,4]=c(fullnames)
      }
    } else { #IF there's no _taxa.csv file, treat the column names as the long names.
      if(i==1){ #Adding names to matrix from column names
        begin=1
        end=name_length[i]
        taxa_just[begin:end,4]=c(cn)
      } else {
        begin=sum(name_length[1:(i-1)])+1
        end=sum(name_length[1:i])
        taxa_just[begin:end,4]=c(cn)
      }
    }
    
    
  } #End for loop pulling names and writing into combined table.
  
  ###NEED PROCESS TO GET SHORT NAMES BASED ON SPECIAL CHARACTERS ##CAK 7.6.2021
  
  if(taxa_file==FALSE){ #If there's no taxa file, go ahead and make short names with the long ones available
    short_names=sub('\\ .*|\\_.*|\\/.*|\\-.*','',taxa_just$FULL_TAXON)
    taxa_just[,3]=short_names
  } else { #If there is a taxa file, go ahead and clean up the short names available
    taxa_just$SHORT_TAXON=sub('\\ .*|\\_.*|\\/.*|\\-.*','',taxa_just$SHORT_TAXON)
  }

  ###MATCHING LISTED TAXA WITH MASTER LIST: SETTING UP MATCHING MATRICES
  
  n_matches=vector("numeric",length=length(taxa_just$ORIG_TAXON)) #tracking how many matches there are for each taxon, will pull the max value to size the matrix into which results are written.
  
  for(i in 1:nrow(taxa_just)){ #Defining size of matrix by maximum length of matches.
    if(APD_format==TRUE){
      y=grep(taxa_just$SHORT_TAXON[i],proposed_names) #Use grep to search taxon names in AML's list
      z=grep(taxa_just$SHORT_TAXON[i],original_names) #Use grep to search taxon names in AML's list
      
      if(length(y)>length(z)){ #I want to take the larger of the two lists of matches...
        n_matches[i]=length(y) #...and write it into the n_matches vector.
      } else {
        n_matches[i]=length(z)
      }
      
    } else {
      y=grep(taxa_just$SHORT_TAXON[i],proposed_names) #Use grep to search taxon names in master list
      n_matches[i]=length(y)
    }
    
  } #end of loop searching by taxon
  
  ###SEARCHING TAXON NAMES AND WRITING MATCH MATRIX
  
  match_matrix=matrix(nrow=length(n_matches),ncol=max(n_matches,na.rm=TRUE))
  match_matrix=as.data.frame(match_matrix)
  
  ###Need to accommodate variation in data submitted - should be able to run on just a list of names from a data table...
  
  ###Encountering an error with "invalid regular expression" within the master$ORIGINAL.NAMES
  
  #master_original_names=sub('\\ .*|\\_.*|\\/.*|\\-.*','',master$ORIGINAL.NAMES) #Issue is a typo in original spreadsheet?
  
  ###END OF "invalid regular expression" fix.
  
  if(APD_format==TRUE){ #If we're working with APD format, then we can search both original and proposed names from AML's list.
    for(i in 1:nrow(taxa_just)){ #For loop searching for original taxa names, and matching them to revised names.
      y=grep(taxa_just$FULL_TAXON[i],proposed_names) #Starting with ideal match - full name to revised name in AMLs list. ###NAMING OF MASTER LIST ITEMS FOR PORTABILITY?
      if(length(y)==0){ #If there's no matches, let's try our second-tier match - full name to an original name
        y=grep(taxa_just$FULL_TAXON[i],original_names) ###ONLY POSSIBLE WITH AML'S LIST - LOGICAL CONTROL FOR THIS?
        if(length(y)==0){ #If we come up short on our second-tier match, then we try our third tier - the short names.
          y=grep(taxa_just$SHORT_TAXON[i],proposed_names)
          if(length(y)==0){ #If we come up short on our third-tier match, then we try our fourth tier - short names vs. original taxa
            y=grep(taxa_just$SHORT_TAXON[i],original_names)
            if(length(y)==0){ #If our last match should fail..
              match_matrix[i,]=NA #Write in NA for suggestions
              taxa_just$COMMENT[i]="no match, check spelling" ###Write in suggestion
            } else { #If our fourth tier search succeeds...
              for(j in 1:length(y)){ #For loop to handle multiple matches.
                z=grep(original_names[y[j]],master$ORIGINAL.NAMES) ###FIXING "invalid regular expressing" issue CAK 5/9/2021 ###REVERT
                match_matrix[i,j]=master$PROPOSED.NAMES[z[j]] #And we want to write each of those matches to the matrix.
              }
              taxa_just$COMMENT[i]="update taxonomy, revise upwards?" ###Write in suggestion
            }
          } else { #If we succeed at our third tier match, then we write it.
            for(j in 1:length(y)){ #For loop to handle multiple matches.
              match_matrix[i,j]=proposed_names[y[j]] #And we want to write each of those matches to the matrix.
            }
            taxa_just$COMMENT[i]="revise upwards?"  ###Write in suggestion
          }
        } else { #If we succeed at second tier, write it
          for(j in 1:length(y)){ #For loop to handle multiple matches.
            z=grep(original_names[y[j]],master$ORIGINAL.NAMES) ###FIXING "invalid regular expressing" issue CAK 5/9/2021 ###REVERT
            match_matrix[i,j]=master$PROPOSED.NAMES[z[j]] #And we want to write each of those matches to the matrix.
          }
          taxa_just$COMMENT[i]="old taxon, update taxonomy"  ###Write in suggestion
        }
      } else { #If there are matches y will be >0 and we write the results
        #Can we refine this a bit further - exact matches with nomenclatural differences?
        z=grep(paste0('^',taxa_just$FULL_TAXON[i],'$'),proposed_names[y]) #Search for exact match!
        if(length(z)==0){ #if no exact match...
          z=grep(paste0('^',taxa_just$FULL_TAXON[i],'-type','$'),proposed_names[y]) #Try searching for "-type" variations
          if(length(z)==0){ #if appending "-type" doesn't do anything...
            for(j in 1:length(y)){ #For loop to handle multiple matches.
              match_matrix[i,j]=proposed_names[y[j]] #And we want to write each of those matches to the matrix.
            } #End of for loop handling mutliple matches
            taxa_just$COMMENT[i]="close! check nomenclature"  ###Write in suggestion
          } else { #If appending "=type" does work
            match_matrix[i,1]=proposed_names[y[z]]
            taxa_just$COMMENT[i]="match! is -type appropriate?"  ###Write in suggestion
          }
        } else { #If there's an exact match of the full name then write it!
          match_matrix[i,1]=proposed_names[y[z]]
          taxa_just$COMMENT[i]="match! good job"  ###Write in suggestion
        }
      }
    } #End of loop matching original and revised names
    
  } else { #If we're not using the APD format, then searching could be a bit simpler
  
    for(i in 1:nrow(taxa_just)){ #For loop searching for original taxa names, and matching them to revised names.
      y=grep(taxa_just$FULL_TAXON[i],proposed_names) #Starting with ideal match - full name to revised name in AMLs list. ###NAMING OF MASTER LIST ITEMS FOR PORTABILITY?
      if(length(y)==0){ #If there's no matches, let's try our second-tier match - full name to an original name
        y=grep(taxa_just$SHORT_TAXON[i],proposed_names) ###ONLY POSSIBLE WITH AML'S LIST - LOGICAL CONTROL FOR THIS?
        if(length(y)==0){ #If we come up short on our second-tier match, then we try our third tier - the short names.
          match_matrix[i,]=NA #Write in NA for suggestions
          taxa_just$COMMENT[i]="no match, check spelling" ###Write in suggestion
        } else { #If we succeed at second tier, write it
          for(j in 1:length(y)){ #For loop to handle multiple matches.
            z=grep(proposed_names[y[j]],master$ORIGINAL.NAMES)
            match_matrix[i,j]=master$PROPOSED.NAMES[z[j]] #And we want to write each of those matches to the matrix.
          }
          taxa_just$COMMENT[i]="old taxon, update taxonomy"  ###Write in suggestion
        }
      } else { #If there are matches y will be >0 and we write the results
        #Can we refine this a bit further - exact matches with nomenclatural differences?
        z=grep(paste0('^',taxa_just$FULL_TAXON[i],'$'),proposed_names[y]) #Search for exact match!
        if(length(z)==0){ #if no exact match...
          z=grep(paste0('^',taxa_just$FULL_TAXON[i],'-type','$'),proposed_names[y]) #Try searching for "-type" variations
          if(length(z)==0){ #if appending "-type" doesn't do anything...
            for(j in 1:length(y)){ #For loop to handle multiple matches.
              match_matrix[i,j]=proposed_names[y[j]] #And we want to write each of those matches to the matrix.
            } #End of for loop handling mutliple matches
            taxa_just$COMMENT[i]="close! check nomenclature"  ###Write in suggestion
          } else { #If appending "=type" does work
            match_matrix[i,1]=proposed_names[y[z]]
            taxa_just$COMMENT[i]="match! is -type appropriate?"  ###Write in suggestion
          }
        } else { #If there's an exact match of the full name then write it!
          match_matrix[i,1]=proposed_names[y[z]]
          taxa_just$COMMENT[i]="match! good job"  ###Write in suggestion
        }
      }
    } #End of loop matching original and revised names
    
  }
  
  match_colnames=vector(mode="character",length=1)
  
  for(i in 1:(ncol(match_matrix))){ 
    x=paste0("sugg_taxon_",i)
    match_colnames[i]=x
  }
  
  colnames(match_matrix)=match_colnames
  
  output=cbind(taxa_just,match_matrix)
  
  count_NA=table(is.na(output$sugg_taxon_1))
  
  print(paste0(count_NA[2],' unmatched taxa'))
  
  write.csv(output,file="harmonizeR_output.csv",row.names=FALSE)
  
} #Closing out function.

