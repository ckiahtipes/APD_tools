###Comparing marine and terrestrial paleoecological records in Nigeria

##Goals
#Read pollen data and harmonize taxa
#Summarize records
#Plot results with data on Marine Productivity

#Necessary functions:

#Zscore

zscore<-function(y1) {
  n1 <- mean(y1)
  n2 <- sd(y1)
  zscore <- (y1-n1)/n2
  zscore
}

#HarmonizeR

library(npreg)
library(rbacon)
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

#Setting logical controls for code development and evaluation

run_SELE_dates=FALSE

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

rec_names=c("DANGBO","TILLA","KW31","NIGERDC2","LACSELE_APD")
#rec_names=c("TILLA","KW31","NIGERDC2","LACSELE_APD")
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
  
  #With this in hand, we can organize results by pollen type and derive a pollen sum.
  
  all_pollen=fossil_data[fossil_taxa[,4]!="Sp" & fossil_taxa[,4]!="X" & fossil_taxa[,4]!="Other" & fossil_taxa[,4]!= "Ss" & fossil_taxa[,4]!="St" & fossil_taxa[,4]!= "Sb" & fossil_taxa[,4]!="S",]
  row.names(all_pollen)=fossil_taxa[fossil_taxa[,4]!="Sp" & fossil_taxa[,4]!="X" & fossil_taxa[,4]!="Other" & fossil_taxa[,4]!= "Ss" & fossil_taxa[,4]!="St" & fossil_taxa[,4]!= "Sb" & fossil_taxa[,4]!="S",3]
  
  herbaceous=fossil_data[fossil_taxa[,4]=="N" | fossil_taxa[,4]=="NL" | fossil_taxa[,4]=="Nq" | fossil_taxa[,4]=="NG",]
  row.names(herbaceous)=fossil_taxa[fossil_taxa[,4]=="N" | fossil_taxa[,4]=="NL" | fossil_taxa[,4]=="Nq" | fossil_taxa[,4]=="NG",3]
  
  arb_pollen=fossil_data[fossil_taxa[,4]=="A" | fossil_taxa[,4]=="AL" | fossil_taxa[,4]=="L",] #Including all Lianas
  row.names(arb_pollen)=fossil_taxa[fossil_taxa[,4]=="A" | fossil_taxa[,4]=="AL" | fossil_taxa[,4]=="L",3]
  
  #Need to exclude Rhizophora
  arb_taxa=row.names(arb_pollen)
  man_finder=grep("Rhizophora",arb_taxa)
  if(length(man_finder>0)){
    mang_pollen=arb_pollen[man_finder,]
    arb_pollen=arb_pollen[-c(man_finder),]
  } else {
    mang_pollen=c(rep(0,ncol(all_pollen)))
  }
  
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
  
  ptype_summary=rbind(arb_sum,mang_pollen,hrb_sum,plm_sum,ind_sum)
  ptype_summary=t(ptype_summary) #Need to transpose table to get the %s
  ptype_pct=(ptype_summary/all_sum)*100 #This works so far...
  ptype_pct=t(ptype_pct)
  
  #Can we intervene somewhere in here and add Rhizophora/Mangroves?
  
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

#At this point, we have all of the pollen datasets. Lac Sele is missing the chronological data and we need to load the marine proxies for terrigenous input.

#Build chronology for Sélé from published 14C Dates.
SELE_depth=as.numeric(row.names(plot_results$LACSELE_APD[[6]]))

if(run_SELE_dates==TRUE){
  Bacon("SELE",postbomb=1,thick=10,depths=SELE_depth)
}

SELE_adm=read.table("Cores/SELE/SELE_140_ages.txt",header=TRUE,sep="\t",row.names="depth")

#Append chronology to Sélé data in array.

plot_results$LACSELE_APD[[3]]=t(SELE_adm)

#Read marine data.



#Does it make any sense to bundle all of the AP_NAP data or NAP data, z-score it, and plot it?

#What about binning by 500/1000 year intervals, then plotting averages with deviations?










###Let's try to plot some West African Data.

WAF_group=c("DANGBO","TILLA","KW31","NIGERDC2","LACSELE_APD")
plot(0,0,pch=NA,xlim=c(-4,4),ylim=c(-20000,0),main="AP_NAP Ratio, West African Records",xlab=NA,axes=FALSE,ylab="yr BP")

axis(1,at=seq(0,4,1),labels = c(WAF_group))

axis(2,at=seq(-30000,0,1000))

for(i in 1:length(WAF_group)){
  x=grep(WAF_group[i],rec_names)
  p_pct=t(plot_results[[x]][[5]])
  p_pct=as.data.frame(p_pct)
  if(WAF_group[i]=="LACSELE_APD"){
    p_chr=plot_results[[x]][[3]][4,]*-1
  } else {
    p_chr=plot_results[[x]][[3]][1,]*-1
  }
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
  NAPz=zscore(AP_NAP[,2])
  AP_NAPz=zscore(AP_NAP[,1])
  AP_NAP=cbind(AP_NAP,AP_NAPz,NAPz)
  #lines(AP_NAP[,1]/(max(AP_NAP[,1],na.rm=TRUE)),p_chr,pch=21,bg=i,type="o",lty=3)
  lines(NAPz,p_chr,pch=21,bg=i,type="o",lty=3)
  #lines(zscore(AP_NAP),p_chr,pch=21,bg=i,type="o",lty=3)
  if(i==1){
    smooth_table=as.data.frame(AP_NAP)
    smooth_table=cbind(smooth_table,t(p_chr))
  } else {
    AP_NAP=cbind(AP_NAP,t(p_chr))
    smooth_table=rbind(smooth_table,AP_NAP)
  }
}

#Let's take aggregated results and apply some kind of smoothing.

smooth_table=na.omit(smooth_table)
smooth_table=smooth_table[smooth_table[,5]>-12000,] #Excluding dates outside of the Iho Eleru chronology
smooth_table=smooth_table[order(smooth_table[,5]),]

hist(smooth_table[,5]) #This is roughly how many data points per 1000 year bin we would get...
hist(smooth_table[,5],breaks=24) #This is the data points for 500 year bins...

#thing=rev(smooth_table[,5])

#Section for testing smoothed AP_NAP index

smoothed_lines=ksmooth(smooth_table[,5],smooth_table$AP_NAPz,bandwidth=500)
spline_smoothed=smooth.spline(smooth_table$Chron1,smooth_table$AP_NAPz,nknots=12)
ss_smoothed=ss(smooth_table$Chron1,smooth_table$AP_NAPz,nknots=12)

plot(smooth_table$AP_NAPz,smooth_table$Chron1)
lines(smoothed_lines$y,smoothed_lines$x)
#lines(smooth(smooth_table$AP_NAPz,kind="3RSR"),smooth_table$Chron1,lty=1)
lines(spline_smoothed$y,spline_smoothed$x,lty=3,col="red")
plot(ss_smoothed,add=TRUE)

#Section for testing smoothed NAP

smoothed_lines=ksmooth(smooth_table[,5],smooth_table$NAPz,bandwidth=500)
spline_smoothed=smooth.spline(smooth_table$Chron1,smooth_table$NAPz,nknots=12)
ss_smoothed=ss(smooth_table$Chron1,smooth_table$NAPz,nknots=12)

plot(smooth_table$NAPz,smooth_table$Chron1)
lines(smoothed_lines$y,smoothed_lines$x)
#lines(smooth(smooth_table$NAPz,kind="3RSR"),smooth_table$Chron1,lty=1)
lines(spline_smoothed$y,spline_smoothed$x,lty=3,col="red")
plot(ss_smoothed,add=TRUE)

#An alternative is binning the results and plotting box-whisker plots

chrono_bins=seq(-12000,-1000,500)
smooth_table[smooth_table[,5]>=chrono_bins[1]&smooth_table[,5]<=chrono_bins[2],4]
bin_count=hist(smooth_table[,5],plot=FALSE)
bin_matrix=matrix(nrow=max(bin_count$counts),ncol=length(chrono_bins))
bin_matrix=as.data.frame(bin_matrix)
colnames(bin_matrix)=seq(-12000,-1000,500)


for(i in 1:length(chrono_bins)){
  if(i==23){
    x=smooth_table[smooth_table[,5]>=chrono_bins[i],2]
  } else {
    x=smooth_table[smooth_table[,5]>=chrono_bins[i]&smooth_table[,5]<=chrono_bins[i+1],4]
  }
  y=c(x,rep(NA,nrow(bin_matrix)-length(x)))
  bin_matrix[,i]=y
}

plot(0,0,pch=NA,xlim=c(-4,4),ylim=c(-12000,0),xlab=NA,axes=FALSE,ylab="yr BP")
boxplot(bin_matrix,horizontal=TRUE,add=TRUE,at=chrono_bins,boxwex=500)
points(smooth_table$NAPz,smooth_table$Chron1,pch=21,bg="black",cex=0.8)
lines(spline_smoothed$y,spline_smoothed$x,lty=3,col="red",lwd=2)
title(main="Zscores of NAP% from Nigerian Pollen Cores")
