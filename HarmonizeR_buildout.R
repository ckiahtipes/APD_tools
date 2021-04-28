###Buildout for Harmonizer function.

#The function is written in base R and has no dependencies.

#Requirements:
  #Character vector listing names of pollen records, must match file names for pollen data and taxa lists (i.e. FC000 = FC000_data.csv, FC000_taxa.csv). <------ Trying to imitate APD format here.
  #Master reference of accepted pollen types.




#Harmonizer function

harmonizer=function(rec_names,master){ #Function needs names of pollen records and a master list. ###consider repairing "rec_names" to something more sensible.
  
  name_length=vector(mode="numeric",length=1)
  fullname_length=vector(mode="numeric",length=1)
  
  for(i in 1:length(rec_names)){ #Read data and get taxa names from columns.
    pol_data=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_data.csv"),header=TRUE) ###Pulling "X" which requires fix below..
    pol_names=read.csv(paste0("pollen/Ngotto/",rec_names[i],"_taxa.csv"),header=TRUE)
    fullnames=pol_names$FullNames
    cn=colnames(pol_data)
    cn=cn[-1] ###Here's the sloppy fix to the "X" problem.
    name_length[i]=length(cn)
    fullname_length[i]=length(fullnames)
  }
  
  
  
  
  
} #Closing out function.

#Test data and function runs.

rec_names=c("FC000","FC300","FC400") #consider repairing "rec_names" to something more sensible.