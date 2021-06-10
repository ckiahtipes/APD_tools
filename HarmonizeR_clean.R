#Harmonizer: Clean

#Updates proposed via conversation with Simon Goring.

###1 Break down big loops into separate functions.

  #i: decompose - measure records, store data, and make taxon list
  #ii: evaluate - search taxa against a master 
  #iii: harmonize - recompile data into a master list (all records) and individual lists

###2 Use lists to cut down number of times a file is read.

  #using lapply and sapply to run things.
  #use functions to hand off important bits of code (products) to be analyzed by the next function.
  #subsetting can handle a lot of the burden that the for loops are handling

###3 Formats are too unique to my own work. Should focus on Tilia-style formats. What's the simplest possible appraoch?

####LOWER BOUND OF 

decompose = function(records){
  path = getwd()
  table_names=c('original_taxa', 'suggested_taxa', 'record_data')
  
  all_data=lapply(1:length(records),function(x){
    lapply(1:length(m_names),function(y){
      matrix(nrow=max(cores_matrix[2,]),ncol=cores_matrix[1,1])
    })
  })
  
  
  
  for(i in 1:length(rec_names)){ #Read data and get taxa names from columns.
    pol_data=read.csv(paste0(path,'/data/',rec_names[i],"_data.csv"),header=TRUE,row.names="X") ###Pulling "X" which requires fix below.. ###CAK_EDITS #read file: fix file reading, note in README.md
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
  
}
