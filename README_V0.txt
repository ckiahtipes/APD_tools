HarmonizeR v.0
Christopher A. Kiahtipes, Ph.D.

INTRODUCTION

This is a first draft of an R function I have written to assist with checking African pollen taxon designations against an accepted list of morphotypes.

The goal of this function is to provide a rapid assessment of a set of pollen taxa that are either being published or uploaded to Neotoma. Its operation is relatively simple and it searches the taxon names from the submitted dataset in Anne-Marie Lézine's expansive pollen morphotype list. Her list is especially useful because it contains a column of "ORIGINAL NAMES" and "PROPOSED NAMES". By searching against this column, we can rapidly update older records. This can also be used to rapidly check your own results.

My hope is that this reduces the time that students and researchers spend copy-pasting names or searching spreadsheets and increases the time that we can spend digging into the botanical and taxonomic resources available to us.

SETUP

Setup is simple. Make sure the harmonizeR.R script and the AML_base_2019.csv files are in your working directory. Whatever datasets you are using need to be saved in a folder called "data".

My preference is to save files in the data folder using their APD code, just like their Tilia counterparts.

FORMATTING

The harmonizeR script is looking for a .csv file in the data folder containing pollen taxa and counts. This can be formatted with taxa either as rows (as in Tilia) or as columns. By default, the script is looking for a single file.

Regardless of whether the taxa are organized by columns or rows, harmonizeR is expecting the first row of the first column to be blank.

I designed the script to meet some unique circumstances and harmonizeR will also accept a two-file system which I use to track and organize pollen taxa and their details. If you want to use the script this way, then the data folder must contain two files per record, one detailing the taxa info (FC000_taxa.csv) and one the data (FC000_data.csv).

USE

In order for this function to work, it must be loaded into the R session. Open the script titled "HarmonizeR_buildout.R" and run it. 

harmonizer(rec_names,master_file,taxa_file=FALSE,Tilia_format=TRUE,APD_format=TRUE)

  rec_names = a single name or concatenated string of names with record names that are to be evaluated.
  
  master_file = a character string identifying the master reference file. Default is "AML_base_2019.csv"
  
  taxa_file = logical indicating whether or not files with taxon information are to be used, specific to CAKs projects. Default is set to FALSE.
  
  Tilia_format = logical indicating whether taxa are organized along the rows (=TRUE) or the columns (=FALSE) of the spreadsheet. Default is set to TRUE.
  
  APD_format = logical indicating whether master spreadsheet contains "ORIGINAL" and "SUGGESTED" columns sensu AML's list. Default is set to TRUE.

TUTORIAL

Here's a test case with a dataset (G16867-1.csv) that I've modified slightly to include a couple common errors (spelling, old taxonomy).

rec_names = "G16867-1"

harmonizer(rec_names)

HarmonizeR combs through the file, searching the taxa names against AML's list and coming up with suggestions. It should give some brief output in the console saying:

"G16867-1"
"2 unmatched taxa"

EVALUATION AND UPDATING TAXA

Open the "harmonizeR_output.csv" file that's been saved in the working directory. This spreadsheet starts with five columns listing the count of this record in the total records submitted (more useful with multiple records), the APD code of the record (also just the record name), the short taxon name (leading character string), the full taxon name (complete original name in data file), and some commentary from the function on what kind of error you might be looking at*. Following this is a long list of columns with iterative column names (sugg_taxon_1, sugg_taxon_2...) filled with the matches from searching against the master spreadsheet**.

*taxonomy is complicated and so are the kind of errors we grapple with, so it is important to know that these are ONLY suggestions.
**the matches are typically in order of how closely the match the original, but this is not always the case.

The last step is for you, the user, to look through your list of taxa as well as the list of proposed names and pick the best one. This will depend on your relationship with the paleoecological record you're working with (is it your data?) and the kind of error you've encountered.

FUTURE DEVELOPMENTS

I'm actively improving the original code and plotting some future developments. 

The original code that this is based on is capable of taking the confirmed suggestions and writing new files to be used for analysis or uploading to Neotoma. This aspect is under development.

Improvements to the harmonizeR function itself will focus on the internal workings (using lists rather than loops) and streamlining output. 





  