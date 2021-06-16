HarmonizeR v.0
Christopher A. Kiahtipes, Ph.D.

INTRODUCTION

This is a first draft of an R function I have written to assist with checking African pollen taxon designations against an accepted list of morphotypes.

The goal of this function is to provide a rapid assessment of a set of pollen taxa that are either being published or uploaded to Neotoma. Its operation is relatively simple and it searches the taxon names from the submitted dataset in Anne-Marie Lézine's expansive pollen morphotype list. Her list is especially useful because it contains a column of "ORIGINAL NAMES" and "PROPOSED NAMES". By searching against this column, we can rapidly update older records. This can also be used to rapidly check your own results.

My hope is that this reduces the time that students and researchers spend copy-pasting names or searching spreadsheets and increases the time that we can spend digging into the botanical and taxonomic resources available to us.


SETUP

Once you've unzipped the files, set it as your working directory in R. Be sure that the main folder is populated with the harmonizeR.R script, the master files (AML_2015_base.csv and AML_2019_base.csv), and a folder called "data". Whatever pollen records/sites you are working on should be saved there as .csv files.

FORMATTING

The harmonizeR script is looking for (at a minimum) a .csv file containing pollen data. This can be formatted with taxa either as rows (as in Tilia) or as columns. HarmonizeR can work with a single dataset, only requiring that the first row or column be taxon names and all other rows/columns

My preference is to work with two files per record - one containing the pollen data and shortened taxon names (taxa as columns) and another file with pollen taxa as rows, with 