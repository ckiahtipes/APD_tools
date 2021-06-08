# APD_tools
Repository for building tools for African Pollen Database uploads and analysis.

#Tools

##HarmonizeR

This function is intended to help data stewards (and paleoecologists in general) deal with harmonizing long lists of taxa. Using a master reference, this function can be used to compare new data or existing African Pollen Database datasets with a master list of accepted morphotypes created by Anne-Marie Lézine.

This is designed to work in a two-phase process.

First, users take their data (or APD data) and run it against a master morphotype list. This returns a table where rows are taxa by record and the columns are:
  1- Original name(s) of the taxon.
  2- Commentary from the function (coded for a range of likely errors).
  3- A list of suggestions based on the quality of the match with the master list

Second, users validate taxa on their own using online databases (tropicos.org or African Plants Database) and save this updated file.



Third, users can read this new data and write new results with updated names.


#Goals

*Build structure of function and test against established data.
*Write tutorial showing basic use of the function
*Distribute function to testers and, eventually, APD data stewards.

#Updates

##1 Jun 2021

Picked up this task after a long absence. Completed review of old code and brought all sections into a new script (HarmonizeR_buildout.R). The code itself works, but the function does not work at the moment. There's a handful of problems that I've identified:

*Need to make searching and writing tables more systematic and less haphazard (large code section at the end).
*Develop systematic naming for master reference and its columns.
*Create mechanisms for giving feedback to users.