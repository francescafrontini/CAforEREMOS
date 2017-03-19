# Correspondence Analysis for the EREMOS pattern extraction tool

This R script is meant to be used with the output of the EREMOS pattern extraction tool.
<eremos.lip6.fr>

NOTICE: none of the scripts here extract patterns from texts. 

In order to run the R script needs:
- a folder containing a file called Model.txt with the descriptions of the patterns and their pattern id (patterns \t number) 
- a subfolder with pattern counts for every text you analyse (pattern_id\tcount)

The R script runs in the subfolder
It returns plots and tables of contributions.
Please take notice of the comments inside the script, and for Version 4.0 of the "culling" parameter. 
When set to 1, it will only perform CA on those patterns that are present at least once in ALL texts.

The python script called transform.py helps people who want to use the R script with own extacted pattern counts.
In particular it will create a <Model.txt> file assigning an ID to your extracted patterns and the subfolder where your count files are transformed so as to use pattern IDs.
