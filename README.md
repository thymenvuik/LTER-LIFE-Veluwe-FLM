this repo is subdivided into 2 parts: the r scripts, and the data. 

Scripts
pipeline biomass-succession is an R pipeline that initialises the input txt file for the biomass succession extension in landis. 
No further data inputs are needed to initialise this pipeline. This script fits inside "build scenario and ecoregions" block

pipeline scenario is an R pipeline that initialises the input txt file for the scenario of any model run in landis
No further data inputs are needed to initialise this pipeline. This script fits inside "build scenario and ecoregions" block.
A functionality that needs to be added is that the used extensions should be in a way connected as inputs to this script, with all necessary error messages for 
extensions names, it is important to not include the name of the extension here, but the name that is on top of the initialisation file of the extension

pipeline species trait data is an r pipeline that initialises the input files (2 csv, and 1 txt file) for biomass succession extension in landis
it is currently specifically made for biomass succession. Core species data is a file that is needed for all succession extensions, but other extensions may require
different input files or input files with the same name and more extensive species traits. A functionality where this script is only run based on a vector input 
with all used extensions would be nice, perhaps with an if loop? 
This script is based on the TRY database and should therefore be included. Other values in this script are based on scientific literature, 
studied as close to the Veluwe as possible, but this was sometimes not possible. Sources are in different document
This pipeline is highly specified to the TRY database, if there are any additions to the TRY database, the code may need to be updated. Unfortunately TRY is not standardized.



data and data inputs
SQL query mortality NBI7, this txt file contains an sql query that should run on the NBI7 (national forest inventory), this query can be used
to quantify the trees that have decreased vitality on the Veluwe. The score that is calculated is per tree the trees with decreased vitality over the total amount of trees.

probMortality_biomass
The result of this query is stored in the txt file probMortality_biomass.txt, the data has been changed slightly for Piceabie and Querspec, 
since they had values that where significantly lower than other trees.

establishmentTreeVeluwe.csv is a table copied from the report "bosverjonging op de Veluwe" from Probos. tabel 3.2 which is used from the probablity of establishment
