
# Searching all Clinical Trials in Germany

## Linking the Clinicaltrials.gov, ICTRP and DRKS registers

This repository contains the R-code for our study on searching all clinical trials
that recruited patients in Germany. To do so, we queried AACT
(Clinicaltrials.gov), the ICTRP and the DRKS. 

To link the trials, we used primary IDs and a Random Forest model based on multiple variables from the registers.

### Data prerequesites

Due to the size of the data, our data files are not included but similar files 
can easily be obtained as follows:

- Clinicaltrials.gov: Download pipe-delimited files from the AACT at https://aact.ctti-clinicaltrials.org/download
- DRKS: Start an appropriate query in the web frontend at https://www.drks.de/drks_web/navigate.do?navigationId=search&reset=true and download the data as XML. To obtain all trials on DRKS, you can for example search all trials that were registered in specific date ranges and download the results of all of these queries.
- ICTRP: Trials can be filtered to include only trials that recruited in Germany using the link https://apps.who.int/trialsearch/AdvSearch.aspx?Country=Germany&ReturnUrl=~/ListBy.aspx?TypeListing=1 and the results can then be exported as XML

To train the Random Forest, some labeled data is necessary. We manually labeled 587 trials as duplicates / non-duplicates. That data is in `rfdat.RData`.

### Scripts

The scripts should be executed in order.

1. Load and transform the data in `1 - Data.R`
2. Build a Random Forest model in `2 - Random Forest.R`
3. Link the databases in `3 - Join.R`

The script `fuzzy_functions.R` contains some functions for calculating similarity scores that serve as features in the Random Forest.

### Result

The resulting table in our study using data from November of 2019 had 36617 rows and 69 variables.