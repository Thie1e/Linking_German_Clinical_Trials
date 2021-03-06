
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

The resulting table in our study using data from November of 2019 had 36625 rows and 53 variables and is in `all_trials_de.RData` as a `tibble`. When re-running the scripts with your own data the result will differ, of course.

```
library(tidyverse)
glimpse(all_trials_de)

Rows: 36,625
Columns: 53
$ main_id               <chr> "337", "516", "948", "1903", "2622", "2717", "28...
$ nct_id                <chr> "NCT03916159", "NCT03835923", "NCT03653858", "NC...
$ sec_id_nct            <list> ["DRKS00017041", "DRKS00015140", <"DRKS00014947...
$ title                 <chr> "Extrauterine Placental Transfusion In Neonatal ...
$ official_title        <chr> "Extrauterine Placental Transfusion In Neonatal ...
$ prim_outcome          <chr> "Hematocrit", "Change in HbA1c", "Montgomery-Asb...
$ sec_outcome           <chr> "Cerebral tissue oxygen saturation;Mean airway p...
$ start_date            <date> 2019-05-01, 2019-02-12, 2018-09-03, 2017-12-01,...
$ completion_date       <date> 2022-11-01, 2020-09-30, 2023-06-02, 2019-12-31,...
$ created_at            <date> 2019-04-05, 2019-02-01, 2018-08-22, 2017-10-05,...
$ study_type            <chr> "Interventional", "Interventional", "Interventio...
$ n                     <dbl> 60, 1500, 47, 2930, 504, 30, 200, 24, 25, 870, 5...
$ countries             <list> ["Germany", "Germany", "Germany", "Germany", "G...
$ agency_class          <list> ["Other", <"Other", "Other", "Other", "Other", ...
$ name                  <list> ["Universitätsklinikum Köln", <"Techniker Krank...
$ overall_status        <chr> "Recruiting", "Recruiting", "Recruiting", "Recru...
$ email_central         <list> [<"benjamin.kuehne@uk-koeln.de", "andre.oberthu...
$ email_result          <list> [NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL...
$ pmid                  <list> [NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL...
$ references            <list> [NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL...
$ ictrp_id              <chr> "DRKS00017041", "DRKS00015140", "DRKS00014947", ...
$ sec_id_ictrp          <list> [<"NCT03916159", "18-232">, <"U1111-1217-6306",...
$ title.ictrp           <chr> "Extrauterine Placental Transfusion In Neonatal ...
$ official_title.ictrp  <chr> "Extrauterine Placental Transfusion In Neonatal ...
$ prim_outcome.ictrp    <chr> "Mean Hematocrit in the first 24 hours of life",...
$ sec_outcome.ictrp     <chr> "2. Vital Parameters during neonatal resuscitati...
$ start_date.ictrp      <date> 2019-05-01, 2019-02-12, 2018-09-06, 2017-12-01,...
$ created_at.ictrp      <date> 2019-03-27, 2019-01-10, 2018-08-06, 2017-10-05,...
$ n.ictrp               <dbl> 60, 1500, 47, 2930, 500, 30, 200, 24, 25, 870, 5...
$ study_type.ictrp      <chr> "Interventional", "Interventional", "Interventio...
$ countries.ictrp       <list> ["Germany", "Germany", "Germany", "Germany", "G...
$ name.ictrp            <chr> "Uniklinik Köln, Klinik und Poliklinik für Kinde...
$ overall_status.ictrp  <chr> "Recruiting", "Recruiting", "Recruiting", "Recru...
$ email_central.ictrp   <chr> "benjamin.kuehne@uk-koeln.de", "leikd.info(at)mr...
$ n_euctr_ids           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
$ drks_id               <chr> "DRKS00017041", "DRKS00015140", "DRKS00014947", ...
$ sec_id_drks           <list> [<"/", "NCT03916159", "/", "18-232">, <"U1111-1...
$ official_title.drks   <chr> "Extrauterine Placental Transfusion In Neonatal ...
$ prim_outcome.drks     <chr> "Mean Hematocrit in the first 24 hours of life",...
$ sec_outcome.drks      <chr> "2. Vital Parameters during neonatal resuscitati...
$ countries.drks        <list> ["Germany", "Germany", "Germany", "Germany", "G...
$ start_date.drks       <date> 2019-05-01, 2019-02-12, 2018-09-06, 2017-12-01,...
$ completion_date.drks  <date> NA, NA, NA, NA, NA, 2018-05-31, NA, 2017-08-30,...
$ n.drks                <dbl> 60, 1500, 47, 2930, 500, 30, 200, 24, 25, 870, 5...
$ overall_status.drks   <chr> "ongoing", "ongoing", "ongoing", "ongoing", "ong...
$ study_type.drks       <chr> "Interventionell", "Interventionell", "Intervent...
$ name.drks             <list> ["Uniklinik Köln, Klinik und Poliklinik für Kin...
$ email_central.drks    <list> ["benjamin.kuehne@uk-koeln.de", "leikd.info(at)...
$ created_at.drks       <date> 2019-03-27, 2019-01-10, 2018-08-06, 2017-10-05,...
$ iit                   <chr> "true", "true", "true", "true", "false", "false"...
$ references.drks       <chr> NA, "Ethikvotum_LeIKD_08.05.2018", NA, NA, NA, N...
$ iit.drks              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
$ summarized_start_date <date> 2019-05-01, 2019-02-12, 2018-09-03, 2017-12-01,...
```
