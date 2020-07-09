
library(tidyverse)
library(beepr)
library(lubridate)
library(furrr)
library(randomForest)
library(caret)
library(doSNOW)
library(cutpointr)

source("fuzzy_functions.R")
load("mod.RData")

#
# AACT
#

load("AACT_2019-10-11/aact_de.RData")

#
# Select and filter
#

any(duplicated(aact_de))
all_trials_de <- aact_de %>% 
    select(nct_id, id_value, brief_title, official_title, 
           prim_outcome, sec_outcome,
           start_date, completion_date, study_first_submitted_date,
           study_type, enrollment, countries, agency_class, name, overall_status, email_central,
           email_result, pmid, references) %>% 
    rename(title = brief_title, 
           sec_id_nct = id_value,
           n = enrollment,
           created_at = study_first_submitted_date) %>% 
    filter(!is.na(n), !is.na(start_date), start_date > "1990-01-01", 
           !is.na(title), n >= 1) %>% 
    rownames_to_column(var = "main_id")


#
# WHO
#

load("WHO/ictrp_de.RData")

#
# Select and filter
#

ictrp_de <- ictrp_de %>% 
    select(ictrp_id, sec_id_ictrp, brief_title, official_title,
           prim_outcome, sec_outcome,
           start_date, created_at, n,
           study_type, countries, 
           name, overall_status, email_central, n_euctr_ids) %>% 
    rename(title = brief_title) %>% 
    filter(!is.na(n), !is.na(start_date), start_date > "1990-01-01", 
           !is.na(title))

#
# Join with AACT
#

# 1. Left Join nct1 + who1
all_trials_de1 <- all_trials_de %>% 
    left_join(ictrp_de %>% mutate(ictrp_id_for_join = ictrp_id), 
              by = c("nct_id" = "ictrp_id_for_join"), 
              suffix = c("", ".ictrp"))
dim(all_trials_de1) # 19683 35
sum(!is.na(all_trials_de1$ictrp_id)) # 16902

#
# 2. Left Join by Random Forest
#

ictrp_de2 <- ictrp_de %>% 
    filter(!(ictrp_id %in% all_trials_de1$nct_id)) 
colnames(ictrp_de2)[-(1:2)] <- paste0(colnames(ictrp_de2)[-(1:2)], ".ictrp")

unmatched_all_trials_de1 <- all_trials_de1[is.na(all_trials_de1$ictrp_id), ] %>% 
    select(-(ictrp_id:n_euctr_ids))

#
# Create Similarity data
#

options(future.fork.enable = T)
plan(multiprocess)
similarities <- future_map_dfr(1:nrow(unmatched_all_trials_de1), .progress = T, .f = function(i) {
    tempdat <- unmatched_all_trials_de1[i, ]
    string_similarities <- lv_stringsim(rep(tempdat$title, nrow(ictrp_de2)),
                                      ictrp_de2$title.ictrp)
    start_date_similarities <- datedist(rep(tempdat$start_date, nrow(ictrp_de2)),
                                     ictrp_de2$start_date.ictrp)
    size_similarities <- sizedist(rep(tempdat$n, nrow(ictrp_de2)),
                               ictrp_de2$n.ictrp)
    result <- tibble(main_id = tempdat$main_id,
                     ictrp_id = ictrp_de2$ictrp_id,
                     string_similarities = round(string_similarities, 4),
                     start_date_similarities = round(start_date_similarities, 4),
                     size_similarities = round(size_similarities, 4))
    result <- result %>%
        # filter(string_similarities > quantile(string_similarities, probs = 0.95),
        #        string_similarities > 0.4,
        #        start_date_similarities > quantile(start_date_similarities, probs = 0.9),
        #        size_similarities > quantile(size_similarities, probs = 0.9))
        filter(string_similarities > 0.4,
               start_date_similarities > 0.97,
               size_similarities > 0.7)
    return(result)
})

similarities <- similarities %>% 
    rename(string_similarities = string_distances,
           start_date_similarities = start_date_distances,
           size_similarities = size_distances)

# 
# Predict similarity data
#
matches_aact_ictrp <- similarities %>% 
    left_join(all_trials_de1 %>% 
                  select(nct_id, main_id, sec_id_nct, title, 
                         official_title, start_date, n, countries),
              by = "main_id") %>% 
    left_join(ictrp_de2 %>% 
                  select(ictrp_id, sec_id_ictrp, title.ictrp, official_title.ictrp,
                         start_date.ictrp, n.ictrp, countries.ictrp),
              by = "ictrp_id") 
matches_aact_ictrp <- matches_aact_ictrp %>% 
    select(main_id, nct_id, sec_id_nct, ictrp_id, sec_id_ictrp, string_similarities, 
           title, title.ictrp, official_title, official_title.ictrp,
           start_date_similarities, start_date, start_date.ictrp,
           size_similarities, n, n.ictrp, countries, countries.ictrp)
matches_aact_ictrp <- matches_aact_ictrp %>% 
    rowwise() %>% 
    mutate(sec_id_nct = paste(sec_id_nct, collapse = "; "),
           sec_id_ictrp = paste(sec_id_ictrp, collapse = "; ")) %>% 
    ungroup()
matches_aact_ictrp <- matches_aact_ictrp %>% 
    left_join(all_trials_de1 %>% 
                  select(main_id, email_central, name, study_type),
              by = "main_id") %>% 
    left_join(ictrp_de2 %>% 
                  select(ictrp_id, email_central.ictrp, name.ictrp, study_type.ictrp),
              by = "ictrp_id") 
matches_aact_ictrp <- matches_aact_ictrp %>% 
    mutate(string_similarities_off_off = lv_stringsim(matches_aact_ictrp$official_title.ictrp,
                                                    matches_aact_ictrp$official_title),
           string_similarities_b_off = lv_stringsim(matches_aact_ictrp$title,
                                                  matches_aact_ictrp$official_title.ictrp),
           string_similarities_off_b = lv_stringsim(matches_aact_ictrp$title.ictrp,
                                                  matches_aact_ictrp$title))
matches_aact_ictrp <- matches_aact_ictrp %>% 
    rowwise() %>% 
    mutate(string_similarities = as.numeric(str_replace(string_similarities, pattern = ",", replacement = ".")),
           start_date_similarities = as.numeric(str_replace(start_date_similarities, pattern = ",", replacement = ".")),
           size_similarities = as.numeric(str_replace(size_similarities, pattern = ",", replacement = ".")),
           best_stringsim_titles = max(as.numeric(string_similarities), 
                                        as.numeric(string_similarities_off_off),
                                        as.numeric(string_similarities_b_off),
                                        as.numeric(string_similarities_off_b), na.rm = T),
           any_matched_id = check_dup_ids(nct_id, sec_id_nct, ictrp_id, sec_id_ictrp),
           any_matched_utn = check_utn(sec_id_nct, sec_id_ictrp),
           sum_matched_name_parts = sum_dup_ids(name, name.ictrp, min_nchar = 4),
           best_stringsim_name = calc_best_stringsim(name, name.ictrp),
           any_matched_email = check_dup_ids2(email_central, email_central.ictrp),
           best_stringsim_email = calc_best_stringsim(email_central, email_central.ictrp),
           perc_matched_countries = sum(unique(countries) %in% unique(countries.ictrp)) / 
               length(unique(c(countries, countries.ictrp))),
           matching_type = study_type == study_type.ictrp)
matches_aact_ictrp$any_matched_id <- as.factor(matches_aact_ictrp$any_matched_id)
matches_aact_ictrp$any_matched_email <- as.factor(matches_aact_ictrp$any_matched_email)
matches_aact_ictrp$same_db <- FALSE

preds <- predict(mod, newdata = matches_aact_ictrp)
table(preds)

matches_aact_ictrp$pred_duplicate <- preds

matches_aact_ictrp %>% 
    filter(pred_duplicate == "true") %>% 
    count(nct_id, wt = n()) %>% 
    arrange(desc(n)) %>%
    pull(n) %>% 
    table
#   1   2   3 
# 764  76   2

# Some studies have 2 or 3 matches in ICTRP 

stopifnot(!any(matches_aact_ictrp$ictrp_id[matches_aact_ictrp$pred_duplicate == "true"] %in%
                   all_trials_de1$ictrp_id))

# Trials that have already been matched
all_trials_de2 <- all_trials_de1 %>% 
    filter(!is.na(ictrp_id))

# Trials with new matches
matches_aact_ictrp1 <- matches_aact_ictrp %>% 
    filter(pred_duplicate == "true") %>% 
    select(main_id, ictrp_id) %>% 
    left_join(all_trials_de1 %>% 
                  select(main_id, nct_id, sec_id_nct, title, official_title,
                         prim_outcome, sec_outcome,
                         start_date, completion_date, created_at,
                         study_type, n, countries,
                         agency_class, name, overall_status, email_central,
                         email_result, pmid, references),
              by = "main_id") 
ictrp_for_join <- ictrp_de 
colnames(ictrp_for_join)[-c(1,2, ncol(ictrp_for_join))] <-
    paste0(colnames(ictrp_for_join)[-c(1, 2, ncol(ictrp_for_join))], ".ictrp")
matches_aact_ictrp1 <- matches_aact_ictrp1 %>% 
    left_join(ictrp_for_join, by = "ictrp_id") 


# Trials that still don't have matches
still_unmatched_ictrp <- all_trials_de1 %>% 
    filter(is.na(ictrp_id),
           !(main_id %in% matches_aact_ictrp1$main_id))

all_trials_de2 <- all_trials_de2 %>% 
    bind_rows(matches_aact_ictrp1) %>% 
    bind_rows(still_unmatched_ictrp) 


#
# Join rest of WHO
#

ictrp_for_join <- ictrp_de %>% 
    filter(!(ictrp_id %in% na.omit(all_trials_de2$ictrp_id))) 
colnames(ictrp_for_join)[-c(1,2, ncol(ictrp_for_join))] <-
    paste0(colnames(ictrp_for_join)[-c(1, 2, ncol(ictrp_for_join))], ".ictrp")
all_trials_de2 <- all_trials_de2 %>% 
    bind_rows(ictrp_for_join) 
# Add main IDs to the last trials from ICTRP:
stopifnot(sum(is.na(all_trials_de2$main_id)) == nrow(ictrp_for_join))
new_ids <- (1:nrow(all_trials_de2))[is.na(all_trials_de2$main_id)]
stopifnot(sum(new_ids %in% all_trials_de2$main_id) == 0)
all_trials_de2$main_id[is.na(all_trials_de2$main_id)] <- new_ids
dim(all_trials_de2) # 35728 35
length(unique(all_trials_de2$main_id)) # 35648

#
# DRKS
#

load("DRKS/all_info_drks.RData")
all_info_drks <- all_info_drks[!duplicated(all_info_drks$drks_id), ]
all_info_drks <- all_info_drks[!duplicated(all_info_drks), ]
all_info_drks <- all_info_drks %>% 
    rename(prim_outcome = prim_outcomes,
           sec_outcome = sec_outcomes,
           sec_id_drks = sec_ID,
           official_title = title_en)

#
# 1. Left Join ICTRP-ID + DRKS primary ID 
# (There are not matches using the primary DRKS ID and the primary AACT ID)
#

all_trials_de3 <- all_trials_de2 %>% 
    left_join(all_info_drks %>% 
                  mutate(ictrp_id = drks_id),
              by = c("ictrp_id"), suffix = c("", ".drks"))
stopifnot(nrow(all_trials_de3) == nrow(all_trials_de2)) 

#
# 2. Random Forest Matching DRKS
#

unmatched_drks <- all_info_drks %>% 
    filter(!(drks_id %in% all_trials_de3$drks_id))
unmatched_all_trials_de3 <- all_trials_de3 %>% 
    filter(!(drks_id %in% all_info_drks$drks_id))

#
# Create Similarity data
#

options(future.fork.enable = T)
plan(multiprocess)
similarities_drks <- future_map_dfr(1:nrow(unmatched_drks), .progress = T, .f = function(i) {
    tempdat <- unmatched_drks[i, ]
    string_similarities <- lv_stringsim(rep(tempdat$official_title, nrow(unmatched_all_trials_de3)),
                                      unmatched_all_trials_de3$official_title)
    size_similarities <- sizedist(rep(tempdat$n, nrow(unmatched_all_trials_de3)),
                               unmatched_all_trials_de3$n)
    start_date_similarities <- datedist(rep(tempdat$start_date, nrow(unmatched_all_trials_de3)),
                                     unmatched_all_trials_de3$start_date)
    result <- tibble(drks_id = tempdat$drks_id,
                     main_id = unmatched_all_trials_de3$main_id,
                     string_similarities = round(string_similarities, 4),
                     start_date_similarities = round(start_date_similarities, 4),
                     size_similarities = round(size_similarities, 4))
    result <- result %>%
        filter(string_similarities > 0.4,
               size_similarities > 0.5)
    return(result)
})

similarities_drks <- similarities_drks %>% 
    rename(string_similarities = string_distances,
           start_date_similarities = start_date_distances,
           size_similarities = size_distances)

matches_all_drks <- similarities_drks %>% 
    left_join(all_trials_de3 %>% 
                  select(nct_id, main_id, sec_id_nct, title, official_title, 
                         prim_outcome, sec_outcome, study_type, countries,
                         start_date, created_at, n, email_central, name, 
                         ictrp_id, sec_id_ictrp, title.ictrp, official_title.ictrp,
                         prim_outcome.ictrp, sec_outcome.ictrp, study_type.ictrp,
                         start_date.ictrp, n.ictrp, email_central.ictrp, name.ictrp,
                         countries.ictrp),
              by = "main_id") %>% 
    left_join(unmatched_drks %>% 
                  select(drks_id, sec_id_drks, official_title, 
                         prim_outcome, sec_outcome, start_date, created_at, n,
                         email_central, name, study_type, countries),
              by = "drks_id", suffix = c("", ".drks"))
matches_all_drks <- matches_all_drks %>% 
    rowwise() %>% 
    mutate(sec_id_nct = paste(sec_id_nct, collapse = "; "),
           sec_id_drks = paste(sec_id_drks, collapse = "; ")) %>% 
    ungroup()
matches_all_drks <- matches_all_drks %>% 
    mutate(string_similarities_off_off = mean(
        c(lv_stringsim(matches_all_drks$official_title,
                       matches_all_drks$official_title.drks),
        lv_stringsim(matches_all_drks$official_title,
                       matches_all_drks$official_title.drks)), 
        na.rm = T),
        string_similarities_b_off = 0.5,
        string_similarities = 0.5,
        string_similarities_off_b = mean(
            c(lv_stringsim(matches_all_drks$title,
                           matches_all_drks$official_title.drks),
            lv_stringsim(matches_all_drks$title,
                           matches_all_drks$official_title.drks)), 
            na.rm = T))
matches_all_drks <- matches_all_drks %>% 
    rowwise() %>% 
    mutate(best_stringsim_titles = max(as.numeric(string_similarities_off_off),
                                       as.numeric(string_similarities_b_off),
                                       as.numeric(string_similarities_off_b), na.rm = T),
           start_date_similarities = datesim(mean(c(start_date, start_date.ictrp), na.rm = T), 
                                             start_date.drks),
           matching_type = study_type == study_type.drks | study_type.ictrp == study_type.drks,
           size_similarities = sizesim(mean(c(n, n.ictrp), na.rm = T),
                                       n.drks),
           any_matched_id = any(check_dup_ids(nct_id, sec_id_nct, drks_id, sec_id_drks),
                                check_dup_ids(ictrp_id, sec_id_ictrp, drks_id, sec_id_drks)),
           any_matched_utn = any(check_utn(sec_id_nct, sec_id_drks),
                                check_utn(sec_id_ictrp, sec_id_drks)),
           sum_matched_name_parts = sum_dup_ids(name, name.drks, min_nchar = 4) +
               sum_dup_ids(name.drks, name.ictrp, min_nchar = 4),
           best_stringsim_name = max(calc_best_stringsim(name, name.drks),
                                      calc_best_stringsim(name.drks, name.ictrp), na.rm = T),
           any_matched_email = any(check_dup_ids2(email_central, email_central.drks),
                                   check_dup_ids2(email_central.drks, email_central.ictrp)),
           perc_matched_countries = sum(unique(countries.drks) %in% unique(c(countries.ictrp, countries))) / 
               length(unique(c(countries, countries.ictrp, countries.drks))),
           best_stringsim_email = max(calc_best_stringsim(email_central, email_central.drks),
                                       calc_best_stringsim(email_central.drks, email_central.ictrp), 
                                       na.rm = T))
matches_all_drks$any_matched_id <- as.factor(matches_all_drks$any_matched_id)
matches_all_drks$any_matched_email <- as.factor(matches_all_drks$any_matched_email)
matches_all_drks$same_db <- FALSE

# Manual median imputation, because caret returned an error here
matches_all_drks_imp <- matches_all_drks %>%
    ungroup %>% 
    mutate(across(where(is.numeric), function(x) {x[is.na(x)] <- mean(x, na.rm = T); x})) %>% 
    mutate(across(where(is.logical), function(x) {x[is.na(x)] <- as.logical(median(x, na.rm = T)); x})) 
preds <- predict(mod, newdata = matches_all_drks_imp)

matches_all_drks$preds_drks <- preds

# Trials that already have a match
all_trials_de4 <- all_trials_de3 %>%
    filter(!is.na(drks_id))

# Trials with new matches
matches_all_drks1 <- matches_all_drks %>% 
    filter(preds_drks == "true") %>% 
    select(main_id, drks_id) %>% 
    left_join(all_trials_de3 %>% 
                  select(main_id:n_euctr_ids),
              by = "main_id") 
drks_for_join <- all_info_drks  
colnames(drks_for_join)[-c(1,2)] <- paste0(colnames(drks_for_join)[-c(1, 2)], ".drks")
matches_all_drks1 <- matches_all_drks1 %>% 
    left_join(drks_for_join, by = "drks_id") 

# Trials that still don't have a match
still_unmatched_drks <- all_trials_de3 %>% 
    filter(is.na(drks_id),
           !(main_id %in% matches_all_drks1$main_id))

all_trials_de4 <- all_trials_de4 %>% 
    bind_rows(matches_all_drks1) %>% 
    bind_rows(still_unmatched_drks) 

all_trials_de4 %>% 
    count(main_id, wt = n()) %>% 
    arrange(desc(n))


#
# Bind rest of DRKS
#

drks_for_join <- all_info_drks %>% 
    filter(!(drks_id %in% na.omit(all_trials_de4$drks_id)))
colnames(drks_for_join)[-c(1,2)] <- paste0(colnames(drks_for_join)[-c(1, 2)], ".drks")
all_trials_de4 <- all_trials_de4 %>% 
    bind_rows(drks_for_join) 
stopifnot(sum(is.na(all_trials_de4$main_id)) == nrow(drks_for_join))
# If main_id is NA, use row number as main_id
new_ids <- (1:nrow(all_trials_de4))[is.na(all_trials_de4$main_id)]
stopifnot(sum(new_ids %in% all_trials_de4$main_id) == 0)
all_trials_de4$main_id[is.na(all_trials_de4$main_id)] <- new_ids
dim(all_trials_de4) # 36617 52

#
# Summarized variables that combine data from multiple registers can for
# example be created like this. We trust the AACT data most, so we start 
# from AACT here.
#

get_start_date <- function(x) {
    start_date <- x$start_date
    if (is.null(start_date) | all(is.na(start_date))) start_date <- unlist(x$start_date.drks)
    if (is.null(start_date) | all(is.na(start_date))) start_date <- unlist(x$start_date.ictrp)
    return(start_date)
}

all_trials_de4 <- all_trials_de4 %>% 
    rowwise() %>% 
    mutate(summarized_start_date = get_start_date(.data)) %>% 
    ungroup()

save(all_trials_de4, file = "all_trials_de4.RData")

