
library(xml2)
library(tidyverse)
library(furrr)
library(beepr)
library(lubridate)

#
# Adjust these paths
#

aact_path <- "AACT_2019-10-11/"
drks_path <- "DRKS/"

#
# Load AACT
#

aact_sponsors <- read_delim(paste0(aact_path, "sponsors.txt"), delim = "|")
aact <- read_delim(paste0(aact_path, "studies.txt"), delim = "|")
aact_countries <- read_delim(paste0(aact_path, "countries.txt"), delim = "|")
aact_id_info <- read_delim(paste0(aact_path, "id_information.txt"), delim = "|")

#
# AACT locations
#

# locations are in location_countries. If a study has multiple locations, it 
# appears multiple times in the table
aact_countries <- aact_countries %>%
    group_by(nct_id) %>% 
    summarise(countries = list(name))
aact <- full_join(aact, aact_countries, by = "nct_id")
aact$countries[aact$countries == "NULL"] <- NA

#
# AACT dates
#

ger_in_aact <- map_lgl(aact$countries, function(x) "germany" %in% tolower(unlist(x)))

aact %>% 
    filter(ger_in_aact) %>% 
    arrange(start_date) %>% 
    mutate(year = year(start_date)) %>% 
    group_by(year) %>% 
    summarise(n = n())

#
# AACT Secondary IDs
#

aact_id_info <- aact_id_info %>% 
    filter(id_type == "secondary_id") %>%
    group_by(nct_id) %>%
    summarise(id_value = list(id_value))

aact_de <- aact %>% 
    filter(ger_in_aact) %>% 
    left_join(aact_id_info)
dim(aact_de) # 20148 66

# 
# AACT sponsors
#

# Make it one row per nct_id via nesting
aact_sponsors <- aact_sponsors %>% 
    group_by(nct_id) %>% 
    summarise(agency_class = list(agency_class),
              lead_or_collaborator = list(lead_or_collaborator),
              name = list(name))
aact_de <- aact_de %>% 
    left_join(aact_sponsors, by = "nct_id")

#
# Contacts
#

central_cont <- read_delim("AACT_2019-10-11/central_contacts.txt", delim = "|")
central_cont <- central_cont %>%
    rename(email_central = email) %>% 
    select(nct_id, email_central) %>% 
    group_by(nct_id) %>% 
    summarize(email_central = list(email_central))
aact_de <- aact_de %>% 
    left_join(central_cont)

result_cont <- read_delim("AACT_2019-10-11/result_contacts.txt", delim = "|")
result_cont <- result_cont %>%
    rename(email_result = email) %>% 
    select(nct_id, email_result) %>% 
    group_by(nct_id) %>% 
    summarize(email_result = list(email_result))
aact_de <- aact_de %>% 
    left_join(result_cont)

# 
# Result references
#
aact_references <- read_delim("AACT_2019-10-11/study_references.txt", delim = "|")
aact_references <- aact_references %>% 
    select(nct_id, pmid, reference_type, citation) %>% 
    filter(reference_type == "results_reference") %>%
    select(-reference_type) %>% 
    group_by(nct_id) %>% 
    summarise(pmid = list(pmid),
              references = list(citation))
aact_de <- left_join(aact_de, aact_references, by = "nct_id")

#
# Primary and secondary outcomes
#
# Design_Outcomes contains information about the outcomes to be measured and 
# Outcomes contains info about the actual outcomes reported when the study completed.
# see https://aact.ctti-clinicaltrials.org/schema

aact_design_outcomes <- read_delim(paste0(aact_path, "design_outcomes.txt"), delim = "|")
aact_design_outcomes <- aact_design_outcomes %>% 
    select(nct_id, outcome_type, measure, time_frame, population) %>% 
    group_by(nct_id) %>% 
    summarise(prim_outcome = paste(measure[outcome_type == "primary"], collapse = ";"),
              sec_outcome = paste(measure[outcome_type == "secondary"], collapse = ";"))
aact_de <- left_join(aact_de, aact_design_outcomes, by = "nct_id")

# Save result 
save(aact_de, file = "aact_de.RData")


#
# DRKS
#


# Extract info from all studies
get_info_drks <- function(xml_path, folder) {
    stud <- read_xml(paste0(folder, xml_path))
    lstud <- as_list(stud)
    # ID 
    ID <- unlist(lstud$trial$drksId)
    # Possible secondary IDs
    if (length(lstud$trial$secondaryIds) > 0) {
        sec_IDs <- map(lstud$trial$secondaryIds, function(x) attr(x, "id"))
        sec_IDs <- unlist(sec_IDs)
    } else {
        sec_IDs <- NA
    }
    if (length(lstud$trial$recruitmentCountries) > 0) {
        locale_countries <- map_chr(lstud$trial$recruitmentCountries[[1]]$contents, 
                                    function(x) unlist(attributes(x)))
        stopifnot("en" %in% locale_countries)
        locale_index <- which(locale_countries == "en")
        recruiting_countries = try(map_chr(lstud$trial$recruitmentCountries,
                                           function(x) x$contents[[locale_index]][[1]]),
                                   silent = T)
        if (length(recruiting_countries) == 0 | "try-error" %in% class(recruiting_countries)) {
            recruiting_countries <- NA
        } else {
            recruiting_countries <- list(recruiting_countries)
        }
    } else {
        print(ID)
        print("No recruiting countries")
        print("---")
        recruiting_countries <- NA
    }
    # Primary and secondary endpoints
    if (length(lstud$trial$study$primaryEndpoint) > 0) {
        locale_outcomes <- map_chr(lstud$trial$study$primaryEndpoint$contents, 
                                    function(x) unlist(attributes(x)))
        if(!("en" %in% locale_outcomes)) {
            locale_index <- which(locale_outcomes == "de")
        } else {
            locale_index <- which(locale_outcomes == "en")
        }
        locale_index <- which(locale_outcomes == "en")
        prim_outcomes = try(map_chr(lstud$trial$study$primaryEndpoint,
                                           function(x) unlist(x[[locale_index]][[1]])),
                                   silent = T)
        if (length(prim_outcomes) == 0 | "try-error" %in% class(prim_outcomes)) {
            prim_outcomes <- NA
        } else {
            prim_outcomes <- prim_outcomes
        }
    } else {
        print(ID)
        print("No primary endpoints")
        print("---")
        prim_outcomes <- NA
    }
    # secondary:
    if (length(lstud$trial$study$secondaryEndpoints) > 0) {
        locale_outcomes <- map_chr(lstud$trial$study$secondaryEndpoints$contents, 
                                    function(x) unlist(attributes(x)))
        if(!("en" %in% locale_outcomes)) {
            locale_index <- which(locale_outcomes == "de")
        } else {
            locale_index <- which(locale_outcomes == "en")
        }
        sec_outcomes = try(map_chr(lstud$trial$study$secondaryEndpoints,
                                           function(x) unlist(x[[locale_index]][[1]])),
                                   silent = T)
        if (length(sec_outcomes) == 0 | "try-error" %in% class(sec_outcomes)) {
            sec_outcomes <- NA
        } else {
            sec_outcomes <- sec_outcomes
        }
    } else {
        sec_outcomes <- NA
    }
    title_en <- tail(unlist(lstud$trial$description$title$contents), 1)
    schedule <- unlist(lstud$trial$recruitement$schedule)
    if (is.null(schedule)) schedule <- NA else schedule <- as.Date(schedule)
    deadline <- unlist(lstud$trial$recruitement$deadline)
    if (is.null(deadline)) deadline <- NA else deadline <- as.Date(deadline)
    created_at <- unlist(lstud$trial$firstDrksPublishDate)
    if (is.null(created_at)) created_at <- NA else created_at <- as.Date(created_at)
    date_type <- unlist(lstud$trial$recruitement$running$contents$localizedContent)
    if (is.null(date_type)) date_type <- NA 
    date_type[date_type == "Geplant"] <- "Planned"
    date_type[date_type == "Tatsächlich"] <- "Actual"
    overall_status <- unlist(lstud$trial$recruitement$status$state)
    if (is.null(overall_status)) overall_status <- NA 
    study_type <- unlist(lstud$trial$study$type$contents$localizedContent)
    if (is.null(study_type)) study_type <- NA 
    # Get primary sponsor
    attrib <- map_chr(lstud$trial$addresses, function(x) attributes(x)$type)
    i_primary <- which(tolower(attrib) == "primary-sponsor")
    if (length(i_primary) < 1) {
        name <- NA
    } else {
        name <- map(i_primary, function(i) unlist(lstud$trial$addresses[[i]]$affiliation))
    }
    # Get email for scientific enquiries
    attrib <- map_chr(lstud$trial$addresses, function(x) attributes(x)$type)
    i_scientific <- which(tolower(attrib) == "scientific-contact")
    if (length(i_scientific) < 1) {
        email_central <- NA
    } else {
        email_central <- map(i_scientific, function(i) unlist(lstud$trial$addresses[[i]]$email))
    }
    #IIT
    iit <- unlist(lstud$trial$investorInitiated)
    stopifnot(length(iit) < 2)
    if (is.null(iit)) iit <- NA
    # References
    ref <- unlist(lstud$trial$publications$publication$value$contents$localizedContent)
    if (is.null(ref)) ref <- NA
    # Size
    target_size <- unlist(lstud$trial$recruitement$targetSize)
    if (is.null(target_size)) target_size <- NA
    
    tibble(drks_id = ID, 
           sec_ID = list(sec_IDs),
           title_en, 
           prim_outcomes,
           sec_outcomes,
           countries = recruiting_countries, 
           start_date = schedule, 
           completion_date = deadline, 
           n = target_size,
           overall_status,
           study_type,
           name = list(unlist(name)),
           email_central = list(unlist(email_central)),
           created_at,
           date_type,
           iit = iit,
           references = ref)
}

studies <- list.files(drks_path, pattern = "xml")

plan(multiprocess)
all_info_drks <- future_map_dfr(studies, function(x) {
    res <- get_info_drks(x, folder = drks_path)
    stopifnot(nrow(res) == 1)
    return(res)
}, .progress = TRUE)
null_countries <- which(map_lgl(all_info_drks$countries, is.null))
all_info_drks$countries[null_countries] <- NA
null_name <- which(map_lgl(all_info_drks$name, is.null))
all_info_drks$name[null_name] <- NA
all_info_drks$n <- as.numeric(all_info_drks$n)

#
# Save
#

save(all_info_drks, file = "DRKS/all_info_drks.RData")

#
# WHO (ICTRP)
#

#
# Load Data 
#

### Adjust this path ###
ictrp <- read_xml("WHO/ICTRP-Results_2019-10-11.xml")
###

ictrp <- as_list(ictrp)

ictrp_dat <- map_dfr(ictrp$Trials_downloaded_from_ICTRP, function(x) {
    if (length(unlist(x$Secondary_ID)) < 1) {
        sec_id <- NA
    } else {
        sec_id <- (str_split(unlist(x$Secondary_ID), ";"))
    }
    if (length(unlist(x$Public_title)) < 1) {
        brief_title <- NA
    } else {
        brief_title <- unlist(x$Public_title)
    }
    if (length(unlist(x$Scientific_title)) < 1) {
        official_title <- NA
    } else {
        official_title <- unlist(x$Scientific_title)
    }
    if (length(unlist(x$Primary_outcome)) < 1) {
        prim_outcome <- NA
    } else {
        prim_outcome <- unlist(x$Primary_outcome)
        prim_outcome <- gsub(pattern = "<.*?>", replacement = ";", x = prim_outcome)
        prim_outcome <- gsub(pattern = ";+", replacement = ";", x = prim_outcome)
    }
    if (length(unlist(x$Secondary_outcome)) < 1) {
        sec_outcome <- NA
    } else {
        sec_outcome <- unlist(x$Secondary_outcome)
        sec_outcome <- gsub(pattern = "<.*?>", replacement = ";", x = sec_outcome)
        sec_outcome <- gsub(pattern = ";+", replacement = ";", x = sec_outcome)
    }
    if (length(unlist(x$Countries)) < 1) {
        countries <- NA
    } else {
        countries <- unname(unlist(strsplit(unlist(x$Countries), split = ";")))
    }
    if (length(unlist(x$Primary_sponsor)) < 1) {
        sponsor <- NA
    } else {
        sponsor <- unlist(x$Primary_sponsor)
    }
    if (length(unlist(x$Study_type)) < 1) {
        study_type <- NA
    } else {
        study_type <- unlist(x$Study_type)
    }
    if (length(unlist(x$Recruitment_Status)) < 1) {
        overall_status <- NA
    } else {
        overall_status <- unlist(x$Recruitment_Status)
    }
    if (length(unlist(x$Contact_Email)) < 1) {
        email <- NA
    } else {
        email <- unlist(x$Contact_Email)
    }
    if (length(unlist(x$Target_size)) < 1) {
        n <- NA
    } else {
        n <- as.numeric(unlist(x$Target_size))
    }
    return(tibble(ID = unlist(x$TrialID), 
                  sec_id_ictrp = sec_id,
                  brief_title = brief_title,
                  official_title = official_title,
                  prim_outcome = prim_outcome,
                  sec_outcome = sec_outcome,
                  countries = list(unique(unlist(countries))),
                  study_type = study_type,
                  overall_status = overall_status,
                  n = n,
                  name = sponsor,
                  email_central = email))
})

ictrp_in_de <- map2_lgl(ictrp_dat$ID, ictrp_dat$countries, function(id, loc) {
    loc <- tolower(loc)
    if (any(c("deutschland", "germany") %in% loc)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
})
ictrp_ids_de <- ictrp_dat$ID[ictrp_in_de]
ictrp_in_aact <- which(ictrp_ids_de %in% aact$nct_id)
drks_ids <- list.files(drks_path)
drks_ids <- gsub(pattern = ".xml", replacement = "", x = drks_ids)
ictrp_in_drks <- which(ictrp_ids_de %in% drks_ids)
ictrp_ids_de_add <- ictrp_ids_de[-c(ictrp_in_aact, ictrp_in_drks)]

#
# WHO time
#
ictrp_time <- map_dfr(ictrp$Trials_downloaded_from_ICTRP, function(x) {
    # start_date is in Date_enrollement [sic]
    if (length(unlist(x$Date_enrollement)) < 1) {
        enrollment <- NA
    } else {
        enrollment <- as.Date(unlist(x$Date_enrollement), format = "%d/%m/%Y")
        if (is.na(enrollment)) {
            # Try different scheme, e.g. for NCT (October 7, 2012)
            if (grepl(pattern = "[a-z]", x = x$Date_enrollement)) {
                enrollment <- mdy(unlist(x$Date_enrollement), quiet = T)
            } else {
                enrollment <- dmy(unlist(x$Date_enrollement), quiet = T)
                if (is.na(enrollment)) enrollment <- ymd(x$Date_enrollement, quiet = T)
            }
        }
    }
    created_at <- as.Date(unlist(x$Date_registration), format = "%d/%m/%Y")
    if (is.na(created_at)) {
        # Try different scheme, e.g. for NCT (October 7, 2012)
        if (grepl(pattern = "[a-z]", x = x$Date_registration)) {
            created_at <- mdy(unlist(x$Date_registration), quiet = T)
        } else {
            created_at <- dmy(unlist(x$Date_registration), quiet = T)
            if (is.na(created_at)) created_at <- ymd(x$Date_registration, quiet = T)
        }
    }
    return(tibble(ID = unlist(x$TrialID), 
                  start_date = enrollment,
                  created_at = created_at,
                  prospective = unlist(x$Prospective_registration)))
})

ictrp_de <- tibble(ictrp_id = ictrp_ids_de) %>% 
    left_join(ictrp_dat, by = c("ictrp_id" = "ID")) %>% 
    left_join(ictrp_time, by = c("ictrp_id" = "ID")) 

interv_strings <- c("Intervention",
                    "interventional",
                    "Interventional", 
                    "Interventional clinical trial of medicinal product", 
                    "Interventional study")
observ_strings <- c("observational",
                    "Observational", 
                    "Observational [Patient Registry]")
other_strings <- c("Expanded Access", "Other", "PMS")
ictrp_de$study_type[ictrp_de$study_type %in% interv_strings] <- "Interventional"
ictrp_de$study_type[ictrp_de$study_type %in% observ_strings] <- "Observational"
ictrp_de$study_type[ictrp_de$study_type %in% other_strings] <- "Other"

#
# Combine EUCTR-Trials that differ only in the country suffix
#

euctr <- ictrp_de %>% 
    filter(str_detect(ictrp_id, "^EUCTR")) %>% 
    mutate(ictrp_id = str_replace(ictrp_id, pattern = "-[A-Z]*$", replacement = "")) 
euctr_counts <- euctr %>% 
    count(ictrp_id, name = "n_euctr_ids")
euctr <- left_join(euctr, euctr_counts)
summarise_strings <- function(x) {
    x <- unlist(x)
    if (all(is.na(x))) return("NA")
    if (all(is.null(x))) return("NA")
    return(names(sort(table(na.omit(x)), decreasing = TRUE)[1]))
}
euctr_compressed <- euctr %>% 
    arrange(ictrp_id) %>% 
    group_by(ictrp_id) %>% 
    summarise(sec_id_ictrp = list(unique(unlist(sec_id_ictrp))),
              brief_title = summarise_strings(brief_title),
              official_title = summarise_strings(official_title),
              prim_outcome = summarise_strings(prim_outcome),
              sec_outcome = summarise_strings(sec_outcome),
              countries = list(unique(unlist(countries))),
              study_type = summarise_strings(study_type),
              overall_status = summarise_strings(overall_status),
              n = median(n),
              name = summarise_strings(name),
              email_central = summarise_strings(email_central),
              start_date = median(start_date),
              created_at = median(created_at),
              prospective = summarise_strings(prospective),
              n_euctr_ids = unique(n_euctr_ids))

# Attach these to ICTRP
ictrp_de <- ictrp_de %>% 
    filter(!str_detect(ictrp_id, "^EUCTR")) %>% 
    mutate(n_euctr_ids = 1) %>% 
    bind_rows(euctr_compressed)
ictrp_de$brief_title[ictrp_de$brief_title == "NA"] <- NA
ictrp_de$official_title[ictrp_de$official_title == "NA"] <- NA
ictrp_de$prim_outcome[ictrp_de$prim_outcome == "NA"] <- NA
ictrp_de$sec_outcome[ictrp_de$sec_outcome == "NA"] <- NA
ictrp_de$countries[ictrp_de$countries == "NA"] <- NA
ictrp_de$study_type[ictrp_de$study_type == "NA"] <- NA
ictrp_de$overall_status[ictrp_de$overall_status == "NA"] <- NA
ictrp_de$name[ictrp_de$name == "NA"] <- NA
ictrp_de$email_central[ictrp_de$email_central == "NA"] <- NA
ictrp_de$prospective[ictrp_de$prospective == "NA"] <- NA

interv_strings <- c("Intervention",
                    "interventional",
                    "Interventional", 
                    "Interventional clinical trial of medicinal product", 
                    "Interventional study")
observ_strings <- c("observational",
                    "Observational", 
                    "Observational [Patient Registry]")
other_strings <- c("Expanded Access", "Other", "PMS")
ictrp_de$study_type[ictrp_de$study_type %in% interv_strings] <- "Interventional"
ictrp_de$study_type[ictrp_de$study_type %in% observ_strings] <- "Observational"
ictrp_de$study_type[ictrp_de$study_type %in% other_strings] <- "Other"

#
# Save
#

save(ictrp_de, file = "WHO/ictrp_de.RData")
