
#
# Funktionen für Fuzzy Matching
#

# String Similarity [0, 1]
lv_stringsim <- function(x, y) {
    require(stringdist)
    x <- tolower(x)
    y <- tolower(y)
    lx <- nchar(x)
    ly <- nchar(y)
    d <- stringdist(x, y, method = "lv")
    1 - d / pmax(lx, ly)
}

# Date similarity [0, 1]
datesim <- function(x, y) {
    d <- abs(x - y) 
    as.numeric(1 - d / (365*25))
}

# Sample Size Similarity [0, 1]
sizesim <- function(x, y) {
    pmin(x, y) / pmax(x, y)
}

clean_ids <- function(x, min_nchar = 6) {
    # Strings auch bei Leerzeichen splitten, z.B. für so etwas wie EudraCT number 2008-000132-40
    # Wörter wie "number", "EUDRACT:" oder "dated" entfernen
    # Generell nach dem Splitten per Leerzeichen kurze Zeichenketten entfernen, für z.B. V2.0
    x <- unlist(x)
    x <- unlist(str_split(x, pattern = "; "))
    x <- unlist(str_split(x, pattern = ";"))
    x <- unlist(str_split(x, pattern = " "))
    x <- x[nchar(x) >= min_nchar]
    x <- tolower(x)
    x <- str_replace(pattern = "euctr", replacement = "", string = x)
    x <- str_replace(pattern = "eudract", replacement = "", string = x)
    x <- str_replace(pattern = "dated", replacement = "", string = x)
    x <- str_replace(pattern = "number", replacement = "", string = x)
    x <- str_replace(pattern = "\\(at\\)", replacement = "@", string = x)
    x <- str_replace(x, pattern = "-[a-z]{2}$", replacement = "")
    return(x)
}

check_dup_ids <- function(x1, x2, y1, y2) {
    x1 <- clean_ids(x1)
    x2 <- clean_ids(x2)
    y1 <- clean_ids(y1)
    y2 <- clean_ids(y2)
    any(c(x1, x2) %in% c(y1, y2))
}

check_dup_ids2 <- function(x, y) {
    x <- clean_ids(x)
    y <- clean_ids(y)
    any(x %in% y)
}

check_utn <- function(x, y) {
    x <- clean_ids(x)
    y <- clean_ids(y)
    matched_ids <- x[which(x %in% y)]
    is_utn <- grepl(pattern = "^u[0-9]{4}-", x = matched_ids)
    if (sum(is_utn) == 1) {
        return(TRUE)
    } else if (sum(is_utn) == 0) {
        return(FALSE)
    } else {
        stop("More than one UTN match found.")
    }
}

sum_dup_ids <- function(x, y, min_nchar) {
    x <- clean_ids(x, min_nchar = min_nchar)
    y <- clean_ids(y, min_nchar = min_nchar)
    sum(unique(x) %in% unique(y), na.rm = TRUE)
}

calc_best_stringsim <- function(x, y) {
    x <- unlist(x)
    x <- tolower(x)
    x <- str_replace(pattern = "\\(at\\)", replacement = "@", string = x)
    x <- x[nchar(x) >= 6]
    y <- unlist(y)
    y <- tolower(y)
    y <- str_replace(pattern = "\\(at\\)", replacement = "@", string = y)
    y <- y[nchar(y) >= 6]
    dat <- expand.grid(x, y) %>% 
        rowwise() %>% 
        mutate(stringsim = lv_stringsim(Var1, Var2)) %>% 
        drop_na()
    if (nrow(dat) == 0) return(0)
    max(dat$stringsim, na.rm = TRUE)
}

calc_similarities <- function(data1, data2, id_var1, id_var2, data2_suffix) {
    plan(multiprocess)
    future_map_dfr(1:nrow(data1), .progress = T, .f = function(i) {
        tempdat <- data1[i, ]
        string_sim <- lv_stringsim(rep(tempdat$title, nrow(data2)),
                                          data2[, paste0("title", data2_suffix)])
        start_date_sim <- datesim(rep(tempdat$start_date, nrow(data2)),
                                         data2[, paste0("start_date", data2_suffix)])
        size_sim <- sizesim(rep(tempdat$n, nrow(data2)),
                                   data2[, paste0("n", data2_suffix)])
        result <- tibble(id = tempdat[[id_var]],
                         id_candidate = data2[[id_var]],
                         string_sim = round(string_sim, 4),
                         start_date_sim = round(start_date_sim, 4),
                         size_sim = round(size_sim, 4))
        result <- result %>%
            filter(string_sim > 0.4,
                   start_date_sim > 0.97,
                   size_sim > 0.5)
        return(result)
    })
}


# add_predictors <- function(similarities, data1, data2, data1_id_var, data2_id_var,
#                            data1_vars, data2_vars, data1_sec_id_var, data2_sec_id_var,
#                            data1_suffix, data2_suffix) {
#     matched <- similarities %>% 
#         left_join(data1 %>% 
#                       select(unique(c(data1_id_var, data1_vars))),
#                   by = data1_id_var) %>% 
#         left_join(data2 %>% 
#                       select(unique(c(data2_id_var, data2_vars))),
#                   by = data2_id_var) 
#     matched <- matched %>% 
#         rowwise() %>% 
#         mutate(!!data1_sec_id_var := paste(!!rlang::sym(data1_sec_id_var), collapse = "; "),
#                !!data2_sec_id_var := paste(!!rlang::sym(data2_sec_id_var), collapse = "; ")) %>% 
#         ungroup()
#     matched <- matched %>% 
#         mutate(string_similarities_off_off = lv_stringsim(matched[[paste0("official_title", data1_suffix)]],
#                                                            matched[[paste0("official_title", data2_suffix)]]),
#                string_similarities_b_off = lv_stringsim(matched[[paste0("title", data1_suffix)]],
#                                                           matched[[paste0("official_title", data2_suffix)]]),
#                string_similarities_off_b = lv_stringsim(matched[[paste0("title", data2_suffix)]],
#                                                           matched[[paste0("official_title", data1_suffix)]]))
#     matched <- matched %>% 
#         rowwise() %>% 
#         mutate(best_stringdist_titles = max(as.numeric(string_similarities), 
#                                             as.numeric(string_similarities_off_off),
#                                             as.numeric(string_similarities_b_off),
#                                             as.numeric(string_similarities_off_b), na.rm = T),
#                # any_matched_id = check_dup_ids(matched[[data1_id_var]], 
#                #                                matched[[data1_sec_id_var]],
#                #                                matched[[data2_id_var]],
#                #                                matched[[data2_sec_id_var]]),
#                # any_matched_utn = check_utn(matched[[data2_sec_id_var]],
#                #                             matched[[data1_sec_id_var]]),
#                # sum_matched_name_parts = sum_dup_ids(matched[[paste0("name", data1_suffix)]],
#                #                                      matched[[paste0("name", data2_suffix)]],
#                #                                      min_nchar = 4),
#                # best_stringdist_name = calc_best_stringdist(matched[[paste0("name", data1_suffix)]], 
#                #                                             matched[[paste0("name", data2_suffix)]]),
#                # any_matched_email = check_dup_ids2(matched[[paste0("email_central", data1_suffix)]],
#                #                                    matched[[paste0("email_central", data2_suffix)]]),
#                # best_stringdist_email = calc_best_stringdist(matched[[paste0("email_central", data1_suffix)]],
#                #                                              matched[[paste0("email_central", data2_suffix)]]),
#                # matching_type = matched[[paste0("study_type", data1_suffix)]] == 
#                #     matched[[paste0("study_type", data2_suffix)]])
#                any_matched_id = check_dup_ids(!!rlang::sym(data1_id_var),
#                                               !!rlang::sym(data1_sec_id_var),
#                                               !!rlang::sym(data2_id_var),
#                                               !!rlang::sym(data2_sec_id_var)),
#                # any_matched_utn = check_utn(!!rlang::sym(data2_sec_id_var),
#                #                             !!rlang::sym(data1_sec_id_var)),
#                sum_matched_name_parts = sum_dup_ids(!!rlang::sym(paste0("name", data1_suffix)),
#                                                     !!rlang::sym(paste0("name", data2_suffix)),
#                                                     min_nchar = 4),
#                best_stringdist_name = calc_best_stringdist(!!rlang::sym(paste0("name", data1_suffix)),
#                                                            !!rlang::sym(paste0("name", data2_suffix))),
#                any_matched_email = check_dup_ids2(!!rlang::sym(paste0("email_central", data1_suffix)),
#                                                   !!rlang::sym(paste0("email_central", data2_suffix))),
#                best_stringdist_email = calc_best_stringdist(!!rlang::sym(paste0("email_central", data1_suffix)),
#                                                             !!rlang::sym(paste0("email_central", data2_suffix))),
#                matching_type = !!rlang::sym(paste0("study_type", data1_suffix)) ==
#                    !!rlang::sym(paste0("study_type", data2_suffix))
#         )
#     matched$any_matched_id <- as.factor(matched$any_matched_id)
#     matched$any_matched_email <- as.factor(matched$any_matched_email)
#     return(matched)
# }

