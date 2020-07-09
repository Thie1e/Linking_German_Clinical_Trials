
library(tidyverse)
library(stringdist)
library(lubridate)
library(furrr)
library(randomForest)
library(caret)
library(doSNOW)

source("fuzzy_functions.R")


#
# AACT
#

load("AACT_2019-10-11/aact_de.RData")

#
# Select and filter
#

aact_de %>% 
    count(nct_id) %>%
    filter(n > 1) 
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
           # completion_date fehlt in diesen Daten
           study_type, countries, 
           # agency_class fehlt in diesen Daten
           name, overall_status, email_central, n_euctr_ids) %>% 
    rename(title = brief_title) %>% 
    filter(!is.na(n), !is.na(start_date), start_date > "1990-01-01", 
           !is.na(title))


#
# Join with AACT
#

# 1. Join nct1 + who1
all_trials_de1 <- all_trials_de %>% 
    left_join(ictrp_de %>% mutate(ictrp_id_for_join = ictrp_id), 
              by = c("nct_id" = "ictrp_id_for_join"), 
              suffix = c("", ".ictrp"))

#
# Join by Random Forest
#

ictrp_de2 <- ictrp_de %>% 
    filter(!(ictrp_id %in% all_trials_de1$nct_id)) 
colnames(ictrp_de2)[-(1:2)] <- paste0(colnames(ictrp_de2)[-(1:2)], ".ictrp")

unmatched_all_trials_de1 <- all_trials_de1[is.na(all_trials_de1$ictrp_id), ] %>% 
    select(-(ictrp_id:n_euctr_ids))

#
# Create Similarity data
#

# The already labeled data set is in rfdat.RData
load("rfdat.RData")

cl <- makeCluster(parallel::detectCores() - 1)
registerDoSNOW(cl)
set.seed(2019)
# A few NAs -> Median Impute
mod <- train(y = ifelse(rfdat$is_duplicate == 1, "true", "false"),
             x = rfdat %>%
                 select(best_stringsim_titles,
                        string_similarities_off_off,
                        string_similarities_b_off,
                        string_similarities_off_b,
                        string_similarities,
                        any_matched_id,
                        any_matched_utn,
                        size_similarities,
                        start_date_similarities,
                        sum_matched_name_parts,
                        best_stringsim_name,
                        any_matched_email,
                        matching_type,
                        perc_matched_countries) %>%
                 as.data.frame(),
             method = "rf", ntree = 20000,
             tuneGrid = data.frame(mtry = 5),
             preProcess = c("medianImpute"),
             trControl = trainControl(method = "repeatedcv", number = 10, 
                                      repeats = 10, allowParallel = T,
                                      classProbs = T,
                                      savePredictions = T))
mod

varImpPlot(mod$finalModel)

confusionMatrix(data = mod$pred %>% 
                    filter(mtry == 5) %>% 
                    pull(pred), 
                reference = mod$pred %>% 
                    filter(mtry == 5) %>% 
                    pull(obs), 
                positive = "true",
                mode = "everything")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction false true
# false  1079   53
# true     61 4677
# 
# Accuracy : 0.9806         
# 95% CI : (0.9767, 0.984)
# No Information Rate : 0.8058         
# P-Value [Acc > NIR] : <2e-16         
# 
# Kappa : 0.9378         
# 
# Mcnemar's Test P-Value : 0.5121         
#                                          
#             Sensitivity : 0.9888         
#             Specificity : 0.9465         
#          Pos Pred Value : 0.9871         
#          Neg Pred Value : 0.9532         
#               Precision : 0.9871         
#                  Recall : 0.9888         
#                      F1 : 0.9880         
#              Prevalence : 0.8058         
#          Detection Rate : 0.7968         
#    Detection Prevalence : 0.8072         
#       Balanced Accuracy : 0.9676         
#                                          
#        'Positive' Class : true     


rfdat$preds <- predict(mod, newdata = rfdat)
# In-sample no errors
rfdat %>% 
    filter(preds == 1 & is_duplicate == 0)
rfdat %>% 
    filter(preds == 0 & is_duplicate == 1)

# Out-of-sample predictions in Cross-Validation
set.seed(2019)
mod_cv1 <- train(y = ifelse(rfdat$is_duplicate == 1, "true", "false"),
              x = rfdat %>%
                  select(best_stringsim_titles,
                      n,
                      string_similarities_off_off, string_similarities_b_off,
                      string_similarities_off_b, string_similarities,
                      any_matched_id, any_matched_utn,
                      size_similarities, start_date_similarities,
                      sum_matched_name_parts, best_stringsim_name,
                      any_matched_email, best_stringsim_email,
                      perc_matched_countries) %>% 
                  as.data.frame(),
              method = "rf", ntree = 10000,
              tuneGrid = data.frame(mtry = 5),
              preProcess = c("medianImpute"),
             trControl = trainControl(method = "cv", number = 10, 
                                      allowParallel = T,
                                      classProbs = T,
                                      savePredictions = T))
mod_cv1
# Random Forest 
# 
# 587 samples
# 15 predictor
# 2 classes: 'false', 'true' 
# 
# Pre-processing: median imputation (12), ignore (3) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 528, 529, 529, 529, 527, 528, ... 
# Resampling results:
#     
#     Accuracy   Kappa    
# 0.9796016  0.9326288
# 
# Tuning parameter 'mtry' was held constant at a value of 5


confusionMatrix(data = mod_cv1$pred$pred, 
                reference = mod_cv1$pred$obs,
                positive = "true",
                mode = "everything")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction false true
# false   107    5
# true      7  468
# 
# Accuracy : 0.9796          
# 95% CI : (0.9646, 0.9894)
# No Information Rate : 0.8058          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.9342          
# 
# Mcnemar's Test P-Value : 0.7728          
#                                           
#             Sensitivity : 0.9894          
#             Specificity : 0.9386          
#          Pos Pred Value : 0.9853          
#          Neg Pred Value : 0.9554          
#               Precision : 0.9853          
#                  Recall : 0.9894          
#                      F1 : 0.9873          
#              Prevalence : 0.8058          
#          Detection Rate : 0.7973          
#    Detection Prevalence : 0.8092          
#       Balanced Accuracy : 0.9640          
#                                           
#        'Positive' Class : true        


# 
# save model
#

save(mod, file = "mod.RData")
