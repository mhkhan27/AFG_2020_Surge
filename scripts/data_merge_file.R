#disaggregated by strata_idp

#columns needs to calculate top 5 
# strata_idp # priority # no_shelter # shelter_concerns # shelter_repairs
# shelter_needs_bad # nfi_items # nfi_needs_bad # heating_coping # debt_pay_how # debt_not_pay
# aid_items # trade_items_which # cash_spend_how # cash_challenge # vendor_challenge_what
# distribution_challenge # challenge_type


rm(list = ls())
# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(tidyverse)
library(readr)
library(stringr)
library(srvyr)
library(survey)
library(readxl)
library(matrixStats)
source("scripts/function/utils.R")


df <- read.csv("outputs/recoding/composite_indicators.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::filter(!is.na(received_aid_mark_2)) %>% dplyr::filter(consent == "yes")


ks<-readxl::read_xlsx(path = "tools/AFG2003a_Tool_1_HH_Survey_Kobo_Final_27052020v4.xlsx",sheet = "survey")
kc<-readxl::read_xlsx(path = "tools/AFG2003a_Tool_1_HH_Survey_Kobo_Final_27052020v4.xlsx",sheet = "choices")

df<-refactor_to_xlsform(data = df,kobo_survey = ks,kobo_choices = kc,label_column = "label::English" )
xls_lt<-butteR::make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::English")

select_one <- c("priority","no_shelter","heating_coping")

select_multiple <- c("shelter_concerns","shelter_repairs","shelter_needs_bad","nfi_items","nfi_needs_bad",
                     "debt_pay_how","debt_not_pay","aid_items","trade_items_which","cash_spend_how",
                     "cash_challenge","vendor_challenge_what","distribution_challenge","challenge_type")



cols_multi2 <- as.character()
for (i in select_multiple) {
  
  cols_multi <- df %>% select(starts_with(paste0(i,"."))) %>% colnames() %>% dput
  cols_multi2 <- c(cols_multi,cols_multi2)
}
cols_to_analyze <- c(cols_multi2,select_one)

dfsvy<-svydesign(ids = ~1,data = df) #will adjust the weights later. 

data_merge_basic_analysis_strata_idp<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "strata_idp") 

data_merge_basic_analysis_overall<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze) %>% 
  mutate(strata_idp = "overall")

data_merge_basic_analysis <- list()
cols_for_subset <- c(select_one,select_multiple)

for (i in cols_for_subset) {
  print(i)
  basic_analysis_sub <- data_merge_basic_analysis_overall %>% select(starts_with(paste0(i)))
  rank_table <- prepare_rank_table(basic_analysis_sub)
  data_merge_basic_analysis[[i]] <-datamerge_from_rank_table(rank_table = rank_table,xlsform_lookup = xls_lt,rank_n = 5)
}

data_merge_top5 <- do.call("bind_cols",data_merge_basic_analysis)
