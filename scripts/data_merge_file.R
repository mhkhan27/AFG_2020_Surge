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
source("scripts/function/datamerge_from_rank_table2.R")


df <- read.csv("outputs/recoding/composite_indicators.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::filter(!is.na(received_aid_mark_2)) %>% dplyr::filter(consent == "yes")


ks<-readxl::read_xlsx(path = "tools/AFG2003a_Tool_1_HH_Survey_Kobo_Final_27052020v4.xlsx",sheet = "survey")
kc<-readxl::read_xlsx(path = "tools/AFG2003a_Tool_1_HH_Survey_Kobo_Final_27052020v4.xlsx",sheet = "choices")

df<-refactor_to_xlsform(data = df,kobo_survey = ks,kobo_choices = kc,label_column = "label::English" )
xls_lt<-make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::English")

select_one <- c("priority","no_shelter","heating_coping")

select_multiple <- c("shelter_repairs","shelter_needs_bad","nfi_items","nfi_needs_bad",
                     "debt_pay_how","debt_not_pay","aid_items","trade_items_which","cash_spend_how",
                     "cash_challenge","vendor_challenge_what","distribution_challenge","challenge_type")

# "shelter_concerns",

cols_multi2 <- as.character()
for (i in select_multiple) {
  
  cols_multi <- df %>% select(starts_with(paste0(i,"."))) %>% colnames() %>% dput
  cols_multi2 <- c(cols_multi,cols_multi2)
}
cols_to_analyze <- c(cols_multi2,select_one)

dfsvy<-svydesign(ids = ~1,data = df) #will adjust the weights later. 

data_merge_basic_analysis_strata_idp<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "strata_idp") 

# data_merge_basic_analysis_overall<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze) %>% 
  # mutate(strata_idp = "overall")


cols_for_subset <- c(select_one,select_multiple)

data_merge_basic_analysis <- list()
data_merge_ind<- list()

for (u in cols_for_subset) {
  col_temp<-u
  
for(i in 1: nrow(data_merge_basic_analysis_strata_idp)){

    
  strata_temp<-data_merge_basic_analysis_strata_idp %>% slice(i) %>% pull(strata_idp) %>% as.character() 
  
  basic_analysis_sub <- data_merge_basic_analysis_strata_idp %>% select(starts_with(col_temp))
  
  rank_table <- prepare_rank_table(basic_analysis_sub %>% slice(i))
  
  results<-datamerge_from_rank_table2(rank_table = rank_table,xlsform_lookup = xls_lt,
                                     rank_n = 5)
  data_merge_basic_analysis[[strata_temp]]<-results$wide %>% 
    mutate(strata=strata_temp) %>% 
    select(strata,everything())
}
data_merge_ind[[col_temp]]<-bind_rows(data_merge_basic_analysis)
}
data_merge <- bind_cols(data_merge_ind)

data_merge[is.na(data_merge)] <- 0 #covert NA to 0


# remove label which value is 0 -------------------------------------------

for (x in names(data_merge)) {
  row_num<-which(data_merge[[x]] == 0)
if(!is_empty(row_num)){
  col_name <- gsub("val","lab",x)
   data_merge[row_num,] [col_name] <- NA
}
}

write.csv(data_merge,"data_merge.csv",na = "")

