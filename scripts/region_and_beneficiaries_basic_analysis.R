rm(list = ls())
# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(stringr)
library(srvyr)
library(survey)

type_of_analysis <- c("region_and_beneficiaries","region_and_modality","displacement")[3]

# read_data ---------------------------------------------------------------

df <- read.csv("outputs/recoding/composite_indicators.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::filter(!is.na(received_aid_mark_2)) %>% dplyr::filter(consent == "yes") #filter data

region <-read.csv("dap/unhcr_hh/Region.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::select(c("Name","Region..name.")) #region data 

df_with_regions <- df %>% left_join(region,by =c ("province"="Name"))

population <-read.csv("dap/unhcr_hh/population.csv",na.strings = c(""," ",NA),stringsAsFactors = F) #population data full

# region and beneficiaries ----------------------------------------------
  
pop <- population %>% dplyr::filter(type == "non_beneficiary" | type== "beneficiary" )
  
pop <- pop%>% 
    mutate(
      pop_global=sum(population),
      strata.region_recieved_aid  = paste0(pop$strata,"_",pop$type))
  
  sf_pop<- "population"
  displacement_type <- c("host","idp")
  
  df_for_grop_analysis <-  df_with_regions %>% dplyr::filter(list_displacement %in% displacement_type |
                                                               received_aid_mark_2 == "no") %>% mutate(
                                                                 benefeciaries_non_benefeciaries = if_else(received_aid_mark_2 == "yes","beneficiary",
                                                                                                           "non_beneficiary")) %>% mutate(
                                                                                                             strata.region_recieved_aid  = paste0(Region..name.,"_",benefeciaries_non_benefeciaries)
                                                                                                           )
  
  
  sf_with_weights<- df_for_grop_analysis %>% 
    group_by(Region..name.,benefeciaries_non_benefeciaries) %>% 
    summarise(sample_strata_num=n()) %>% as.data.frame() %>% mutate(
      strata.region_recieved_aid  = paste0(Region..name.,"_",benefeciaries_non_benefeciaries)
    ) %>% 
    right_join(pop,by = c("strata.region_recieved_aid"))%>% mutate(
      sample_global = sum(sample_strata_num),
      survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
    ) %>% select(c("strata","benefeciaries_non_benefeciaries","strata.region_recieved_aid",	
                   "sample_strata_num","population",	"pop_global",	"sample_global",	"survey_weight")) %>% 
    select(c("strata.region_recieved_aid","survey_weight"))
  

data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights)
  

# colums to analyze -------------------------------------------------------

cols_not_to_ana<- c("X", "start", "end", "date", "deviceid", "end_survey", "audit", 
                    "enumerator_id", "enumerator_gender","intro", "consent", 
                      "Region..name.","strata.region_recieved_aid","benefeciaries_non_benefeciaries",
                    "thankyou", "X_id" ,"X_uuid" , "X_submission_time" ,"X_validation_status" ,                         
                   "X_index","province","district","village","enumerator_uuid")
  
col_to_analyze <- data_for_analysis %>% select(-cols_not_to_ana) %>% dplyr::select(-ends_with("_other"))   %>% colnames()
  
  
# basic_analysis ----------------------------------------------------------
  
dfsvy<-svydesign(ids = ~1,data = data_for_analysis,weights = formula(paste0("~", "survey_weight")))
  
is_not_empty<-function(x){ all(is.na(x))==FALSE}
  cols_to_analyze<-data_for_analysis[col_to_analyze] %>% select(-ends_with("Other"), -ends_with(".other")) %>%
    select_if(.,is_not_empty) %>% colnames()

basic_analysis_region<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "region" )
  
  
  