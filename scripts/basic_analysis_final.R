rm(list = ls())
# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(tidyr)
library(stringr)
library(srvyr)
library(survey)
library(matrixStats)
source("scripts/function/utils.R")
source("scripts/function/datamerge_from_rank_table2.R")

type_of_analysis <- c("region_and_beneficiaries","region_and_modality","displacement","displacement_all")[4]

# read_data ---------------------------------------------------------------

df <- read.csv("outputs/recoding/composite_indicators.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::filter(!is.na(received_aid_mark_2)) %>% dplyr::filter(consent == "yes") #filter data

region <-read.csv("dap/unhcr_hh/Region.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::select(c("Name","Region..name.")) #region data 

df_with_regions <- df %>% left_join(region,by =c ("province"="Name"))

population <-read.csv("dap/unhcr_hh/population.csv",na.strings = c(""," ",NA),stringsAsFactors = F) #population data full

# region and beneficiaries ----------------------------------------------

if ( type_of_analysis == "region_and_beneficiaries") {

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


sf_with_weights_for_write<- df_for_grop_analysis %>% 
    group_by(Region..name.,benefeciaries_non_benefeciaries) %>% 
    summarise(sample_strata_num=n()) %>% as.data.frame() %>% mutate(
      strata.region_recieved_aid  = paste0(Region..name.,"_",benefeciaries_non_benefeciaries)
    ) %>% 
    right_join(pop,by = c("strata.region_recieved_aid"))%>% mutate(
      sample_global = sum(sample_strata_num),
      survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
    ) %>% select(c("strata","benefeciaries_non_benefeciaries","strata.region_recieved_aid",	
    "sample_strata_num","population",	"pop_global",	"sample_global",	"survey_weight")) 

sf_with_weights <- sf_with_weights_for_write %>%   select(c("strata.region_recieved_aid","survey_weight"))

data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights)

}

# region and modality ----------------------------------------------------------------

if ( type_of_analysis == "region_and_modality") {

pop <- population %>% dplyr::filter(type == "unconditional_cash" | type== "conditional_cash" |
                                      type == "voucher" | type == "in_kind")

pop <- pop%>% 
  mutate(
    pop_global=sum(population),
    strata.region_and_modality  = paste0(pop$strata,"_",pop$type))

sf_pop<- "population"
displacement_type <- c("host","idp")

df_for_grop_analysis <-  df_with_regions %>% dplyr::filter(list_displacement %in% displacement_type) %>% 
         mutate( strata.region_and_modality  = paste0(Region..name.,"_",received_aid_type))


df_for_grop_analysis <- df_for_grop_analysis %>% filter(!is.na(received_aid_type))

sf_with_weights_for_write<- df_for_grop_analysis %>% 
  group_by(Region..name.,received_aid_type) %>% 
  summarise(sample_strata_num=n()) %>% as.data.frame() %>% mutate(
    strata.region_and_modality = paste0(Region..name.,"_",received_aid_type)
  ) %>% 
  right_join(pop,by="strata.region_and_modality") %>% mutate(
    sample_global = sum(sample_strata_num),
    survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
  ) %>% select(c("strata","strata.region_and_modality","received_aid_type",	
                 "sample_strata_num","population",	"pop_global",	"sample_global",	"survey_weight")) 
  

 sf_with_weights <- sf_with_weights_for_write %>% select(c("strata.region_and_modality","survey_weight"))

data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights) %>% filter(!is.na(survey_weight))

}


# displacement  -----------------------------------------------------------

if ( type_of_analysis == "displacement") {
  
pop <- population %>% dplyr::filter(type == "population_group")

pop <- pop%>% 
  mutate(
    pop_global=sum(population))

sf_pop<- "population"

df_for_grop_analysis <-  df_with_regions %>% filter(received_aid_mark_2 != "no")

sf_with_weights_for_write <- df_for_grop_analysis %>% 
  group_by(list_displacement) %>% 
  summarise(sample_strata_num=n()) %>% 
  right_join(pop, by=c("list_displacement" = "strata")) %>% as.data.frame() %>% mutate(
    sample_global = sum(sample_strata_num),
    survey_weight = (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)) %>% 
      select(c("list_displacement","sample_strata_num","population",	"pop_global",	"sample_global","survey_weight"))

sf_with_weights <- sf_with_weights_for_write %>% select(c("list_displacement","survey_weight"))

data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights) %>% filter(!is.na(survey_weight))
}
# displacement2  -----------------------------------------------------------

if ( type_of_analysis == "displacement_all") {
  
  pop <- population %>% dplyr::filter(type == "population_group")
  
  pop <- pop%>% 
    mutate(
      pop_global=sum(population))
  
  sf_pop<- "population"
  
  df_for_grop_analysis <-  df_with_regions 
  
  sf_with_weights_for_write <- df_for_grop_analysis %>% 
    group_by(list_displacement) %>% 
    summarise(sample_strata_num=n()) %>% 
    right_join(pop, by=c("list_displacement" = "strata")) %>% as.data.frame() %>% mutate(
      sample_global = sum(sample_strata_num),
      survey_weight = (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)) %>% 
    select(c("list_displacement","sample_strata_num","population",	"pop_global",	"sample_global","survey_weight"))
  
  sf_with_weights <- sf_with_weights_for_write %>% select(c("list_displacement","survey_weight"))
  
  data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights) %>% filter(!is.na(survey_weight))
}
write.csv(data_for_analysis,paste0(paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_working_data.csv")))
write.csv(sf_with_weights_for_write,paste0(paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_survey_weights.csv")))
# colums to analyze -------------------------------------------------------

cols_not_to_ana<- c("X", "start", "end", "date", "deviceid", "end_survey", "audit", 
                    "enumerator_id", "enumerator_gender","intro", "consent", 
                    "Region..name.",
                    "thankyou", "X_id" ,"X_uuid" , "X_submission_time" ,"X_validation_status" ,                         
                    "X_index","province","district","village","enumerator_uuid")

col_to_analyze <- data_for_analysis %>% select(-cols_not_to_ana) %>% dplyr::select(-ends_with("_other"))   %>% colnames()


# basic_analysis ----------------------------------------------------------

dfsvy<-svydesign(ids = ~1,data = data_for_analysis,weights = formula(paste0("~", "survey_weight")))

is_not_empty<-function(x){ all(is.na(x))==FALSE}
cols_to_analyze<-data_for_analysis[col_to_analyze] %>% select(-ends_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% colnames()

cols_to_factors <- c("condition_shelter_tools_upg",
                     "condition_labour") #"condition_shelter_materials_upg","condition_sleeping_mats"

dfsvy$variables<-dfsvy$variables %>%
  mutate_at(.vars=cols_to_factors, .funs=forcats::fct_expand,
            c( "still_functional","finished","broken","lost","stolen","traded","given_away"))

# cols_to_factors2 <- c("last_replacement_tent","last_shelter_tools_upg","last_shelter_materials_upg","last_labour",
#                       "last_sleeping_mats","received_aid_mark","last_shelter_repair_items","last_emergency_shelter_kit",
#                       "received_aid_mark_2","i.aid_received_same")
# 
cols_to_factors2 <- c("last_emergency_shelter_kit","last_shelter_tools_upg","last_labour","received_aid_mark",
                      "received_aid_mark_2","i.aid_received_same")

dfsvy$variables<-dfsvy$variables %>%
  mutate_at(.vars=cols_to_factors2, .funs=forcats::fct_expand,
            c( "yes","no"))

cols_to_factors3 <- c("trade_items_why")

dfsvy$variables<-dfsvy$variables %>%
  mutate_at(.vars=cols_to_factors3, .funs=forcats::fct_expand,
            c( "needed_money"," did_not_need","other"))

cols_to_factors4 <- c("i.shelter_needs_met_total","i.nfi_needs_met_total")

dfsvy$variables<-dfsvy$variables %>%
  mutate_at(.vars=cols_to_factors4, .funs=forcats::fct_expand,
            c( "completely_met","almost_met","mostly_met", "partially_met","not_met"))

# 
# cols_to_factors5 <- c("condition_shelter_repair_items")
# 
# dfsvy$variables<-dfsvy$variables %>%
#   mutate_at(.vars=cols_to_factors5, .funs=forcats::fct_expand, c("still_functional",
#                "finished", "broken","lost", "stolen","traded", "given_away"))

dfsvy$variables$i.main_modality <- dfsvy$variables$i.main_modality  %>% as.factor()
dfsvy$variables$received_aid_mark_2 <- dfsvy$variables$received_aid_mark_2  %>% as.factor()
dfsvy$variables$region <- dfsvy$variables$region

if (type_of_analysis != "displacement_all") {

basic_analysis_region<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "region" )
basic_analysis_main_modality<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "i.main_modality" )
basic_analysis_displacement_report<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "i.displace_report" )
basic_analysis_gender_hoh <-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "i.gender_hoh" )
basic_analysis_vulnerable_blanket<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "i.vulnerable_blanket" )
basic_analysis_received_aid_mark2<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = "received_aid_mark_2" )
basic_analysis_region_and_received_aid_mark2<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = c("region","received_aid_mark_2"))
basic_analysis_region_and_main_modality<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                 aggregation_level = c("region","i.main_modality"))

}

if (type_of_analysis == "displacement_all") {
  basic_analysis_received_aid_mark_2<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                                aggregation_level = "received_aid_mark_2" )
  basic_analysis_displacement_overall<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze) %>% mutate(
    strata = "overall")
  basic_analysis_gender_hoh <-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                        aggregation_level = "i.gender_hoh" )
  basic_analysis_vulnerable_blanket<-butteR::mean_prop_working(design = dfsvy,list_of_variables = cols_to_analyze,
                                                               aggregation_level = "i.vulnerable_blanket" )
}


# write_csv ---------------------------------------------------------------

  # write.csv(basic_analysis_region,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_region.csv"))
  # write.csv(basic_analysis_main_modality,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_main_modality.csv"))
  # write.csv(basic_analysis_displacement_report,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_displacement_report.csv"))
  # write.csv(basic_analysis_gender_hoh,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_gender_hoh.csv"))
  # write.csv(basic_analysis_vulnerable_blanket,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_vulnerable_blanket.csv"))
  # write.csv(basic_analysis_received_aid_mark2,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_received_aid_mark2.csv"))
  # write.csv(basic_analysis_region_and_received_aid_mark2,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_region_and_received_aid_mark2.csv"))
  # write.csv(basic_analysis_region_and_main_modality,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_region_and_main_modality.csv"))


# data_merge --------------------------------------------------------------

if (type_of_analysis != "displacement_all") {
mean_table <- c("basic_analysis_region","basic_analysis_main_modality","basic_analysis_displacement_report",
                "basic_analysis_gender_hoh","basic_analysis_vulnerable_blanket","basic_analysis_received_aid_mark2",
                "basic_analysis_region_and_received_aid_mark2","basic_analysis_region_and_main_modality")
}

if (type_of_analysis == "displacement_all") {
  
  mean_table <- c("basic_analysis_received_aid_mark_2","basic_analysis_displacement_overall",
                  "basic_analysis_gender_hoh","basic_analysis_vulnerable_blanket")
}
for (v in mean_table) {
  
strata_col <- if_else(v == "basic_analysis_region","region",
                    if_else(v =="basic_analysis_received_aid_mark_2","received_aid_mark_2",
                            if_else(v == "basic_analysis_displacement_overall","strata",
                      if_else(v =="basic_analysis_main_modality","i.main_modality",
                              if_else(v =="basic_analysis_received_aid_mark2","received_aid_mark_2",
                              if_else(v =="basic_analysis_displacement_report","i.displace_report",
                                      if_else(v =="basic_analysis_gender_hoh","i.gender_hoh",
                                              if_else(v =="basic_analysis_vulnerable_blanket","i.vulnerable_blanket",NULL
                                                     ))))))))

if(v == "basic_analysis_region_and_received_aid_mark2") {
  strata_col <- "region_and_received_aid_mark2"
  basic_analysis_region_and_received_aid_mark2$region_and_received_aid_mark2 <- paste0(basic_analysis_region_and_received_aid_mark2$region,"_and_",
                                                                                       basic_analysis_region_and_received_aid_mark2$received_aid_mark_2)
}
if(v == "basic_analysis_region_and_main_modality") {
  strata_col <- "region_and_main_modality"
  basic_analysis_region_and_main_modality$region_and_main_modality <- paste0(basic_analysis_region_and_main_modality$region,"_and_",
                                                                             basic_analysis_region_and_main_modality$i.main_modality)
}


ks<-readxl::read_xlsx(path = "tools/AFG2003a_Tool_1_HH_Survey_Kobo_Final_27052020v4.xlsx",sheet = "survey")
kc<-readxl::read_xlsx(path = "tools/AFG2003a_Tool_1_HH_Survey_Kobo_Final_27052020v4.xlsx",sheet = "choices")

data_for_analysis<-refactor_to_xlsform(data = data_for_analysis,kobo_survey = ks,kobo_choices = kc,label_column = "label::English" )
xls_lt<-make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::English")

select_one <- c("priority","no_shelter","heating_coping")

select_multiple <- c("shelter_concerns","shelter_repairs","shelter_needs_bad","nfi_items","nfi_needs_bad",
                     "debt_pay_how","debt_not_pay","aid_items","trade_items_which","cash_spend_how",
                     "cash_challenge","vendor_challenge_what","distribution_challenge","challenge_type")


# cols_multi2 <- as.character()
# for (i in select_multiple) {
#   
#   cols_multi <- df %>% select(starts_with(paste0(i,"."))) %>% colnames() %>% dput
#   cols_multi2 <- c(cols_multi,cols_multi2)
# }
# cols_to_analyze <- c(cols_multi2,select_one)

data_merge_basic_analysis_strata_idp<- get(v)

cols_for_subset <- c(select_one,select_multiple)

data_merge_basic_analysis <- list()
data_merge_ind<- list()

for (u in cols_for_subset) {
  col_temp<-u
  
  for(i in 1: nrow(data_merge_basic_analysis_strata_idp)){
    
    
    strata_temp<-data_merge_basic_analysis_strata_idp %>% slice(i) %>% pull(strata_col) %>% as.character() 
    
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
  if(is_not_empty(row_num)){
    col_name <- gsub("val","lab",x)
    data_merge[row_num,] [col_name] <- NA
  }
}

# compile_with_basic ------------------------------------------------------
   data_merge2<-bind_cols(get(v),data_merge)

write.csv(data_merge2,paste0("outputs/basic_analysis/",type_of_analysis,"/",str_replace_all(Sys.Date(),"-","_"),"_data_merge_",v,".csv"),na="")
}

# if (type_of_analysis == "dispalacement_all") {
#   overall <-read.csv("outputs/basic_analysis/displacement_all/2020_06_25_data_merge_basic_analysis_displacement_overall.csv")
#   displacement_report <-read.csv("outputs/basic_analysis/displacement_all/2020_06_25_data_merge_basic_analysis_displacement_report2.csv")
#   
#   a <- data.frame(a = colnames(overall) ,
#   tf =colnames(overall) %in% colnames(displacement_report))
# }
