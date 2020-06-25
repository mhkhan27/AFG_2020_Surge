rm(list = ls())

# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(stringr)
library(srvyr)
library(survey)

# read_data ---------------------------------------------------------------

df <- read.csv("inputs/clean_dataset/tool1/cleaned_data.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::filter(!is.na(received_aid_mark_2)) %>% dplyr::filter(consent == "yes") 

response_rate<-get_na_response_rates(df) %>% mutate(
  response_rate = nrow(df) - num_non_response
)

region <-read.csv("dap/unhcr_hh/Region.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::select(c("Name","Region..name.")) #region data 

df_with_regions <- df %>% left_join(region,by =c ("province"="Name"))
df_for_recoding<- df_with_regions %>% rename_at("Region..name.",function(x){x<-"region"})


# variables ----------------------------------------------------------------

aid_spent_total <- df_for_recoding[,c("aid_spent_food","aid_spent_nfi", "aid_spent_heating",
                     "aid_spent_rent", "aid_spent_shelter","aid_spent_health",
                     "aid_spent_transport",  "aid_spent_fuel",  "aid_spent_edu",
                     "aid_spent_savings", "aid_spent_debt", "aid_spent_other")]

unsafe_shelter <- c("tent","makeshift", "collective_centre","open_space","damaged_house",
                    "unfinished")
prod_hoh <- df_for_recoding[,c("male_18_59","female_18_59")]
unprod_hoh <- df_for_recoding[,c("male_less_1", "female_less_1", "male_1_4","female_1_4",
"male_5_17","female_5_17","male_60_over","female_60_over")]

hh_charity <- df_for_recoding [,df_for_recoding %>% select(starts_with("cash_flow.")) %>% colnames()]


# composite indicators ----------------------------------------------------

df_recoded <- df_for_recoding %>% mutate(
  i.aid_spent_total= rowSums(aid_spent_total,na.rm = T),
  i.aid_received_same = if_else(received_aid_mark == "yes" & received_aid_mark_2 == "yes","yes",
                               if_else(received_aid_mark == "yes" & received_aid_mark_2 == "no","no",NULL, NULL)),
  i.age_hoh = if_else(resp_hoh == "yes", resp_age ,hoh_age,NULL),
  i.gender_hoh = if_else(resp_hoh == "yes", resp_gender, hoh_gender,NULL),
  i.disabled_hoh = if_else(resp_hoh == "yes" ,resp_disability, hoh_disability,NULL),
  i.hh_contain_elderly = if_else(male_60_over > 0 | female_60_over > 0,"yes","no",NULL),
  i.hh_contain_child_under_5 = if_else(male_less_1 >0 | female_less_1 > 0| male_1_4 > 0 |female_less_1>0,"yes","no",NULL),
  i.prod_hoh = rowSums(prod_hoh,na.rm = T),
  i.unprod_hoh = rowSums(unprod_hoh,na.rm = T),
  hoh_female_or_child = if_else(i.gender_hoh == "female" | i.age_hoh < 19 ,"yes","no",NULL),
  i.male_less_1_perc = male_less_1/household_total,
  i.female_less_1_perc = female_less_1/household_total,
  i.male_1_4_perc = male_1_4/household_total,
  i.female_1_4_perc = female_1_4/household_total,
  i.male_5_17_perc = male_5_17/household_total,
  i.female_5_17_perc = female_5_17/household_total,
  i.male_18_59_perc = male_18_59/household_total,
  i.female_18_59_perc = female_18_59/household_total,
  i.male_60_over_perc = male_60_over/household_total,
  i.female_60_over_perc = female_60_over/household_total,
  i.hhs_disabled_members = if_else(disability>0,"yes","no",NULL),
  i.unsafe_shelter = if_else(shelter_type %in% unsafe_shelter,"yes","no",NULL),
  i.hhs_no_adult_male_working = if_else(male_18_59 == 0 & breadwinner == 0, "yes","no",NULL),
  i.hhs_no_src_livelihood = if_else(breadwinner == 0 , "yes","no"),
  
  hh_charity_rowsum = rowSums(hh_charity,na.rm = T),
  i.hh_charity= if_else(hh_charity_rowsum > 0 & cash_flow.work != 1,"yes","no",NULL),
  i.income_casual = if_else(breadwinner == 1 & income_source == "unskilled","yes","no",NULL),
  i.host_report = if_else(area_origin == "yes" & idp_returnee == "no","yes","no",NULL),
  i.idp_report = if_else(area_origin == "no" & refugee == "no","yes","no",NULL),
  i.special_idp_report = if_else(area_origin == "yes" & idp_returnee == "yes" & returnee == "no","yes","no",NULL),
  i.returness_report = if_else(area_origin == "yes" & idp_returnee == "yes" & returnee == "yes","yes","no",NULL),
  i.refugee_report = if_else(area_origin == "no" &  refugee == "yes","yes","no",NULL)) %>% mutate(
  i.dep_ratio_8_or_more = i.unprod_hoh/i.prod_hoh,
  i.displace_report = if_else(received_aid_mark_2 == "yes" & i.host_report == "yes","host",
                              if_else(received_aid_mark_2 == "yes" & i.idp_report == "yes","idp",
                                      if_else(received_aid_mark_2 == "yes" & i.special_idp_report == "yes","special_idp",
                                              if_else(received_aid_mark_2 == "yes" & i.returness_report == "yes","returnee",
                                                      if_else(received_aid_mark_2 == "yes" & i.refugee_report == "yes","refugee","non_ben",NULL))))),
  
  i.list_disp_mod = if_else(received_aid_mark == "yes" & list_displacement == "host","host",
                              if_else(received_aid_mark == "yes" & list_displacement == "idp","idp",
                                      if_else(received_aid_mark == "yes" & list_displacement == "special_idp","special_idp",
                                              if_else(received_aid_mark == "yes" & list_displacement == "unknown","unknown",
                                              if_else(received_aid_mark == "yes" & list_displacement == "returnee","returnee",
                                                      if_else(received_aid_mark == "yes" & list_displacement == "refugee","refugee","non_ben",NULL)))))),
  
  i.elderly_hoh = if_else(i.age_hoh > 59, "yes","no",NULL),
  i.female_child_hoh_adult = if_else(hoh_female_or_child == "yes" & male_18_59 == 0 & cash_flow.remittances == 0,"yes","no",NULL),
  i.shelter_needs_met_total = if_else(shelter_needs_met == "completely_met" | shelter_needs_met_non_bene == "completely_met" , "completely_met",
                                      if_else(shelter_needs_met == "almost_met" | shelter_needs_met_non_bene == "almost_met" , "almost_met",
                                              if_else(shelter_needs_met == "mostly_met" | shelter_needs_met_non_bene == "mostly_met" , "mostly_met",
                                                      if_else(shelter_needs_met == "partially_met" | shelter_needs_met_non_bene == "partially_met" , "partially_met",
                                                              if_else(shelter_needs_met == "not_met" | shelter_needs_met_non_bene == "not_met" , "not_met","ERROR",NULL))))),
  
  i.nfi_needs_met_total = if_else(nfi_needs_met == "completely_met" | nfi_needs_met_non_bene == "completely_met" , "completely_met",
                                      if_else(nfi_needs_met == "almost_met" | nfi_needs_met_non_bene == "almost_met" , "almost_met",
                                              if_else(nfi_needs_met == "mostly_met" | nfi_needs_met_non_bene == "mostly_met" , "mostly_met",
                                                      if_else(nfi_needs_met == "partially_met" | nfi_needs_met_non_bene == "partially_met" , "partially_met",
                                                              if_else(nfi_needs_met == "not_met" | nfi_needs_met_non_bene == "not_met" , "not_met","ERROR",NULL))))),
  i.main_modality= if_else(modality == "unconditional_cash", "unconditional_cash",
                           if_else(modality == "conditional_cash","conditional_cash" ,
                                   if_else(modality == "in_kind","in_kind",
                                           if_else(modality == "voucher","voucher",
                                                   if_else(modality == "cash_in_kind" & modality_maj == "conditional_cash","conditional_cash",
                                                           if_else(modality == "cash_in_kind" & modality_maj == "unconditional_cash","unconditional_cash",
                                                                   if_else(modality == "cash_in_kind" & modality_maj == "in_kind","in_kind",
                                                                           if_else(modality == "cash_in_kind" & modality_maj == "voucher","voucher", "error",NULL))))))))
  ) %>%  mutate(
  i.displacement_same = if_else(i.list_disp_mod == i.displace_report , "yes","no",NULL),
  i.elderly_disabled_hoh = if_else(i.elderly_hoh == "yes" & i.disabled_hoh == "yes","yes","no",NULL),
  i.modality_same = if_else(i.main_modality == received_aid_type,"yes","no",NULL),
  i.aid_perc_food = aid_spent_food/i.aid_spent_total,
  i.aid_perc_nfi =  aid_spent_nfi/i.aid_spent_total,
  i.aid_perc_heating =  aid_spent_heating/i.aid_spent_total,
  i.aid_perc_rent =  aid_spent_rent/i.aid_spent_total,
  i.aid_perc_shelter =  aid_spent_shelter/i.aid_spent_total,
  i.aid_perc_health= aid_spent_health/i.aid_spent_total,
  i.aid_perc_transport= aid_spent_transport/i.aid_spent_total,
  i.aid_perc_fuel= aid_spent_fuel/i.aid_spent_total,
  i.aid_perc_edu =  aid_spent_edu/i.aid_spent_total,
  i.aid_perc_savings = aid_spent_savings/i.aid_spent_total,
  i.aid_perc_debt= aid_spent_debt/i.aid_spent_total,
  i.aid_perc_other= aid_spent_other/i.aid_spent_total,
  i.vulnerable_blanket = if_else(i.elderly_hoh == "yes" | i.disabled_hoh == "yes" | i.female_child_hoh_adult == "yes" |
                                   tazkera == "none" | i.dep_ratio_8_or_more > .8,"yes","no",NULL)
  )


write.csv(df_recoded,paste0("outputs/recoding/","composite_indicators.csv"))
write.csv(df_recoded,paste0("outputs/recoding/",str_replace_all(Sys.Date(),"-","_"),"_composite_indicators.csv"))
write.csv(response_rate,paste0("outputs/basic_analysis/",str_replace_all(Sys.Date(),"-","_"),"_response_rate.csv"))
