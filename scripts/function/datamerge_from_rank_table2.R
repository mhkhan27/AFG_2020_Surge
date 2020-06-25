datamerge_from_rank_table2<-function(rank_table, xlsform_lookup, rank_n){
  if(!is.null(xlsform_lookup)){
    rank_table<-rank_table %>% left_join(xlsform_lookup %>% select(xml_format, question_name,starts_with("choice_label")), by=c("option" ="xml_format"))
    }
  
  
if(rank_table%>% filter(rank_last==rank_n) %>% pull(num_tie)>1){
    result<-list()
    nth_rank_value<-rank_table %>% filter(rank_last==rank_n) %>% pull(value)
    number_records_with_tie<-rank_table %>% filter(value>=nth_rank_value) %>% nrow()
    number_records_gt_rank<-number_records_with_tie - rank_n
    tie_records<- rank_table %>%
      filter(value==nth_rank_value) %>%
      mutate(number_to_remove=number_records_gt_rank)
    
    
    result[["ranks_with_ties"]]<- rank_table
    result[["ranks_filtered"]]<-rank_table%>% filter(rank_last<=rank_n)
    result[["tied_records"]]<- tie_records
    # print(paste("top",rank_n, "in", sm,"were tied", sep = " "))
    # interactive_title<-paste0("You need to remove",number_records_gt_rank, "record(s)", "from the tie_records table")
    # tie_break_remove <- select.list(tie_records$option,multiple = T, title = interactive_title)
    
    # if(!is.null(tie_break_remove)){
    #   tie<-result$ranks_with_ties %>%
    #     group_by(value) %>%
    #     filter(!option%in% tie_break_remove) %>% ungroup() %>%
    #     mutate(rank_final= 1:n()) %>% filter(rank_final<=rank_n)
    #   result[["ranks_long_for_dm"]]<-tie
    # }else{
      result[["ranks_long_for_dm"]]<-rank_table%>%
        filter(rank_last<=rank_n) %>%
        rename(rank_final="rank_last")
      
    # }
    
    
    results_long<-result$ranks_long_for_dm %>%
      mutate(question_rank_lab=paste0(question_name,"_lab_",rank_final),
             question_rank_val=paste0(question_name,"_val_",rank_final)) %>% ungroup() %>%
      select(question_rank_lab,question_rank_val, value,starts_with("choice"))
    
    labels_wide<- results_long %>% pivot_wider(-c(question_rank_val,value), names_from=c("question_rank_lab"), values_from = c("choice_label::English"))
    values_wide<- results_long %>% pivot_wider(-c(question_rank_lab,"choice_label::English"), names_from=c("question_rank_val"), values_from = c("value"))
    results_wide<- cbind(labels_wide, values_wide)
    result[["wide"]]<-results_wide
    
    
    result$ranks_long_for_dm %>%  pivot_wider(names_from = option, values_from = value)
    
  }

if(rank_table%>% filter(rank_last==rank_n) %>% pull(num_tie)==1){
  result<-list()
  result[["ranks_long_for_dm"]]<-rank_table%>%
    filter(rank_last<=rank_n) %>%
    rename(rank_final="rank_last")
  results_long<-result$ranks_long_for_dm %>%
    mutate(question_rank_lab=paste0(question_name,"_lab_",rank_final),
           question_rank_val=paste0(question_name,"_val_",rank_final)) %>% ungroup() %>%
    select(question_rank_lab,question_rank_val, value,starts_with("choice"))
  

  labels_wide<- results_long %>% pivot_wider(-c(question_rank_val,value), names_from=c("question_rank_lab"), values_from = c("choice_label::English"))
  values_wide<- results_long %>% pivot_wider(-c(question_rank_lab,"choice_label::English"), names_from=c("question_rank_val"), values_from = c("value"))
  results_wide<- cbind(labels_wide, values_wide)
  result[["wide"]]<-results_wide
  result$ranks_long_for_dm %>%  pivot_wider(names_from = option, values_from = value)
  
  }
  return(result)
}
