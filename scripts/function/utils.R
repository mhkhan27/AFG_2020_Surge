
facted_histogram<-function(data){
  data_num<- data %>% select_if(is.numeric)
  data_num_long<- data %>% pivot_longer(names_to="question", values_to= "response")
  vlines<-data_num_long %>% group_by(question) %>%
    summarise(mean= mean(response, na.rm=T),
              upper_ci= mean+(1.96*sd(response, na.rm=T)),
              lower_ci=mean-(1.96*sd(response, na.rm=T))
           )


  output_plot<-ggplot(data_num_long_wstats, aes(x=response))+
    geom_histogram(bins=100)+
    facet_wrap(~question)+
    geom_vline(data=vlines,aes(xintercept= lower_ci))+
    geom_vline(data=vlines, aes(xintercept=upper_ci))
  return(output_plot)

}

make_xlsform_lookup_table<- function(kobo_survey, kobo_choices, label_column){
  question_label_col<- paste0("question_",label_column)
  choice_label_col<- paste0("choice_",label_column)
  kobo_survey %>%
    select(type, name,all_of(label_column)) %>%
    rename_all(,.funs = function(x){paste0("question_",x)}) %>%
    filter(str_detect(question_type, "select")) %>%
    select(1:3) %>%
    mutate(question_list_name=str_replace_all(question_type,
                                              c("select_one"="", "select_multiple"="")) %>% trimws()) %>%
    right_join(kobo_choices %>% select(1:3) %>%
                 rename_all(,.funs = function(x){paste0("choice_",x)}),
               by=c("question_list_name"= "choice_list_name")) %>%
    mutate(xml_format=paste0(question_name,".",choice_name))
}


refactor_to_xlsform<-function(data,kobo_survey,kobo_choices ,label_column = "label::english" ){
  xls_lt<-make_xlsform_lookup_table(kobo_survey ,kobo_choices,label_column )
  xls_lt_select_questions<-xls_lt %>%
    filter(str_detect(question_type,"select")) %>%
    filter(question_name %in% colnames(data))
  for(i in 1: length(unique(xls_lt_select_questions$question_name))){
    col_temp <- unique(xls_lt_select_questions$question_name)[i]
    print(col_temp)
    choices_temp<-xls_lt_select_questions %>% filter(question_name==col_temp) %>% pull(choice_name)
    data<-data %>%
      mutate(!!col_temp:= forcats::fct_expand(as.factor(!!sym(col_temp)), choices_temp))
  }
  return(data)

}

add_rowwise_nfi_food_medians<-function(df){
  df<-df %>%
    mutate(i.food_item_median= matrixStats::rowMedians(as.matrix(df[,food_prices]),na.rm=T),
           i.nfi_item_median= matrixStats::rowMedians(as.matrix(df[,nfi_prices]),na.rm=T)
    )
  return(df)
}


percentage_pt_change<-function(x,y){
  red_tri<-".\\triangles\\black_triangle_up.png"
  green_tri<-".\\triangles\\black_triangle_down.png"
  grey_tri<-".\\triangles\\black_triangle_sideways.png"
  results<-list()
  x_minus_y<-(x-y)*100
  triangle<-x_minus_y %>%
    mutate_all(.funs = ~case_when(.> 5~red_tri,
                                  .< -5~green_tri,
                                  TRUE~grey_tri)) %>%
    rename_all(~paste0("@",.,".tri"))
  results[["percent"]]<-round(x*100,0)
  results[["triangle_cols"]]<- triangle
  return(results)
}

percent_diff_dm<-function(x,y){
  red_tri<-".\\triangles\\red_triangle.png"
  green_tri<-".\\triangles\\green_triangle.png"
  grey_tri<-".\\triangles\\grey_triangle.png"
  results<-list()
  x2<-round(((x-y)/y)*100,0)
  triangles<-x2 %>%
    mutate_all(.funs = ~case_when(.==0~grey_tri,
                                  .>0~red_tri,
                                  is.na(.)~ "",
                                  TRUE~ green_tri)) %>%
    rename_all(.funs = ~ paste0("@",.,".tri"))
  x_abs<-x2%>%
    abs() %>%
    rename_all(.funs = ~paste0(.,".perc"))
  results[["current_round"]]<-x
  results[["percent_diff"]]<-x_abs
  results[["triangle"]]<-triangles
  return(results)
}




check_dot<-function(x){
  stringr::str_ends(string = x,pattern ="\\." )
}


prepare_rank_table<-function(rank_cols_analyzed){
  rank_table<-rank_cols_analyzed %>%
    pivot_longer(cols = 1:ncol(.),names_to="option", values_to = "value") %>%
    arrange(desc(value)) %>%
    mutate(rank_last=rank(desc(value),ties.method = "last")) %>% arrange(rank_last) %>%
    group_by(value) %>%
    mutate(num_tie=n()) %>%
    arrange(rank_last)
  return(rank_table)
}



return_top_sm<-function(df,
                        rank_col,
                        rank_n,
                        tie_break_remove,
                        aggregation_level=NULL, xlsform_lookup =NULL){

  result<-list()
  if(check_dot(rank_col)==F){
    rank_col<-paste0(rank_col,".")
  }
  cols_to_rank<-df %>%
    select(starts_with(rank_col)) %>% colnames()
  rank_cols_analyzed<- butteR::mean_proportion_table(design = df,list_of_variables = cols_to_rank, aggregation_level=aggregation_level)

  rank_table<- prepare_rank_table(rank_cols_analyzed = rank_cols_analyzed)

  # rank_table<-rank_cols_analyzed %>%
  #   pivot_longer(cols = 1:ncol(.),names_to="option", values_to = "value") %>%
  #   arrange(desc(value)) %>%
  #   mutate(rank_last=rank(desc(value),ties.method = "last")
  #   ) %>%
  #   group_by(value) %>%
  #   mutate(num_tie=n()) %>%
  #   arrange(rank_last)
  if(!is.null(xlsform_lookup)){
    rank_table<-rank_table %>% left_join(xlsform_lookup %>% select(xml_format, question_name,starts_with("choice_label")), by=c("option" ="xml_format"))}


  if(rank_table%>% filter(rank_last==rank_n) %>% pull(num_tie)>1){
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
    interactive_title<-paste0("You need to remove",number_records_gt_rank, "record(s)", "from the tie_records table")
    tie_break_remove <- select.list(tie_records$option,multiple = T, title = interactive_title)

    if(!is.null(tie_break_remove)){
      tie<-result$ranks_with_ties %>%
        group_by(value) %>%
        filter(!option%in% tie_break_remove) %>% ungroup() %>%
        mutate(rank_final= 1:n()) %>% filter(rank_final<=rank_n)
      result[["ranks_long_for_dm"]]<-tie
    }else{
      result[["ranks_long_for_dm"]]<-rank_table%>%
        filter(rank_last<=rank_n) %>%
        rename(rank_final="rank_last")

    }


    results_long<-result$ranks_long_for_dm %>%
      mutate(question_rank_lab=paste0(question_name,"_lab_",rank_final),
             question_rank_val=paste0(question_name,"_val_",rank_final)) %>% ungroup() %>%
      select(question_rank_lab,question_rank_val, value,starts_with("choice"))

    labels_wide<- results_long %>% pivot_wider(-c(question_rank_val,value), names_from=c("question_rank_lab"), values_from = c("choice_label::english"))
    values_wide<- results_long %>% pivot_wider(-c(question_rank_lab,"choice_label::english"), names_from=c("question_rank_val"), values_from = c("value"))
    results_wide<- cbind(labels_wide, values_wide)
    result[["wide"]]<-results_wide


    result$ranks_long_for_dm %>%  pivot_wider(names_from = option, values_from = value)

  }
  return(result)
}



datamerge_from_rank_table<-function(rank_table, xlsform_lookup, rank_n){
  if(!is.null(xlsform_lookup)){
    rank_table<-rank_table %>% left_join(xlsform_lookup %>% select(xml_format, question_name,starts_with("choice_label")), by=c("option" ="xml_format"))}


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
    interactive_title<-paste0("You need to remove",number_records_gt_rank, "record(s)", "from the tie_records table")
    tie_break_remove <- select.list(tie_records$option,multiple = T, title = interactive_title)

    if(!is.null(tie_break_remove)){
      tie<-result$ranks_with_ties %>%
        group_by(value) %>%
        filter(!option%in% tie_break_remove) %>% ungroup() %>%
        mutate(rank_final= 1:n()) %>% filter(rank_final<=rank_n)
      result[["ranks_long_for_dm"]]<-tie
    }else{
      result[["ranks_long_for_dm"]]<-rank_table%>%
        filter(rank_last<=rank_n) %>%
        rename(rank_final="rank_last")

    }


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

