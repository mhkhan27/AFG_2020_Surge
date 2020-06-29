



Load_Audit<-function(data, 
                     path.to.zip,path.to.unzip,
                     copy.zip=TRUE,
                     path.to.copy.zip, 
                     filter.column="consent",
                     filter.on= "yes",
                     uuid.column="X_uuid",
                     delete.unzipped=TRUE,
                     days.ago.reported=0){
  if(copy.zip==TRUE){
    file.copy(path.to.zip, path.to.copy.zip)}
  
  unzip(path.to.zip, exdir = path.to.unzip)
  all_uuid_df<-data.frame(all_uuids=basename(dirname(list.files(path_unzip, recursive=TRUE))),
                          all_paths=dirname(list.files(path_unzip, recursive=TRUE, full.names = TRUE)))
  data$filter.col<- data[[filter.column]]
  filtered_uuid_df<- all_uuid_df[all_uuid_df$all_uuids %in% data[data$filter.col==filter.on,uuid.column],]
  filtered_audit_dirs<-filtered_uuid_df[,"all_paths"] %>% as.character()
  filtered_audit_csvs<-list.files(filtered_audit_dirs, recursive = TRUE, full.names=TRUE)
  data<-filtered_audit_csvs %>% 
    purrr::map(readr::read_csv)
  names(data)<-filtered_uuid_df$all_uuids
  if(delete.unzipped==TRUE){
    delete_dir<-list.files(path_unzip,full.names = TRUE)
    unlink(delete_dir, recursive=TRUE)
  }
  return(data)
  
}
    





