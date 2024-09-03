join_gestions <- function(data_directory){
  
  data_list <- list()
  
  for (parquet_field in list.files(path = data_directory, pattern = "*.parquet", full.names = TRUE)) {
    db_name <- tools::file_path_sans_ext(basename(parquet_field))
    
    parquet_data <- read_parquet(parquet_field)
    
    data_list[[db_name]] <- parquet_data
  }
  merge_data <- bind_rows(data_list)
  
  if ("JUN_REINT" %in% names(merge_data)) {
    merge_data <- merge_data %>% 
      select(bd,gestion:JUN,JUN_REINT,JUL:DIC) 
  }else{
    merge_data <- merge_data %>% 
      select(bd,gestion:DIC) 
  }
  
  return(merge_data)
}