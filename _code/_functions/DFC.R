data_format_convert <- function(data_to_convert, type = "large",col_ini = "ENE", col_fin = "DIC", names = "mes",values = "cantidad"){
  if(type == "large"){
    resultado <- data_to_convert %>% 
      pivot_longer(cols = c(as.symbol(col_ini):as.symbol(col_fin)), names_to = names, values_to = values) %>% 
      filter(!is.na(get(values)))
  }
  if(type == "short"){
    resultado <- data_to_convert %>% 
      pivot_wider(names_from = as.symbol(names), values_from = as.symbol(values))
  }
  return(resultado)
}