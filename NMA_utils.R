require(stringr)
require(dplyr)

load_dataframe_from_string <- function(data_str){
  # R mangles column names that have special characters like square brackets,
  # but we can re-set the names to their original values.
  column_names <- data_str %>% strsplit("\\n") %>% "[["(1) %>% "[["(1) %>% strsplit("\\t") %>% "[["(1)
  df <- data_str %>% textConnection %>% read.delim(row.names=NULL) %>% setNames(nm=column_names)
}

extract_array <- function(my_prefix, df){
  pat <- paste0('^', my_prefix, '\\[')
  my_cols <- colnames(df) %>% grep(pat, ., value=TRUE) %>% sort
  if (length(my_cols) > 1){
    do.call(rbind, df[my_cols]) %>% t
  } else {
    df[[my_cols]]
  }
}

load_matrix_data_from_df <- function(df){
  array_prefixes <- colnames(df) %>% 
    str_extract_all("(^.+)\\[") %>% 
    unlist %>% 
    gsub('[', '', ., fixed=TRUE) %>%
    unique
  
  array_prefixes %>% setNames(nm=.) %>% lapply(extract_array, df)
}

test_load_matrix_data_from_df <- function(){
  
  data_str <- "na[]	t[,1]	t[,2]	t[,3]	r[,1]	n[,1]	r[,2]	n[,2]	r[,3]	n[,3]	ID	year
3	1	3	4	1472	20251	652	10396	723	10374	GUSTO-1	1993
2	1	2	NA	3	65	3	64	NA	NA	ECSG	1985
2	1	2	NA	12	159	7	157	NA	NA	TIMI-1	1987"
  
  df <- data_str %>% load_dataframe_from_string 
  
  df %>% load_matrix_data_from_df
  
}