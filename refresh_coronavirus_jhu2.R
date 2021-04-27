
refresh_coronavirus_jhu2 <- function(){
  
  
  df <- NULL
  
  df <- readr::read_csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv")
  
  df <- as.data.frame(df)
  df$province <- ifelse(is.na(df$province), "", df$province)
  
  
  if(base::is.null(df)){
    base::message("Could not refresh the coronavirus dataset, please check your connection")
  } else{
    df$location <- ifelse(df$province == "", df$country, paste(df$province, df$country, sep = ", "))
    
    
    # Fixes before merging in codes
    df$location <- gsub("Korea, South", "South Korea",  df$location)
    df$location <- gsub("Bonaire, Sint Eustatius and Saba",
                        "Bonaire and Sint Eustatius and Saba",  df$location)
    df$location <- gsub("^\\, ", "",  df$location )
    
    #get code table
    iso_3166_2_code_table <- readr::read_csv("https://github.com/RamiKrispin/coronavirus/raw/dev-covid19r/data_raw/iso_3166_2_code_table.csv")
    
    # left join codes in
    df <- base::merge(df, iso_3166_2_code_table,
                      all.x = TRUE, by = "location")
  
    
    col_order <- c("date", "province", "country",  "lat", "long", "type", "cases")
    
    
    return(df[,col_order])
  }
}

