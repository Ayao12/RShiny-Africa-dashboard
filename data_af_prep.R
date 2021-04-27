
last_date_covd <-  max(coronavirus::coronavirus$date)

diff_date <- as.numeric(difftime(as.POSIXct(Sys.time()), last_date_covd, units = "hours"))

## Date de mise jour des données: 2 jours et demi pour la veille 


if ( diff_date < 58) {
  library(coronavirus)
}else if(class(try(data_update <- refresh_coronavirus_jhu2(), silent = TRUE)) == "try-error") {
  library(coronavirus)
}else{
  try(coronavirus::update_dataset(silence = TRUE))
  
  coronavirus <- data_update
   
}

data_Af_prep <- function(all){
  Africa <-  all %>%
    janitor::clean_names() %>% 
    select(name, alpha_2, region, sub_region, intermediate_region) %>%
    filter(region == "Africa")
  
  Africa[15,1] <- "Ivory Coast"
  Africa[14,1] <- "Democratic Republic of the Congo"
  Africa[13,1] <- "Republic of Congo"
  Africa[54,1] <- "United Republic of Tanzania"
  Africa[20,1] <- "Swaziland"
  Africa[27,1] <- "Guinea Bissau"
  Africa[16,1] <- "Djibouti"
  
  
  
  for (i in 1:nrow(Africa)) {
    if (is.na(Africa$intermediate_region[i])) {
      Africa$intermediate_region[i] <- Africa$sub_region[i]
    }
  }
  
  Africa$sub_region <- NULL
  names(Africa) <- c("country", "iso_a3", "region", "sub_regin")
  
  
  coronavirus$country <- str_replace_all(coronavirus$country,
                                         "Cote d'Ivoire", "Ivory Coast")
  
  coronavirus[coronavirus$country == "Congo (Kinshasa)","country"] <- "Democratic Republic of the Congo"
  coronavirus[coronavirus$country == "Congo (Brazzaville)" ,"country"] <- "Republic of Congo"
  coronavirus[coronavirus$country == "Tanzania" ,"country"] <- "United Republic of Tanzania"
  coronavirus[coronavirus$country == "Eswatini" ,"country"] <- "Swaziland"
  coronavirus[coronavirus$country == "Guinea-Bissau" ,"country"] <- "Guinea Bissau"
  coronavirus[coronavirus$country == "Djibouti" ,"country"] <- "Djibouti"
  
  
  
  # !diagnostics suppress = country, type, cases, total_cases, confirmed
  # !diagnostics suppress=  group_by, summarise, pivot_wider, arrange
  
  data_africa_covid <- coronavirus %>%
    filter(country %in% Africa$country) %>% 
    select(date, country, type, cases)%>% 
    split(.$date) %>% 
    purrr::map(.f = ~tidyr::pivot_wider(.x, names_from = type,
                                        values_from = cases)) %>% 
    map_dfr(~ as.data.frame(.x))
  
  data_africa_covid$iso_a3 <- vector(mode = "character", length = nrow(data_africa_covid))
  data_africa_covid$subregion <- vector(mode = "character", length = nrow(data_africa_covid))
  
  for (p in unique(data_africa_covid$country)) {
    data_africa_covid[data_africa_covid$country == p,"subregion"] <- Africa$sub_regin[Africa$country == p]
    data_africa_covid[data_africa_covid$country == p,"iso_a3"] <- Africa$iso_a3[Africa$country == p]
  }
  
  
  

  rm(Africa, all, coronavirus)
 return(data_africa_covid)
}


# replace <0 by 0
#-----------------------------------------------------
replcace_inf_0 <- function (x){
  
  x <- ifelse(x<0, 0, x)
  return(x)
}


