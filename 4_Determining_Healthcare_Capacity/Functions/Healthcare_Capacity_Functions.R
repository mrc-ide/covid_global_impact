select_LMICs <- function(world_bank_data, LMIC_countries) {
  world_bank_data <- world_bank_data %>% 
    filter(country_code %in% LMIC_countries)
}

append_info <- function(hospital_beds_data, world_bank_metadata, country_selection) {
  
  hospital_beds_data <- hospital_beds_data %>%
    filter(country_code %in% country_selection)
  
  world_bank_metadata <- world_bank_metadata %>%
    filter(country_code %in% country_selection)
  
  hospital_beds_data <- hospital_beds_data %>%
    left_join(world_bank_metadata, "country_code") %>%
    select(country_name, country_code, region, income_group, everything())
  
  return(hospital_beds_data)
}

extract_latest_value <- function(world_bank_data, variable) {
  latest_data <- data.frame(country_code = character(), type = character(), value = double(), year = integer(), stringsAsFactors = FALSE)
  for (i in 1:nrow(world_bank_data)) {
    temp <- world_bank_data[i, ]
    temp_country_code <- temp$country_code
    na_trimmed_values <- na.trim(unlist(temp[5:length(temp)])) 
    if (length(na_trimmed_values) == 0) {
      temp_value <- NA
      temp_year <- NA
    } else {
      temp_value <- unname(na_trimmed_values[length(na_trimmed_values)])
      temp_year <- max(names(na_trimmed_values))
    }
    latest_data <- rbind(latest_data, data.frame(country_code = temp_country_code,
                                                 type = variable,
                                                 value = temp_value, 
                                                 year = temp_year))
  }
  return(latest_data)
}

detect_rows_all_NAs <- function(world_bank_data) {
  all_NAs <- c()
  for (i in 1:nrow(world_bank_data)) {
    temp <- world_bank_data[i, ]
    if (sum(!is.na(temp)) == 0) {
      all_NAs <- c(all_NAs, i)
    } else {
      
    }
  }
  return(all_NAs)
}
