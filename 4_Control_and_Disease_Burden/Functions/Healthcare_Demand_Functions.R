# Generate Standard Population
generate_standard_population <- function(country_name) {
  population <- get_population(country_name)
  raw_pop <- population$n
  raw_total_pop <- sum(raw_pop)
  adj_pop <- 50000000/raw_total_pop * raw_pop
  return(adj_pop)
}

# Generate Income Strata Specific Total ICU Beds
generate_ICU_beds <- function(standard_population, income_strata, income_strata_healthcare_capacity) {
  tot_pop <- sum(standard_population)
  if (income_strata == "LIC") {
    ICU_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$ICU_beds[1]
  } else if (income_strata == "LMIC") {
    ICU_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$ICU_beds[2]
  } else if (income_strata == "UMIC") {
    ICU_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$ICU_beds[3]
  } else if (income_strata == "HIC") {
    ICU_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$ICU_beds[4]
  } else {
    return("incorrect")
  }
  return(ICU_beds)
}

# Generate Income Strata Specific Total ICU Beds
generate_hosp_beds <- function(standard_population, income_strata, income_strata_healthcare_capacity) {
  tot_pop <- sum(standard_population)
  if (income_strata == "LIC") {
    hosp_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$hosp_beds[1]
  } else if (income_strata == "LMIC") {
    hosp_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$hosp_beds[2]
  } else if (income_strata == "UMIC") {
    hosp_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$hosp_beds[3]
  } else if (income_strata == "HIC") {
    hosp_beds <- (tot_pop / 1000) *  income_strata_healthcare_capacity$hosp_beds[4]
  } else {
    return("incorrect")
  }
  return(hosp_beds)
}