# Maternal Mortality
maternal_mortality <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Maternal_Mortality_Ratio.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(maternal_mortality)[2:length(colnames(maternal_mortality))] <- gsub('X', '', colnames(maternal_mortality)[2:length(colnames(maternal_mortality))])
maternal_mortality <- maternal_mortality %>%
  gather(year, maternal_mortality, -country_code)

# Electricity
electricity <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Electricity.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(electricity)[2:length(colnames(electricity))] <- gsub('X', '', colnames(electricity)[2:length(colnames(electricity))])
electricity <- electricity %>%
  gather(year, electricity, -country_code)

# Proportion Population 0-14
prop_pop <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Pop_Prop.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(prop_pop)[2:length(colnames(prop_pop))] <- gsub('X', '', colnames(prop_pop)[2:length(colnames(prop_pop))])
prop_pop <- prop_pop %>%
  gather(year, prop_pop, -country_code)

# Teacher Pupil Ratio 
school_ratio <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Teacher_Pupil.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(school_ratio)[2:length(colnames(school_ratio))] <- gsub('X', '', colnames(school_ratio)[2:length(colnames(school_ratio))])
school_ratio <- school_ratio %>%
  gather(year, school_ratio, -country_code)

# Rural
rural <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Rural_Percentage.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(rural)[2:length(colnames(rural))] <- gsub('X', '', colnames(rural)[2:length(colnames(rural))])
rural <- rural %>%
  gather(year, rural, -country_code)

# Domestic Expenditure
domestic_GDP <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Domestic_Healthcare_Expenditure_GDP.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(domestic_GDP)[2:length(colnames(domestic_GDP))] <- gsub('X', '', colnames(domestic_GDP)[2:length(colnames(domestic_GDP))])
domestic_GDP <- domestic_GDP %>%
  gather(year, domestic_GDP, -country_code)

# Infant Mortality
infant_mortality <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_Infant_Mortality_Ratio.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(infant_mortality)[2:length(colnames(infant_mortality))] <- gsub('X', '', colnames(infant_mortality)[2:length(colnames(infant_mortality))])
infant_mortality <- infant_mortality %>%
  gather(year, infant_mortality, -country_code)

# School Enrollment
school_enrollment <- read.csv("Data/Hospital_Bed_Capacity_Data/World_Bank_Covariates/World_Bank_School_Enrollment.csv", stringsAsFactors = FALSE, header = TRUE, fileEncoding = 'UTF-8-BOM')
colnames(school_enrollment)[2:length(colnames(school_enrollment))] <- gsub('X', '', colnames(school_enrollment)[2:length(colnames(school_enrollment))])
school_enrollment <- school_enrollment %>%
  gather(year, school_enrollment, -country_code)
