# Load required libraries
library(tidycensus)
library(tidyverse)
library(dplyr)


# Set Census API key 
Sys.getenv("CENSUS_API_KEY")

# Defining the year for data extraction
years <- c(2022)

# Function to get population, income, socioeconomic, homeownership, and race demographics data for a specific year
get_data_for_year <- function(year) {
  
  # Fetching county-level data for Virginia and North Carolina
  county_data <- get_acs(
    geography = "county",
    variables = c(
      population = "B01003_001E",    # Total population
      median_income = "B19013_001E", # Median household income
      poverty = "B17021_002E",  # Poverty rate
      unemployment = "B23025_005E",  # Unemployment rate
      population_under_5 = "B01001_003E",  # Population under 5 years
      population_over_65 = "B01001_020E",  # Population over 65 years
      homeownership = "B25003_002E",  # Owner-occupied housing units
      # Race demographics
      white_population = "B03002_003E",  # White population
      black_population = "B03002_004E",  # Black or African American population
      asian_population = "B03002_006E",  # Asian population
      native_population = "B03002_005E",  # American Indian and Alaska Native
      pacific_islander = "B02001_006"  # Pacific Icelander 
    ),
    year = year,
    survey = "acs5",
    state = c("51")  # FIPS codes for Virginia (51) 
  )
  
  # Fetching place-level data for Virginia 
  place_data <- get_acs(
    geography = "place",
    variables = c(
      population = "B01003_001E",    # Total population
      median_income = "B19013_001E", # Median household income
      poverty = "B17021_002E",  # Poverty rate
      unemployment = "B23025_005E",  # Unemployment rate
      population_under_5 = "B01001_003E",  # Population under 5 years
      population_over_65 = "B01001_020E",  # Population over 65 years
      homeownership = "B25003_002E",  # Owner-occupied housing units
      # Race demographics
      white_population = "B03002_003E",  # White population
      black_population = "B03002_004E",  # Black or African American population
      asian_population = "B03002_006E",  # Asian population
      native_population = "B03002_005E",  # American Indian and Alaska Native
      pacific_islander = "B02001_006"  # Pacific Icelander 
    ),
    year = year,
    survey = "acs5",
    state = c("51")  # FIPS codes for Virginia (51)
  )
  
  #merging county data with place data
  virginia_data_combined <- bind_rows(county_data, place_data)
  
  #now fetching data for disabilty and carownership through ACS
  variables <- c(
    disability = "S1810_C02_001",  # Population with a disability (total)
    car_ownership = "B25044_003"   # Households with no vehicle available
  )
  
  # Get the ACS data for Virginia for 2022 places
  virginia_dis_place <- get_acs(
    geography = "place",
    variables = variables,
    state = "VA",
    year = 2022,
    survey = "acs5",
    output = "wide"
  )
  
  variables <- c(
    disability = "S1810_C02_001",  # Population with a disability (total)
    car_ownership = "B25044_003"   # Households with no vehicle available
  )
  
  # Get the ACS data for Virginia for 2022 places
  virginia_dis_county <- get_acs(
    geography = "county",
    variables = variables,
    state = "VA",
    year = 2022,
    survey = "acs5",
    output = "wide"
  )
  #merging county data with place data
  virginia_dis_combined <- bind_rows(virginia_dis_county, virginia_dis_place)
  
  # Clean up and reshape the data
  virginia_data_combined <- virginia_data_combined %>%
    select(GEOID, NAME, variable, estimate) %>%
    spread(key = variable, value = estimate)
  
  # Split 'NAME' column into 'county' and 'state'
  virginia_data_combined <- virginia_data_combined %>%
    separate(NAME, into = c("county", "state"), sep = ", ")
  
  # Add the 'year' column
  virginia_data_combined <- virginia_data_combined %>%
    mutate(year = year) %>%
    relocate(year, .after = state)
  
  return(virginia_data_combined)
}

# Relocate population column
virginia_data_combined <- virginia_data_combined %>%
  relocate(population, .after = year)

# Clean 'county' names
virginia_data_combined$county <- gsub("\\bcity\\b", "City", virginia_data_combined$county, ignore.case = TRUE)

# View the first few rows of the data
head(virginia_data_combined)

# Load proposal data 
proposal_dataset <- read.csv("/Users/sikandar/Desktop/proposal_dataset.csv")

# Merge the county data with the proposal data using GEOID
cfpf_complete_dataset <- proposal_dataset %>%
  left_join(virginia_data_combined, by = "GEOID")

# View the merged dataset
head(cfpf_complete_dataset)

# Save the final merged data
write.csv(cfpf_complete_dataset, "/Users/sikandar/Documents/Work/Research /Disaster Relief Fuding/merged_dataset_with_race.csv", row.names = FALSE)

