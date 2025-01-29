# Load required libraries
library(tidycensus)
library(tidyverse)
library(dplyr)


# Set your Census API key (replace with your actual API key if needed)
Sys.getenv("CENSUS_API_KEY")

# Define the years for data extraction
years <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

# Function to get population, income, socioeconomic, homeownership, and race demographics data for a specific year
get_data_for_year <- function(year) {
  
  # Fetching county-level data for Virginia and North Carolina
  county_data_test <- get_acs(
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
    state = c("51", "37")  # FIPS codes for Virginia (51) and North Carolina (37)
  )
  
  # Clean up and reshape the data
  county_data_clean_test <- county_data_test %>%
    select(GEOID, NAME, variable, estimate) %>%
    spread(key = variable, value = estimate)
  
  # Split 'NAME' column into 'county' and 'state'
  county_data_clean_test <- county_data_clean_test %>%
    separate(NAME, into = c("county", "state"), sep = ", ")
  
  # Add the 'year' column
  county_data_clean_test <- county_data_clean_test %>%
    mutate(year = year) %>%
    relocate(year, .after = state)
  
  return(county_data_clean_test)
}

# Fetch and combine data for all years
all_county_data <- map_dfr(years, get_data_for_year)


# Relocate population column
all_county_data <- all_county_data %>%
  relocate(population, .after = year)

# Clean 'county' names
all_county_data$county <- gsub("\\bcity\\b", "City", all_county_data$county, ignore.case = TRUE)

# View the first few rows of the data
head(all_county_data)

# Combine the two datasets by rows
va_2022 <- bind_rows(va_place_data, va_data_2022)


# Load proposal data 
proposal_dataset <- read.csv("/Users/sikandar/Desktop/proposal_dataset.csv")


# Merge the county data with the proposal data using GEOID
final_data <- proposal_dataset %>%
  left_join(va_2022, by = "GEOID")

# View the merged dataset
head(final_data)

# Save the final merged data
write.csv(final_data, "/Users/sikandar/Documents/Work/Research /Disaster Relief Fuding/merged_dataset_with_race.csv", row.names = FALSE)

cat("Final merged dataset with race demographics has been saved.")
