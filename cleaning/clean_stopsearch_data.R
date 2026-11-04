library(janitor)
library(tidyverse)
library(readr)
library(lubridate)
library(skimr)

Sys.setenv(VROOM_CONNECTION_SIZE = "5000000")

read_and_name_file <- function(filename, month) {
  #' Read a single stopsearch csv with name `filename` in month directory `month`

  file = read_csv(paste0("data/stop-and-search/", month, "/", filename))
  
  # String lengths to extract police force from filename
  prefix_len = str_length("xxxx-xx-")
  suffix_len = str_length("-stop-and-search.csv")
  filename_len = str_length(filename)
  
  # Add police force as column (since information was previously only held in the filename)
  file['police force'] <- substring(filename, prefix_len + 1, filename_len-suffix_len)
  
  return(file)
}

merge_police_forces <- function(month){
  #' Merge all stop and search csv files in month `month`
  
  # List all the files in this month's directory
  files = list.files(path = paste0("data/stop-and-search/", month),
                     pattern = "*-stop-and-search.csv")
  
  # Merge files
  merged <- do.call(rbind, lapply(files, read_and_name_file, month = month))
  return(merged)
}

financial_year <- function(date) {
  #' Compute financial year from a datetime 
  fy <- year(date) - (month(date) < 4 | (month(date) == 4 & mday(date) < 6))
  paste0(fy, "/", (fy+1)%%100)
}

# List all months in the stop and search directory
months <- list.files(path = "data/stop-and-search")

# Merge all stop and search files
stopsearch_dirty <- do.call(rbind, lapply(months, merge_police_forces))

# Clean the stopsearch data
stopsearch_unfiltered <- stopsearch_dirty %>%
  clean_names() %>%
  mutate(
    # Map object of search to generalised crime type
    crime_type = case_when(
      object_of_search %in% c("Controlled drugs", "Psychoactive substances") ~ "drugs",
      object_of_search %in% c("Article for use in theft", "Stolen goods") ~ "theft",
      object_of_search %in% c("Offensive weapons", "Crossbows", "Firearms") ~ "weapons",
      endsWith(object_of_search, "unavailable") ~ "unknown",
      is.na(object_of_search) ~ "unknown",
      .default = "other"
    ),
    
    police_force = gsub("-", "_", police_force),
    
    # Extract features from datetime
    financial_year = financial_year(date),
    previous_financial_year = financial_year(date - years(1)),
    month = month(date),
    year = year(date),
    
    # Map self defined ethnicities to be more general
    self_defined_ethnicity_simple = case_when(
      endsWith(self_defined_ethnicity, "Chinese") ~ "other", # Needs to match arrests data
      startsWith(self_defined_ethnicity, "Chinese") ~ "other", 
      endsWith(self_defined_ethnicity, "Not stated") ~ "unknown",
      startsWith(self_defined_ethnicity, "Not Stated") ~ "unknown",
      startsWith(self_defined_ethnicity, "Mixed") ~ "mixed",
      startsWith(self_defined_ethnicity, "Black") ~ "black",
      startsWith(self_defined_ethnicity, "Asian") ~ "asian",
      startsWith(self_defined_ethnicity, "White") ~ "white",
      startsWith(self_defined_ethnicity, "Other") ~ "other",
      .default = self_defined_ethnicity
    ),
    
    # Replace NA with unknown and make lower
    self_defined_ethnicity_simple = replace_na(self_defined_ethnicity_simple, "unknown"),
    officer_defined_ethnicity = tolower(replace_na(officer_defined_ethnicity, "unknown"))
  ) %>%
  filter(
    financial_year < "2024/25", # Remove financial years for which we do not have full dataset
    financial_year > "2014/15"
  )

stopsearch_no_vehicles <- stopsearch_unfiltered %>%
  filter(type != 'Vehicle search')

stopsearch <- stopsearch_no_vehicles %>%
  filter(self_defined_ethnicity_simple %in% c("white", "black", "asian"),
         officer_defined_ethnicity %in% c("white", "black", "asian"))

stopsearch_aggregated <- stopsearch %>%
  group_by(officer_defined_ethnicity, police_force, crime_type, previous_financial_year) %>%
  summarise(number_of_stops = n())

# Save data
write.csv(stopsearch_unfiltered, file = "data/cleaned/stopsearch_unfiltered.csv")
write.csv(stopsearch_aggregated, file="data/cleaned/stopsearch_aggregated.csv")
