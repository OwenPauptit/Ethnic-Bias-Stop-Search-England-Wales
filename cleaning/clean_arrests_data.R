library(janitor)
library(tidyverse)
library(readr)

# Arrests data before 2019 has different ethnic grouping

arrests_dirty_pre_2019 <- read_csv("data/arrests/arrests-open-data-tables-mar24(OD 5+1).csv",
                                    col_types = list("number_of_arrests" = 'c')) %>%
  filter(
    financial_year < "2019/20",
    financial_year >= "2015/16" # Arrests categories change in 2015/16 - not directly comparable to earlier
  )

arrests_pre_2019 <- arrests_dirty_pre_2019  %>%
  rename(
    police_force = police_force_area
  ) %>%
  mutate(
    number_of_arrests = as.integer(gsub(',', '', number_of_arrests)),
    
    ethnicity = case_when(
      self_defined_ethnicity_group %in% c("Asian or Asian British") ~ "asian",
      self_defined_ethnicity_group == "Black or Black British" ~ "black",
      self_defined_ethnicity_group == "Not stated" ~ "unknown",
      tolower(self_defined_ethnicity_group) == "chinese/other" ~ "other",
      .default = tolower(self_defined_ethnicity_group)
    ),
    
    police_force = case_when(
      startsWith(police_force, "Metropolitan") ~ "metropolitan",
      startsWith(police_force, "London") ~ "city of london",
      .default = tolower(police_force)
    ),
    
    police_force = gsub(" ", "_", police_force),
    police_force = gsub("-", "_", police_force),
    
    crime_type = case_when(
      reason_for_arrest %in% c("2015/16 onwards - Robbery", 
                               "2015/16 onwards - Theft offences",
                               "Burglary", 
                               "Robbery",
                               "Theft and handling stolen goods") ~ "theft",
      reason_for_arrest %in% c("2015/16 onwards - Drug offences",
                               "Drug offences") ~ "drugs",
      reason_for_arrest == "2015/16 onwards - Possession of weapons offences" ~ "weapons", # No weapons category for 2014/15
      .default = "other"
    )
  ) %>%
  group_by(financial_year, police_force, ethnicity, crime_type) %>%
  summarise(number_of_arrests = sum(number_of_arrests))

# Post 2019 Arrests data - Chinese is now grouped with Asian, need to use 19+1 grouping to regroup 
# Chinese with other in order to match pre 2019 data

arrests_dirty_post_2019 <- read_csv("data/arrests/arrests-open-data-tables-mar24(OD 19+1).csv",
                                    col_types = list("number_of_arrests" = 'c'))

arrests_post_2019 <- arrests_dirty_post_2019 %>%
  rename(
    police_force = police_force_area
  ) %>%
  mutate(
    number_of_arrests = as.integer(gsub(',', '', number_of_arrests)),
    
    ethnicity = case_when(
      self_defined_ethnicity == "Chinese" ~ "other", # Need to put Chinese with other to match pre-2019
      self_defined_ethnicity_group %in% c("2019/20 onwards - Asian or Asian British", "Asian or Asian British") ~ "asian",
      self_defined_ethnicity_group == "Black or Black British" ~ "black",
      self_defined_ethnicity_group == "Not stated" ~ "unknown",
      tolower(self_defined_ethnicity_group) == "2019/20 onwards - other ethnic group" ~ "other",
      .default = tolower(self_defined_ethnicity_group)
    ),
    
    police_force = case_when(
      startsWith(police_force, "Metropolitan") ~ "metropolitan",
      startsWith(police_force, "London") ~ "city of london",
      .default = tolower(police_force)
    ),
    
    police_force = gsub(" ", "_", police_force),
    police_force = gsub("-", "_", police_force),
    
    crime_type = case_when(
      reason_for_arrest %in% c("2015/16 onwards - Robbery", 
                        "2015/16 onwards - Theft offences") ~ "theft",
      reason_for_arrest == "2015/16 onwards - Drug offences" ~ "drugs",
      reason_for_arrest == "2015/16 onwards - Possession of weapons offences" ~ "weapons",
      .default = "other"
    )
  ) %>%
  group_by(financial_year, police_force, ethnicity, crime_type) %>%
  summarise(number_of_arrests = sum(number_of_arrests))

# Concatenate pre and post 2019

arrests <- rbind(arrests_pre_2019, arrests_post_2019)

write.csv(arrests, file="data/cleaned/arrests.csv")
