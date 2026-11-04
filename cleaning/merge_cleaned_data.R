library(tidyverse)
library(readr)

arrests <- read_csv("data/cleaned/arrests.csv")[,-1]
census <- read_csv("data/cleaned/census_cleaned.csv")[,-1]
stopsearch <- read_csv("data/cleaned/stopsearch_aggregated.csv")[,-1]

stopsearch_arrests <- merge(stopsearch, 
      arrests,
      by.x = c("police_force", "officer_defined_ethnicity", "previous_financial_year", "crime_type"),
      by.y = c("police_force", "ethnicity", "financial_year", "crime_type")
)

stopsearch_arrests <- merge(stopsearch_arrests, census,
      by = "police_force")

write.csv(stopsearch_arrests, "data/cleaned/stopsearch_arrests.csv", row.names=FALSE)
