# Overview

This repository contains all the code for Pauptit, O., Inacio De Carvalho, V., Lindgren, F. and Sterratt, D. "Assessing Ethnic Bias in Stop and Search Across
Police Forces in England and Wales". Submitted to The Journal of the Royal Statistical Society Series A: Statistics in Society for peer review (2026).

The data were retrieved from: https://data.police.uk/ (stop and search, force boundaries), https://www.gov.uk/government/statistics/stop-and-search-and-arrests-year-ending-march-2023 (arrests), https://www.ons.gov.uk/datasets/RM032/editions/2021/versions/1#get-data (census) and https://geoportal.statistics.gov.uk/datasets/66df1640526a4528a78736208daf83ed_0/explore (local authority to police force area lookup).

## Cleaning

The data were cleaned with the filed available in /cleaning, in the following order:

 1. clean_arrests_data.R, clean_census_data.R, clean_maps_data.R, clean_stopsearch_data.R
 
 2. merge_cleaned_data.R
 
 3. remove_forces_with_missing_data.R

## Analysis

Running `analysis.Rmd` will produce the plots and tabular output used in the paper.
