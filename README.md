# Overview

This repository contains all the code required to reach the conclusions made in: "A Hierarchical Bayesian Analysis of Ethnic Bias in Police Stop and Search in England and Wales".

The data were retrieved from: https://data.police.uk/ (stop and search, force boundaries), https://www.gov.uk/government/statistics/stop-and-search-and-arrests-year-ending-march-2023 (arrests), https://www.ons.gov.uk/datasets/RM032/editions/2021/versions/1#get-data (census) and https://geoportal.statistics.gov.uk/datasets/66df1640526a4528a78736208daf83ed_0/explore (local authority to police force area lookup).

## Cleaning

The data were cleaned with the filed available in /cleaning, in the following order:

 1. clean_arrests_data.R, clean_census_data.R, clean_maps_data.R, clean_stopsearch_data.R
 
 2. merge_cleaned_data.R
 
 3. remove_forces_with_missing_data.R

## Descriptive analysis 

The short descriptive analysis was performed using code from: /analysis/descriptive_stats.Rmd

## Stan model

The Stan model code is in: /fitting/model.stan

It uses frequentist estimates found in /fitting/frequentist_analysis.Rmd

The model was fit remotely using: /fitting/remote_stan_fit.R

A .csv file is created alongside trace plots, summary statistics and plots of posterior predictive checks

The plots found in the report are created in: /analysis/analyse_stan_fit.R

## INLA model

The INLA model is fitted and analysed in: /analysis/fit_with_inla.R

Sensitivity analysis is also conducted in /analysis/fit_with_inla.R

All visualisations included in the report, when created, will be stored in /visualisations
