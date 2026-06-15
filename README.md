# Project-1-WEA-Resampling
For training purposes, create a repo and recreate results from Project 1 in spatial fisheries management work at NWFSC done by OE&amp;I team.

Files downloaded from the team's Google Drive folder "Project-1 WEA Resampling". 

## Description
Effects of spatial closures for offshore energy infrastructure on fisheries abundance estimation. Code accompanies O'Brien et al 2026 paper titled "Quantifying potential impacts of ocean-use sectors on biomass and density estimates for fisheries stock assessments" by Kaitlyn A. O’Brien, Kelly S. Andrews, Brian J. Langseth, and Aaron M. Berger at the NOAA Northwest Fisheries Science Center. 

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## How this repository is organized, by folder name
/data =  data for sdmTMB models. Within the /data folder, the .rds files are the data used for each model. 

/model = code for running sdmTMB models. Within the /model folder, the folders /Depth and No_Depth indicate whether depth was used as a covariate or not. The subfolders within Depth or No_Depth indicate which spatial and temporal scenarios (18 in total) was modeled, as further described in O'Brien et al., 2026. 

The files in the main folder of the repository are _______________

## Data used for this study
For more information about the data from the NOAA West Coast Groundfish Bottom Trawl Survey used in this study, please refer to: https://www.fisheries.noaa.gov/west-coast/science-data/us-west-coast-groundfish-bottom-trawl-survey 

These bottom trawl survey data can be accessed in the Northwest Fisheries Science Center (NWFSC) Survey repository: https://pfmc-assessments.github.io/nwfscSurvey/index.html))

Instructions for downloading the data are available here: https://pfmc-assessments.github.io/nwfscSurvey/index.html

The location of these data may be updated over time to improve data management, accessibility, and findability. 
