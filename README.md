# Overview
This repository contains code, data, and other documentation for a study on the effects of spatial closures for offshore energy infrastructure on fisheries abundance estimation. Code accompanies O'Brien et al 2026 paper titled "Quantifying potential impacts of ocean-use sectors on biomass and density estimates for fisheries stock assessments" by Kaitlyn O’Brien, Kelly Andrews, Brian Langseth, and Aaron Berger at the NOAA Northwest Fisheries Science Center. These files were downloaded from the team's Google Drive folder "Project-1 WEA Resampling" and added to this repository in June 2026.

## Citation
O’Brien, K.A., Andrews, K.S., Langseth, B.J., and Berger, A.M. 2026. Quantifying potential impacts of ocean-use sectors on biomass and density estimates for fisheries stock assessments. Fisheries Research. DOI: https://doi.org/10.1016/j.fishres.2026.107749

## Repo structure
/data =  data for sdmTMB models. Within the /data folder, the .rds files are the data used for each model. 

/model = rds files for fitted sdmTMB models. Within the /model folder, the folders /Depth and No_Depth indicate whether depth was used as a covariate or not. The subfolders within Depth or No_Depth indicate which spatial and temporal scenarios (18 in total) were modeled, as further described in O'Brien et al., 2026.

The files in the main folder of the repository are codes to run the spatiotemporal models, fit each retrospective analysis per species, and codes for analysis and figure creation. 

## Developers
Kaitlyn O’Brien was the lead developer for this study. As of June 2026, she can be reached at kaobrien4 [at] alaska.edu. Kaitlyn’s GitHub account name is kaobrien-fish. Question may be directed to her or to Kelly Andrews at the Northwest Fisheries Science Center by emailing him at kelly.andrews [at] noaa.gov 

## Data used for this study
For more information about the data from the NOAA West Coast Groundfish Bottom Trawl Survey used in this study, please refer to: https://www.fisheries.noaa.gov/west-coast/science-data/us-west-coast-groundfish-bottom-trawl-survey 

These bottom trawl survey data can be accessed in the Northwest Fisheries Science Center (NWFSC) Survey repository: https://pfmc-assessments.github.io/nwfscSurvey/index.html))

Instructions for downloading the data are available here: https://pfmc-assessments.github.io/nwfscSurvey/index.html

The location of these data may be updated over time to improve data management, accessibility, and findability. 

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## License
This content was created by U.S. Government employees as part of their official duties. This content is not subject to copyright in the United States (17 U.S.C. §105) and is in the public domain within the United States of America. Additionally, copyright is waived worldwide through the MIT License.

<img src="https://raw.githubusercontent.com/nmfs-fish-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" width="200" style="height: 75px !important;"   alt="NOAA Fisheries Logo">

