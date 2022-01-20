# COVID_Vaccine_Accessibility
## Introduction
The global Covid-19 pandemic has caused numerous deaths and illnesses and posed unprecedented social and economic challenges to many countries. One of the key strategies to contain the pandemic is mass vaccination. While it is essential to ensure safe and easy accessibility to Covid-19 vaccines for all communities, limited research has been carried out to understand and validate the spatial accessibility of these vaccines. This study addresses this gap by measuring and validating the spatial accessibility to Covid-19 vaccines with a particular focus on England, United Kingdom. More specifically, we compare three floating catchment area (FCA) methods with differing parameters for measuring the small-scale spatial accessibility to vaccination services. Then, we calibrate these accessibility measurements using a beta regression model and the reported vaccination uptake rates. The results show that the three-step FCA method with a distance parameter of 30 miles is the optimal model for measuring the spatial accessibility to Covid-19 vaccines. The findings provide an improved understanding of the spatial inequality of vaccine services. Further, the framework of calibrating spatial accessibility to vaccine services is generalisable to other types of healthcare services. 

The preprint of this paper is available [here](https://osf.io/preprints/socarxiv/xvnps/) on SocArXiv. (The paper is pending and will be publicly available after approved by a moderator.)

## Key analysis steps:
1. Run **Data/data_preprocss.ipynb** to download and process data;
1. Run **driving_cost_calculation_OSRM.ipynb** to compute the Euclidean distance between MSOAs and vaccine sites that are within 30 miles. This notebook will generate a file of euclidean_distance_30mile.csv.
1. Run **distance_cost_computation.py** to compute the distance matrix. The input to this Python file is *euclidean_distance_30mile.csv*.
1. Run **FCA_result_calculation.ipynb** to compute the accessibility scores using different methods and parameters. This notebook will generate the data file of *fca_result_before_norm_exclude_London.csv*.
1. Run **FCA_result_plot.ipynb** to plot the results and generate the data file of *accessibility_imd_ethnic_exclude.csv*.
1. Run **beta_reg_15_accessibility_scores.Rmd** (in RStudio) to implement and compare beta regression models for vaccine uptake rates based on 15 different  accessibility results. The input of this Rmd file is *accessibility_imd_ethnic_exclude.csv*.
1. Run **beta_reg_optimal_model.Rmd** (in RStudio) to analyse the optimal beta regression model.


## Key data files:
1. **accessibility_imd_ethnic_exclude.csv**: a dataframe consisting of 15 accessibility scores/IMD/car ownership/ethnic composition for all MSOAs in England, excluding the Greater London. Note that the accessibility score in this file is in the unit of 10e(-5).
1. **euclidean_distance_30mile.csv**: a dataframe of MSOAs and vaccine sites that are less than 30 miles away. The cut-off distance of 30 miles is used as the largest bandwidth of the FCA methods is 30 miles. This file is generated by the notebook driving_cost_calculation_OSRM.ipynb. (Due to its large size, this file is not added in this repo)
