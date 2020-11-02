# The effect of internationally imported cases on internal spread of COVID-19: a mathematical modelling study

## Contents in the repository
The repo contains all the functions and scripts required to reproduce the results of (this paper)[https://www.medrxiv.org/content/10.1101/2020.07.12.20152298v1]
and to run user-specific analyses, with for example, private flight data and/or prevalence & incidence estimates. In the absense of prevalence and incidence
estimats, the analysis defaults to using September 2020 estimates as per the paper. The repo does not contain the OAG air passenger volume dataset, as this
is not publicly available and must be purchased. However, we have set up a pipeline for users with their own flight data for any number of countries that runs 
our analysis, as long as the data is in the correct format. For more details, see 

## Summary of the methods
* We use the methods from [Reconstructing the early global dynamics...](https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-020-01790-9) to estimate time-varying underascertainment rates globally
* We use estimates of prevalence in each departure country and air passenger volumes (under various assumed scenarios, i.e. May 2019, May 2020, Sept 2019 and Sept 2020 traveller volumes) to estimate the number of expected imported cases into each arrival country
* We also calculate incidence estimates within each country
* Finally, we calculate the ratio of expected imported cases and the internal incidence and plot the resulting ratio on a Map for all countries that we have data for

To download the code, clone this repository using the command

```sh
git clone https://github.com/thimotei/covid_travel_restrictions
```

To reproduce all of the figures in the manuscript and the quoted results in the main text, run the script 
```r
scripts/main_script.R
```

Most of the other scripts are to produce figures or results for the sensitivity analyses in the Supplementary Material.

## Re-running analysis with user-specific flight data

Users wishing to run our analysis with their own flight and/or prevalence + incidence estimates should run

```r
scripts/user_friendly_analysis_example.R
```

Everything required to run the whole analysis is in this script. This script also includes example dataframes, outlining what data is required and
what format it needs to be in. Users do not need to have data for all countries to run this analysis. Any countries with air passenger, prevalence
and incidence estimates (prevalence and incidence estimates are provided and default to September 2020 in the absense of any user data for these). 
