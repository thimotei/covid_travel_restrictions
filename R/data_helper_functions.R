# # new version of the function, much more streamlined
# DEPRACTED - CHECK TO SEE IF NOT NEEDED BEFORE DELETING 
# get_adjusted_case_data_national <- function()
# {
#   
#   asymptomatic_mid <- 0.5
#   asymptomatic_low <- 0.1
#   asymptomatic_high <- 0.7
#   
#   ecdc_case_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
#     dplyr::rename(new_cases = cases,
#                   new_deaths = deaths,
#                   country = countriesAndTerritories,
#                   iso_code = countryterritoryCode) %>%
#     dplyr::mutate(date = lubridate::dmy(dateRep)) %>%
#     dplyr::filter(new_cases >= 0)
#   
#   under_reporting_path <- here("data/under_reporting_estimates/")
#   files <- dir(path = under_reporting_path,
#                pattern = "*.rds")
#   
#   under_reporting_raw_data <- dplyr::tibble(countryCode = files) %>% 
#     dplyr::mutate(file_contents = purrr::map(countryCode, 
#                                              ~ readRDS(file.path(under_reporting_path, .)))) %>% 
#     tidyr::unnest(cols = c(file_contents)) %>%
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
#     dplyr::group_by(countryCode) %>%
#     dplyr::select(date, everything()) %>%
#     dplyr::select(date, countryCode, everything()) %>%
#     dplyr::group_by(countryCode) %>%
#     dplyr::ungroup() %>%
#     dplyr::rename(iso_code = countryCode)
#   
#   
#   under_reporting_and_case_data <- ecdc_case_data %>% 
#     dplyr::left_join(under_reporting_raw_data) %>%
#     dplyr::group_by(country) %>%
#     dplyr::arrange(country, date) %>%
#     tidyr::drop_na() %>%
#     dplyr::select(date, iso_code, country, new_cases, new_deaths, popData2019, estimate, lower, upper)
#   
#   
#   dataOut <- under_reporting_and_case_data %>%
#     dplyr::group_by(country) %>%
#     dplyr::mutate(new_cases_smoothed             = zoo::rollmean(new_cases, k = 7, fill = NA),
#                   new_cases_adjusted_mid         = new_cases/estimate,
#                   new_cases_adjusted_low         = new_cases/upper,
#                   new_cases_adjusted_high        = new_cases/lower,
#                   new_cases_adjusted_smooth_mid  = zoo::rollmean(new_cases_adjusted_mid, k = 7, fill = NA),
#                   new_cases_adjusted_smooth_low  = zoo::rollmean(new_cases_adjusted_low, k = 7, fill = NA),
#                   new_cases_adjusted_smooth_high = zoo::rollmean(new_cases_adjusted_high, k = 7, fill = NA)) %>%
#     dplyr::mutate(cumulative_incidence_mid  = cumsum(new_cases_adjusted_mid)/(popData2019*(1 - asymptomatic_mid)),
#                   cumulative_incidence_low  = cumsum(new_cases_adjusted_low)/(popData2019*(1 - asymptomatic_low)),
#                   cumulative_incidence_high = cumsum(new_cases_adjusted_high)/(popData2019*(1 - asymptomatic_high))) %>%
#     dplyr::mutate(date_infection  = date - 9) %>%
#     dplyr::mutate(cumulative_incidence_mid = dplyr::case_when(cumulative_incidence_mid >= 1 ~ 1,
#                                                               cumulative_incidence_mid <= 0 ~ 0,
#                                                               cumulative_incidence_mid > 0 & cumulative_incidence_mid < 1 ~ cumulative_incidence_mid)) %>%
#     dplyr::mutate(cumulative_incidence_low = dplyr::case_when(cumulative_incidence_low > 1 ~ 1,
#                                                               cumulative_incidence_low < 0 ~ 0,
#                                                               cumulative_incidence_low > 0 & cumulative_incidence_low < 1 ~ cumulative_incidence_low)) %>%
#     dplyr::mutate(cumulative_incidence_high = dplyr::case_when(cumulative_incidence_high > 1 ~ 1,
#                                                                cumulative_incidence_high < 0 ~ 0,
#                                                                cumulative_incidence_high > 0 & cumulative_incidence_high < 1 ~ cumulative_incidence_high)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "United States of America" ~ "USA",
#                                              country != "United States of America" ~ country)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
#                                              country != "United Kingdom" ~ country))
#   
#   
#   return(dataOut)
#   
# }
# 
# #--- uses the sum of all new cases, after adjusting for under-reporting and asymptomatic infections
# #--- as a proxy for prevalence. function performs this calculation for every country we have data for
# globalPrevalenceEstimates <- function()
# {
#   
#   asymptomaticEstimateMid <- 0.50
#   asymptomaticEstimateLow <- 0.10
#   asymptomaticEstimateHigh <- 0.70
#   
#   allDatRaw <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
#     dplyr::rename(date = dateRep, 
#                   country = countriesAndTerritories,
#                   countryCode = countryterritoryCode) %>%
#     dplyr::mutate(date = lubridate::dmy(date))
#   
#   countryCodesLookUp <- allDatRaw %>%
#     dplyr::select(country, 
#                   countryCode) %>% 
#     dplyr::distinct()
#   
#   
#   data_path <- here("data/under_reporting_estimates/")
#   files <- dir(path = data_path,
#                pattern = "*.rds")
#   
#   dataTmp <- dplyr::tibble(countryCode = files) %>% 
#     dplyr::mutate(file_contents = purrr::map(countryCode, 
#                                              ~ readRDS(file.path(data_path, .)))
#                   
#     ) %>% 
#     tidyr::unnest(cols = c(file_contents)) %>%
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
#     dplyr::group_by(countryCode) %>%
#     dplyr::select(date, everything()) %>%
#     dplyr::left_join(countryCodesLookUp) %>%
#     dplyr::left_join(allDatRaw) %>%
#     dplyr::select(date, country, countryCode, everything()) %>%
#     dplyr::group_by(countryCode) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
#                                              country != "Cote dIvoire" ~ country)) 
#   
#   # can this not be replaced with a call to wpp
#   worldPopulationEstimatesRaw <- readr::read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")
#   
#   worldPopulationEstimatesClean <- worldPopulationEstimatesRaw %>%
#     dplyr::filter(Variant == "Medium" & Time == "2020") %>% 
#     dplyr::select(country = Location, population = PopTotal) %>%
#     dplyr::mutate(population = population*1000) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
#                                              country != "Bolivia (Plurinational State of)" ~ country)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Iran (Islamic Republic of)" ~ "Iran",
#                                              country != "Iran (Islamic Republic of)" ~ country)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Republic of Moldova" ~ "Moldova",
#                                              country != "Republic of Moldova" ~ country)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Russian Federation" ~ "Russia",
#                                              country != "Russian Federation" ~ country)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
#                                              country != "Sint Maarten (Dutch part)" ~ country)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Republic of Korea" ~ "South Korea",
#                                              country != "Republic of Korea" ~ country)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
#                                              country != "Venezuela (Bolivarian Republic of)" ~ country)) %>%
#     rbind(c("Kosovo", 1810366))
#   
#   allTogether <- dataTmp %>% 
#     dplyr::left_join(worldPopulationEstimatesClean) %>%
#     dplyr::group_by(country) %>%
#     dplyr::select(date, country, countryCode, cases, estimate, lower, upper, population) %>%
#     dplyr::arrange(country, date)
#   
#   
#   newCaseEstimatesRecent <- allDatRaw %>%
#     dplyr::filter(Sys.Date() - 9 < date) %>% 
#     dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
#     dplyr::group_by(country) %>%
#     dplyr::mutate(cases = dplyr::case_when(cases < 0 & date == "2020-07-03" & country == "United Kingdom" ~ 674,
#                                            cases >= 0 ~ as.double(cases))) %>%
#     dplyr::summarise(totalNewCases = sum(cases)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
#                                              country != "Cote dIvoire" ~ country)) 
#   
#   
#   caseEstimatesTotal <- allDatRaw %>%
#     dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
#     dplyr::group_by(country) %>%
#     dplyr::summarise(totalCases = sum(cases)) %>%
#     dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
#                                              country != "Cote dIvoire" ~ country)) 
#   
#   
#   recentUnderreportingEstimates <- allTogether %>%
#     dplyr::filter(date == max(date)) %>% 
#     dplyr::group_by(country) %>%
#     dplyr::select(estimate, lower, upper)
#   
#   # turn off scientific notation
#   options(scipen=999)
#   
#   mostRecentEstimatesTogether <- recentUnderreportingEstimates %>% 
#     dplyr::left_join(newCaseEstimatesRecent) %>%
#     dplyr::right_join(caseEstimatesTotal) %>%
#     dplyr::left_join(worldPopulationEstimatesClean) %>%
#     tidyr::drop_na() %>%
#     dplyr::mutate(population = as.numeric(population)) %>%
#     dplyr::mutate(propCurrentlyInfMid  = (totalNewCases/(estimate*asymptomaticEstimateMid)/population),
#                   propCurrentlyInfLow  = (totalNewCases/(upper*(1 - asymptomaticEstimateLow))/population),
#                   propCurrentlyInfHigh = (totalNewCases/(lower*(1 - asymptomaticEstimateHigh))/population)) %>%
#     dplyr::mutate(propCurrentlyInfMid = signif(propCurrentlyInfMid, 2),
#                   propCurrentlyInfLow = signif(propCurrentlyInfLow, 2),
#                   propCurrentlyInfHigh = signif(propCurrentlyInfHigh, 2)) %>%
#     dplyr::select(country, totalCases, totalNewCases, estimate, lower, upper, population, propCurrentlyInfMid, propCurrentlyInfLow, propCurrentlyInfHigh)
#   
#   return(mostRecentEstimatesTogether)
#   
# }

ecdc_data_function <- function()
{
  ecdc_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::select(date = dateRep, 
                  country = countriesAndTerritories,
                  iso_code = countryterritoryCode,
                  cases,
                  deaths,
                  population = popData2019) %>%
    dplyr::mutate(date = lubridate::dmy(date)) 
  
  return(ecdc_data)
}

under_reporting_data_function <- function(ecdc_data_arg)
{
  
  data_path <- here::here("data/under_reporting_estimates/")
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  under_reporting_data_raw <- dplyr::tibble(iso_code = files) %>% 
    dplyr::mutate(file_contents = purrr::map(iso_code, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(iso_code = stringr::str_remove(iso_code, "result_")) %>% 
    dplyr::mutate(iso_code = stringr::str_remove(iso_code, ".rds")) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::select(date, everything()) %>%
    dplyr::left_join(ecdc_data_arg) %>%
    dplyr::select(date, country, iso_code, everything()) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " "))
  
  return(under_reporting_data_raw)
}

global_prevalence_estimates_function <- function(ecdc_data_arg, under_reporting_data_arg, date_arg)
{
  date_arg <- as.Date(date_arg)
  if(date_arg - Sys.Date() > 13)
  {
    date_arg_new <- date_arg
  }
  else(date_arg - Sys.Date() <= 13)
  {
    date_arg_new <- Sys.Date() - 13
    
  }
  
  
  asymptomatic_estimate_mid <- 0.50
  asymptomatic_estimate_low <- 0.10
  asymptomatic_estimate_high <- 0.70
  
  new_case_estimates_recent <- ecdc_data_arg %>%
    dplyr::filter(date_arg - 9 < date) %>% 
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::mutate(iso_code = countrycode::countrycode(country, "country.name", "iso3c", custom_match = c('Kosovo' = 'RKS'))) %>%
    dplyr::group_by(country, iso_code, population) %>%
    dplyr::summarise(total_new_cases = sum(cases))
  
  # turn off scientific notation
  options(scipen=999)
  
  under_reporting_estimates_at_date <- under_reporting_data_arg %>%
    dplyr::group_by(iso_code) %>%
    dplyr::filter(date == date_arg_new - 13)
  
  most_recent_estimates_together <- under_reporting_estimates_at_date %>% 
    dplyr::left_join(new_case_estimates_recent) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::mutate(prevalence_mid  = (total_new_cases/(estimate*asymptomatic_estimate_mid)/population),
                  prevalence_low  = (total_new_cases/(upper*(1 - asymptomatic_estimate_low))/population),
                  prevalence_high = (total_new_cases/(lower*(1 - asymptomatic_estimate_high))/population)) %>%
    dplyr::select(date, country, total_new_cases, estimate, lower, upper, population, prevalence_mid, prevalence_low, prevalence_high) %>%
    dplyr::arrange(country)
  
  return(most_recent_estimates_together)
  
}

oxford_restrictions_fun <- function(month_arg)
{
  #--- downloading and cleaning the oxford travel restriction ratings
  oxford_url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
  oxford_data <- oxford_url %>% readr::read_csv()
  
  #--- keeping Aprils data, cleaning and keeping just maximum rating in April
  oxford_travel_neat <- oxford_data %>%
    dplyr::select(CountryCode, Date, restriction_rating = "C8_International travel controls")  %>%
    dplyr::mutate(date = lubridate::ymd(Date)) %>%
    dplyr::select(-Date) %>%
    dplyr::filter(lubridate::month(date) == month_arg) %>%
    dplyr::group_by(CountryCode) %>%
    dplyr::rename(iso_code = CountryCode) %>%
    dplyr::group_by(iso_code) %>%    
    dplyr::summarise(travel_restrictions_rating = max(restriction_rating)) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "country.name", custom_match = c("RKS" = "Kosovo"))) %>%
    dplyr::select(country, iso_code, travel_restrictions_rating) %>%
    dplyr::arrange(country)
  
  return(oxford_travel_neat)
}

#--- function which reads in 2020 data and scales it down (for rating 4 countries)
#--- using the ratio of 2020 to 2019 flight volumes
oag_2020_april_data_function <- function()
{
  
  #--- reading in 2019 and 2020 OAG data
  oag_2019_data_raw <- readr::read_csv(here::here("data", "raw_data/oag_flight_data_2019.csv"))
  oag_2020_data_raw <- readr::read_csv(here::here("data", "flight_data_all_2020_Feb_Apr_final_monthly.csv"))
  
  #--- cleaning the 2019 data
  oag_2019_data_neat <- oag_2019_data_raw %>% 
    dplyr::select(iso_code_dep = dep_country_code,
                  iso_code_arr = arr_country_code,
                  total_travellers = bookings,
                  date = month_rev)
  
  #--- filtering for just April 2019, moving from 2 letter to 3 letter codes and cleaning
  oag_2019_april <- oag_2019_data_neat %>%
    dplyr::filter(date == "2019-04") %>%
    dplyr::mutate(iso_code_dep = countrycode::countrycode(iso_code_dep, "iso2c", "iso3c"),
                  iso_code_arr = countrycode::countrycode(iso_code_arr, "iso2c", "iso3c")) %>%
    dplyr::arrange(iso_code_dep, iso_code_arr)
  
  #--- filtering for just April 2019, moving from 2 letter to 3 letter codes and cleaning
  oag_april_2020_neat <- oag_2020_data_raw %>% 
    dplyr::select(iso_code_dep = dep_country_code,
                  iso_code_arr = arr_country_code, 
                  total_travellers = flight_volume_total, 
                  date = year_month_rev) %>%
    dplyr::mutate(iso_code_dep = countrycode::countrycode(iso_code_dep, "iso2c", "iso3c"),
                  iso_code_arr = countrycode::countrycode(iso_code_arr, "iso2c", "iso3c")) %>%
    dplyr::filter(date == "2020-04") %>%
    dplyr::select(-date) %>%
    dplyr::arrange(iso_code_dep, iso_code_arr)
  
  #--- downloading the oxford travel restriction ratings
  oxford_travel_neat <- oxford_restrictions_fun(month = 4)
  
  #--- 2019 April data for countries with level 3 or lower restriction rating
  #--- to scale down the level 4 countries
  countries_level_3_or_lower <- oxford_travel_neat %>% 
    dplyr::filter(travel_restrictions_rating != 4) %>%
    dplyr::pull(iso_code)
  
  #--- calculating average scaling factor from 2019 to 2020
  oag_april_2019_tmp <- oag_2019_april %>%
    dplyr::filter(iso_code_dep %in% countries_level_3_or_lower) %>%
    dplyr::rename(traveller_volume_2019 = total_travellers) %>%
    dplyr::select(-date) %>%
    dplyr::filter(iso_code_dep != iso_code_arr) %>%
    dplyr::group_by(iso_code_dep, iso_code_arr) %>%
    dplyr::summarise(traveller_volume_2019 = sum(traveller_volume_2019))
  
  oag_april_2020_tmp <- oag_april_2020_neat %>%
    dplyr::filter(iso_code_dep %in% countries_level_3_or_lower) %>%
    dplyr::rename(traveller_volume_2020 = total_travellers) %>%
    dplyr::filter(iso_code_dep != iso_code_arr) %>%
    dplyr::group_by(iso_code_dep, iso_code_arr) %>%
    dplyr::summarise(traveller_volume_2020 = sum(traveller_volume_2020))
  
  scaling_factor_2019_to_2020 <- oag_april_2019_tmp %>%
    dplyr::left_join(oag_april_2020_tmp) %>%
    dplyr::filter(traveller_volume_2019 != 0) %>%
    dplyr::mutate(scaling_factor = traveller_volume_2020/traveller_volume_2019) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::summarise(scaling_factor_mean = mean(scaling_factor)) %>%
    dplyr::pull(scaling_factor_mean)
  
  
  scaling_factor_2019_to_2020 <- oag_april_2019_tmp %>%
    dplyr::left_join(oag_april_2020_tmp) %>%
    dplyr::filter(traveller_volume_2019 != 0) %>%
    dplyr::mutate(scaling_factor = traveller_volume_2020/traveller_volume_2019) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::summarise(scaling_factor_mean = mean(scaling_factor)) %>%
    dplyr::pull(scaling_factor_mean)
  
  #--- 2020 data set to be used in analysis
  #--- after scaling down for reduction between 2019 and 2020 
  #--- and cleaning removing intra-country travel
  
  oag_april_2020_scaled <- oag_april_2020_neat %>%
    dplyr::group_by(iso_code_dep, iso_code_arr) %>%
    dplyr::filter(iso_code_dep != iso_code_arr) %>%
    dplyr::group_by(iso_code_dep, iso_code_arr) %>%
    dplyr::summarise(total_travellers = sum(total_travellers)) %>%
    dplyr::mutate(scaled_travellers = dplyr::case_when(!(iso_code_dep %in% countries_level_3_or_lower) ~ total_travellers*scaling_factor_2019_to_2020,
                                                       iso_code_dep %in% countries_level_3_or_lower  ~ total_travellers)) %>%
    dplyr::mutate(origin_country = countrycode::countrycode(iso_code_dep, "iso3c", 'country.name', 
                                                            custom_match = c('RKS' = 'Kosovo')),
                  destination_country = countrycode::countrycode(iso_code_arr, "iso3c", 'country.name', 
                                                                 custom_match = c('RKS' = 'Kosovo')))
  
  return(oag_april_2020_scaled)
  
}

