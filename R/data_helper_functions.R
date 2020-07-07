
# up to date version of the function
getUnderReportingCaseDeathPopulationAndTestingData <- function()
{
  
  adHocTestData <- getAdHocTestingData()
  
  owidData <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
    dplyr::left_join(adHocTestData, by = c("location", "iso_code", "date")) 
  
  underReportingPath <- "covid_travel_restrictions/data/under_reporting_estimates/"
  files <- dir(path = underReportingPath,
               pattern = "*.rds")
  
  underReportingRawData <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(underReportingPath, .)))) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)
  
  
  underReportingAndTestingData <- owidData %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::select(date, iso_code, country, new_cases, new_deaths, total_cases, total_deaths, new_tests_smoothed_per_thousand, population, estimate, lower, upper, testing_effort_smoothed) %>%
    dplyr::mutate(new_tests_smoothed_per_thousand = dplyr::case_when(country == "United Kingdom" | country == "United States of America (the)" ~ testing_effort_smoothed,
                                                                     country != "United Kingdom" | country != "United States of America (the)" ~ new_tests_smoothed_per_thousand)) %>%
    dplyr::select(-testing_effort_smoothed)
  
  return(underReportingAndTestingData)
  
}

# new version of the function, much more streamlined
getAdjustedCaseDataNational <- function()
{
  
  asymptomatic_mid <- 0.5
  asymptomatic_low <- 0.1
  asymptomatic_high <- 0.7
  
  
  ecdcCaseData <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::rename(new_cases = cases,
                  new_deaths = deaths,
                  country = countriesAndTerritories,
                  iso_code = countryterritoryCode) %>%
    dplyr::mutate(date = lubridate::ymd(dateRep)) %>%
    dplyr::filter(new_cases >= 0)
  
  underReportingPath <- "covid_travel_restrictions/data/under_reporting_estimates/"
  files <- dir(path = underReportingPath,
               pattern = "*.rds")
  
  underReportingRawData <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(underReportingPath, .)))) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)
  
  
  underReportingAndCaseData <- ecdcCaseData %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(country, date) %>%
    tidyr::drop_na() %>%
    dplyr::select(date, iso_code, country, new_cases, new_deaths, popData2019, estimate, lower, upper)
  
  
  dataOut <- underReportingAndCaseData %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_cases_smoothed             = zoo::rollmean(new_cases, k = 7, fill = NA),
                  new_cases_adjusted_mid         = new_cases/estimate,
                  new_cases_adjusted_low         = new_cases/upper,
                  new_cases_adjusted_high        = new_cases/lower,
                  new_cases_adjusted_smooth_mid  = zoo::rollmean(new_cases_adjusted_mid, k = 7, fill = NA),
                  new_cases_adjusted_smooth_low  = zoo::rollmean(new_cases_adjusted_low, k = 7, fill = NA),
                  new_cases_adjusted_smooth_high = zoo::rollmean(new_cases_adjusted_high, k = 7, fill = NA)) %>%
    dplyr::mutate(cumulative_incidence_mid  = cumsum(new_cases_adjusted_mid)/(popData2019*(1 - asymptomatic_mid)),
                  cumulative_incidence_low  = cumsum(new_cases_adjusted_low)/(popData2019*(1 - asymptomatic_low)),
                  cumulative_incidence_high = cumsum(new_cases_adjusted_high)/(popData2019*(1 - asymptomatic_high))) %>%
    dplyr::mutate(date_infection  = date - 10) %>%
    dplyr::mutate(cumulative_incidence_mid = dplyr::case_when(cumulative_incidence_mid >= 1 ~ 1,
                                                              cumulative_incidence_mid <= 0 ~ 0,
                                                              cumulative_incidence_mid > 0 & cumulative_incidence_mid < 1 ~ cumulative_incidence_mid)) %>%
    dplyr::mutate(cumulative_incidence_low = dplyr::case_when(cumulative_incidence_low > 1 ~ 1,
                                                              cumulative_incidence_low < 0 ~ 0,
                                                              cumulative_incidence_low > 0 & cumulative_incidence_low < 1 ~ cumulative_incidence_low)) %>%
    dplyr::mutate(cumulative_incidence_high = dplyr::case_when(cumulative_incidence_high > 1 ~ 1,
                                                               cumulative_incidence_high < 0 ~ 0,
                                                               cumulative_incidence_high > 0 & cumulative_incidence_high < 1 ~ cumulative_incidence_high)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United States of America" ~ "USA",
                                             country != "United States of America" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                             country != "United Kingdom" ~ country))
  
  
  return(dataOut)
  
}

#--- uses the sum of all new cases, after adjusting for under-reporting and asymptomatic infections
#--- as a proxy for prevalence. function performs this calculation for every country we have data for
globalPrevalenceEstimates <- function()
{
  
  asymptomaticEstimateMid <- 0.50
  asymptomaticEstimateLow <- 0.10
  asymptomaticEstimateHigh <- 0.70
  
  allDatRaw <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::rename(date = dateRep, 
                  country = countriesAndTerritories,
                  countryCode = countryterritoryCode) %>%
    dplyr::mutate(date = lubridate::ymd(date))
  
  countryCodesLookUp <- allDatRaw %>%
    dplyr::select(country, 
                  countryCode) %>% 
    unique()
  
  
  data_path <- "covid_travel_restrictions/data/under_reporting_estimates/"
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  dataTmp <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::left_join(countryCodesLookUp) %>%
    dplyr::left_join(allDatRaw) %>%
    dplyr::select(date, country, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
                                             country != "Cote dIvoire" ~ country)) 
  
  
  worldPopulationEstimatesRaw <- readr::read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")
  
  worldPopulationEstimatesClean <- worldPopulationEstimatesRaw %>%
    dplyr::filter(Variant == "Medium" & Time == "2020") %>% 
    dplyr::select(country = Location, population = PopTotal) %>%
    dplyr::mutate(population = population*1000) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                             country != "Bolivia (Plurinational State of)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Iran (Islamic Republic of)" ~ "Iran",
                                             country != "Iran (Islamic Republic of)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Republic of Moldova" ~ "Moldova",
                                             country != "Republic of Moldova" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Russian Federation" ~ "Russia",
                                             country != "Russian Federation" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
                                             country != "Sint Maarten (Dutch part)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Republic of Korea" ~ "South Korea",
                                             country != "Republic of Korea" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                             country != "Venezuela (Bolivarian Republic of)" ~ country)) %>%
    rbind(c("Kosovo", 1810366))
  
  allTogether <- dataTmp %>% 
    dplyr::left_join(worldPopulationEstimatesClean) %>%
    dplyr::group_by(country) %>%
    dplyr::select(date, country, countryCode, cases, estimate, lower, upper, population) %>%
    dplyr::arrange(country, date)
  
  
  newCaseEstimatesRecent <- allDatRaw %>%
    dplyr::filter(Sys.Date() - 9 < date) %>% 
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalNewCases = sum(cases)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
                                             country != "Cote dIvoire" ~ country)) 
  
  
  caseEstimatesTotal <- allDatRaw %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalCases = sum(cases)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
                                             country != "Cote dIvoire" ~ country)) 
  
  
  recentUnderreportingEstimates <- allTogether %>%
    dplyr::filter(date == max(date)) %>% 
    dplyr::group_by(country) %>%
    dplyr::select(estimate, lower, upper)
  
  # turn off scientific notation
  options(scipen=999)
  
  mostRecentEstimatesTogether <- recentUnderreportingEstimates %>% 
    dplyr::left_join(newCaseEstimatesRecent) %>%
    dplyr::right_join(caseEstimatesTotal) %>%
    dplyr::left_join(worldPopulationEstimatesClean) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::mutate(propCurrentlyInfMid  = (totalNewCases/(estimate*asymptomaticEstimateMid)/population),
                  propCurrentlyInfLow  = (totalNewCases/(upper*(1 - asymptomaticEstimateLow))/population),
                  propCurrentlyInfHigh = (totalNewCases/(lower*(1 - asymptomaticEstimateHigh))/population)) %>%
    dplyr::mutate(propCurrentlyInfMid = signif(propCurrentlyInfMid, 2),
                  propCurrentlyInfLow = signif(propCurrentlyInfLow, 2),
                  propCurrentlyInfHigh = signif(propCurrentlyInfHigh, 2)) %>%
    dplyr::select(country, totalCases, totalNewCases, estimate, lower, upper, population, propCurrentlyInfMid, propCurrentlyInfLow, propCurrentlyInfHigh)
  
  return(mostRecentEstimatesTogether)
  
}