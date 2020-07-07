
# up to date version of the function
getUnderReportingCaseDeathPopulationAndTestingData <- function()
{
  
  adHocTestData <- getAdHocTestingData()
  
  owidData <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
    dplyr::left_join(adHocTestData, by = c("location", "iso_code", "date")) 
  
  underReportingPath <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted_not_age_adjusted/"
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
  
  
  ecdcCaseData <- rio::import("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-06-21.xlsx") %>%
    dplyr::rename(new_cases = cases,
                  new_deaths = deaths,
                  country = countriesAndTerritories,
                  iso_code = countryterritoryCode) %>%
    dplyr::mutate(date = lubridate::ymd(dateRep)) %>%
    dplyr::filter(new_cases >= 0)
  
  underReportingPath <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted_not_age_adjusted/"
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

getAdjustedCaseDataRegional <- function()
{
  
  regional_estimates <- regionalHPCBayesianData("~/Dropbox/bayesian_underreporting_estimates/regional_data/results_for_paper/") %>%
    dplyr::rename(iso_code = country_code)
  regional_case_death_data <- getRegionalCaseDeathTimeSeries() %>%
    dplyr::rename(iso_code = country_code)
  
  populations <- readr::read_csv("covid_underreporting/data/regional populations.csv") %>%
    dplyr::rename(iso_code = country_code)
  
  asymptomatic_mid <- 0.5
  asymptomatic_low <- 0.1
  asymptomatic_high <- 0.7
  
  
  underReportingAndCaseData <- regional_case_death_data %>% 
    dplyr::group_by(country) %>%
    dplyr::left_join(regional_estimates) %>%
    dplyr::group_by(country) %>%
    dplyr::left_join(populations) %>%
    dplyr::arrange(country, date) %>%
    tidyr::drop_na() %>%
    dplyr::select(date, iso_code, country, new_cases, new_deaths, population, estimate, lower, upper)
  
  dataOut <- underReportingAndCaseData %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_cases_smoothed             = zoo::rollmean(new_cases, k = 7, fill = NA),
                  new_cases_adjusted_mid         = new_cases/estimate,
                  new_cases_adjusted_low         = new_cases/upper,
                  new_cases_adjusted_high        = new_cases/lower,
                  new_cases_adjusted_smooth_mid  = zoo::rollmean(new_cases_adjusted_mid, k = 7, fill = NA),
                  new_cases_adjusted_smooth_low  = zoo::rollmean(new_cases_adjusted_low, k = 7, fill = NA),
                  new_cases_adjusted_smooth_high = zoo::rollmean(new_cases_adjusted_high, k = 7, fill = NA)) %>%
    dplyr::mutate(cumulative_incidence_mid  = cumsum(new_cases_adjusted_mid)/(population*(1 - asymptomatic_mid)),
                  cumulative_incidence_low  = cumsum(new_cases_adjusted_low)/(population*(1 - asymptomatic_low)),
                  cumulative_incidence_high = cumsum(new_cases_adjusted_high)/(population*(1 - asymptomatic_high))) %>%
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
    dplyr::ungroup() 
  
  return(dataOut)
}

#--- simple function to combine the map data (i.e. coordinates of countries) and our estimates
combineMapAndIncidenceData <- function(dataToPlot)
{
  
  ggplot2::map_data("world") %>%
    dplyr::rename(country = region) %>%
    dplyr::mutate(iso_code = countrycode::countrycode(country, 'country.name', 'iso3c')) %>% 
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::filter(country != "Antarctica") %>%
    dplyr::left_join(dataToPlot, by = "country") %>%
    dplyr::select(country, lat, long, group, country, cumulative_incidence_mid)
  
}


getAdjustedRegionalCaseAndSerologyData <- function()
{
  
  allAdjustedCaseData <- getAdjustedCaseDataRegional()  
  
  serology_study_results <- readr::read_csv("covid_underreporting/data/regional serology estimates.csv") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(percentage_positive_mid = percentage_positive_mid/100,
                  lower_ci = signif(binom_min(positive_samples, sample_size), 2)/100,
                  upper_ci = signif(binom_max(positive_samples, sample_size), 2)/100)
  
  regional_lookup_table <- readr::read_csv("covid_underreporting/data/regional_codes_look_up.csv")
  
  adjusted_cases_and_serology <- allAdjustedCaseData %>%
    dplyr::left_join(serology_study_results, by = c("iso_code")) %>%
    dplyr::left_join(regional_lookup_table)
  
  return(adjusted_cases_and_serology)
  
}

#--- regional data functions

#--- merge UK death data from different sources - from Kath Sherratt
getRawUKRegionalDeathData <- function()
{
  
  # Sourcing public deaths in hospitals 
  # Available by English region + for England
  
  # Hospital deaths ---------------------------------------------------------
  
  # NHS Hospital deaths = England regions
  date <- paste0(format(Sys.Date()-1, "%d"), "-", months(Sys.Date()), "-", format(Sys.Date(), "%Y"))
  date <- sub("^0+", "", date)
  hosp_deaths_path <- paste0("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/", format(Sys.Date(), "%m"), "/COVID-19-total-announced-deaths-", date, ".xlsx")
  
  # Deaths in hospitals with covid test
  hosp_deaths_tested <- openxlsx::read.xlsx(hosp_deaths_path, sheet = 3, startRow = 16, detectDates = TRUE, colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  hosp_deaths_tested <- hosp_deaths_tested[,1:(ncol(hosp_deaths_tested) - 2)]
  hosp_deaths_tested <- setNames(data.frame(t(hosp_deaths_tested[,-1])), hosp_deaths_tested[,1])
  row.names(hosp_deaths_tested) <- NULL
  hosp_deaths_tested$date <- seq.Date(from = as.Date("2020-02-29"), by = "day", length.out = nrow(hosp_deaths_tested))
  hosp_deaths_tested <- hosp_deaths_tested[, c(1, 3:10)]
  hosp_deaths_tested <- tidyr::pivot_longer(hosp_deaths_tested, cols = -date, names_to = "region", values_to = "deaths_tested")
  
  # Deaths in hospitals without test but with covid on death certificate
  hosp_deaths_untested <- openxlsx::read.xlsx(hosp_deaths_path, sheet = 4, startRow = 16, detectDates = TRUE, colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  hosp_deaths_untested <- hosp_deaths_untested[,1:(ncol(hosp_deaths_untested) - 2)]
  hosp_deaths_untested <- setNames(data.frame(t(hosp_deaths_untested[,-1])), hosp_deaths_untested[,1])
  row.names(hosp_deaths_untested) <- NULL
  hosp_deaths_untested$date <- seq.Date(from = as.Date("2020-02-29"), by = "day", length.out = nrow(hosp_deaths_untested))
  hosp_deaths_untested <- tidyr::pivot_longer(hosp_deaths_untested, cols = -date, names_to = "region", values_to = "deaths_untested")
  
  # Join tested + untested
  deaths <- dplyr::left_join(hosp_deaths_tested, hosp_deaths_untested, by = c("date", "region")) %>%
    dplyr::mutate(deaths = deaths_tested + deaths_untested,
                  region = stringr::str_replace_all(region, "Of", "of"),
                  region = stringr::str_replace_all(region, "And", "and")) %>%
    dplyr::select(date, region, deaths)
  
}

#--- get tibble of time-series of cases and deaths by region
getRegionalUKCaseDeathData <- function()
{
  
  # downloading and combining raw UK death data
  ukRegionalDeaths <- getRawUKRegionalDeathData() %>%
    dplyr::filter(region != "England")
  
  # cleaning data into simple time-series by region
  # DEPRACTED
  # ukRegionalDeaths <- hospDeathsByTrust %>% 
  #   dplyr::tibble() %>%
  #   tidyr::pivot_longer(cols = ends_with("/20")) %>%
  #   dplyr::select(region, date = name, deaths = value) %>%
  #   dplyr::mutate(date = paste0(date, "20"),
  #                 region = dplyr::case_when(region == "London " ~ "London",
  #                                           region != "London " ~ region)) %>%
  #   dplyr::mutate(date = lubridate::mdy(date)) %>%
  #   dplyr::mutate(region = dplyr::case_when(region == "East Of England" ~ "East of England",
  #                                           region != "East Of England" ~ region)) %>%
  #   dplyr::group_by(date, region) %>%
  #   dplyr::summarise(deaths = sum(deaths)) %>%
  #   dplyr::arrange(region, date) 
  
  # downloading UK case data and cleaning into simple time-series by region
  ukRegionalCases <- NCoVUtils::get_uk_regional_cases() %>%
    dplyr::filter(region != "Scotland" & region != "Wales" & region != "Northern Ireland") %>%
    dplyr::select(-country) %>%
    dplyr::mutate(region = ifelse(region %in% c('West Midlands', 'East Midlands'), 'Midlands', region),
                  region = ifelse(region %in% c('North East', 'Yorkshire and The Humber'), 'North East and Yorkshire', region)) %>%
    dplyr::group_by(date, region) %>%
    dplyr::summarise(cases = sum(cases)) %>%
    dplyr::arrange(date, region) 
  
  
  ukRegionalData <- ukRegionalCases %>% 
    dplyr::right_join(ukRegionalDeaths, by = c("date", "region")) %>%
    dplyr::mutate(cases  = tidyr::replace_na(cases, 0),
                  deaths = tidyr::replace_na(deaths, 0)) %>% 
    dplyr::mutate(country_code = dplyr::case_when(region == "East of England" ~ "EOE",
                                                  region == "London" ~ "LON",
                                                  region == "Midlands" ~ "MID",
                                                  region == "North East and Yorkshire" ~ "NEY",
                                                  region == "North West" ~ "NOW",
                                                  region == "South East" ~ "SOE",
                                                  region == "South West" ~ "SOW")) %>%
    dplyr::mutate(country  = dplyr::case_when(region == "East of England" ~ "EOE",
                                              region == "London" ~ "LON",
                                              region == "Midlands" ~ "MID",
                                              region == "North East and Yorkshire" ~ "NEY",
                                              region == "North West" ~ "NOW",
                                              region == "South East" ~ "SOE",
                                              region == "South West" ~ "SOW")) %>%
    dplyr::select(date, country_code, country, new_cases = cases, new_deaths = deaths)
  
  return(ukRegionalData)
  
}

#--- pulls regional case and death time-series of interest from the various
#--- sources necessary depending on which regions are of interest
#--- current we use London, New York and Geneva and we use the data in the
#--- seroprevalence comparison in figure 3
getRegionalCaseDeathTimeSeries <- function()
{
  
  newYorkData <- NCoVUtils::get_us_regional_cases() %>%
    dplyr::filter(state == "New York") %>%
    dplyr::rename(iso_code = state,
                  new_cases = cases,
                  new_deaths = deaths) %>%
    dplyr::select(-fips) %>%
    dplyr::mutate(country_code = "NYC",
                  country = "NYC") %>%
    dplyr::select(date, country_code, country, new_cases, new_deaths)
  
  #---- DEPRECATED as time series wasn't long enough for useful comparison
  #---- Using London seroprevalence as comparison now instead
  # wuhanData <- getWuhanCaseDeathTimeSeries() %>%
  #   dplyr::select(date, country, new_cases, new_deaths) %>%
  #   dplyr::rename(iso_code = country) %>%
  #   dplyr::mutate(iso_code = "WUH") %>%
  #   dplyr::mutate(new_deaths = dplyr::case_when(new_deaths < 0 ~ 0,
  #                                               new_deaths >= 0 ~ new_deaths))
  # 
  
  londonData <- getRegionalUKCaseDeathData() %>%
    dplyr::arrange(country_code) %>%
    dplyr::group_by(country_code) %>%
    dplyr::arrange(country_code, date)
  
  genevaData <- readr::read_csv("covid_19/COVID19_Fallzahlen_CH_total_v2.csv") %>%
    dplyr::arrange(abbreviation_canton_and_fl, date) %>%
    dplyr::group_by(abbreviation_canton_and_fl) %>%
    padr::pad(by = "date")  %>%
    dplyr::arrange(abbreviation_canton_and_fl, date) %>%
    dplyr::mutate(date,
                  country = abbreviation_canton_and_fl,
                  new_cases = ncumul_conf - dplyr::lag(ncumul_conf), 
                  new_deaths = ncumul_deceased - dplyr::lag(ncumul_deceased)) %>%
    dplyr::select(date, country, new_cases, new_deaths) %>%
    tidyr::drop_na() %>%
    dplyr::filter(country == "GE") %>% 
    dplyr::ungroup() %>%
    dplyr::select(-abbreviation_canton_and_fl) %>%
    dplyr::mutate(country_code = "GEN",
                  country = "GE") %>%
    dplyr::select(date, country_code, country, new_cases, new_deaths)
  
  regionalDataTogether <- dplyr::bind_rows(newYorkData, genevaData, londonData)
  
  return(regionalDataTogether)
  
}

#--- reads in and cleans the estimates computed on the HPC from shared Dropbox folder
regionalHPCBayesianData <- function(data_path)
{
  
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  data <- dplyr::tibble(country_code = files) %>% 
    dplyr::mutate(file_contents = purrr::map(country_code, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(country_code = stringr::str_remove(country_code, "result_")) %>% 
    dplyr::mutate(country_code = stringr::str_remove(country_code, ".rds")) 
  
  return(data)
  
}

#--- the testing data for the UK, US and Brazil are less easily formatted and accessed
#--- we put together this function which scrapes the testing data from OWID and combines two
#--- different time-series into a single smoothed time-series
getAdHocTestingData <- function()
{
  
  testing_data_all <- readr::read_csv("https://covid.ourworldindata.org/data/testing/covid-testing-all-observations.csv")
  
  testing_data_UK_people_tested <- testing_data_all %>%
    dplyr::filter(Entity == "United Kingdom - people tested")
  
  testing_data_UK_tests_performed <- testing_data_all %>%
    dplyr::filter(Entity == "United Kingdom - tests performed")
  
  testing_data_UK_clean <- testing_data_UK_people_tested %>% dplyr::filter(Date <= "2020-05-03") %>%
    dplyr::bind_rows(testing_data_UK_tests_performed %>% dplyr::filter(Date > "2020-05-04")) %>%
    dplyr::mutate(location = "United Kingdom") %>%
    dplyr::rename(date = Date, 
                  iso_code = "ISO code", 
                  new_tests = "Daily change in cumulative total",
                  testing_effort_smoothed = "7-day smoothed daily change per thousand") %>%
    dplyr::select(date, location, iso_code, new_tests, testing_effort_smoothed)
  
  testing_data_US_units_unclear <- testing_data_all %>%
    dplyr::filter(Entity == "United States - units unclear")
  
  
  testing_data_US_tests_performed <- testing_data_all %>%
    dplyr::filter(Entity == "United States - tests performed")
  
  
  testing_data_US_clean <- testing_data_US_units_unclear %>% 
    dplyr::filter(Date <= "2020-05-19") %>%
    dplyr::bind_rows(testing_data_US_tests_performed %>% dplyr::filter(Date > min(Date) + 7)) %>%
    dplyr::mutate(location = "United States") %>%
    dplyr::rename(date = Date, 
                  iso_code = "ISO code", 
                  new_tests = "Daily change in cumulative total",
                  testing_effort_smoothed = "7-day smoothed daily change per thousand") %>%
    dplyr::select(date, location, iso_code, new_tests, testing_effort_smoothed)
  
  
  both_together <- testing_data_UK_clean %>%
    dplyr::bind_rows(testing_data_US_clean)
  
  return(both_together)
  
}


#--- uses the sum of all new cases, after adjusting for under-reporting and asymptomatic infections
#--- as a proxy for prevalence. function performs this calculation for every country we have data for
globalPrevalenceEstimates <- function()
{
  
  asymptomaticEstimateMid <- 0.50
  asymptomaticEstimateLow <- 0.23
  asymptomaticEstimateHigh <- 0.70
  
  allDatRaw <- rio::import("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-06-21.xlsx") %>%
    dplyr::rename(date = dateRep, 
                  country = countriesAndTerritories,
                  countryCode = countryterritoryCode) %>%
    dplyr::mutate(date = lubridate::ymd(date))
  
  countryCodesLookUp <- allDatRaw %>%
    dplyr::select(country, 
                  countryCode) %>% 
    unique()
  
  
  data_path <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted_not_age_adjusted/"
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

getAgeAdjustedCFR <- function()
{
  
  library(wpp2019)

  data(popM)
  data(popF)
  
  male_age_strat_age_data <- popM %>% 
    dplyr::select(country_code, age, "2020") %>%
    dplyr::mutate(iso3c = countrycode::countrycode(country_code, "iso3n", destination = 'iso3c')) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(age = dplyr::case_when(age == "0-4" ~ "0-9",
                                         age == "5-9" ~ "0-9",
                                         age == "10-14" ~ "10-19",
                                         age == "15-19" ~ "10-19",
                                         age == "20-24" ~ "20-29",
                                         age == "25-29" ~ "20-29",
                                         age == "30-34" ~ "30-39",
                                         age == "35-39" ~ "30-39",
                                         age == "40-44" ~ "40-49",
                                         age == "45-49" ~ "40-49",
                                         age == "50-54" ~ "40-49",
                                         age == "55-59" ~ "50-59",
                                         age == "60-64" ~ "60-69",
                                         age == "65-69" ~ "60-69",
                                         age == "70-74" ~ "70-79",
                                         age == "75-79" ~ "70-79",
                                         age == "80-84" ~ "80+",
                                         age == "85-89" ~ "80+",
                                         age == "90-94" ~ "80+",
                                         age == "95-99" ~ "80+",
                                         age == "100+" ~ "80+")) %>%
    dplyr::rename(population = "2020") %>%
    dplyr::group_by(iso3c, age) %>%
    dplyr::arrange(age) %>%
    dplyr::summarise(population_male = sum(population))
  
  
  female_age_strat_age_data <- popF %>% 
    dplyr::select(country_code, age, "2020") %>%
    dplyr::mutate(iso3c = countrycode::countrycode(country_code, "iso3n", destination = 'iso3c')) %>%
    dplyr::mutate(age = dplyr::case_when(age == "0-4" ~ "0-9",
                                         age == "5-9" ~ "0-9",
                                         age == "10-14" ~ "10-19",
                                         age == "15-19" ~ "10-19",
                                         age == "20-24" ~ "20-29",
                                         age == "25-29" ~ "20-29",
                                         age == "30-34" ~ "30-39",
                                         age == "35-39" ~ "30-39",
                                         age == "40-44" ~ "40-49",
                                         age == "45-49" ~ "40-49",
                                         age == "50-54" ~ "40-49",
                                         age == "55-59" ~ "50-59",
                                         age == "60-64" ~ "60-69",
                                         age == "65-69" ~ "60-69",
                                         age == "70-74" ~ "70-79",
                                         age == "75-79" ~ "70-79",
                                         age == "80-84" ~ "80+",
                                         age == "85-89" ~ "80+",
                                         age == "90-94" ~ "80+",
                                         age == "95-99" ~ "80+",
                                         age == "100+" ~ "80+")) %>%
    tidyr::drop_na() %>%
    dplyr::rename(population = "2020") %>%
    dplyr::group_by(iso3c, age) %>%
    dplyr::arrange(age) %>%
    dplyr::summarise(population_female = sum(population)) 
  
  
  age_strat_age_data <- male_age_strat_age_data %>%
    dplyr::left_join(female_age_strat_age_data, by = c("iso3c", "age")) %>% 
    dplyr::mutate(population_male = population_male*1000,
                  population_female = population_female*1000,
                  population = population_male + population_female)
  
  
  age_stratified_cfr <- readr::read_csv("covid_underreporting/data/age_stratified_cfr.csv")
  
  
  age_adjusted_cfr <- age_strat_age_data %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarise(weighted_cfr_mid = weighted.mean(age_stratified_cfr$cfr_mid, population)*100,
                     weighted_cfr_low = weighted.mean(age_stratified_cfr$cfr_low, population)*100,
                     weighted_cfr_high = weighted.mean(age_stratified_cfr$cfr_high, population)*100)
 
  return(age_adjusted_cfr)
  
}

