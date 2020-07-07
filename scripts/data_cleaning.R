here::here() %>% setwd()

source("covid_travel_restrictions/R/flight_data_cleaning_utils.R")
source("covid_travel_restrictions/R/data_helper_functions.R")
source("covid_travel_restrictions/R/plotting_helper_functions.R")

open_sky_may_2019   <- readr::read_csv(here("data","data/open_sky_may_2019.gz"))
open_sky_may_2020   <- readr::read_csv(here("data","open_sky_may_2020.gz"))

airport_lookup <- read.delim(here("data", "airports.dat"), sep = ",") %>%
  dplyr::select(country = Country, three_letter_code, four_letter_code)

flight_data_clean_may_2019   <- clean_flight_data(open_sky_may_2019)
flight_data_clean_may_2020   <- clean_flight_data(open_sky_may_2020)


total_flights_may_2019 <- flight_data_clean_may_2019 %>%
  dplyr::filter(origin_country != destination_country) %>%
  dplyr::group_by(origin_country, destination_country) %>%
  dplyr::summarise(total_flights_2019 = dplyr::n()) %>%
  dplyr::mutate(origin_country_iso_code = countrycode::countrycode(origin_country, "country.name", "iso3c"),
                destination_country_iso_code = countrycode::countrycode(destination_country, "country.name", "iso3c")) %>%
  dplyr::ungroup() %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_flights_2019)

total_flights_may_2020 <- flight_data_clean_may_2020 %>%
  dplyr::filter(origin_country != destination_country) %>%
  dplyr::group_by(origin_country, destination_country) %>%
  dplyr::summarise(total_flights_2020 = dplyr::n()) %>%
  dplyr::mutate(origin_country_iso_code = countrycode::countrycode(origin_country, "country.name", "iso3c"),
                destination_country_iso_code = countrycode::countrycode(destination_country, "country.name", "iso3c")) %>%
  dplyr::ungroup() %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_flights_2020)


total_flights_may_2019_2020 <- total_flights_may_2019 %>% 
  dplyr::ungroup() %>%
  dplyr::left_join(total_flights_may_2020) %>%
  dplyr::mutate(scaling_factor = total_flights_2020/total_flights_2019) %>%
  dplyr::mutate(scaling_factor = dplyr::case_when(scaling_factor > 1  ~ 1,
                                                  scaling_factor <= 1 ~ scaling_factor)) %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, scaling_factor) %>%
  tidyr::drop_na()


oag_traveller_data <- readr::read_csv(here("data","Kathy flight_data_all_2019_monthly.csv"))
 
  
oag_traveller_data_may_2020 <- oag_traveller_data %>%
  dplyr::filter(month == 5) %>%
  dplyr::select(origin_country_iso_code_2 = dep_country_code, destination_country_iso_code_2 = arr_country_code, bookings) %>%
  dplyr::mutate(origin_country_iso_code   = countrycode::countrycode(origin_country_iso_code_2, "iso2c", "iso3c"),
                destination_country_iso_code = countrycode::countrycode(destination_country_iso_code_2, "iso2c", "iso3c")) %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, bookings) %>%
  dplyr::group_by(origin_country_iso_code, destination_country_iso_code) %>%
  dplyr::summarise(total_passengers = sum(bookings)) %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_passengers)


# where 0.3090604 is the mean of the existing scaling factors. There are only 860 off scaling factors, so most
# journeys take the mean, rather than specific scaling factors

may_travel_data <- oag_traveller_data_may_2020 %>%
  dplyr::full_join(total_flights_may_2019_2020) %>%
  dplyr::mutate(scaling_factor = dplyr::case_when(is.na(scaling_factor) == TRUE  ~ 0.3090604,
                                                  is.na(scaling_factor) == FALSE ~ scaling_factor)) %>%
  dplyr::mutate(origin_country      =  countrycode::countrycode(origin_country_iso_code, "iso3c", "country.name"),
                destination_country =  countrycode::countrycode(destination_country_iso_code, "iso3c", "country.name")) %>%
  dplyr::mutate(scaled_travellers = total_passengers*scaling_factor) %>%
  dplyr::select(origin_country, destination_country, origin_country_iso_code, destination_country_iso_code, total_passengers, scaling_factor, scaled_travellers)

# using the scaled traveller numbers and prevalence to calculate expected number of imported cases
# from all origin countries, to all destination countries

prevalence_data_country_A <- globalPrevalenceEstimates() %>%
  dplyr::mutate(iso_code = countrycode::countrycode(country, "country.name", 'iso3c')) %>%
  dplyr::mutate(iso_code_dep = iso_code) %>% 
  dplyr::ungroup(country) %>%
  dplyr::select(origin_country_iso_code = iso_code_dep, prevalence = propCurrentlyInfMid) %>%
  tidyr::drop_na()

asymptomatic_prop <- 0.5
traveller_reduction <- 0.5


imported_cases <- may_travel_data %>%
  dplyr::left_join(prevalence_data_country_A) %>%
  dplyr::select(origin_country, destination_country, origin_country_iso_code, destination_country_iso_code, scaled_travellers, prevalence) %>%
  dplyr::mutate(expected_imported_cases_all = scaled_travellers*prevalence*traveller_reduction) %>%
  dplyr::group_by(destination_country_iso_code) %>%
  dplyr::summarise(expected_imported_cases = sum(expected_imported_cases_all, na.rm = TRUE))

# calculating the incidence in each destination country
incidence_data_country_B <-  getAdjustedCaseDataNational() %>%
  dplyr::group_by(iso_code) %>%
  dplyr::filter(date > "2020-05-01" & date < "2020-05-31") %>%
  dplyr::mutate(incidence_estimate = new_cases_adjusted_mid/(1 - asymptomatic_prop)) %>%
  dplyr::summarise(new_cases_adjusted_mean = sum(incidence_estimate)/30) %>%
  dplyr::mutate(destination_country_iso_code = iso_code) %>%
  dplyr::select(destination_country_iso_code, new_cases_adjusted_mean)

imported_cases_and_incidence_together <- imported_cases %>%
  dplyr::left_join(incidence_data_country_B) %>%
  dplyr::mutate(importation_per_incidence = expected_imported_cases/new_cases_adjusted_mean) %>%
  dplyr::mutate(importation_per_incidence = dplyr::na_if(importation_per_incidence, "Inf")) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(risk_rating = dplyr::case_when(importation_per_incidence <= 0.01 ~ "Green",
                                               importation_per_incidence >  0.01 & importation_per_incidence < 0.1 ~ "Amber",
                                               importation_per_incidence >= 0.1 ~ "Red")) %>%
  dplyr::rename(iso_code = destination_country_iso_code)


may_2020_risk_map_data <- combineMapAndIncidenceData(imported_cases_and_incidence_together)
may_2020_risk_map <- mapPlottingFunction(may_2020_risk_map_data)

