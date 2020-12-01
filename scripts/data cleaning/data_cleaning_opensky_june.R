#--- script for cleaning the Opensky data, to rp

source(here("R","flight_data_cleaning_utils.R"))
source(here("R","data_helper_functions.R"))
source(here("R","plotting_helper_functions.R"))

open_sky_june_2019   <- readr::read_csv("data/raw_data/opensky_june_2019.csv.gz")
open_sky_june_2020   <- readr::read_csv("data/raw_data/opensky_june_2020.csv.gz")

#--- reading in airport lookup table
airport_lookup <- read.delim(here("data", "airports.dat"), sep = ",") %>%
    dplyr::select(country = Country, three_letter_code, four_letter_code)

#--- cleaning flight data, keeping only the total flights between countries
flight_data_clean_june_2019   <- clean_flight_data(open_sky_june_2019)
flight_data_clean_june_2020   <- clean_flight_data(open_sky_june_2020)

#--- summarising 2019 data
total_flights_june_2019 <- flight_data_clean_june_2019 %>%
    dplyr::filter(origin_country != destination_country) %>%
    dplyr::group_by(origin_country, destination_country) %>%
    dplyr::summarise(total_flights_2019 = dplyr::n()) %>%
    dplyr::mutate(origin_country_iso_code = countrycode::countrycode(origin_country, "country.name", "iso3c"),
                  destination_country_iso_code = countrycode::countrycode(destination_country, "country.name", "iso3c")) %>%
    dplyr::ungroup() %>%
    dplyr::select(origin_country_iso_code, destination_country_iso_code, total_flights_2019)

#--- summarising 2020 data
total_flights_june_2020 <- flight_data_clean_june_2020 %>%
    dplyr::filter(origin_country != destination_country) %>%
    dplyr::group_by(origin_country, destination_country) %>%
    dplyr::summarise(total_flights_2020 = dplyr::n()) %>%
    dplyr::mutate(origin_country_iso_code = countrycode::countrycode(origin_country, "country.name", "iso3c"),
                  destination_country_iso_code = countrycode::countrycode(destination_country, "country.name", "iso3c")) %>%
    dplyr::ungroup() %>%
    dplyr::select(origin_country_iso_code, destination_country_iso_code, total_flights_2020)

#--- putting 2019 and 2020 data together and calculating all scaling factors, capped at 1 where 
#--- for some reason more flights are occurring
total_flights_june_2019_2020 <- total_flights_june_2019 %>% 
    dplyr::ungroup() %>%
    dplyr::left_join(total_flights_june_2020) %>%
    dplyr::mutate(scaling_factor = total_flights_2020/total_flights_2019) %>%
    dplyr::mutate(scaling_factor = dplyr::case_when(scaling_factor > 1  ~ 1,
                                                    scaling_factor <= 1 ~ scaling_factor)) %>%
    dplyr::select(origin_country_iso_code, destination_country_iso_code, scaling_factor) %>%
    tidyr::drop_na()

#--- calculating the mean reduction over all flight paths so that we can use the mean
#--- in the absence of an estimate. Comes out to 0.372

total_flights_june_2019_2020 %>%
    dplyr::ungroup() %>%
    dplyr::summarise(average = mean(scaling_factor))

#--- writing the results to a .csv
write.csv(total_flights_june_2019_2020, "data/flight_reduction_scaling_factors_june.csv")

