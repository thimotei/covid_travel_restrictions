#--- cleaning OAG dataset for September
#--- OAG data is not public, therefore it is not in the public repo
oag_data_all <- readr::read_csv("data/raw_data/oag_flight_data_2019.csv")

#--- filtering just for September 2019
oag_traveller_data_september_2019 <- oag_data_all %>%
    dplyr::filter(month == 9) %>%
    dplyr::select(origin_country_iso_code_2 = dep_country_code, destination_country_iso_code_2 = arr_country_code, bookings) %>%
    dplyr::mutate(origin_country_iso_code   = countrycode::countrycode(origin_country_iso_code_2, "iso2c", "iso3c"),
                  destination_country_iso_code = countrycode::countrycode(destination_country_iso_code_2, "iso2c", "iso3c")) %>%
    dplyr::select(origin_country_iso_code, destination_country_iso_code, bookings) %>%
    dplyr::group_by(origin_country_iso_code, destination_country_iso_code) %>%
    dplyr::summarise(total_passengers = sum(bookings)) %>%
    dplyr::select(origin_country_iso_code, destination_country_iso_code, total_passengers)

write.csv(oag_traveller_data_september_2019, "data/raw_data/oag_data_september_2019.csv")
