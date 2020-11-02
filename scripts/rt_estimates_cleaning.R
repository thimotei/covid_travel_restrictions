rt_estimates <- readr::read_csv("data/rt_estimates.csv")

rt_estimates_reduced <- rt_estimates %>% 
    dplyr::mutate(iso_code = countrycode::countrycode(country, "country.name", "iso3c",
                                                      custom_match = c("Kosovo" = "XXK"))) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::mutate(country_name = countrycode::countrycode(iso_code, "iso3c", "country.name",
                                                     custom_match = c("XXK" = "Kosovo"))) %>%
    dplyr::select(country = country_name, iso_code, median, lower_90, upper_90)


write.csv(rt_estimates_reduced, "data/rt_estimates_clean.csv")
