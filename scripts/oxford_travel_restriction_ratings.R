oxford_url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
oxford_data <- oxford_url %>% readr::read_csv()

oxford_travel <- oxford_data %>%
  dplyr::select(CountryCode, Date, restriction_rating = "C8_International travel controls")  %>%
  dplyr::mutate(date = lubridate::ymd(Date)) %>%
  dplyr::select(-Date) %>%
  dplyr::filter(lubridate::month(date) == 4) %>%
  dplyr::group_by(CountryCode) %>%
  dplyr::rename(iso_code = CountryCode) %>%
  dplyr::group_by(iso_code) %>%    
  dplyr::summarise(travel_restrictions_rating = max(restriction_rating))    

oxford_travel_neat <- oxford_travel %>%
  dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "country.name")) %>%
  dplyr::select(country, iso_code, travel_restrictions_rating) %>%
  dplyr::arrange(country)
