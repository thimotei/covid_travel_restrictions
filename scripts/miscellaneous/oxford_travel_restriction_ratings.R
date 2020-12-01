oxford_url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
oxford_data <- oxford_url %>% readr::read_csv()

oxford_travel <- oxford_data %>%
  dplyr::select(CountryCode, Date, restriction_rating = "C8_International travel controls")  %>%
  dplyr::mutate(date = lubridate::ymd(Date)) %>%
  dplyr::select(-Date) %>%
  dplyr::filter(lubridate::month(date) == 5) %>%
  dplyr::group_by(CountryCode) %>%
  dplyr::rename(iso_code = CountryCode) %>%
  dplyr::group_by(iso_code) %>%    
  dplyr::summarise(travel_restrictions_rating = max(restriction_rating))    

oxford_travel_neat <- oxford_travel %>%
  dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "country.name")) %>%
  dplyr::select(country, iso_code, travel_restrictions_rating) %>%
  dplyr::rename(destination_country_iso_code = iso_code) %>%
  dplyr::arrange(country)

totals <- oxford_travel_neat %>% 
  dplyr::group_by(travel_restrictions_rating) %>%
  dplyr::summarise(totals = n()) 

#---- May 2019/2020 comparison

may_2019_data <- oag_traveller_data_may_2019
may_2020_data <- may_travel_data %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_passengers = scaled_travellers)

may_2019_summary <- may_2019_data %>% 
  dplyr::left_join(oxford_travel_neat) %>%
  dplyr::group_by(travel_restrictions_rating) %>% 
  dplyr::summarise(may_2019_totals = sum(total_passengers, na.rm = TRUE))

may_2020_summary <- may_2020_data %>% 
  dplyr::left_join(oxford_travel_neat) %>%
  dplyr::group_by(travel_restrictions_rating) %>% 
  dplyr::summarise(may_2020_totals = sum(total_passengers, na.rm = TRUE))

may_2019_summary %>%
  dplyr::left_join(may_2020_summary) %>%
  dplyr::mutate(ratio = may_2020_totals/may_2019_totals)

oxford_travel_neat %>%
  dplyr::group_by(travel_restrictions_rating) %>%
  dplyr::summarise(n())

#--- september 2019/20 comparison

sept_2019_data <- oag_traveller_data_september_2019
sept_2020_data <- september_travel_data %>%
  dplyr::select(origin_country_iso_code, destination_country_iso_code, total_passengers = scaled_travellers)

sept_2019_summary <- sept_2019_data %>% 
  dplyr::left_join(oxford_travel_neat) %>%
  dplyr::group_by(travel_restrictions_rating) %>% 
  dplyr::summarise(sept_2019_totals = sum(total_passengers, na.rm = TRUE))

sept_2020_summary <- sept_2020_data %>% 
  dplyr::left_join(oxford_travel_neat) %>%
  dplyr::group_by(travel_restrictions_rating) %>% 
  dplyr::summarise(sept_2020_totals = sum(total_passengers, na.rm = TRUE))
  
sept_2019_summary %>%
  dplyr::left_join(sept_2020_summary) %>%
  dplyr::mutate(ratio = sept_2020_totals/sept_2019_totals)

oxford_travel_neat %>%
  dplyr::group_by(travel_restrictions_rating) %>%
  dplyr::summarise(n())




