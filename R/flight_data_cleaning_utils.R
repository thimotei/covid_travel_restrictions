# functions

clean_flight_data <- function(open_sky_data)
{
  
  flights_by_origin <- open_sky_data %>% 
    dplyr::mutate(date = lubridate::ymd(day))%>%
    dplyr::select(date, four_letter_code = origin) %>%
    dplyr::left_join(airport_lookup) %>%
    #tidyr::drop_na() %>%
    dplyr::rename(origin_country = country,
                  origin_code = four_letter_code) %>%
    dplyr::select(-three_letter_code)
  
  flights_by_destination <- open_sky_data %>% 
    dplyr::mutate(date = lubridate::ymd(day))%>%
    dplyr::select(date, four_letter_code = destination) %>%
    dplyr::left_join(airport_lookup) %>%
    #tidyr::drop_na() %>%
    dplyr::rename(destination_country = country,
                  destination_code = four_letter_code) %>%
    dplyr::select(-three_letter_code)
  
  
  flights_by_origin$destination_code <- flights_by_destination$destination_code
  flights_by_origin$destination_country <- flights_by_destination$destination_country
  
  flight_data_out <- flights_by_origin %>%
    tidyr::drop_na() %>%
    dplyr::select(date, origin_code, destination_code, origin_country, destination_country) %>%
    dplyr::rowwise() %>%
    dplyr::filter(origin_country != destination_country)
  
  return(flight_data_out)
  
}

