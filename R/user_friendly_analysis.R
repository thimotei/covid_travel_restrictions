library(here)
setwd(here::here())

source(here("R","packages.R"))
source(here("R","flight_data_cleaning_utils.R"))
source(here("R","data_helper_functions.R"))
source(here("R","plotting_helper_functions.R"))

#---- EXAMPLE DATA -----#

flight_data_arg <- dplyr::tibble(
    origin_country_iso_code = c("GBR", "USA"),
    destination_country_iso_code = c("USA", "GBR"),
    month = c(8,4),
    bookings = c(1000, 2000))

prevalence_estimates_default_sept <- dplyr::tibble(
    origin_country_iso_code = c("GBR, USE"), 
    prevalence_mid  = c(0.01, 0.02),
    prevalence_low  = c(0.001, 0.002),
    prevalence_high = c(0.02, 0.04)
)

incidence_estimates_default_sept <- dplyr::tibble(
    origin_country_iso_code = c("GBR, USE"), 
    incidence_mid  = c(5000, 10000),
    incidence_low  = c(3000, 7000),
    incidence_high = c(8000, 13000)
)

#---- FUNCTION TO RE RUN ANALYSIS ----#

produce_map_of_results <- function(flight_data_arg,
                                   month_arg = 9,
                                   prevalence_estimates_arg = prevalence_estimates_default_sept,
                                   incidence_estimates_arg  = incidence_estimates_default_sept,
                                   oxford_travel_restrictions_condition = FALSE)
{
    if(oxford_travel_restrictions_condition == TRUE)
    {
        oxford_travel_sept <- oxford_restrictions_fun(month = month_arg)
        
        countries_level_3_or_lower_may <- oxford_travel_may %>% 
            dplyr::filter(travel_restrictions_rating != month_arg) %>%
            dplyr::pull(iso_code)
        
        countries_level_3_or_lower_sept <- oxford_travel_sept %>% 
            dplyr::filter(travel_restrictions_rating != month_arg) %>%
            dplyr::pull(iso_code)
    }
    countries_level_3_or_lower_sept
}


produce_map_of_results(flight_data_arg,
                       oxford_travel_restrictions_condition = TRUE)

oxford_restrictions_fun(month = 9)
