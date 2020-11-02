#---- FUNCTION TO RE RUN ANALYSIS ----#

produce_map_of_results <- function(flight_data_arg = flight_data_example,
                                   month_arg = 9,
                                   prevalence_estimates_arg = prevalence_estimates_example,
                                   incidence_estimates_arg  = incidence_estimates_example,
                                   oxford_travel_restrictions_condition = FALSE,
                                   plot_arg = TRUE)
{
    
    if(oxford_travel_restrictions_condition == TRUE)
    {
        oxford_travel_sept <- oxford_restrictions_fun(month = month_arg)
        
        countries_level_3_or_lower <- oxford_travel_sept %>% 
            dplyr::filter(travel_restrictions_rating != month_arg) %>%
            dplyr::pull(iso_code)
    }

    all_together <- flight_data_example %>%
        dplyr::left_join(prevalence_estimates_arg, by = "origin_country_iso_code")
    
    
    to_plot <- all_together %>%
        dplyr::mutate(imported_cases_by_country_mid  = prevalence_mid*bookings,
                      imported_cases_by_country_low  = prevalence_low*bookings,
                      imported_cases_by_country_high = prevalence_high*bookings) %>%
        dplyr::group_by(destination_country_iso_code) %>%
        dplyr::summarise(imported_cases_mid  = sum(imported_cases_by_country_mid),
                         imported_cases_low  = sum(imported_cases_by_country_low),
                         imported_cases_high = sum(imported_cases_by_country_high)) %>%
        dplyr::left_join(incidence_estimates_arg,  by = "destination_country_iso_code") %>%
        dplyr::mutate(risk_rating_decimal_mid  = imported_cases_mid/incidence_mid,
                      risk_rating_decimal_low  = imported_cases_low/incidence_high,
                      risk_rating_decimal_high = imported_cases_high/incidence_low) %>%
        dplyr::mutate_at(.vars = vars(starts_with("risk")),
                         .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                                 include.lowest = T, 
                                                 labels = c("Green",
                                                            "Amber",
                                                            "Red"))}) %>%
        dplyr::rename(iso_code = destination_country_iso_code,
                      imported_cases_scenario_1_mid  = risk_rating_decimal_mid,
                      imported_cases_scenario_1_low  = risk_rating_decimal_low,
                      imported_cases_scenario_1_high = risk_rating_decimal_high)

    if(plot_arg == TRUE)
    {
        return(mapPlottingFunctionUserDefined(to_plot))
    }
    
    else
    {
        return(to_plot)
    }
}

country_specific_results <- function(country)
{
    
    iso_code_arg <- countrycode::countrycode(country, "country.name", "iso3c")
    
    output_dataframe <- produce_map_of_results(oxford_travel_restrictions_condition = TRUE,
                           plot = FALSE) %>%
        dplyr::filter(iso_code %in% iso_code_arg)
    
    return(output_dataframe)
    
}
