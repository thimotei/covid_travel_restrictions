library(here)
setwd(here::here())

source(here("R","packages.R"))
source(here("R","flight_data_cleaning_utils.R"))
source(here("R","data_helper_functions.R"))
source(here("R","plotting_helper_functions.R"))

#--- reading in the flight-path specific scaling factors between May 2019 and May 2020
total_flights_june_2019_2020 <- 
    readr::read_csv(here("data", "data/flight_reduction_scaling_factors_june.csv.csv")) %>%
    dplyr::select(-X1)

#--- importing flight data. It is not in the public repo, as the data is not publicly available
oag_traveller_data_june_2019 <- 
    readr::read_csv(here("data", "raw_data/oag_data_june_2019")) 


#--- computing which countries had travel rating of 1, 2 or 3 in May and September
oxford_travel_june <- oxford_restrictions_fun(month = 6)

#--- 2019 April data for countries with level 3 or lower restriction rating
#--- to scale down the level 4 countries

countries_level_3_or_lower_sept <- oxford_travel_june %>% 
    dplyr::filter(travel_restrictions_rating != 4) %>%
    dplyr::pull(iso_code)

#--- computing reduction in flights scaling factor for all pairs of countries for May 2019/20
june_travel_data <- oag_traveller_data_june_2019 %>%
    dplyr::full_join(total_flights_june_2019_2020,
                     by = c("origin_country_iso_code", "destination_country_iso_code")) %>%
    dplyr::mutate(scaling_factor = 
                      if_else(is.na(scaling_factor), 0.407, scaling_factor)) %>%
    dplyr::mutate(origin_country      =  
                      countrycode::countrycode(origin_country_iso_code, "iso3c", "country.name"),
                  destination_country = 
                      countrycode::countrycode(destination_country_iso_code, "iso3c", "country.name")) %>%
    dplyr::mutate(scaled_travellers = 
                      dplyr::case_when( origin_country_iso_code %in% countries_level_3_or_lower_sept  ~ total_passengers*scaling_factor,
                                        !origin_country_iso_code %in% countries_level_3_or_lower_sept  ~ total_passengers)) %>%
    dplyr::select(origin_country, destination_country,
                  origin_country_iso_code, destination_country_iso_code,
                  total_passengers, scaling_factor, scaled_travellers)
#--- 0.407 is the mean of the existing scaling factors. We use this in the absence of data for countries without
#--- estimates in the OpenSky dataset


#--- using the scaled traveller numbers and prevalence to calculate expected number of imported cases
#--- from all origin countries, to all destination countries
ecdc_data <- ecdc_data_function()
under_reporting_data <- under_reporting_data_function(ecdc_data)

june_dates <- seq(as.Date("2020-06-01"), as.Date("2020-06-30"), by="days")


#-------------------- CALCULATING PREVALENCE --------------------#
#--- calculating mean prevalence in May and September
prevalence_june_all <- june_dates %>%
    purrr::map(
        ~global_prevalence_estimates_function(ecdc_data, under_reporting_data, .x)) 

prevalence_june <- prevalence_june_all %>%
    dplyr::bind_rows(.id = "country") %>%
    dplyr::group_by(iso_code) %>%
    dplyr::summarise(prevalence_mid = mean(prevalence_mid/9),
                     prevalence_low = mean(prevalence_low/9),
                     prevalence_high = mean(prevalence_high/9)) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", 'country.name', 
                                                     custom_match = c('RKS' = 'Kosovo',
                                                                      'XKX' = 'Kosovo'))) %>%
    dplyr::select(origin_country_iso_code = iso_code, country, prevalence_mid, prevalence_low, prevalence_high)

#--- assumptions about asymptomatic infections
asymptomatic_prop_mid     <- 0.5
asymptomatic_prop_low     <- 0.1
asymptomatic_prop_high    <- 0.7

#--- for sensitivity analysis where we assume levels of death under-ascertainment 
under_ascertainment_estimate_1 <- 0.5
under_ascertainment_estimate_2 <- 0.2


#-------------------- CALCULATING EXPECTED IMPORTS --------------------#
imported_cases_june_pre_sum <- june_travel_data %>%
    dplyr::left_join(prevalence_june) %>%
    dplyr::select(origin_country, destination_country, origin_country_iso_code, 
                  destination_country_iso_code, total_passengers, scaled_travellers, 
                  prevalence_mid, prevalence_low, prevalence_high) %>%
    dplyr::group_by(destination_country_iso_code,
                    destination_country) %>%
    dplyr::mutate(
        expected_imported_cases_scenario_1_mid   = total_passengers*prevalence_mid,
        expected_imported_cases_scenario_1_low   = total_passengers*prevalence_low,
        expected_imported_cases_scenario_1_high  = total_passengers*prevalence_high,
        expected_imported_cases_scenario_2_mid   = scaled_travellers*prevalence_mid,
        expected_imported_cases_scenario_2_low   = scaled_travellers*prevalence_low,
        expected_imported_cases_scenario_2_high  = scaled_travellers*prevalence_high)


#--- removing unnecessary columns for smooth joining
imported_cases_june_pre_sum_reduced <- imported_cases_june_pre_sum %>% 
    dplyr::select(origin_country_iso_code,
                  destination_country_iso_code,
                  expected_imported_cases_scenario_1_mid,
                  expected_imported_cases_scenario_1_low,
                  expected_imported_cases_scenario_1_high,
                  expected_imported_cases_scenario_2_mid,
                  expected_imported_cases_scenario_2_low,
                  expected_imported_cases_scenario_2_high)


#--- summing over all origin countries into each destination country and averaging over all days in the month
imported_cases_june <- imported_cases_june_pre_sum_reduced %>%
    dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")),
                        .funs = function(x){sum(x, na.rm = T)/31}) %>%
    dplyr::arrange(destination_country)

#-------------------- CALCULATING INCIDENCE --------------------#
#--- calculate all case data, adjusted for under-ascertainment
adjusted_case_data       <- get_adjusted_case_data_national()

# calculating the incidence in each destination country in May
incidence_june <-  adjusted_case_data %>%
    dplyr::group_by(iso_code) %>%
    dplyr::filter(date %in% june_dates) %>%
    dplyr::mutate(
        incidence_estimate_mid = new_cases_adjusted_mid/(1 - asymptomatic_prop_mid),
        incidence_estimate_low = new_cases_adjusted_low/(1 - asymptomatic_prop_low),
        incidence_estimate_high = new_cases_adjusted_high/(1 - asymptomatic_prop_high)) %>%
    dplyr::summarise(
        new_cases_adjusted_mean_mid  = mean(incidence_estimate_mid),
        new_cases_adjusted_mean_low  = mean(incidence_estimate_low),
        new_cases_adjusted_mean_high = mean(incidence_estimate_high)) %>%
    dplyr::mutate(destination_country_iso_code = iso_code) %>%
    dplyr::select(destination_country_iso_code,
                  new_cases_adjusted_mean_mid, 
                  new_cases_adjusted_mean_low, 
                  new_cases_adjusted_mean_high)


#-------------------- PUTTING IMPORTS AND INCIDENCE TOGETHER --------------------#
#--- calculating risk ratings in May
imported_cases_and_incidence_together_june <- imported_cases_june %>%
    dplyr::left_join(incidence_june) %>%
    dplyr::rename_at(.vars = vars(starts_with("expected")), 
                     .funs = function(x){sub(pattern = "expected_", replacement = "", x)}) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_mid")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_mid, "Inf"))) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_low")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_high, "Inf"))) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_high")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_low, "Inf"))) %>%
    tidyr::drop_na()  %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = list(~pmin(pmax(.,0),1)))

#--- labelling risk ratings in May
imported_cases_and_incidence_together_labels_june <- 
    imported_cases_and_incidence_together_june %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(iso_code = destination_country_iso_code,
                  incidence_june_mid  = new_cases_adjusted_mean_mid,
                  incidence_june_low  = new_cases_adjusted_mean_low,
                  incidence_june_high = new_cases_adjusted_mean_high)


#--- Putting everything together 
imported_cases_and_incidence_together_labels <- imported_cases_and_incidence_together_labels_june %>%
    dplyr::left_join(imported_cases_and_incidence_together_labels_june) %>%
    dplyr::select(iso_code, destination_country, imported_cases_scenario_1_mid, imported_cases_scenario_1_low,
                  imported_cases_scenario_1_high, imported_cases_scenario_2_mid, imported_cases_scenario_2_low,
                  imported_cases_scenario_2_high, incidence_june_mid, incidence_june_low, incidence_june_high)


#-------- AUSTRALIA SPECIFIC RESULTS, QUOTED IN MAIN TEXT -----#
imported_cases_australia_june <- imported_cases_june %>%
    dplyr::filter(destination_country == "Australia") %>%
    dplyr::mutate(imports_australia_june = paste0(signif(expected_imported_cases_scenario_2_mid, 3), " (",
                                                  signif(expected_imported_cases_scenario_2_low, 3),  "-", 
                                                  signif(expected_imported_cases_scenario_2_high, 3),  ")")) %>%
    dplyr::select(imports_australia_june)

incidence_june_australia <- incidence_june %>%
    dplyr::filter(destination_country_iso_code == "AUS") %>%
    dplyr::mutate(incidence = paste0(signif(new_cases_adjusted_mean_mid,  3), " (",
                                              signif(new_cases_adjusted_mean_low,  3), "-",
                                              signif(new_cases_adjusted_mean_high, 3), ")")) %>%
    dplyr::select(incidence)

imported_cases_australia_june

incidence_june_australia
