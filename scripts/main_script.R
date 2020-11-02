library(here)
setwd(here::here())

source(here("R","packages.R"))
source(here("R","flight_data_cleaning_utils.R"))
source(here("R","data_helper_functions.R"))
source(here("R","plotting_helper_functions.R"))

#--- reading in the flight-path specific scaling factors between May 2019 and May 2020
total_flights_may_2019_2020 <- 
    readr::read_csv(here("data", "flight_reduction_scaling_factors_may.csv")) %>%
    dplyr::select(-X1)

#--- reading in the flight-path specific scaling factors between September 2019 and September 2020
total_flights_september_2019_2020 <- 
    readr::read_csv(here("data", "flight_reduction_scaling_factors_september.csv")) %>%
    dplyr::select(-X1)

#--- importing flight data. It is not in the public repo, as the data is not publicly available
oag_traveller_data_may_2019 <- 
    readr::read_csv(here("data", "raw_data/oag_data_may_2019")) 

#--- importing flight data. It is not in the public repo, as the data is not publicly available
oag_traveller_data_september_2019 <- 
    readr::read_csv(here("data", "raw_data/oag_data_september_2019")) %>%
    dplyr::select(-X1)

#--- computing which countries had travel rating of 1, 2 or 3 in May and September
oxford_travel_may <- oxford_restrictions_fun(month = 5)
oxford_travel_sept <- oxford_restrictions_fun(month = 9)

#--- 2019 April data for countries with level 3 or lower restriction rating
#--- to scale down the level 4 countries
countries_level_3_or_lower_may <- oxford_travel_may %>% 
    dplyr::filter(travel_restrictions_rating != 4) %>%
    dplyr::pull(iso_code)

countries_level_3_or_lower_sept <- oxford_travel_sept %>% 
    dplyr::filter(travel_restrictions_rating != 4) %>%
    dplyr::pull(iso_code)


#--- computing reduction in flights scaling factor for all pairs of countries for May 2019/20
may_travel_data <- oag_traveller_data_may_2019 %>%
    dplyr::full_join(total_flights_may_2019_2020,
                     by = c("origin_country_iso_code", "destination_country_iso_code")) %>%
    dplyr::mutate(scaling_factor = 
                      if_else(is.na(scaling_factor), 0.372, scaling_factor)) %>%
    dplyr::mutate(origin_country      =  
                      countrycode::countrycode(origin_country_iso_code, "iso3c", "country.name"),
                  destination_country = 
                      countrycode::countrycode(destination_country_iso_code, "iso3c", "country.name")) %>%
    dplyr::mutate(scaled_travellers = 
                      dplyr::case_when(origin_country_iso_code %in% countries_level_3_or_lower_may  ~ total_passengers*scaling_factor,
                                       !origin_country_iso_code %in% countries_level_3_or_lower_may ~ total_passengers)) %>%
    dplyr::select(origin_country, destination_country,
                  origin_country_iso_code, destination_country_iso_code,
                  total_passengers, scaling_factor, scaled_travellers)
#--- 0.372 is the mean of the existing scaling factors. We use this in the absence of data for countries without
#--- estimates in the OpenSky dataset

may_travel_data %>% 
    dplyr::summarise(sf_mean_may  = mean(scaling_factor),
                     sf_lower_may = min(scaling_factor),
                     sf_upper_may = max(scaling_factor))

#--- computing reduction in flights scaling factor for all pairs of countries for May 2019/20
september_travel_data <- oag_traveller_data_september_2019 %>%
    dplyr::full_join(total_flights_september_2019_2020,
                     by = c("origin_country_iso_code", "destination_country_iso_code")) %>%
    dplyr::mutate(scaling_factor = 
                      if_else(is.na(scaling_factor), 0.521, scaling_factor)) %>%
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
#--- 0.521. is the mean of the existing scaling factors. We use this in the absence of data for countries without
#--- estimates in the OpenSky dataset


september_travel_data <- oag_traveller_data_september_2019 %>%
    dplyr::full_join(total_flights_september_2019_2020,
                     by = c("origin_country_iso_code", "destination_country_iso_code"))


#--- using the scaled traveller numbers and prevalence to calculate expected number of imported cases
#--- from all origin countries, to all destination countries
ecdc_data <- ecdc_data_function()
under_reporting_data <- under_reporting_data_function(ecdc_data)

may_dates       <- seq(as.Date("2020-05-01"), as.Date("2020-05-31"), by="days")
september_dates <- seq(as.Date("2020-09-01"), as.Date("2020-09-30"), by="days")

#-------------------- CALCULATING PREVALENCE --------------------#
#--- calculating mean prevalence in May and September
prevalence_may_all <- may_dates %>%
    purrr::map(
        ~global_prevalence_estimates_function(ecdc_data, under_reporting_data, .x)) 

prevalence_september_all <- september_dates %>%
    purrr::map(
        ~global_prevalence_estimates_function(ecdc_data, under_reporting_data, .x)) 

prevalence_may <- prevalence_may_all %>%
    dplyr::bind_rows(.id = "country") %>%
    dplyr::group_by(iso_code) %>%
    dplyr::summarise(prevalence_mid = mean(prevalence_mid/9),
                     prevalence_low = mean(prevalence_low/9),
                     prevalence_high = mean(prevalence_high/9)) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", 'country.name', 
                                                      custom_match = c('RKS' = 'Kosovo',
                                                                       'XKX' = 'Kosovo'))) %>%
    dplyr::select(origin_country_iso_code = iso_code, country, prevalence_mid, prevalence_low, prevalence_high)

prevalence_september <- prevalence_september_all %>%
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
imported_cases_may_pre_sum <- may_travel_data %>%
    dplyr::left_join(prevalence_may) %>%
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

imported_cases_september_pre_sum <- september_travel_data %>%
    dplyr::left_join(prevalence_september) %>%
    dplyr::select(origin_country, destination_country, origin_country_iso_code, 
                  destination_country_iso_code, total_passengers, scaled_travellers, 
                  prevalence_mid, prevalence_low, prevalence_high) %>%
    dplyr::group_by(destination_country_iso_code,
                    destination_country) %>%
    dplyr::mutate(
        expected_imported_cases_scenario_3_mid   = total_passengers*prevalence_mid,
        expected_imported_cases_scenario_3_low   = total_passengers*prevalence_low,
        expected_imported_cases_scenario_3_high  = total_passengers*prevalence_high,
        expected_imported_cases_scenario_4_mid   = scaled_travellers*prevalence_mid,
        expected_imported_cases_scenario_4_low   = scaled_travellers*prevalence_low,
        expected_imported_cases_scenario_4_high  = scaled_travellers*prevalence_high)

#--- removing unnecessary columns for smooth joining
imported_cases_may_pre_sum_reduced <- imported_cases_may_pre_sum %>% 
    dplyr::select(origin_country_iso_code,
                  destination_country_iso_code,
                  expected_imported_cases_scenario_1_mid,
                  expected_imported_cases_scenario_1_low,
                  expected_imported_cases_scenario_1_high,
                  expected_imported_cases_scenario_2_mid,
                  expected_imported_cases_scenario_2_low,
                  expected_imported_cases_scenario_2_high)

imported_cases_september_pre_sum_reduced <- imported_cases_september_pre_sum %>% 
    dplyr::select(origin_country_iso_code,
                  destination_country_iso_code,
                  expected_imported_cases_scenario_3_mid,
                  expected_imported_cases_scenario_3_low,
                  expected_imported_cases_scenario_3_high,
                  expected_imported_cases_scenario_4_mid,
                  expected_imported_cases_scenario_4_low,
                  expected_imported_cases_scenario_4_high)

#--- summing over all origin countries into each destination country and averaging over all days in the month
imported_cases_may <- imported_cases_may_pre_sum_reduced %>%
    dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")),
                        .funs = function(x){sum(x, na.rm = T)/31}) %>%
    dplyr::arrange(destination_country)

imported_cases_september <- imported_cases_september_pre_sum_reduced %>%
    dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")),
                        .funs = function(x){sum(x, na.rm = T)/31}) %>%
    dplyr::arrange(destination_country)

# #--- joining imported cases in May and September together
imported_cases <- imported_cases_may %>%
     dplyr::left_join(imported_cases_september)

#-------------------- CALCULATING INCIDENCE --------------------#
#--- calculate all case data, adjusted for under-ascertainment
adjusted_case_data       <- get_adjusted_case_data_national()

# calculating the incidence in each destination country in May
incidence_may <-  adjusted_case_data %>%
    dplyr::group_by(iso_code) %>%
    dplyr::filter(date %in% may_dates) %>%
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

# calculating the incidence in each destination country in September
incidence_september <-  adjusted_case_data %>%
    dplyr::group_by(iso_code) %>%
    dplyr::filter(date %in% september_dates) %>%
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
imported_cases_and_incidence_together_may <- imported_cases_may %>%
    dplyr::left_join(incidence_may) %>%
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

#--- calculating risk ratings in September
imported_cases_and_incidence_together_september <- imported_cases_september %>%
    dplyr::left_join(incidence_september) %>%
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
imported_cases_and_incidence_together_labels_may <- 
    imported_cases_and_incidence_together_may %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(iso_code = destination_country_iso_code,
                  incidence_may_mid  = new_cases_adjusted_mean_mid,
                  incidence_may_low  = new_cases_adjusted_mean_low,
                  incidence_may_high = new_cases_adjusted_mean_high)

#--- labelling risk ratings in September
imported_cases_and_incidence_together_labels_september <- 
    imported_cases_and_incidence_together_september %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(iso_code = destination_country_iso_code,
                  incidence_sept_mid  = new_cases_adjusted_mean_mid,
                  incidence_sept_low  = new_cases_adjusted_mean_low,
                  incidence_sept_high = new_cases_adjusted_mean_high)

#--- Putting everything together ready for plots
imported_cases_and_incidence_together_labels <- imported_cases_and_incidence_together_labels_may %>%
    dplyr::left_join(imported_cases_and_incidence_together_labels_september) %>%
    dplyr::select(iso_code, destination_country, imported_cases_scenario_1_mid, imported_cases_scenario_1_low,
                  imported_cases_scenario_1_high, imported_cases_scenario_2_mid, imported_cases_scenario_2_low,
                  imported_cases_scenario_2_high, imported_cases_scenario_3_mid, imported_cases_scenario_3_low,
                  imported_cases_scenario_3_high, imported_cases_scenario_4_mid, imported_cases_scenario_4_low,
                  imported_cases_scenario_4_high, incidence_may_mid, incidence_may_low, incidence_may_high,
                  incidence_sept_mid, incidence_sept_low, incidence_sept_high)


#--- Summary of results, quoted in the main text, including Rt condition

imported_cases_september %>%
    dplyr::filter(destination_country_iso_code == "CHN") %>%
    dplyr::select(expected_imported_cases_scenario_4_mid)

imported_cases_september %>%
    dplyr::filter(destination_country_iso_code == "CHN") %>%
    dplyr::select(expected_imported_cases_scenario_4_low)

imported_cases_september %>%
    dplyr::filter(destination_country_iso_code == "CHN") %>%
    dplyr::select(expected_imported_cases_scenario_4_high)

imported_cases_september %>%
    dplyr::filter(destination_country_iso_code == "NZL") %>%
    dplyr::select(expected_imported_cases_scenario_4_mid)

imported_cases_september %>%
    dplyr::filter(destination_country_iso_code == "NZL") %>%
    dplyr::select(expected_imported_cases_scenario_4_low)

imported_cases_september %>%
    dplyr::filter(destination_country_iso_code == "NZL") %>%
    dplyr::select(expected_imported_cases_scenario_4_high)

chinaimports <- 54.5 (26.4 - 109)
new_zealand_imports <-  5.70 (2.69 - 11.9)

incidence_september %>%
    dplyr::filter(destination_country_iso_code == "CHN") %>%
    dplyr::select(new_cases_adjusted_mean_mid)

incidence_september %>%
    dplyr::filter(destination_country_iso_code == "CHN") %>%
    dplyr::select(new_cases_adjusted_mean_low)

incidence_september %>%
    dplyr::filter(destination_country_iso_code == "CHN") %>%
    dplyr::select(new_cases_adjusted_mean_high)

incidence_september %>%
    dplyr::filter(destination_country_iso_code == "NZL") %>%
    dplyr::select(new_cases_adjusted_mean_mid)

incidence_september %>%
    dplyr::filter(destination_country_iso_code == "NZL") %>%
    dplyr::select(new_cases_adjusted_mean_low)

incidence_september %>%
    dplyr::filter(destination_country_iso_code == "NZL") %>%
    dplyr::select(new_cases_adjusted_mean_high)

china_incidence <- 45.5 (24.3 - 245)
new_zealand_incidence <- 9.58 (3.92 - 28.8)

#--- Scenario A, condition less than 0.01
scenario_a_less_1_mid <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_1_mid < 0.01) %>%
    nrow()

scenario_a_less_1_low <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_1_low < 0.01) %>%
    nrow()

scenario_a_less_1_high <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_1_high < 0.01) %>%
    nrow()

less_than_1_scenario_a <- paste0(scenario_a_less_1_mid, " (",
       scenario_a_less_1_high, " - ",
       scenario_a_less_1_low, ")")


#--- Scenario B
scenario_b_less_1_mid <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_2_mid < 0.01) %>%
    nrow()

scenario_b_less_1_low <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_2_low < 0.01) %>%
    nrow()

scenario_b_less_1_high <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_2_high < 0.01) %>%
    nrow()

less_than_1_scenario_b <-paste0(scenario_b_less_1_mid, " (",
       scenario_b_less_1_high, " - ",
       scenario_b_less_1_low , ")")

#--- Scenario C
scenario_c_less_1_mid <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_3_mid < 0.01) %>%
    nrow()

scenario_c_less_1_low <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_3_low < 0.01) %>%
    nrow()

scenario_c_less_1_high <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_3_high < 0.01) %>%
    nrow()

less_than_1_scenario_c <-paste0(scenario_c_less_1_mid, " (",
       scenario_c_less_1_high, " - ",
       scenario_c_less_1_low , ")")

#--- Scenario D
scenario_d_less_1_mid <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_4_mid < 0.01) %>%
    nrow()

scenario_d_less_1_low <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_4_low < 0.01) %>%
    nrow()

scenario_d_less_1_high <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(imported_cases_scenario_4_high < 0.01) %>%
    nrow()

less_than_1_scenario_d <-paste0(scenario_d_less_1_mid, " (",
       scenario_d_less_1_high, " - ",
       scenario_d_less_1_low , ")")


#-- all results
less_than_1_scenario_a
less_than_1_scenario_b
less_than_1_scenario_c
less_than_1_scenario_d

#--- Scenario A, condition between 0.01 and 0.1
scenario_a_between_1_10_mid <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_1_mid & imported_cases_scenario_1_mid <= 0.1) %>%
    nrow()

scenario_a_between_1_10_low <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_1_low & imported_cases_scenario_1_low <= 0.1) %>%
    nrow()

scenario_a_between_1_10_high <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_1_high & imported_cases_scenario_1_high <= 0.1) %>%
    nrow()

between_1_10_scenario_a <- paste0(scenario_a_between_1_10_mid, " (",
       scenario_a_between_1_10_high, " - ",
       scenario_a_between_1_10_low, ")")


#--- Scenario B
scenario_b_between_1_10_mid <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_2_mid & imported_cases_scenario_2_mid <= 0.1) %>%
    nrow()

scenario_b_between_1_10_low <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_2_low & imported_cases_scenario_2_low <= 0.1) %>%
    nrow()

scenario_b_between_1_10_high <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_2_high & imported_cases_scenario_2_high <= 0.1) %>%
    nrow()

between_1_10_scenario_b <- paste0(scenario_b_between_1_10_mid, " (",
       scenario_b_between_1_10_high, " - ",
       scenario_b_between_1_10_low, ")")

#--- Scenario C
scenario_c_between_1_10_mid <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_3_mid & imported_cases_scenario_3_mid <= 0.1) %>%
    nrow()

scenario_c_between_1_10_low <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_3_low & imported_cases_scenario_3_low <= 0.1) %>%
    nrow()

scenario_c_between_1_10_high <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_3_high & imported_cases_scenario_3_high <= 0.1) %>%
    nrow()

between_1_10_scenario_c <- paste0(scenario_c_between_1_10_mid, " (",
       scenario_c_between_1_10_low, " - ",
       scenario_c_between_1_10_high, ")")

#--- Scenario D
scenario_d_between_1_10_mid <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_4_mid & imported_cases_scenario_4_mid <= 0.1) %>%
    nrow()

scenario_d_between_1_10_low <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_4_low & imported_cases_scenario_4_low <= 0.1) %>%
    nrow()

scenario_d_between_1_10_high <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.01 <= imported_cases_scenario_4_high & imported_cases_scenario_4_high <= 0.1) %>%
    nrow()

between_1_10_scenario_d <- paste0(scenario_d_between_1_10_mid, " (",
       scenario_d_between_1_10_low, " - ",
       scenario_d_between_1_10_high, ")")

#--- all results
between_1_10_scenario_a
between_1_10_scenario_b
between_1_10_scenario_c
between_1_10_scenario_d

#--- Rt estimate condition

rt_estimates_raw <- readr::read_csv("data/rt_estimates_clean.csv")
rt_estimates <- rt_estimates_raw %>%
    dplyr::mutate(rt_estimate = paste0(median, " (", lower_90, "-", upper_90, ")")) %>%
    dplyr::select(iso_code, rt_estimate) 

#--- Scenario A
rt_condition_countries_scenario_a_mid <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter((0.95 <= median & median <= 1.05) | imported_cases_scenario_1_mid > 0.01) %>%
    nrow()

rt_condition_countries_scenario_a_low <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.95 <= median & median <= 1.05 | imported_cases_scenario_1_low > 0.01) %>%
    nrow()


rt_condition_countries_scenario_a_high <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.95 <= median & median <= 1.05 | imported_cases_scenario_1_high > 0.01) %>%
    nrow()

rt_condition_scenario_a <- paste0(rt_condition_countries_scenario_a_mid, " (",
                                  rt_condition_countries_scenario_a_low, " - ", 
                                  rt_condition_countries_scenario_a_high, ")")

#--- Scenario B
rt_condition_countries_scenario_b_mid <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter((0.95 <= median & median <= 1.05) | imported_cases_scenario_2_mid > 0.01) %>%
    nrow()

rt_condition_countries_scenario_b_low <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.95 <= median & median <= 1.05 | imported_cases_scenario_2_low > 0.01) %>%
    nrow()


rt_condition_countries_scenario_b_high <- imported_cases_and_incidence_together_may %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.95 <= median & median <= 1.05 | imported_cases_scenario_2_high > 0.01) %>%
    nrow()

rt_condition_scenario_b <- paste0(rt_condition_countries_scenario_b_mid, " (",
                                  rt_condition_countries_scenario_b_low, " - ",
                                  rt_condition_countries_scenario_b_high, ")")

#--- Scenario C
rt_condition_countries_scenario_c_mid <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter((0.95 <= median & median <= 1.05) | imported_cases_scenario_3_mid > 0.01) %>%
    nrow()

rt_condition_countries_scenario_c_low <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.95 <= median & median <= 1.05 | imported_cases_scenario_3_low > 0.01) %>%
    nrow()


rt_condition_countries_scenario_c_high <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.95 <= median & median <= 1.05 | imported_cases_scenario_3_high > 0.01) %>%
    nrow()

rt_condition_scenario_c <- paste0(rt_condition_countries_scenario_c_mid, " (",
                                  rt_condition_countries_scenario_c_low, " - ",
                                  rt_condition_countries_scenario_c_high, ")")

#--- Scenario D
rt_condition_countries_scenario_d_mid <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter((0.95 <= median & median <= 1.05) | imported_cases_scenario_4_mid > 0.01) %>%
    nrow()

rt_condition_countries_scenario_d_low <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter(0.95 <= median & median <= 1.05 | imported_cases_scenario_4_low > 0.01) %>%
    nrow()


rt_condition_countries_scenario_d_high <- imported_cases_and_incidence_together_september %>%
    dplyr::rename(iso_code = destination_country_iso_code) %>%
    dplyr::left_join(rt_estimates_raw) %>%
    dplyr::filter((0.95 <= median & median <= 1.05) | imported_cases_scenario_4_high > 0.01) %>%
    nrow()

rt_condition_scenario_d <- paste0(rt_condition_countries_scenario_d_mid, " (",
                                  rt_condition_countries_scenario_d_low, " - ",
                                  rt_condition_countries_scenario_d_high, ")")

rt_condition_scenario_a
rt_condition_scenario_b
rt_condition_scenario_c
rt_condition_scenario_d

#-------------------- PLOTTING RESULTS --------------------#
#--- making figure 1 - map of risk of imported cases
list(`SI_low`  = list(name = "SI_low",
                      scenarios = "low"),
     `SI_high` = list(name = "SI_high",
                      scenarios = "high"),
     `main`    = list(name = "main",
                      scenarios = "mid")) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs",
                                  paste("figure_2_",.x$name,".pdf",sep="")),
                         plot = mapPlottingFunction(
                             imported_cases_and_incidence_together_labels,
                             scenarios = .x$scenarios),
                         device = "pdf",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))


#--- bar plot of required reduction in flights
#--- have changed the relevant scenario to scenario 1, as this is now the most plausible

required_reduction <- imported_cases_and_incidence_together_september %>% 
    inner_join(imported_cases_september) %>%
    dplyr::select(iso_code = destination_country_iso_code,
                  starts_with("expected_imported_cases_scenario_4"),
                  starts_with("new_cases_adjusted_mean"), 
                  starts_with("imported_cases_scenario_4")) %>%
    rename_at(.vars = vars(starts_with("imported_cases_scenario_4")),
              .funs = ~sub(pattern = "imported_cases_scenario_4",
                           replacement = "importation_per_incidence",
                           x = .)) %>%
    dplyr::filter(importation_per_incidence_mid > 0.01) %>%
    dplyr::mutate(
        required_reduction_in_passengers_low = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_4_low),
        required_reduction_in_passengers_mid = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_4_mid),
        required_reduction_in_passengers_high = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_4_high)) %>%
    dplyr::mutate_at(.vars = vars(starts_with("required")),
                     .funs = function(x){pmin(1,pmax(0,x))}) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "iso.name.en"))

required_reduction_all <- imported_cases_and_incidence_together_september %>% 
    inner_join(imported_cases_september) %>%
    dplyr::select(iso_code = destination_country_iso_code,
                  starts_with("expected_imported_cases_scenario_4"),
                  starts_with("new_cases_adjusted_mean"), 
                  starts_with("imported_cases_scenario_4")) %>%
    rename_at(.vars = vars(starts_with("imported_cases_scenario_4")),
              .funs = ~sub(pattern = "imported_cases_scenario_4",
                           replacement = "importation_per_incidence",
                           x = .)) %>%
    #dplyr::filter(importation_per_incidence_mid > 0.01) %>%
    dplyr::mutate(
        required_reduction_in_passengers_low = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_4_low),
        required_reduction_in_passengers_mid = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_4_mid),
        required_reduction_in_passengers_high = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_4_high)) %>%
    dplyr::mutate_at(.vars = vars(starts_with("required")),
                     .funs = function(x){pmin(1,pmax(0,x))}) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "iso.name.en"))

figure_2_data <- barDataFunction(required_reduction)  %>%
    dplyr::mutate(country = plyr::revalue(country, c("Congo (the Democratic Republic of the)" = "DRC",
                                                     "Bahamas (the)" = "Bahamas",
                                                     "Sudan (the)"   = "Sudan",
                                                     "Sint Maarten (Dutch part)" = "Sint Maarten",
                                                     "Netherlands (the)" = "Netherlands",
                                                     "Niger (the)" = "Niger",
                                                     "Virgin Islands (U.S.)" = "Virgin Islands")))

figure_2 <- barPlottingFunction(figure_2_data, interval = TRUE)

ggplot2::ggsave(here("outputs","figure_2.pdf"),
                figure_2,
                device = "pdf",
                width = 12, 
                height = 4.5)


figure_3_data <-
    figure_2_data %>%
    mutate_at(.vars = vars(starts_with("importation")),
              .funs = list(trim = as.numeric,
                           label = ~cut(.,
                                        breaks = c(0, 0.01, 0.1, 1),
                                        include.lowest = T, 
                                        labels = c("Green",
                                                   "Amber",
                                                   "Red")))) %>%
    mutate_at(.vars = vars(matches("trim")),
              .funs = function(x){pmin(0.995, pmax(0.005,x))})


figure_3 <- scatterPlottingFunction(figure_3_data, interval = FALSE)

ggplot2::ggsave(here("outputs","figure_3.pdf"),
                figure_3, #+ facet_grid(importation_per_incidence_mid_label ~ region),
                device = "pdf",
                width = 9, 
                height = 6)

#-------------------- SUMMARY RESULTS QUOTED IN MAIN TEXT --------------------#

summary_results_with_rt_condition <- imported_cases_and_incidence_together_labels %>%
    select_at(vars(starts_with("imported_cases_scenario"))) %>%
    filter(iso_code %in% countries_to_include) %>%
    pivot_longer(cols = starts_with("imported_cases_scenario")) %>%
    group_by(name, value) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = value, values_from = n) %>%
    mutate(under1 = Green, under10 = Green + Amber)

summary_results_without_rt_condition <- imported_cases_and_incidence_together_labels %>%
    select_at(vars(starts_with("imported_cases_scenario"))) %>%
    pivot_longer(cols = starts_with("imported_cases_scenario")) %>%
    group_by(name, value) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = value, values_from = n) %>%
    mutate(under1 = Green, under10 = Green + Amber) %>%
    select(-`NA`)

#-------------------- SUPPLEMENTARY TABLE --------------------#


table_data <- barDataFunction(required_reduction_all)  %>%
    dplyr::mutate(country = plyr::revalue(country, c("Congo (the Democratic Republic of the)" = "DRC",
                                                     "Bahamas (the)" = "Bahamas",
                                                     "Sudan (the)"   = "Sudan",
                                                     "Sint Maarten (Dutch part)" = "Sint Maarten",
                                                     "Netherlands (the)" = "Netherlands",
                                                     "Niger (the)" = "Niger",
                                                     "Virgin Islands (U.S.)" = "Virgin Islands"))) %>%
    dplyr::left_join(rt_estimates)


table_data  %>% scatterTableFunction %>%
    write_csv(here("outputs", "table_S2.csv"))

#--- making the arbitrary thresholds figure, Figure S6
source(here("scripts","arbitrary_thresholds_figure.R"))
source(here("scripts","importation_risks.R"))

