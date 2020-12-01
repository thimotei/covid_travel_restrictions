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

#--- travel reduction assumptions
traveller_reduction_1     <- 0.75
traveller_reduction_2     <- 0.50

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
        expected_imported_cases_scenario_3_mid   = total_passengers*prevalence_mid*traveller_reduction_1,
        expected_imported_cases_scenario_3_low   = total_passengers*prevalence_low*traveller_reduction_1,
        expected_imported_cases_scenario_3_high  = total_passengers*prevalence_high*traveller_reduction_1,
        expected_imported_cases_scenario_4_mid   = scaled_travellers*prevalence_mid*traveller_reduction_2,
        expected_imported_cases_scenario_4_low   = scaled_travellers*prevalence_low*traveller_reduction_2,
        expected_imported_cases_scenario_4_high  = scaled_travellers*prevalence_high*traveller_reduction_2)

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

imported_cases_and_incidence_together_labels_final <- imported_cases_and_incidence_together_labels %>%
    dplyr::select(iso_code, destination_country,
                  imported_cases_scenario_1_mid  = imported_cases_scenario_3_mid,
                  imported_cases_scenario_1_low  = imported_cases_scenario_3_low, 
                  imported_cases_scenario_1_high = imported_cases_scenario_3_high, 
                  imported_cases_scenario_2_mid  = imported_cases_scenario_4_mid,
                  imported_cases_scenario_2_low  = imported_cases_scenario_4_low,
                  imported_cases_scenario_2_high = imported_cases_scenario_4_high)

#-------------------- PLOTTING RESULTS --------------------#
#--- making figure 1 - map of risk of imported cases
list(`main`    = list(name = "main",
                      scenarios = c("mid", "low", "high"))) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs",
                                  "figure_S10.pdf"),
                         plot = mapPlottingFunctionTravelVolumeSensitivity(
                             imported_cases_and_incidence_together_labels_final,
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

