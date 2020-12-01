#--- if the main results have not been run yet, run the script below
source("scripts/main_script_computing_all_results.R")

july_dates <- seq(as.Date("2020-07-01"), as.Date("2020-07-31"), by="days")

#-------------------- CALCULATING PREVALENCE --------------------#
#--- calculating mean prevalence in May and September
prevalence_july <- september_dates %>%
    purrr::map(
        ~global_prevalence_estimates_function(ecdc_data, under_reporting_data, .x)) 

prevalence_july <- prevalence_september_all %>%
    dplyr::bind_rows(.id = "country") %>%
    dplyr::group_by(iso_code) %>%
    dplyr::summarise(prevalence_mid = mean(prevalence_mid/9),
                     prevalence_low = mean(prevalence_low/9),
                     prevalence_high = mean(prevalence_high/9)) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", 'country.name', 
                                                     custom_match = c('RKS' = 'Kosovo',
                                                                      'XKX' = 'Kosovo'))) %>%
    dplyr::select(origin_country_iso_code = iso_code, country, prevalence_mid, prevalence_low, prevalence_high)


#-------------------- CALCULATING EXPECTED IMPORTS --------------------#
imported_cases_may_pre_sum <- may_travel_data %>%
    dplyr::left_join(prevalence_july) %>%
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
    dplyr::left_join(prevalence_july) %>%
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
incidence_july <-  adjusted_case_data %>%
    dplyr::group_by(iso_code) %>%
    dplyr::filter(date %in% july_dates) %>%
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
    dplyr::left_join(incidence_july) %>%
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
    dplyr::left_join(incidence_july) %>%
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
                                  paste("figure_S6_",.x$name,".pdf",sep="")),
                         plot = mapPlottingFunction(
                             imported_cases_and_incidence_together_labels,
                             scenarios = .x$scenarios),
                         device = "pdf",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))

list(`SI_low`  = list(name = "SI_low",
                      scenarios = "low"),
     `SI_high` = list(name = "SI_high",
                      scenarios = "high"),
     `main`    = list(name = "main",
                      scenarios = "mid")) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs",
                                  paste("figure_S6_",.x$name,".png",sep="")),
                         plot = mapPlottingFunction(
                             imported_cases_and_incidence_together_labels,
                             scenarios = .x$scenarios),
                         device = "png",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))

