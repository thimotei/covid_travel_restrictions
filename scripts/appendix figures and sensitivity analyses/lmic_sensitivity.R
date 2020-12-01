#--- if results have not been run yet, run the script below
source("scripts/main_script_computing_all_results.R")

#-------------------- LMIC SENSITIVITY ANALYSIS TO MAKE FIGURE S4 --------------------#

#--- sensitivity analysis, scaling all LMIC prevalence data up by 50%, 80% and 90%

list_of_lmic_countries <- c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda",
                            "Argentina", "Armenia", "Azerbaijan", "Bangladesh", "Belarus", "Belize",
                            "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil",
                            "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", 
                            "Central African Republic", "Chad", "China", "Colombia", "Comoros", 
                            "Democratic Republic of Congo", "Congo", "Cook Islands", "Costa Rica",
                            "Côte d'Ivoire", "Cuba", "Djibouti", "Dominica", "Dominican Republic", 
                            "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Ethiopia",
                            "Fiji", "Gabon", "Gambia", "Georgia", "Ghana", "Grenada", "Guatemala", 
                            "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "India", "Indonesia",
                            "Iran", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kenya", "Kiribati",
                            "North Korea", "Kosovo", "Kyrgyzstan", "Laos", "Lebanon", "Lesotho", "Liberia",
                            "Libya", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali",
                            "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia",
                            "Moldova", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", 
                            "Myanmar", "Namibia", "Nauru", "Nepal", "Nicaragua", "Niger", "Nigerial",
                            "Niue", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru",
                            "Philippines", "Rwanda", "Saint Helena", "Samoa", "São Tomé and Príncipe",
                            "Senegal", "Serbia", "Sierra Leone", "Solomon Islands", "Somalia", 
                            "South Africa", "South Sudan", "Sri Lanka", "Saint Lucia", 
                            "Saint Vincent and the Grenadines", "Sudan", "Suriname", "Swaziland",
                            "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", 
                            "Togo", "Tokelau", "Tonga", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu",
                            "Uganda", "Ukraine", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam",
                            "Wallis and Futuna", "West Bank and Gaza Strip", "Yemen", "Zambia", "Zimbabwe")


lmics <- dplyr::tibble(country_name = list_of_lmic_countries) %>%
    dplyr::mutate(iso_code = countrycode::countrycode(country_name, "country.name", "iso3c", custom_match = c("Kosovo" = "RKS",
                                                                                                              "Micronesia" = "FSM")))

lmics_iso_code_list <- lmics %>%
    dplyr::pull(iso_code)

#--- LMIC sensitivity, adjusting the prevalences

prevalence_data_sensitivity_tmp_1 <- prevalence_september %>%
    dplyr::filter(origin_country_iso_code %in% lmics_iso_code_list) %>%
    dplyr::mutate(prevalence_mid_U50 = prevalence_mid/under_ascertainment_estimate_1,
                  prevalence_low_U50 = prevalence_low/under_ascertainment_estimate_1,
                  prevalence_high_U50 = prevalence_high/under_ascertainment_estimate_1,
                  prevalence_mid_U80 = prevalence_mid/under_ascertainment_estimate_2,
                  prevalence_low_U80 = prevalence_low/under_ascertainment_estimate_2,
                  prevalence_high_U80 = prevalence_high/under_ascertainment_estimate_2)

prevalence_data_sensitivity_tmp_2 <- prevalence_september %>%
    dplyr::filter(!(origin_country_iso_code %in% lmics_iso_code_list)) %>%
    dplyr::mutate(prevalence_mid_U50 = prevalence_mid,
                  prevalence_low_U50 = prevalence_low,
                  prevalence_high_U50 = prevalence_high,
                  prevalence_mid_U80 = prevalence_mid,
                  prevalence_low_U80 = prevalence_low,
                  prevalence_high_U80 = prevalence_high)

prevalence_data_sensitivity <- prevalence_data_sensitivity_tmp_1 %>%
    dplyr::bind_rows(prevalence_data_sensitivity_tmp_2) 

imported_cases_pre_sum_2020_sensitivity <- september_travel_data %>%
    dplyr::left_join(prevalence_data_sensitivity) %>%
    dplyr::select(origin_country, destination_country, origin_country_iso_code, 
                  destination_country_iso_code, total_passengers, scaled_travellers, 
                  prevalence_mid_U50, prevalence_low_U50, prevalence_high_U50,
                  prevalence_mid_U80, prevalence_low_U80, prevalence_high_U80) %>%
    dplyr::group_by(destination_country_iso_code,
                    destination_country) %>%
    dplyr::mutate(
        expected_imported_cases_scenario_U50_mid   = total_passengers*prevalence_mid_U50,
        expected_imported_cases_scenario_U50_low   = total_passengers*prevalence_low_U50,
        expected_imported_cases_scenario_U50_high  = total_passengers*prevalence_high_U50,
        expected_imported_cases_scenario_U80_mid   = total_passengers*prevalence_mid_U80,
        expected_imported_cases_scenario_U80_low   = total_passengers*prevalence_low_U80,
        expected_imported_cases_scenario_U80_high  = total_passengers*prevalence_high_U80,
        scaled_travellers = NA)

#--- removing unnecessary columns for smooth joining
imported_cases_pre_sum_2020_reduced_sensitivity <- imported_cases_pre_sum_2020_sensitivity %>% 
    dplyr::select(origin_country_iso_code,
                  destination_country_iso_code,
                  expected_imported_cases_scenario_U50_mid, 
                  expected_imported_cases_scenario_U50_low, 
                  expected_imported_cases_scenario_U50_high,
                  expected_imported_cases_scenario_U80_mid, 
                  expected_imported_cases_scenario_U80_low, 
                  expected_imported_cases_scenario_U80_high)

imported_cases_sensitivity <- imported_cases_pre_sum_2020_reduced_sensitivity %>%
    dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")),
                        .funs = function(x){sum(x, na.rm = T)/31}) %>%
    dplyr::arrange(destination_country)

incidence_data_sensitivity_tmp_1 <- incidence_september %>%
    dplyr::filter(destination_country_iso_code %in% lmics_iso_code_list) %>%
    dplyr::mutate(new_cases_adjusted_mean_mid_U50  = new_cases_adjusted_mean_mid/under_ascertainment_estimate_1,
                  new_cases_adjusted_mean_low_U50  = new_cases_adjusted_mean_low/under_ascertainment_estimate_1,
                  new_cases_adjusted_mean_high_U50 = new_cases_adjusted_mean_high/under_ascertainment_estimate_1,
                  new_cases_adjusted_mean_mid_U80  = new_cases_adjusted_mean_mid/under_ascertainment_estimate_2,
                  new_cases_adjusted_mean_low_U80  = new_cases_adjusted_mean_low/under_ascertainment_estimate_2,
                  new_cases_adjusted_mean_high_U80 = new_cases_adjusted_mean_high/under_ascertainment_estimate_2)

incidence_data_sensitivity_tmp_2 <- incidence_september %>%
    dplyr::filter(!(destination_country_iso_code %in% lmics_iso_code_list)) %>%
    dplyr::mutate(new_cases_adjusted_mean_mid_U50  = new_cases_adjusted_mean_mid,
                  new_cases_adjusted_mean_low_U50  = new_cases_adjusted_mean_low,
                  new_cases_adjusted_mean_high_U50 = new_cases_adjusted_mean_high,
                  new_cases_adjusted_mean_mid_U80  = new_cases_adjusted_mean_mid,
                  new_cases_adjusted_mean_low_U80  = new_cases_adjusted_mean_low,
                  new_cases_adjusted_mean_high_U80 = new_cases_adjusted_mean_high)

incidence_data_sensitivity <- incidence_data_sensitivity_tmp_1 %>%
    dplyr::bind_rows(incidence_data_sensitivity_tmp_2) 

imported_cases_and_incidence_together_sensitivity <- imported_cases_sensitivity %>%
    dplyr::left_join(incidence_data_sensitivity) %>%
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

imported_cases_and_incidence_together_sensitivity_labels <- imported_cases_and_incidence_together_sensitivity %>%
    dplyr::rename(imported_cases_scenario_2_low = imported_cases_scenario_U50_low, 
                  imported_cases_scenario_2_mid = imported_cases_scenario_U50_mid,
                  imported_cases_scenario_2_high = imported_cases_scenario_U50_high,
                  imported_cases_scenario_3_low = imported_cases_scenario_U80_low, 
                  imported_cases_scenario_3_mid = imported_cases_scenario_U80_mid,
                  imported_cases_scenario_3_high = imported_cases_scenario_U80_high) %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(iso_code = destination_country_iso_code) 


scenario_A_labels <- imported_cases_and_incidence_together_labels %>%
    dplyr::select(iso_code, destination_country, imported_cases_scenario_1_mid,
                  imported_cases_scenario_1_low, imported_cases_scenario_1_high)


imported_cases_and_incidence_together_sensitivity_labels_all <- imported_cases_and_incidence_together_sensitivity_labels %>%
    dplyr::left_join(scenario_A_labels) %>% 
    dplyr::select(iso_code, destination_country,  imported_cases_scenario_1_low, 
                  imported_cases_scenario_1_mid,  imported_cases_scenario_1_high,
                  imported_cases_scenario_2_low,  imported_cases_scenario_2_mid,
                  imported_cases_scenario_2_high, imported_cases_scenario_3_low, 
                  imported_cases_scenario_3_mid,  imported_cases_scenario_3_high)

list(`SI` = list(name = "SI",
                 scenarios = c("low", "mid", "high"))) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs", "figure_S5.pdf"),
                         plot = mapPlottingFunction(
                             imported_cases_and_incidence_together_sensitivity_labels_all,
                             scenarios = .x$scenarios,
                             sensitivity = TRUE),
                         device = "pdf",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))


list(`SI` = list(name = "SI",
                 scenarios = c("low", "mid", "high"))) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs", "figure_S5.png"),
                         plot = mapPlottingFunction(
                             imported_cases_and_incidence_together_sensitivity_labels_all,
                             scenarios = .x$scenarios,
                             sensitivity = TRUE),
                         device = "png",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))

