library(here)
setwd(here::here())

source(here("R","packages.R"))
source(here("R","flight_data_cleaning_utils.R"))
source(here("R","data_helper_functions.R"))
source(here("R","plotting_helper_functions.R"))

#--- reading in the cleaned data files
total_flights_may_2019_2020 <- 
    readr::read_csv(here("data", "flight_reduction_scaling_factors_may.csv")) %>%
    dplyr::select(-X1)

#--- flight data imported below is not in repo, as its not public
oag_traveller_data_may_2020 <- 
    readr::read_csv(here("data", "raw_data/oag_data_may_2019"))

#--- computing reduction in flights scaling factor for all pairs of countries
may_travel_data <- oag_traveller_data_may_2020 %>%
    dplyr::full_join(total_flights_may_2019_2020,
                     by = c("origin_country_iso_code", "destination_country_iso_code")) %>%
    dplyr::mutate(scaling_factor = 
                      if_else(is.na(scaling_factor), 0.372, scaling_factor)) %>%
    dplyr::mutate(origin_country      =  
                      countrycode::countrycode(origin_country_iso_code, "iso3c", "country.name"),
                  destination_country = 
                      countrycode::countrycode(destination_country_iso_code, "iso3c", "country.name")) %>%
    dplyr::mutate(scaled_travellers = total_passengers*scaling_factor) %>%
    dplyr::select(origin_country, destination_country,
                  origin_country_iso_code, destination_country_iso_code,
                  total_passengers, scaling_factor, scaled_travellers)
#--- 0.372 is the mean of the existing scaling factors. We use this in the absence of data for countries without
#--- estimates in the OpenSky dataset


#--- using the scaled traveller numbers and prevalence to calculate expected number of imported cases
#--- from all origin countries, to all destination countries
#prevalence_data <- globalPrevalenceEstimates()

ecdc_data <- ecdc_data_function()
under_reporting_data <- under_reporting_data_function(ecdc_data)

june_dates <- seq(as.Date("2020-06-01"), as.Date("2020-06-30"), by="days")
july_dates <- seq(as.Date("2020-07-01"), as.Date("2020-07-31"), by="days")

#--- calculating prevalence time series for each country
prevalence_june_all <- june_dates %>%
    purrr::map(
        ~global_prevalence_estimates_function(ecdc_data, under_reporting_data, .x)) 

prevalence_july_all <- july_dates %>%
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

prevalence_july <- prevalence_july_all %>%
    dplyr::bind_rows(.id = "country") %>%
    dplyr::group_by(iso_code) %>%
    dplyr::summarise(prevalence_mid = mean(prevalence_mid/9),
                     prevalence_low = mean(prevalence_low/9),
                     prevalence_high = mean(prevalence_high/9)) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", 'country.name', 
                                                     custom_match = c('RKS' = 'Kosovo',
                                                                      'XKX' = 'Kosovo'))) %>%
    dplyr::select(origin_country_iso_code = iso_code, country, prevalence_mid, prevalence_low, prevalence_high)

# if we drop NAs we lose Kosovo

asymptomatic_prop_mid     <- 0.5
asymptomatic_prop_low     <- 0.1
asymptomatic_prop_high    <- 0.7
traveller_reduction_1     <- 0.75
traveller_reduction_2     <- 0.50
under_ascertainment_estimate_1 <- 0.5
under_ascertainment_estimate_2 <- 0.2
under_ascertainment_estimate_3 <- 0.1

#--- DEPRACTED 
#traveller_reduction_2 <- 0.75

imported_cases_pre_sum <- may_travel_data %>%
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
        expected_imported_cases_scenario_2_mid   = scaled_travellers*prevalence_mid*traveller_reduction_1,
        expected_imported_cases_scenario_2_low   = scaled_travellers*prevalence_low*traveller_reduction_1,
        expected_imported_cases_scenario_2_high  = scaled_travellers*prevalence_high*traveller_reduction_1,
        expected_imported_cases_scenario_3_mid   = scaled_travellers*prevalence_mid*traveller_reduction_2,
        expected_imported_cases_scenario_3_low   = scaled_travellers*prevalence_low*traveller_reduction_2,
        expected_imported_cases_scenario_3_high  = scaled_travellers*prevalence_high*traveller_reduction_2)
# expected_imported_cases_scenario_4_mid   = total_passengers*prevalence_mid*traveller_reduction_2,
# expected_imported_cases_scenario_4_low   = total_passengers*prevalence_low*traveller_reduction_2,
# expected_imported_cases_scenario_4_high  = total_passengers*prevalence_high*traveller_reduction_2) 

#--- removing unnecessary columns for smooth joining
imported_cases_pre_sum_reduced <- imported_cases_pre_sum %>% 
    dplyr::select(origin_country_iso_code,
                  destination_country_iso_code,
                  expected_imported_cases_scenario_1_mid,
                  expected_imported_cases_scenario_1_low,
                  expected_imported_cases_scenario_1_high,
                  expected_imported_cases_scenario_2_mid,
                  expected_imported_cases_scenario_2_low,
                  expected_imported_cases_scenario_2_high,
                  expected_imported_cases_scenario_3_mid,
                  expected_imported_cases_scenario_3_low,
                  expected_imported_cases_scenario_3_high)

#--- adding in new version of scenario 4, using the 2020 dataset

oag_april_2020_scaled <- oag_2020_april_data_function()

oag_april_2020_scaled_neat <- oag_april_2020_scaled %>% 
    dplyr::select(origin_country,
                  destination_country, 
                  origin_country_iso_code = iso_code_dep,
                  destination_country_iso_code = iso_code_arr,
                  total_passengers = total_travellers, scaled_travellers)

imported_cases_pre_sum_2020 <- oag_april_2020_scaled_neat %>%
    dplyr::left_join(prevalence_july) %>%
    dplyr::select(origin_country, destination_country, origin_country_iso_code, 
                  destination_country_iso_code, total_passengers, scaled_travellers, 
                  prevalence_mid, prevalence_low, prevalence_high) %>%
    dplyr::group_by(destination_country_iso_code,
                    destination_country) %>%
    dplyr::mutate(
        expected_imported_cases_scenario_4_mid   = total_passengers*prevalence_mid,
        expected_imported_cases_scenario_4_low   = total_passengers*prevalence_low,
        expected_imported_cases_scenario_4_high  = total_passengers*prevalence_high,
        scaled_travellers = NA)

#--- removing unnecessary columns for smooth joining
imported_cases_pre_sum_2020_reduced <- imported_cases_pre_sum_2020 %>% 
    dplyr::select(origin_country_iso_code,
                  destination_country_iso_code,
                  expected_imported_cases_scenario_4_mid,
                  expected_imported_cases_scenario_4_low,
                  expected_imported_cases_scenario_4_high)


#--- joining first 3 scenarios with 4th scenario (where we use 2020 data directly)
imported_cases_pre_sum_together <- imported_cases_pre_sum_reduced %>%
    dplyr::left_join(imported_cases_pre_sum_2020_reduced)

imported_cases <- imported_cases_pre_sum_together %>%
    dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")),
                        .funs = function(x){sum(x, na.rm = T)/30}) %>%
    dplyr::arrange(destination_country)

# calculating the incidence in each destination country
adjusted_case_data       <- get_adjusted_case_data_national()
incidence_data_country_B <-  adjusted_case_data %>%
    dplyr::group_by(iso_code) %>%
    dplyr::filter(date >= "2020-07-01" & date <= "2020-07-31") %>%
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

#--- reordering the scenarios (swapping A and D) for both numerical and label tibbles
#--- there is probably a cleverer way of doing this but this works

imported_cases_reordered <- imported_cases %>%
    dplyr::rename(old_1_mid  = expected_imported_cases_scenario_1_mid,
                  old_1_low  = expected_imported_cases_scenario_1_low,
                  old_1_high = expected_imported_cases_scenario_1_high,
                  old_4_mid  = expected_imported_cases_scenario_4_mid,
                  old_4_low  = expected_imported_cases_scenario_4_low,
                  old_4_high = expected_imported_cases_scenario_4_high) %>%
    dplyr::rename(expected_imported_cases_scenario_1_mid  = old_4_mid,
                  expected_imported_cases_scenario_1_low  = old_4_low,
                  expected_imported_cases_scenario_1_high = old_4_high,
                  expected_imported_cases_scenario_4_mid  = old_1_mid,
                  expected_imported_cases_scenario_4_low  = old_1_low,
                  expected_imported_cases_scenario_4_high = old_1_high) %>% 
    dplyr::select(imported_cases %>% colnames())

imported_cases_and_incidence_together <- imported_cases_reordered %>%
    dplyr::left_join(incidence_data_country_B) %>%
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

imported_cases_and_incidence_together_labels <- 
    imported_cases_and_incidence_together %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(iso_code = destination_country_iso_code) 

imported_cases_and_incidence_together_labels_final <- imported_cases_and_incidence_together_labels %>%
    dplyr::select(iso_code, destination_country,
                  imported_cases_scenario_1_mid  = imported_cases_scenario_2_mid,
                  imported_cases_scenario_1_low  = imported_cases_scenario_2_low, 
                  imported_cases_scenario_1_high = imported_cases_scenario_2_high, 
                  imported_cases_scenario_2_mid  = imported_cases_scenario_3_mid,
                  imported_cases_scenario_2_low  = imported_cases_scenario_3_low,
                  imported_cases_scenario_2_high = imported_cases_scenario_3_high)

#--- making figure 1 - map of risk of imported cases
list(`main`    = list(name = "main",
                      scenarios = c("mid", "low", "high"))) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs",
                                  "figure_S9.pdf"),
                         plot = mapPlottingFunctionTravelVolumeSensitivity(
                             imported_cases_and_incidence_together_labels_final,
                             scenarios = .x$scenarios),
                         device = "pdf",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))


#--- bar plot of required reduction in flights
#--- have changed the relevant scenario to scenario 1, as this is now the most plausible
required_reduction <- imported_cases_and_incidence_together %>% 
    inner_join(imported_cases_reordered) %>%
    dplyr::select(iso_code = destination_country_iso_code,
                  starts_with("expected_imported_cases_scenario_1"),
                  starts_with("new_cases_adjusted_mean"), 
                  starts_with("imported_cases_scenario_1")) %>%
    rename_at(.vars = vars(starts_with("imported_cases_scenario_1")),
              .funs = ~sub(pattern = "imported_cases_scenario_1",
                           replacement = "importation_per_incidence",
                           x = .)) %>%
    dplyr::filter(importation_per_incidence_mid > 0.01) %>%
    dplyr::mutate(
        required_reduction_in_passengers_low = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_1_low),
        required_reduction_in_passengers_mid = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_1_mid),
        required_reduction_in_passengers_high = 
            (1 - 0.01*new_cases_adjusted_mean_mid/expected_imported_cases_scenario_1_high)) %>%
    dplyr::mutate_at(.vars = vars(starts_with("required")),
                     .funs = function(x){pmin(1,pmax(0,x))}) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "iso.name.en"))


figure_2_data <- barDataFunction(required_reduction)
figure_2      <- barPlottingFunction(figure_2_data, interval = TRUE)

ggplot2::ggsave(here("outputs","figure_2.png"),
                figure_2,
                width = 9, 
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

ggplot2::ggsave(here("outputs","figure_3.png"),
                figure_3, #+ facet_grid(importation_per_incidence_mid_label ~ region),
                width = 9, 
                height = 6)


figure_2_data  %>% scatterTableFunction %>%
    write_csv(here("outputs", "table_fig_3.csv"))

tmp <- readr::read_csv("outputs/table_fig_3.csv")

source(here("scripts","arbitrary_thresholds_figure.R"))
source(here("scripts","importation_risks.R"))

#--- sensitivity analysis, scaling all LMIC prevalence data up by 50%, 80% and 90%

list_of_lmic_iso_codes <- c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda",
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


lmics <- dplyr::tibble(country_name = list_of_lmic_iso_codes) %>%
    dplyr::mutate(iso_code = countrycode::countrycode(country_name, "country.name", "iso3c", custom_match = c("Kosovo" = "RKS",
                                                                                                              "Micronesia" = "FSM")))

lmics_iso_code_list <- lmics %>%
    dplyr::pull(iso_code)

#--- scaling down expected imported cases by under-ascertainment estimates
imported_cases_and_incidence_together_sensitivity <- imported_cases_and_incidence_together %>%
    dplyr::mutate(imported_cases_scenario_1_mid = 
                      dplyr::case_when(
                          destination_country_iso_code %in% lmics_iso_code_list ~ imported_cases_scenario_1_mid/under_ascertainment_estimate_1,
                          !(destination_country_iso_code %in% lmics_iso_code_list) ~ imported_cases_scenario_1_mid),
                  imported_cases_scenario_1_low = 
                      dplyr::case_when(
                          destination_country_iso_code %in% lmics_iso_code_list ~ imported_cases_scenario_1_low/under_ascertainment_estimate_1,
                          !(destination_country_iso_code %in% lmics_iso_code_list) ~ imported_cases_scenario_1_low),
                  imported_cases_scenario_1_high = 
                      dplyr::case_when(
                          destination_country_iso_code %in% lmics_iso_code_list ~ imported_cases_scenario_1_high/under_ascertainment_estimate_1,
                          !(destination_country_iso_code %in% lmics_iso_code_list) ~ imported_cases_scenario_1_high),
                  imported_cases_scenario_2_mid = 
                      dplyr::case_when(
                          destination_country_iso_code %in% lmics_iso_code_list ~ imported_cases_scenario_2_mid/under_ascertainment_estimate_2,
                          !(destination_country_iso_code %in% lmics_iso_code_list) ~ imported_cases_scenario_2_mid),
                  imported_cases_scenario_2_low = 
                      dplyr::case_when(
                          destination_country_iso_code %in% lmics_iso_code_list ~ imported_cases_scenario_2_low/under_ascertainment_estimate_2,
                          !(destination_country_iso_code %in% lmics_iso_code_list) ~ imported_cases_scenario_2_low),
                  imported_cases_scenario_2_high = 
                      dplyr::case_when(
                          destination_country_iso_code %in% lmics_iso_code_list ~ imported_cases_scenario_2_high/under_ascertainment_estimate_2,
                          !(destination_country_iso_code %in% lmics_iso_code_list) ~ imported_cases_scenario_2_high))

imported_cases_and_incidence_together_sensitivity <- imported_cases_and_incidence_together_sensitivity %>%
    dplyr::select(-imported_cases_scenario_3_mid, -imported_cases_scenario_3_low, -imported_cases_scenario_3_high,
                  -imported_cases_scenario_4_mid, -imported_cases_scenario_4_low, -imported_cases_scenario_4_high)

imported_cases_and_incidence_together_labels_sensitivity <- 
    imported_cases_and_incidence_together_sensitivity %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(iso_code = destination_country_iso_code) 

#--- making figures S5 for sensitivity analysis


list(`SI` = list(name = "SI",
                 scenarios = c("low", "mid", "high"))) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs", "figure_S5.png"),
                         plot = mapPlottingFunction(
                             imported_cases_and_incidence_together_labels_sensitivity,
                             scenarios = .x$scenarios,
                             sensitivity = TRUE),
                         device = "png",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))

