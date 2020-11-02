#--- inclusion based on Rt estimates appendix section ---#
#--- RUN MAIN SCRIPT UP UNTIL PLOTTING SECTION, AS THIS SCRIPT
#--- REQUIRES THE SAME ESTIMATES

rt_estimates_raw <- readr::read_csv("data/rt_estimates_clean.csv")
rt_estimates <- rt_estimates_raw %>%
    dplyr::mutate(rt_estimate = paste0(median, " (", lower_90, "-", upper_90, ")")) %>%
    dplyr::select(iso_code, rt_estimate) 

rt_estimates_tipping_point_countries <- rt_estimates_raw %>%
    dplyr::filter(0.95 <= median & median <= 1.05) %>%
    dplyr::pull(iso_code)

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

countries_to_include <- rt_estimates_raw %>% 
    dplyr::filter(0.95 <= median & median <= 1) %>%
    dplyr::pull(iso_code)

figure_2_data <- barDataFunction(required_reduction)  %>%
    dplyr::filter(iso_code %in% countries_to_include) %>%
    dplyr::mutate(country = plyr::revalue(country, c("Congo (the Democratic Republic of the)" = "DRC",
                                                     "Bahamas (the)" = "Bahamas",
                                                     "Sudan (the)"   = "Sudan",
                                                     "Sint Maarten (Dutch part)" = "Sint Maarten",
                                                     "Netherlands (the)" = "Netherlands",
                                                     "Niger (the)" = "Niger",
                                                     "Virgin Islands (U.S.)" = "Virgin Islands")))

figure_2 <- barPlottingFunction(figure_2_data, interval = TRUE)

ggplot2::ggsave(here("outputs","figure_S7.pdf"),
                figure_2,
                device = "pdf",
                width = 6, 
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

ggplot2::ggsave(here("outputs","figure_S8.pdf"),
                figure_3, #+ facet_grid(importation_per_incidence_mid_label ~ region),
                device = "pdf",
                width = 9, 
                height = 6)

