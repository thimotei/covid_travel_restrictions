source(here::here("scripts/main_script_computing_all_results.R"))

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
#--- have changed the relevant scenario to scenario 4, as this is now the most plausible

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

figure_2_data <- barDataFunction(required_reduction)  %>%
    dplyr::mutate(country = plyr::revalue(country, c("Congo (the Democratic Republic of the)" = "DRC",
                                                     "Bahamas (the)" = "Bahamas",
                                                     "Sudan (the)"   = "Sudan",
                                                     "Sint Maarten (Dutch part)" = "Sint Maarten",
                                                     "Netherlands (the)" = "Netherlands",
                                                     "Niger (the)" = "Niger",
                                                     "Virgin Islands (U.S.)" = "Virgin Islands")))

figure_2 <- barPlottingFunction(figure_2_data, interval = TRUE)

ggplot2::ggsave(here("outputs","figure_3.pdf"),
                figure_2,
                device = "pdf",
                width = 12, 
                height = 4.5)




#--- plotting figure S3 here, as it uses the same data as figure 3

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

ggplot2::ggsave(here("outputs","figure_S.pdf"),
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
