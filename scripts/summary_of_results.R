#--- Lower than 1%

#--- Scenario 1

median_scenario_1 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_1_mid < 0.01) %>%
    nrow()

low_scenario_1 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_1_low < 0.01) %>%
    nrow()

high_scenario_1 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_1_high < 0.01) %>%
    nrow()

scenario_1_green <- paste0(median_scenario_1, " (", low_scenario_1, " - ", high_scenario_1, ")")

#--- Scenario 2

median_scenario_2 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_2_mid < 0.01) %>%
    nrow()

low_scenario_2 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_2_low < 0.01) %>%
    nrow()

high_scenario_2 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_2_high < 0.01) %>%
    nrow()

scenario_2_green <- paste0(median_scenario_2, " (", low_scenario_2, " - ", high_scenario_2, ")")

#--- Scenario 3

median_scenario_3 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_3_mid < 0.01) %>%
    nrow()

low_scenario_3 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_3_low < 0.01) %>%
    nrow()

high_scenario_3 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_3_high < 0.01) %>%
    nrow()

scenario_3_green <- paste0(median_scenario_3, " (", low_scenario_3, " - ", high_scenario_3, ")")


#--- Scenario 4

median_scenario_4 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_4_mid < 0.01) %>%
    nrow()

low_scenario_4 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_4_low < 0.01) %>%
    nrow()

high_scenario_4 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(imported_cases_scenario_4_high < 0.01) %>%
    nrow()

scenario_4_green <- paste0(median_scenario_4, " (", low_scenario_4, " - ", high_scenario_4, ")")

#--- Between 1% and 10%

#--- Scenario 1

median_scenario_1_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_1_mid & imported_cases_scenario_1_mid <= 0.1) %>%
    nrow()

low_scenario_1_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_1_low & imported_cases_scenario_1_low <= 0.1) %>%
    nrow()

high_scenario_1_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_1_high & imported_cases_scenario_1_high <= 0.1) %>%
    nrow()

scenario_1_amber <- paste0(median_scenario_1_1_10, " (", low_scenario_1_1_10, " - ", high_scenario_1_1_10, ")")

#--- Scenario 2

median_scenario_2_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_2_mid & imported_cases_scenario_2_mid <= 0.1) %>%
    nrow()

low_scenario_2_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_2_low & imported_cases_scenario_2_low <= 0.1) %>%
    nrow()

high_scenario_2_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_2_high & imported_cases_scenario_2_high <= 0.1) %>%
    nrow()

scenario_2_amber <-paste0(median_scenario_2_1_10, " (", low_scenario_2_1_10, " - ", high_scenario_2_1_10, ")")

#--- Scenario 3

median_scenario_3_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_3_mid & imported_cases_scenario_3_mid <= 0.1) %>%
    nrow()

low_scenario_3_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_3_low & imported_cases_scenario_3_low <= 0.1) %>%
    nrow()

high_scenario_3_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_3_high & imported_cases_scenario_3_high <= 0.1) %>%
    nrow()

scenario_3_amber <-paste0(median_scenario_3_1_10, " (", low_scenario_3_1_10, " - ", high_scenario_3_1_10, ")")


#--- Scenario 4

median_scenario_4_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_4_mid & imported_cases_scenario_4_mid <= 0.1) %>%
    nrow()

low_scenario_4_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_4_low & imported_cases_scenario_4_low <= 0.1) %>%
    nrow()

high_scenario_4_1_10 <- imported_cases_and_incidence_together_september %>%
    dplyr::left_join(imported_cases_and_incidence_together_may, by = "destination_country_iso_code") %>%
    dplyr::filter(0.01 <= imported_cases_scenario_4_high & imported_cases_scenario_4_high <= 0.1) %>%
    nrow()

scenario_4_amber <- paste0(median_scenario_4_1_10, " (", low_scenario_4_1_10, " - ", high_scenario_4_1_10, ")")

#--- all results together

scenario_1_green_plus_amber <- paste0(median_scenario_1 + median_scenario_1_1_10, " (",
                                      low_scenario_1 + low_scenario_1_1_10, " - ",
                                      high_scenario_1 + high_scenario_1_1_10, ")")


scenario_2_green_plus_amber <- paste0(median_scenario_2 + median_scenario_2_1_10, " (", 
                                      low_scenario_2 + low_scenario_2_1_10, " - ", 
                                      high_scenario_2 + high_scenario_2_1_10, ")")

scenario_3_green_plus_amber <- paste0(median_scenario_3 + median_scenario_3_1_10, " (", 
                                      low_scenario_3 + low_scenario_3_1_10, " - ", 
                                      high_scenario_3 + high_scenario_3_1_10, ")")

scenario_4_green_plus_amber <- paste0(median_scenario_4 + median_scenario_4_1_10, " (", 
                                      low_scenario_4 + low_scenario_4_1_10, " - ", 
                                      high_scenario_4 + high_scenario_4_1_10, ")")

total_countries_may_scenarios  <- imported_cases_and_incidence_together_may %>% nrow()
total_countries_sept_scenarios <- imported_cases_and_incidence_together_september %>% nrow()

scenario_1_red <- paste0(total_countries_may_scenarios - (median_scenario_1 + median_scenario_1_1_10), " (", 
                                             total_countries_may_scenarios - (low_scenario_1 + low_scenario_1_1_10), " - ",
                                             total_countries_may_scenarios - (high_scenario_1 + low_scenario_1_1_10), ")")

scenario_2_red <- paste0(total_countries_may_scenarios - (median_scenario_2 + median_scenario_2_1_10), " (", 
                                             total_countries_may_scenarios - (low_scenario_2 + low_scenario_2_1_10), " - ",
                                             total_countries_may_scenarios - (high_scenario_2 + low_scenario_2_1_10), ")")

scenario_3_red <- paste0(total_countries_sept_scenarios - (median_scenario_3 + median_scenario_3_1_10), " (", 
                         total_countries_sept_scenarios - (low_scenario_3 + low_scenario_3_1_10), " - ",
                         total_countries_sept_scenarios - (high_scenario_3 + low_scenario_3_1_10), ")")

scenario_4_red <- paste0(total_countries_sept_scenarios - (median_scenario_4 + median_scenario_4_1_10), " (", 
                         total_countries_sept_scenarios - (low_scenario_4 + low_scenario_4_1_10), " - ",
                         total_countries_sept_scenarios - (high_scenario_4 + low_scenario_4_1_10), ")")

dplyr::tibble(scenario = c(1,2,3,4),
              green = c(scenario_1_green, scenario_2_green, scenario_3_green, scenario_4_green),
              amber = c(scenario_1_amber, scenario_2_amber, scenario_3_amber, scenario_4_amber),
              green_plus_amber = c(scenario_1_green_plus_amber, scenario_2_green_plus_amber,
                                   scenario_3_green_plus_amber, scenario_4_green_plus_amber),
              red = c(scenario_1_red, scenario_2_red, scenario_3_red, scenario_4_red))


 