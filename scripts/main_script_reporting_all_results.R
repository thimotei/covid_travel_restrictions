source(here::here("scripts/main_script_computing_all_results.R"))

#--- loading in Rt estimates for results with Rt conditions
rt_estimates_raw <- readr::read_csv("data/rt_estimates_clean.csv")
rt_estimates <- rt_estimates_raw %>%
    dplyr::mutate(rt_estimate = paste0(median, " (", lower_90, "-", upper_90, ")")) %>%
    dplyr::select(iso_code, rt_estimate) 


#--- Summary of results, quoted in the main text, including Rt condition
#--- performing China and New Zealand case studies, results quoted
#--- in the Discussion of the main text

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

#--- Rt estimate conditions
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
