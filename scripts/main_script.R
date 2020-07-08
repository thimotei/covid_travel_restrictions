library(here)
here::here() %>% setwd()

source(here("R","flight_data_cleaning_utils.R"))
source(here("R","data_helper_functions.R"))
source(here("R","plotting_helper_functions.R"))

#--- reading in the cleaned data files
total_flights_may_2019_2020 <- 
  readr::read_csv(here("data",
                       "flight_reduction_scaling_factors.csv"))
oag_traveller_data_may_2020 <- 
  readr::read_csv(here("data",
                       "oag_data_may_2019_2020"))

#--- computing reduction in flights scaling factor for all pairs of countries
may_travel_data <- oag_traveller_data_may_2020 %>%
  dplyr::full_join(total_flights_may_2019_2020,
                   by = c("origin_country_iso_code", "destination_country_iso_code")) %>%
  dplyr::mutate(scaling_factor = 
                 if_else(is.na(scaling_factor), 0.3090604, scaling_factor)) %>%
  dplyr::mutate(origin_country      =  
                  countrycode::countrycode(origin_country_iso_code, "iso3c", "country.name"),
                destination_country = 
                  countrycode::countrycode(destination_country_iso_code, "iso3c", "country.name")) %>%
  dplyr::mutate(scaled_travellers = total_passengers*scaling_factor) %>%
  dplyr::select(origin_country, destination_country,
                origin_country_iso_code, destination_country_iso_code,
                total_passengers, scaling_factor, scaled_travellers)
#--- 0.3090604 is the mean of the existing scaling factors. We use this in the absence of data for countries without
#--- estimates in the OpenSky dataset


#--- using the scaled traveller numbers and prevalence to calculate expected number of imported cases
#--- from all origin countries, to all destination countries
prevalence_data_country_A <- globalPrevalenceEstimates() %>%
  dplyr::mutate(iso_code = countrycode::countrycode(country, "country.name", 'iso3c')) %>%
  dplyr::mutate(iso_code_dep = iso_code) %>% 
  dplyr::ungroup(country) %>%
  dplyr::select(origin_country_iso_code = iso_code_dep, 
                country,
                prevalence = propCurrentlyInfMid) 
# if we drop NAs we lose Kosovo

asymptomatic_prop     <- 0.5
traveller_reduction_1 <- 0.75
traveller_reduction_2 <- 0.5

imported_cases <- may_travel_data %>%
  dplyr::left_join(prevalence_data_country_A) %>%
  dplyr::select(origin_country, destination_country, origin_country_iso_code, destination_country_iso_code, total_passengers, scaled_travellers, prevalence) %>%
  dplyr::group_by(destination_country_iso_code) %>%
  dplyr::mutate(expected_imported_cases_scenario_1  = total_passengers*prevalence,
                expected_imported_cases_scenario_2  = scaled_travellers*prevalence,
                expected_imported_cases_scenario_3  = total_passengers*prevalence*traveller_reduction_1,
                expected_imported_cases_scenario_4  = total_passengers*prevalence*traveller_reduction_2)  %>%
  dplyr::summarise(expected_imported_cases_scenario_1 = sum(expected_imported_cases_scenario_1, na.rm = TRUE)/30,
                   expected_imported_cases_scenario_2 = sum(expected_imported_cases_scenario_2, na.rm = TRUE)/30,
                   expected_imported_cases_scenario_3 = sum(expected_imported_cases_scenario_3, na.rm = TRUE)/30,
                   expected_imported_cases_scenario_4 = sum(expected_imported_cases_scenario_4, na.rm = TRUE)/30)

# calculating the incidence in each destination country
incidence_data_country_B <-  getAdjustedCaseDataNational() %>%
  dplyr::group_by(iso_code) %>%
  dplyr::filter(date > "2020-05-26" & date < "2020-06-26") %>%
  dplyr::mutate(incidence_estimate_mid = new_cases_adjusted_mid/(1 - asymptomatic_prop_mid),
                incidence_estimate_low = new_cases_adjusted_low/(1 - asymptomatic_prop_low),
                incidence_estimate_high = new_cases_adjusted_high/(1 - asymptomatic_prop_high)) %>%
  dplyr::summarise(new_cases_adjusted_mean_mid  = mean(incidence_estimate_mid),
                   new_cases_adjusted_mean_low  = mean(incidence_estimate_low),
                   new_cases_adjusted_mean_high = mean(incidence_estimate_high)) %>%
  dplyr::mutate(destination_country_iso_code = iso_code) %>%
  dplyr::select(destination_country_iso_code,
                new_cases_adjusted_mean_mid, 
                new_cases_adjusted_mean_low, 
                new_cases_adjusted_mean_high)

imported_cases_and_incidence_together <- imported_cases %>%
  dplyr::left_join(incidence_data_country_B) %>%
  dplyr::mutate(importation_per_incidence_scenario_1 = dplyr::na_if(expected_imported_cases_scenario_1/new_cases_adjusted_mean, "Inf"),
                importation_per_incidence_scenario_2 = dplyr::na_if(expected_imported_cases_scenario_2/new_cases_adjusted_mean, "Inf"),
                importation_per_incidence_scenario_3 = dplyr::na_if(expected_imported_cases_scenario_3/new_cases_adjusted_mean, "Inf"),
                importation_per_incidence_scenario_4 = dplyr::na_if(expected_imported_cases_scenario_4/new_cases_adjusted_mean, "Inf")) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(importation_per_incidence_scenario_1 = dplyr::case_when(importation_per_incidence_scenario_1 < 0 ~ 0,
                                                                        importation_per_incidence_scenario_1 >  1 ~ 1,
                                                                        importation_per_incidence_scenario_1 >= 0 && importation_per_incidence_scenario_1 <= 1 ~ importation_per_incidence_scenario_1)) %>%
  dplyr::mutate(importation_per_incidence_scenario_2 = dplyr::case_when(importation_per_incidence_scenario_2 < 0 ~ 0,
                                                                        importation_per_incidence_scenario_2 >  1 ~ 1,
                                                                        importation_per_incidence_scenario_2 >= 0 && importation_per_incidence_scenario_2 <= 1 ~ importation_per_incidence_scenario_2)) %>%
  dplyr::mutate(importation_per_incidence_scenario_3 = dplyr::case_when(importation_per_incidence_scenario_3 < 0 ~ 0,
                                                                        importation_per_incidence_scenario_3 >  1 ~ 1,
                                                                        importation_per_incidence_scenario_3 >= 0 && importation_per_incidence_scenario_3 <= 1 ~ importation_per_incidence_scenario_3)) %>%
  dplyr::mutate(importation_per_incidence_scenario_4 = dplyr::case_when(importation_per_incidence_scenario_4 < 0 ~ 0,
                                                                        importation_per_incidence_scenario_4 >  1 ~ 1,
                                                                        importation_per_incidence_scenario_4 >= 0 && importation_per_incidence_scenario_4 <= 1 ~ importation_per_incidence_scenario_4)) %>%
  dplyr::mutate(risk_rating_scenario_1 = dplyr::case_when(importation_per_incidence_scenario_1 <= 0.01 ~ "Green",
                                                          importation_per_incidence_scenario_1 >  0.01 & importation_per_incidence_scenario_1 < 0.1 ~ "Amber",
                                                          importation_per_incidence_scenario_1 >= 0.1 ~ "Red")) %>%
  dplyr::mutate(risk_rating_scenario_2 = dplyr::case_when(importation_per_incidence_scenario_2 <= 0.01 ~ "Green",
                                                          importation_per_incidence_scenario_2 >  0.01 & importation_per_incidence_scenario_2 < 0.1 ~ "Amber",
                                                          importation_per_incidence_scenario_2 >= 0.1 ~ "Red")) %>%
  dplyr::mutate(risk_rating_scenario_3 = dplyr::case_when(importation_per_incidence_scenario_3 <= 0.01 ~ "Green",
                                                          importation_per_incidence_scenario_3 >  0.01 & importation_per_incidence_scenario_3 < 0.1 ~ "Amber",
                                                          importation_per_incidence_scenario_3 >= 0.1 ~ "Red")) %>%
  dplyr::mutate(risk_rating_scenario_4 = dplyr::case_when(importation_per_incidence_scenario_4 <= 0.01 ~ "Green",
                                                          importation_per_incidence_scenario_4 >  0.01 & importation_per_incidence_scenario_4 < 0.1 ~ "Amber",
                                                          importation_per_incidence_scenario_4 >= 0.1 ~ "Red")) %>%
  dplyr::rename(iso_code = destination_country_iso_code) 


#--- making figure 1 - map of risk of imported cases
may_2020_map_data <- combineMapAndIncidenceData(imported_cases_and_incidence_together)

may_2020_map_data_scenario_1 <- may_2020_map_data %>%
  dplyr::select(country, lat, long, group, importation_per_incidence_scenario_1, risk_rating_scenario_1) %>%
  dplyr::rename(importation_per_incidence = importation_per_incidence_scenario_1,
                risk_rating = risk_rating_scenario_1)

may_2020_map_data_scenario_2 <- may_2020_map_data %>%
  dplyr::select(country, lat, long, group, importation_per_incidence_scenario_2, risk_rating_scenario_2) %>%
  dplyr::rename(importation_per_incidence = importation_per_incidence_scenario_2,
                risk_rating = risk_rating_scenario_2)

may_2020_map_data_scenario_3 <- may_2020_map_data %>%
  dplyr::select(country, lat, long, group, importation_per_incidence_scenario_3, risk_rating_scenario_3) %>%
  dplyr::rename(importation_per_incidence = importation_per_incidence_scenario_3,
                risk_rating = risk_rating_scenario_3)

may_2020_map_data_scenario_4 <- may_2020_map_data %>%
  dplyr::select(country, lat, long, group, importation_per_incidence_scenario_4, risk_rating_scenario_4) %>%
  dplyr::rename(importation_per_incidence = importation_per_incidence_scenario_4,
                risk_rating = risk_rating_scenario_4)


p1 <- mapPlottingFunction(may_2020_map_data_scenario_1)
p2 <- mapPlottingFunction(may_2020_map_data_scenario_2)
p3 <- mapPlottingFunction(may_2020_map_data_scenario_3)
p4 <- mapPlottingFunction(may_2020_map_data_scenario_4)

p_together <- ggpubr::ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom", labels = c("A", "B", "C", "D"))
p_together

ggplot2::ggsave("covid_travel_restrictions/figures/figure_1.png",
                p_together,
                width = 16, 
                height = 8)


#--- bar plot of required reduction in flights

required_reduction <- imported_cases_and_incidence_together %>% 
  dplyr::select(iso_code, expected_imported_cases_scenario_2, new_cases_adjusted_mean, importation_per_incidence = importation_per_incidence_scenario_2) %>%
  dplyr::distinct() %>% 
  dplyr::filter(importation_per_incidence > 0.01) %>%
  dplyr::mutate(required_reduction_in_passengers = new_cases_adjusted_mean/expected_imported_cases_scenario_2*0.01) %>% 
  dplyr::mutate(required_reduction_in_passengers = (1 - required_reduction_in_passengers)) %>%
  dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "iso.name.en"))


figure_2 <- required_reduction %>% 
  dplyr::mutate(country = dplyr::case_when(country == "Dominican Republic (the)" ~ "Dominican Republic",
                                           country == "Korea (the Republic of)" ~ "South Korea",
                                           country == "Netherland (the)" ~ "Netherlands",
                                           country == "United Arab Emirates (the)" ~ "UAE",
                                           country == "United States of America (the)" ~ "USA",
                                           country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                           country != "Dominican Republic" || country !="South Korea" || country !="South Korea" || country != "Netherlands" || country != "UAE" || country != "USA" || country != "Venezuela" ~ country)) %>%
  ggplot2::ggplot() + 
  ggplot2::geom_col(ggplot2::aes(x = country, y = required_reduction_in_passengers), fill = "#58508d", alpha = 0.8) + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_continuous(labels = scales::percent) + 
  ggplot2::labs(x = "Country", y = "Required reduction in passengers to achieve Green rating")

ggplot2::ggsave("covid_travel_restrictions/figures/figure_2.png",
                figure_2,
                width = 9, 
                height = 6)


