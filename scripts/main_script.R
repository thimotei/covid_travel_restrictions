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
  dplyr::select(origin_country, destination_country, origin_country_iso_code, 
                destination_country_iso_code, total_passengers, scaled_travellers, prevalence) %>%
  dplyr::group_by(destination_country_iso_code,
                  destination_country) %>%
  dplyr::mutate(expected_imported_cases_scenario_1  = total_passengers*prevalence,
                expected_imported_cases_scenario_2  = scaled_travellers*prevalence,
                expected_imported_cases_scenario_3  = total_passengers*prevalence*traveller_reduction_1,
                expected_imported_cases_scenario_4  = total_passengers*prevalence*traveller_reduction_2) %>%
  dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")),
                      .funs = function(x){sum(x, na.rm=T)/30})

# calculating the incidence in each destination country
incidence_data_country_B <-  getAdjustedCaseDataNational() %>%
  dplyr::group_by(iso_code) %>%
  dplyr::filter(date > "2020-05-26" & date < "2020-06-26") %>%
  dplyr::mutate(incidence_estimate = new_cases_adjusted_mid/(1 - asymptomatic_prop)) %>%
  dplyr::summarise(new_cases_adjusted_mean = mean(incidence_estimate)) %>%
  dplyr::mutate(destination_country_iso_code = iso_code) %>%
  dplyr::select(destination_country_iso_code, new_cases_adjusted_mean)

imported_cases_and_incidence_together <- imported_cases %>%
  dplyr::left_join(incidence_data_country_B) %>%
  dplyr::rename_at(.vars = vars(starts_with("expected")), 
                   .funs = function(x){sub(pattern = "expected_", replacement = "", x)}) %>%
  dplyr::mutate_at(.vars = vars(starts_with("imported")),
                   .funs = list(~dplyr::na_if(./new_cases_adjusted_mean, "Inf"))) %>%
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


#--- making figure 1 - map of risk of imported cases
p_together <- mapPlottingFunction(imported_cases_and_incidence_together_labels)

ggplot2::ggsave(here("outputs","figure_1.png"),
                p_together,
                width = 16, 
                height = 8, units = "in", dpi = 300)


#--- bar plot of required reduction in flights
required_reduction <- imported_cases_and_incidence_together %>% 
  inner_join(imported_cases) %>%
  dplyr::select(iso_code = destination_country_iso_code,
                expected_imported_cases_scenario_2,
                new_cases_adjusted_mean, 
                importation_per_incidence = imported_cases_scenario_2) %>%
  dplyr::distinct() %>% 
  dplyr::filter(importation_per_incidence > 0.01) %>%
  dplyr::mutate(required_reduction_in_passengers = new_cases_adjusted_mean/expected_imported_cases_scenario_2*0.01) %>% 
  dplyr::mutate(required_reduction_in_passengers = (1 - required_reduction_in_passengers)) %>%
  dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", "iso.name.en"))


figure_2_data <- barDataFunction(required_reduction)
figure_2      <- barPlottingFunction(figure_2_data)
  
ggplot2::ggsave(here("outputs","figure_2.png"),
                figure_2,
                width = 9, 
                height = 6)


figure_3_data <-
  figure_2_data %>%
  mutate(label = cut(importation_per_incidence, breaks = c(0, 0.01, 0.1, 1),
                     include.lowest = T, 
                     labels = c("Green",
                                "Amber",
                                "Red"))) %>%
  mutate(importation_per_incidence_trim =
           pmin(0.995,pmax(0.005,importation_per_incidence)))


figure_3 <- scatterPlottingFunction(figure_3_data)
  
ggplot2::ggsave(here("outputs","figure_3.png"),
                figure_3,
                width = 9, 
                height = 6)

