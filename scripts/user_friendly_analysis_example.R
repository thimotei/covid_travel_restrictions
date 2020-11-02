library(here)
setwd(here::here())

source(here("R","packages.R"))
source(here("R","flight_data_cleaning_utils.R"))
source(here("R","data_helper_functions.R"))
source(here("R","plotting_helper_functions.R"))

#---- EXAMPLE DATA -----#
#--- USER-SPECIFIC DATA MUST OF OF THE SAME FORMAT AS THIS
flight_data_example <- dplyr::tibble(
    origin_country_iso_code = c("GBR", "GBR", "BRA", "BRA", "USA", "USA"),
    destination_country_iso_code = c("USA", "BRA", "GBR", "USA", "GBR", "BRA"),
    month = 9,
    bookings = c(1000, 2000, 1500, 2000, 2500, 3000))

prevalence_estimates_example <- dplyr::tibble(
    origin_country_iso_code = c("GBR", "USA", "BRA"), 
    prevalence_mid  = c(0.01, 0.02, 0.015),
    prevalence_low  = c(0.001, 0.002, 0.01),
    prevalence_high = c(0.02, 0.04, 0.02)
)

incidence_estimates_example <- dplyr::tibble(
    destination_country_iso_code = c("GBR", "USA", "BRA"), 
    incidence_mid  = c(5000, 10000, 12000),
    incidence_low  = c(3000, 7000, 10000),
    incidence_high = c(8000, 13000, 15000)
)

#--- PREVALENCE AND INCIDENCE ESTIMATES INCLUDED BY DEFAULT (for July)
prevalence_estimates_default <- read.csv("data/september_prevalence_estimates.csv")
incidence_estimates_default  <- read.csv("data/september_incidence_estimates.csv")

#---- PRODUCING MAP
produce_map_of_results(oxford_travel_restrictions_condition = TRUE,
                       plot = TRUE)

#--- returning country-specific dataframe
country_specific_results(country = "United Kingdom")
