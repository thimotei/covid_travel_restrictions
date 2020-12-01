source(here::here("scripts/main_script_computing_all_results.R"))

#-------------------- PLOTTING RESULTS --------------------#
#--- making figure 2 - map of risk of imported cases
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

list(`SI_low`  = list(name = "SI_low",
                      scenarios = "low"),
     `SI_high` = list(name = "SI_high",
                      scenarios = "high"),
     `main`    = list(name = "main",
                      scenarios = "mid")) %>%
    purrr::map(
        ~ggplot2::ggsave(filename = 
                             here("outputs",
                                  paste("figure_2_",.x$name,".png",sep="")),
                         plot = mapPlottingFunction(
                             imported_cases_and_incidence_together_labels,
                             scenarios = .x$scenarios),
                         device = "png",
                         width = 16, 
                         height = 8, units = "in", dpi = 300))



#--- bar plot of required reduction in flights
#--- have changed the relevant scenario to scenario 4, as this is now the most plausible
figure_3 <- barPlottingFunction(figure_3_data, interval = TRUE)

ggplot2::ggsave(here("outputs","figure_3.pdf"),
                figure_3,
                device = "pdf",
                width = 12, 
                height = 4.5)

ggplot2::ggsave(here("outputs","figure_3.png"),
                figure_3,
                device = "png",
                width = 12, 
                height = 4.5)

#--- SUPPLEMENTARY FIGURES (unless they are built in a standalone script, i.e. the LMIC figures, traveller numbers sensitivity and arbitrary thresholds)
#--- plotting figure S1
figure_S1 <- scatterPlottingFunction(figure_S1_data, interval = FALSE)

ggplot2::ggsave(here("outputs","figure_S1.pdf"),
                figure_S1,
                device = "pdf",
                width = 9, 
                height = 6)

ggplot2::ggsave(here("outputs","figure_S1.png"),
                figure_S1,
                device = "png",
                width = 9, 
                height = 6)

#--- figures S2 and S3 are saved at the same time Figure 2 is (although they are named "SI_high" and "SI_low" by default)

#--- plotting figure S4
#--- if no results have been run yet, source the two scripts below
source("scripts/main_script_computing_all_results.R")

world <- make_world() %>%
    mutate(origin_country_iso_code = iso_a3)


importation_destinations <- figure_3_data %>% # get the data for the countries of interest
    filter(importation_per_incidence_mid >= 0.1) %>% # only want countries in Red
    select(destination_country_iso_code = iso_code) # set iso code for DESTINATION

importation_map <- tidyr::crossing(importation_destinations,
                                   origin_country_iso_code = world$iso_a3) %>%
    left_join(select(world, geometry, origin_country_iso_code = iso_a3))

importations_mask <- inner_join(world %>%
                                    mutate(destination_country_iso_code = iso_a3),
                                importation_destinations)  %>%
    mutate(countrylabel = countrycode::countrycode(destination_country_iso_code,
                                                   origin = "iso3c",
                                                   destination = "country.name"))

importations <- 
    dplyr::select(ungroup(imported_cases_september_pre_sum),
                  origin_country_iso_code,
                  destination_country_iso_code,
                  value = expected_imported_cases_scenario_4_mid) %>%
    {left_join(importation_map, .)} %>%
    mutate(value = ifelse(origin_country_iso_code == destination_country_iso_code,
                          NA,
                          value)) %>%
    group_by(destination_country_iso_code) %>%
    mutate(p = value/sum(value, na.rm=T)) %>%
    mutate(value = pmax(0.0000003399664, value)) %>%
    
    mutate(countrylabel = countrycode::countrycode(destination_country_iso_code,
                                                   origin = "iso3c",
                                                   destination = "country.name"))

# mutate(value = ifelse(destination_country_iso_code == origin_country_iso_code,
#                       NA,
#                       value)) %>%
importations_plot <-
    ggplot(data=importations) + 
    geom_sf(aes(geometry = geometry,
                fill = pmin(p,0.5)),
            size = 0.1) +
    facet_wrap(~countrylabel, ncol = 4) +
    theme_map(world = TRUE) +
    scale_fill_gradient(low = "white", 
                        high = covid_pal["Red"],
                        na.value = covid_pal["No data"],
                        limits = c(0,0.5),
                        breaks = seq(0,0.5,by=0.1),
                        labels = function(x){ifelse(x == 0.5, "0.5+",x)},
                        name = "Proportion of imported cases in destination from origin") + #,
    #limits = c(0.0001, NA),
    #trans = "log10") +
    geom_sf(data = importations_mask,
            aes(geometry = geometry),
            fill = "#FDB863",
            color = NA,
            size = 0.1) + 
    guides(fill = guide_colourbar(barwidth = 10,
                                  title.vjust = 0.75,
                                  title.hjust = 1,
                                  frame.colour = "black",
                                  frame.linewidth = 1))

ggsave(plot = importations_plot, device = "pdf", 
       filename = here("outputs",paste0("figure_S4.pdf")),  
       width = 15, height = 20, units = "in", dpi = 600)

ggsave(plot = importations_plot, device = "png", 
       filename = here("outputs",paste0("figure_S4.png")),  
       width = 15, height = 20, units = "in", dpi = 600)

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
