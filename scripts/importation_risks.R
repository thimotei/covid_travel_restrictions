world <- make_world() %>%
    mutate(origin_country_iso_code = iso_a3)


importation_destinations <- figure_2_data %>% # get the data for the countries of interest
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
       filename = here("outputs",paste0("figure_S3.pdf")),  
       width = 30, height = 15, units = "in", dpi = 600)
