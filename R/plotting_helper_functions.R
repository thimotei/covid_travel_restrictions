
covid_pal <- c(magrittr::set_names(x = RColorBrewer::brewer.pal(3, "Purples"),
                                   value = c("Green", "Amber", "Red")),
               "No data" = rgb(red = 1, green = 0.9, blue = 0.9))

theme_map <- function(world = FALSE) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif", color = "#22211d"),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_line(color = "#ebebe5", size = 0),
      panel.grid.major = ggplot2::element_line(color = "#ebebe5", size = 0),
      #plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0,0,0,0), "mm"),
      legend.position = if(world == TRUE){"bottom"}
    )
}

theme_fig2 <- function(world = FALSE) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif", color = "#22211d"),
      #axis.line = ggplot2::element_blank(),
      panel.spacing.x = unit(1, "lines"),
      panel.grid.minor = ggplot2::element_line(color = "#ebebe5", size = 0),
      panel.grid.major.x = ggplot2::element_line(color = "#ebebe5", size = 0),
      #plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
      panel.border = ggplot2::element_blank(),
      #plot.margin = grid::unit(c(0,0,0,0), "mm"),
      legend.position = if(world == TRUE){"bottom"}
    )
}

mapPlottingFunction <- function(x)
{
  
  world <- rnaturalearth::ne_countries(scale = "small", 
                                       returnclass = "sf") %>%
    dplyr::mutate(iso_code = iso_a3) %>%
    #dplyr::rename(country = region) %>%
    #dplyr::mutate(iso_code = countrycode::countrycode(country, 'country.name', 'iso3c')) %>% 
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c",
                                                     destination = 'iso.name.en')) %>%
    dplyr::filter(country != "Antarctica") %>%
    dplyr::left_join(x, by = "iso_code") %>%
    dplyr::select(country, iso_code, geometry)
  
  toPlot <- dplyr::full_join(x, world) %>%
    # dplyr::rename(imported_cases_and_incidence_together,
    #               country = destination_country) %>%
    tidyr::gather(key, value, starts_with("imported")) %>%
    dplyr::mutate(key = readr::parse_number(key),
                  key = LETTERS[key],
                  key = forcats::fct_recode(
                    key,
                    "A: Traveller levels same as May 2019" = "A",
                    "B: Traveller levels scaled down by reductions in OpenSky May 2020" = "B",
                    "C: Traveller levels scaled down by 25%" = "C",
                    "D: Traveller levels scaled down by 50%" = "D")) %>%
    dplyr::ungroup %>%
    dplyr::rename(risk_rating = value) %>%
    dplyr::mutate(risk_rating = factor(risk_rating, 
                                       levels = c("Green",
                                                  "Amber",
                                                  "Red"), ordered = T),
                  risk_rating = forcats::fct_explicit_na(risk_rating,
                                                         na_level = "No data"))
  
  plotOutput <- ggplot(data = toPlot,
                       aes(geometry = geometry)) +
    geom_sf(aes(fill = risk_rating),
            size = 0.25) +
    facet_wrap(~key, ncol = 2) +
    theme_map(world = TRUE) +
    ggplot2::scale_fill_manual(
      values = covid_pal,
      name = "Expected imported cases as percentage of estimated local incidence",
      breaks = names(covid_pal),
      labels = c("Less than 1%", "Between 1% and 10%", "Greater than 10%", "No data")) +
    coord_sf(crs = 54009)
  
  plotOutput
  
}


barPlottingFunction <- function(x){
  
  ggplot2::ggplot(data = x) + 
    ggplot2::geom_col(ggplot2::aes(x = country,
                                   y = required_reduction_in_passengers), 
                      fill = "#58508d", alpha = 0.8) + 
    theme_fig2(world = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 1, 
                                                       vjust =0.5)) +
    ggplot2::scale_y_continuous(labels = scales::percent) + 
    ggplot2::labs(x = "Country", y = "Required reduction in passengers to reduce\nimported cases to less than 1% of estimated local incidence") +
    ggplot2::facet_grid(. ~ region_abb, scales = "free_x", 
                        space = "free_x") 
}


barDataFunction <- function(x){
  dplyr::mutate(x,
                country = 
                  dplyr::case_when(
                    country == "Dominican Republic (the)" ~ "Dominican Republic",
                    country == "Korea (the Republic of)" ~ "South Korea",
                    country == "Netherland (the)" ~ "Netherlands",
                    country == "United Arab Emirates (the)" ~ "UAE",
                    country == "United States of America (the)" ~ "USA",
                    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                    TRUE ~ country))  %>%
    dplyr::mutate(region = countrycode::countrycode(sourcevar = iso_code,
                                                    origin = "iso3c",
                                                    destination = "un.region.name"
    )) %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(region_abb = ifelse(region == "Oceania", "Oc.", region)) %>%
    dplyr::arrange(region_abb, 
                   required_reduction_in_passengers, 
                   country) %>%
    dplyr::mutate(country = forcats::fct_inorder(country)) %>% ungroup
}

scatterPlottingFunction <- function(x){
  ggplot2::ggplot(data = x,
                  aes(x = expected_imported_cases_scenario_2, 
                      y = importation_per_incidence_trim)) +
    ggplot2::scale_x_continuous(trans = "log10", 
                                expand = ggplot2::expansion(mult = c(0.1, 0.1))) +
    # geom_rect(xmin = log(0.01), xmax = log(1e4),
    #           ymin = boot::logit(0.1), ymax = boot::logit(0.995),
    #           fill = covid_pal["Red"]) +
    # geom_rect(xmin = log(0.01), xmax = log(1e4),
    #           ymin = boot::logit(0.01), ymax = boot::logit(0.1),
    #           fill = covid_pal["Amber"]) +
    ggplot2::geom_hline(yintercept = 0.1, lty = 2, alpha = 0.25) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel(aes(label = iso_code,
                                  color = label,
                                  fill = label), size = 1.5, 
                              segment.color = "black",
                              label.size = 0.1, force = 3,
                              min.segment.length = 0, segment.size = 0.25) +
    ggplot2::scale_y_continuous(trans = "logit",
                                labels = scales::percent_format(accuracy = 1),
                                breaks = c(0.01, 0.99, 0.5,  0.25, 0.75, 0.05, 0.95),
                                expand = ggplot2::expansion(mult = c(0.1, 0.1),
                                                            add = c(1, 0))) +
    theme_fig2(world = TRUE) +
    ggplot2::xlab("Expected number of imported cases") +
    ggplot2::ylab("Expected number of imported cases\nas percentage of local incidence") +
    ggplot2::ggtitle(label    = "Traveller levels scaled down by reductions in OpenSky May 2020",
                     subtitle = "Countries with imported cases at least 1% of estimated local incidence") +
    ggplot2::annotation_logticks(sides = "b") +
    ggplot2::scale_fill_manual(
      values = covid_pal,
      name = "Expected number of imported cases as percentage of estimated local incidence",
      breaks = names(covid_pal),
      labels = c("Less than 1%", 
                 "Between 1% and 10%", 
                 "Greater than 10%", 
                 "No data")) +
    ggplot2::scale_color_manual(
      values = c("Green" = "white",
                 "Amber" = "black",
                 "Red" = "white",
                 "No data" = "black"),
      name = "Expected number of imported cases as percentage of estimated local incidence",
      breaks = c("Green", "Amber", "Red", "No data"),
      labels = c("Less than 1%", "Between 1% and 10%", "Greater than 10%", "No data")) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(size = 2,
                          label = c("AUT","CHN"))))
  
  
}


tileDataFunction <- function(x){
  x %>%
    dplyr::filter(!is.na(origin_country) & 
                    !is.na(destination_country)) %>%
    tidyr::complete(origin_country_iso_code,
                    destination_country_iso_code) %>%
    dplyr::mutate(origin_region = 
                    countrycode::countrycode(
                      origin_country_iso_code,
                      "iso3c",
                      "un.region.name"),
                  destination_region =
                    countrycode::countrycode(
                      destination_country_iso_code,
                      "iso3c",
                      "un.region.name")) %>%
    dplyr::mutate(origin_region = 
                    ifelse(
                      origin_country_iso_code == "TWN",
                      "Asia",
                      origin_region),
                  destination_region = 
                    ifelse(
                      destination_country_iso_code == "TWN",
                      "Asia",
                      destination_region)) %>%
    dplyr::mutate(origin_region      = factor(origin_region),
                  destination_region = factor(destination_region)) %>%
    dplyr::arrange(origin_region, 
                   destination_region,
                   origin_country_iso_code, 
                   destination_country_iso_code) %>%
    dplyr::mutate(origin_country_iso_code      = 
                    forcats::fct_inorder(origin_country_iso_code),
                  destination_country_iso_code =
                    forcats::fct_inorder(destination_country_iso_code))
}

tilePlottingFunction <- function(x){
  ggplot2::ggplot(data = x,
         aes(x = origin_country_iso_code, 
             y = destination_country_iso_code)) +
    ggplot2::geom_tile(aes(fill = scaling_factor)) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    theme_fig2(world = F) +
    ggplot2::theme(
      axis.text   = ggplot2::element_text(size = 2),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::xlab("Origin") +
    ggplot2::ylab("Destination") +
    ggplot2::scale_fill_gradient(
      low      = covid_pal["Green"],
      high     = covid_pal["Red"], 
      na.value = covid_pal["No data"],
      limits   = c(0,1),
      name     = "Scaling\nFactor")
}