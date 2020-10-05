
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

make_world <- function(){
  rnaturalearth::ne_countries(scale = "small", 
                                       returnclass = "sf") %>%
    dplyr::mutate(iso_code = iso_a3) %>%
    #dplyr::rename(country = region) %>%
    #dplyr::mutate(iso_code = countrycode::countrycode(country, 'country.name', 'iso3c')) %>% 
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c",
                                                     destination = 'iso.name.en')) %>%
    dplyr::filter(country != "Antarctica")
}

mapPlottingFunction <- function(x, scenarios = c("mid"), sensitivity = FALSE)
{
  
  world <- make_world() %>%
    dplyr::left_join(x, by = "iso_code") %>%
    dplyr::select(country, iso_code, geometry)
  
  toPlot <- dplyr::full_join(x, world) %>%
    # dplyr::rename(imported_cases_and_incidence_together,
    #               country = destination_country) %>%
    tidyr::gather(key, value, starts_with("imported")) %>%
    dplyr::mutate(scenario = str_extract(key, "([^\\_]+$)")) %>%
    dplyr::filter(scenario %in% scenarios) %>%
    dplyr::mutate(scenario = factor(scenario,
                                    levels = scenarios,
                                    labels = stringr::str_to_title(scenarios)),
                  key = readr::parse_number(key),
                  key = LETTERS[key]) 
  
  if (length(scenarios) == 1L & sensitivity == FALSE){
    toPlot <- toPlot  %>% 
      dplyr::mutate(key = forcats::fct_recode(
        key,
        "A: May 2019 (OAG) scaled by April 2020/April 2019 (OAG)" = "A",
        "B: May 2019 (OAG) scaled by OpenSky reduction" = "B",
        "C: May 2019 (OAG) scaled by OpenSky reduction and reduced further 50%" = "C",
        "D: May 2019 (OAG)" = "D"))
  }
  if (sensitivity == TRUE){
    toPlot <- toPlot  %>% 
      dplyr::mutate(key = forcats::fct_recode(
        key,
        "A" = "A",
        "B" = "B"))
  }
  
  toPlot <- toPlot %>% dplyr::ungroup(.) %>%
    dplyr::rename(risk_rating = value) %>%
    dplyr::mutate(risk_rating = factor(risk_rating, 
                                       levels = c("Green",
                                                  "Amber",
                                                  "Red"), ordered = T),
                  risk_rating = forcats::fct_explicit_na(risk_rating,
                                                         na_level = "No data"))

  key.labs <- c("Scenario A", "Scenario A (50% under-ascertainment)", "Scenario A (80% under-ascertainment)")
  names(key.labs) <- c("A", "B", "C")
    
  scenario.labs <- c("Lower 95% CrI", "Median", "Upper 95% CrI")
  names(scenario.labs) <- c("Low", "Mid", "High")
  
  plotOutput <- ggplot(data = toPlot,
                       aes(geometry = geometry)) +
    geom_sf(aes(fill = risk_rating),
            size = 0.25)  +
    theme_map(world = TRUE) +
    ggplot2::scale_fill_manual(
      values = covid_pal,
      name = "Expected imported cases as percentage of estimated local incidence",
      breaks = names(covid_pal),
      labels = c("Less than 1%", "Between 1% and 10%", "Greater than 10%", "No data")) +
    coord_sf(crs = sf::st_crs(54009))
  
  if (length(scenarios) == 1L){
    plotOutput <- plotOutput +
      facet_wrap(~key, ncol = 2)
  } 
  else {
    plotOutput <- plotOutput +
      facet_grid(scenario ~ key,
                 labeller = labeller(scenario = scenario.labs,
                                     key = key.labs))
  }
  
  plotOutput
  
}


barPlottingFunction <- function(x, interval = FALSE){
  
  barPlot <- ggplot2::ggplot(data = x) + 
    ggplot2::geom_col(ggplot2::aes(x = country,
                                   y = required_reduction_in_passengers_mid), 
                      fill = "#58508d", alpha = 0.8) + 
    theme_fig2(world = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 1, 
                                                       vjust =0.5)) +
    ggplot2::scale_y_continuous(labels = scales::percent) + 
    ggplot2::labs(x = "Country", y = "Required reduction in passengers to\nreduce imported cases to less than 1% of\nestimated local incidence") +
    ggplot2::facet_grid(. ~ region_abb, scales = "free_x", 
                        space = "free_x") 
  
  if (interval){
    barPlot <- barPlot + geom_linerange(aes(x = country,
                                            ymin = required_reduction_in_passengers_low,
                                            ymax = required_reduction_in_passengers_high))
  }
  
  barPlot
  
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
                   required_reduction_in_passengers_mid, 
                   country) %>%
    dplyr::mutate(country = forcats::fct_inorder(country)) %>% ungroup
}

#--- have changed scenario to 1 (used to be 2) as this is most plausible scenario 
#--- after revising the data sources
scatterPlottingFunction <- function(x, interval = FALSE){
  scatterPlot <- ggplot2::ggplot(data = x,
                                 aes(x = expected_imported_cases_scenario_1_mid, 
                                     y = importation_per_incidence_mid_trim)) +
    ggplot2::scale_x_continuous(trans = "log10", 
                                expand = ggplot2::expansion(mult = c(0.1, 0.1))) +
    # geom_rect(xmin = log(0.01), xmax = log(1e4),
    #           ymin = boot::logit(0.1), ymax = boot::logit(0.995),
    #           fill = covid_pal["Red"]) +
    # geom_rect(xmin = log(0.01), xmax = log(1e4),
    #           ymin = boot::logit(0.01), ymax = boot::logit(0.1),
    #           fill = covid_pal["Amber"]) +
    ggplot2::geom_hline(yintercept = 0.1, lty = 2, alpha = 0.25) +
    ggplot2::geom_point(aes(shape = region)) +
    scale_shape_manual(name = "Region",
                       values = c("Africa"    = 2,
                                  "Americas"  = 1,
                                  "Asia"      = 0,
                                  "Europe"    = 4,
                                  "Oceania"   = 3))
  
  if (interval){
    scatterPlot <- scatterPlot +
      geom_segment(aes(x    = expected_imported_cases_scenario_2_low,
                       xend = expected_imported_cases_scenario_2_high,
                       y    = importation_per_incidence_mid_trim,
                       yend = importation_per_incidence_mid_trim)) +
      geom_segment(aes(x    = expected_imported_cases_scenario_2_mid,
                       xend = expected_imported_cases_scenario_2_mid,
                       y    = importation_per_incidence_low_trim,
                       yend = importation_per_incidence_high_trim
      )) +
      facet_wrap(~region)
  }
  
  scatterPlot <- scatterPlot +
    ggrepel::geom_label_repel(aes(label = iso_code,
                                  color = importation_per_incidence_mid_label,
                                  fill  = importation_per_incidence_mid_label),
                              size = 1.5, 
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
    ggplot2::ggtitle(label    = "Traveller numbers using OAG data for April 2020",
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
                          label = c("AUT","CHN")))) +
    theme(legend.box = "vertical")
  
  scatterPlot
  
  
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

unfill_vec <- function(x) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}

scatterTableFunction <- function(x){
  dplyr::arrange(x, region, importation_per_incidence_mid, country) %>%
    dplyr::mutate_at(.vars = dplyr::vars(tidyselect::starts_with("importation"),
                                         tidyselect::starts_with("expected")),
                     .funs = round, digits = 3) %>%
    dplyr::mutate(importation_per_incidence_CI = 
                    sprintf("%0.3f (%0.3f, %0.3f)", 
                            importation_per_incidence_mid,
                            importation_per_incidence_low,
                            importation_per_incidence_high),
                  expected_imported_CI = 
                    sprintf("%0.1f (%0.1f, %0.1f)", 
                            expected_imported_cases_scenario_1_mid,
                            expected_imported_cases_scenario_1_low,
                            expected_imported_cases_scenario_1_high),
                  new_cases_CI = 
                    sprintf("%0.0f (%0.0f, %0.0f)", 
                            new_cases_adjusted_mean_mid,
                            new_cases_adjusted_mean_low,
                            new_cases_adjusted_mean_high)) %>%
    dplyr::select(Region                     = region, 
                  Country                    = country,
                  iso3c                      = iso_code,
                  `Expected imported cases`  = expected_imported_CI,
                  `Local cases`              = new_cases_CI,
                  `Imported cases per local` = importation_per_incidence_CI) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(Region = unfill_vec(Region),
                  Region = ifelse(is.na(Region), "", Region))
}
