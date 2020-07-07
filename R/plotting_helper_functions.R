library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
library(mapproj)

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
  
  toPlot <- full_join(x, world) %>%
    # dplyr::rename(imported_cases_and_incidence_together,
    #               country = destination_country) %>%
    gather(key, value, starts_with("expected")) %>%
    mutate(key = readr::parse_number(key),
           key = LETTERS[key],
           key = fct_recode(key,
                            "A: Traveller levels same as May 2019" = "A",
                            "B: Traveller levels scaled down by reductions in OpenSky May 2020" = "B",
                            "C: Traveller levels scaled down by 50%" = "C",
                            "D: Traveller levels scaled down by 25%" = "D")) %>%
    ungroup %>%
    rename(risk_rating = value) %>%
    mutate(risk_rating = factor(risk_rating, levels = c("Green",
                                                        "Amber",
                                                        "Red"), ordered = T),
           risk_rating = forcats::fct_explicit_na(risk_rating,
                                                  na_level = "No data"))
  
  plotOutput <-ggplot(data = toPlot,
                      aes(geometry = geometry)) +
    geom_sf(aes(fill = risk_rating),
            size = 0.25) +
    facet_wrap(~key, ncol = 2) +
    theme_map(world = TRUE) +
    ggplot2::scale_fill_manual(
      values = c(magrittr::set_names(x = RColorBrewer::brewer.pal(3, "Purples"),
                                     value = c("Green", "Amber", "Red")),
                 "No data" = rgb(red = 1, green = 0.9, blue = 0.9)),
      name = "Expected imported cases as percentage of estimated local incidence",
      breaks = c("Green", "Amber", "Red", "No data"),
      labels = c("Less than 1%", "Between 1% and 10%", "Greater than 10%", "No data")) +
    coord_sf(crs = 54009)
  
  plotOutput
  
}


