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

mapPlottingFunction <- function(dataInput)
{
  
  plotOutput <- dataInput %>%
    ggplot2::ggplot(ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = risk_rating)) + 
    ggplot2::scale_fill_manual(values = c("Red" =  "#FF0000", "Amber" =  "#FFBF00", "Green" = "#00FF00"),
                               name = "Expected imported cases as percentage of estimated local incidence",
                               breaks=c("Green", "Amber", "Red"),
                               labels = c("Less than 1%", "Between 1% and 10%", "Greater than 10%")) + 
    ggplot2::geom_path(ggplot2::aes(x = long, y = lat, group = group), size = 0.3) + 
    theme_map(world = TRUE)
  
}

combineMapAndIncidenceData <- function(dataToPlot)
{
  
  ggplot2::map_data("world") %>%
    dplyr::rename(country = region) %>%
    dplyr::mutate(iso_code = countrycode::countrycode(country, 'country.name', 'iso3c')) %>% 
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::filter(country != "Antarctica") %>%
    dplyr::left_join(dataToPlot, by = "iso_code") %>%
    dplyr::select(country, lat, long, group, country)
  
}

