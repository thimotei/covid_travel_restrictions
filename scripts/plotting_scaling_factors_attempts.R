p <- plotly::plot_ly(
  total_flights_may_2019_2020, x = ~origin_country_iso_code, y = ~destination_country_iso_code, z = ~scaling_factor,
  color = ~origin_country_iso_code) %>%
  plotly::add_markers(size = 3) %>%
  #marker = list(size = 5) %>%
  plotly::layout(
    scene = list(xaxis = list(title = 'Origin country'),
                 yaxis = list(title = 'Destination country'),
                 zaxis = list(title = 'Scaling factor'))
  )


flights_df <- matrix(c(total_flights_may_2019_2020$origin_country_iso_code, total_flights_may_2019_2020$destination_country_iso_code), nc = 2, byrow = TRUE)

airport_network <- igraph::graph_from_edgelist(flights_df, directed = TRUE)

airport_network_2 <- igraph::simplify(airport_network,
                                      remove.multiple = TRUE,
                                      edge.attr.comb = list(weight = "sum"))

igraph::plot.igraph(airport_network_2, 
                    layout = igraph::layout.circle,
                    main="circle",
                    vertex.size = 5,
                    vertex.color = rgb(0.1,0.7,0.8,0.5))

new_el <- airport_network_2 %>% igraph::as_edgelist() %>%
  dplyr::tibble(origin_country_iso_code = .[,1], destination_country_iso_code = .[,2]) %>%
  dplyr::select(-.)


airport_lookup <- read.delim(here("data", "airports.dat"), sep = ",")

airport_lookup_join_1 <- airport_lookup %>%
  dplyr::select(origin_country_iso_code = three_letter_code, lat, long)

airport_lookup_join_2 <- airport_lookup %>%
  dplyr::select(destination_country_iso_code = three_letter_code, lat, long)

new_el %>% 
  dplyr::left_join(airport_lookup_join_1) %>% 
  dplyr::left_join(airport_lookup_join_2) %>% View()



get_geocoded_coords <- function(country){
  coords <- geocode(country) %>%
    as.list() %>%
    paste0(collapse = ",")
}


p <- plotly::plot_ly(
  total_flights_may_2019_2020, x = ~destination_country_iso_code, y = ~scaling_factor,
  color = ~origin_country_iso_code) %>%
  plotly::add_markers(size = 3) %>%
  #marker = list(size = 5) %>%
  plotly::layout(
    scene = list(xaxis = list(title = 'Origin country'),
                 yaxis = list(title = 'Destination country'),
                 zaxis = list(title = 'Scaling factor'))
  )
p


flight_reduction_plot <- total_flights_may_2019_2020 %>%
  ggplot2::ggplot() + 
  ggplot2::geom_point(ggplot2::aes(x = destination_country_iso_code,
                                   y = scaling_factor,
                                   color = origin_country_iso_code,
                                   group = origin_country_iso_code)) +
  ggplot2::labs(x = "Destination country",
               y = "Flight reduction scale factor",
               color = "Country of origin") + 
  ggplot2::theme_minimal() + 
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) + 
  ggplot2::scale_colour_viridis_d(
    alpha = 0.8,
    begin = 0,
    end = 0.93,
    direction = 1,
    option = "D",
    aesthetics = "colour"
  )
  
  

ggplot2::ggsave(here("outputs","flight_reduction.png"),
                plot = flight_reduction_plot, 
                width = 14,
                height = 7,
                units = "in")

el_coords <-
  as_edgelist(g_2) %>%
  as_tibble() %>%
  full_join(node_coords, by = c("V1" = "value")) %>%
  full_join(node_coords, by = c("V2" = "value")) %>%
  drop_na(V1, V2)

map_world <- map_data(map = "world") %>%
  dplyr::filter(region != "Antarctica")

base_gg <- ggplot() +
  geom_polygon(data = map_world,
               aes(long, lat, group = group, fill = region), 
               show.legend = FALSE,
               alpha = 0.25,
               color = "white") +
  geom_segment(data = el_coords, 
               aes(x = long.x, xend = long.y,
                   y = lat.x, yend = lat.y),
               # arrow = arrow(length = unit(0.1, "inches"), # optional arrows
               # type = "closed"),
               size = 0.25,
               alpha = 0.5) +
  geom_point(data = node_coords,
             aes(long, lat), color = "red") +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Geospatial Network",
       subtitle = "Plot Example",
       caption = "Brendan Knapp")



