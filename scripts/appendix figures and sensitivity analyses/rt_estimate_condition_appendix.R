#--- inclusion based on Rt estimates appendix section ---#
#--- source the script below if it has not already been run
source("scripts/main_script_computing_all_results.R")

countries_to_include <- rt_estimates_raw %>% 
    dplyr::filter(0.95 <= median & median <= 1) %>%
    dplyr::pull(iso_code)


#--- plotting figure S8
figure_S8_data <- barDataFunction(required_reduction)  %>%
    dplyr::filter(iso_code %in% countries_to_include) %>%
    dplyr::mutate(country = plyr::revalue(country, c("Congo (the Democratic Republic of the)" = "DRC",
                                                     "Bahamas (the)" = "Bahamas",
                                                     "Sudan (the)"   = "Sudan",
                                                     "Sint Maarten (Dutch part)" = "Sint Maarten",
                                                     "Netherlands (the)" = "Netherlands",
                                                     "Niger (the)" = "Niger",
                                                     "Virgin Islands (U.S.)" = "Virgin Islands")))

figure_S8 <- barPlottingFunction(figure_S8_data, interval = TRUE, sensitivity = TRUE)

ggplot2::ggsave(here("outputs","figure_S8.pdf"),
                figure_S8,
                device = "pdf",
                width = 6, 
                height = 4.5)

ggplot2::ggsave(here("outputs","figure_S8.png"),
                figure_S8,
                device = "png",
                width = 6, 
                height = 4.5)


#--- plotting figure S9
figure_S9_data <-
    figure_S8_data %>%
    mutate_at(.vars = vars(starts_with("importation")),
              .funs = list(trim = as.numeric,
                           label = ~cut(.,
                                        breaks = c(0, 0.01, 0.1, 1),
                                        include.lowest = T, 
                                        labels = c("Green",
                                                   "Amber",
                                                   "Red")))) %>%
    mutate_at(.vars = vars(matches("trim")),
              .funs = function(x){pmin(0.995, pmax(0.005,x))})


figure_S9 <- scatterPlottingFunction(figure_S9_data, interval = FALSE)

ggplot2::ggsave(here("outputs","figure_S9.pdf"),
                figure_S9,
                device = "pdf",
                width = 9, 
                height = 6)

ggplot2::ggsave(here("outputs","figure_S9.png"),
                figure_S9,
                device = "png",
                width = 9, 
                height = 6)

