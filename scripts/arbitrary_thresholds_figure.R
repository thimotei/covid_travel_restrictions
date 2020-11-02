seirEquations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -r0*gamma*(S*I)/nGlobal
    dE <- (r0*gamma * S * I)/nGlobal - (delta * E)
    dI <- (delta * E) - (gamma * I)
    dR <- gamma*I
    return(list(c(dS, dE, dI, dR)))
  })
}

mean_infectious_period <- 7 # days
mean_latent_period <- 5
r0Global    <- 3
gammaGlobal <- 1/mean_infectious_period
nGlobal     <- 1000
deltaGlobal <- 1/mean_latent_period

parametersValues <- c(
  r0 = r0Global,        # average number of secondary infectious (/person)
  gamma  = gammaGlobal, # infectious contact rate (/person/day)
  n = nGlobal - 1,
  delta = deltaGlobal
)



initialValues <- c(
  S = nGlobal - 1,  # number of susceptibles at time = 0
  E = 1,
  I = 0,  # number of infectious at time = 0
  R = 0   # number of recovered (and immune) at time = 0
)

maxDays <- 90
timeStep <- 0.01
timeValues <- seq(0, maxDays, timeStep) # days


seirValues <- deSolve::ode(
  y = initialValues,
  times = timeValues,
  func = seirEquations,
  parms = parametersValues 
) %>% dplyr::as_tibble()


incidence_wider <- seirValues %>% 
  dplyr::mutate(incidence = (S*I/nGlobal)*gammaGlobal*r0Global) %>%
  dplyr::mutate(incidence_increase_lowest  = incidence*1.001,
                incidence_increase_median  = incidence*1.01,
                incidence_increase_upper = incidence*1.1) %>%
  dplyr::select(-S, -E, -I, -R)

# incidence_longer <- incidence_wider %>%
#   tidyr::pivot_longer(cols = c("incidence",
#                                "incidence_increase_lowest",
#                                "incidence_increase_median",
#                                "incidence_increase_upper"),
#                       names_to = "increase_level",
#                       values_to = "incidence") %>%
#   dplyr::arrange(increase_level)

# DEPRACTED
# p1 <- incidence_longer %>%
#   ggplot2::ggplot(ggplot2::aes(x = time, y = incidence)) +
#   ggplot2::geom_area(ggplot2::aes(group = increase_level, color = increase_level)) + 
#   ggplot2::theme_minimal() +
#   ggplot2::labs(x = "Time (days)", y = "Incidence", title = "A") +
#   ggplot2::scale_color_viridis_d(begin = 0,
#                                  end = 0.8,
#                                  labels = c("No increase", 
#                                             "0.1% increase",
#                                             "1% increase",
#                                             "10% increase")) + 
#   ggplot2::guides(color = ggplot2::guide_legend("")) 


restrictions_day_1 <- 50
restrictions_day_2 <- 60

incidence_wider_before_peak <- seirValues %>% 
  dplyr::mutate(incidence = as.numeric((S*I/nGlobal)*gammaGlobal*r0Global)) %>%
  dplyr::mutate(incidence_increase_lowest = dplyr::case_when(time <  restrictions_day_1 ~ incidence*1.001, 
                                                             time >= restrictions_day_1  ~ incidence),
                incidence_increase_median = dplyr::case_when(time <  restrictions_day_1 ~ incidence*1.01,  
                                                             time >= restrictions_day_1 ~ incidence),
                incidence_increase_upper  = dplyr::case_when(time <  restrictions_day_1 ~ incidence*1.1,
                                                             time >= restrictions_day_1 ~ incidence))

incidence_wider_after_peak <- seirValues %>% 
  dplyr::mutate(incidence = as.numeric((S*I/nGlobal)*gammaGlobal*r0Global)) %>%
  dplyr::mutate(incidence_increase_lowest = dplyr::case_when(time <  restrictions_day_2 ~ incidence*1.001, 
                                                             time >= restrictions_day_2  ~ incidence),
                incidence_increase_median = dplyr::case_when(time <  restrictions_day_2 ~ incidence*1.01,  
                                                             time >= restrictions_day_2 ~ incidence),
                incidence_increase_upper  = dplyr::case_when(time <  restrictions_day_2 ~ incidence*1.1,
                                                             time >= restrictions_day_2 ~ incidence))



p1 <- incidence_wider %>%
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = incidence, fill = "incidence"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence, ymax = incidence_increase_lowest, fill = "incidence_increase_lowest"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_lowest, ymax = incidence_increase_median, fill = "incidence_increase_median"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_median, ymax = incidence_increase_upper, fill = "incidence_increase_upper"), alpha = 0.7, size = 0.5, color = "black") + 
  #ggplot2::geom_vline(xintercept = restrictions_day_1, linetype = "dashed") + 
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Time (days)", y = "Incidence", title = "A") +
  ggplot2::scale_fill_manual(name = " ",
                             values = c("incidence" = viridis::viridis(10)[3], 
                                        "incidence_increase_lowest" = viridis::viridis(10)[5],
                                        "incidence_increase_median" = viridis::viridis(10)[7],
                                        "incidence_increase_upper"  = viridis::viridis(10)[9]),
                             labels = c("No increase", 
                                        "0.1% increase",
                                        "1% increase",
                                        "10% increase"))


p2 <- incidence_wider_before_peak %>%
  #dplyr::filter(time > 14 & time < 18) %>%
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = incidence, fill = "incidence"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence, ymax = incidence_increase_lowest, fill = "incidence_increase_lowest"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_lowest, ymax = incidence_increase_median, fill = "incidence_increase_median"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_median, ymax = incidence_increase_upper, fill = "incidence_increase_upper"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_vline(xintercept = restrictions_day_1, linetype = "dashed") + 
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Time (days)", y = "Incidence", title = "B") +
  ggplot2::scale_fill_manual(name = " ",
                             values = c("incidence" = viridis::viridis(10)[3], 
                                        "incidence_increase_lowest" = viridis::viridis(10)[5],
                                        "incidence_increase_median" = viridis::viridis(10)[7],
                                        "incidence_increase_upper"  = viridis::viridis(10)[9]),
                             labels = c("No increase", 
                                        "0.1% increase",
                                        "1% increase",
                                        "10% increase"))



p3 <- incidence_wider_after_peak %>%
  #dplyr::filter(time > 16 & time < 20) %>%
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = incidence, fill = "incidence"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence, ymax = incidence_increase_lowest, fill = "incidence_increase_lowest"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_lowest, ymax = incidence_increase_median, fill = "incidence_increase_median"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_median, ymax = incidence_increase_upper, fill = "incidence_increase_upper"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_vline(xintercept = restrictions_day_2, linetype = "dashed") + 
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Time (days)", y = "Incidence", title = "C") +
  ggplot2::scale_fill_manual(name = " ",
                             values = c("incidence" = viridis::viridis(10)[3], 
                                        "incidence_increase_lowest" = viridis::viridis(10)[5],
                                        "incidence_increase_median" = viridis::viridis(10)[7],
                                        "incidence_increase_upper"  = viridis::viridis(10)[9]),
                             labels = c("No increase", 
                                        "0.1% increase",
                                        "1% increase",
                                        "10% increase"))

p4 <- incidence_wider_before_peak %>%
  dplyr::filter(time > 40 & time < 55) %>%
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = incidence, fill = "incidence"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence, ymax = incidence_increase_lowest, fill = "incidence_increase_lowest"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_lowest, ymax = incidence_increase_median, fill = "incidence_increase_median"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_median, ymax = incidence_increase_upper, fill = "incidence_increase_upper"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_vline(xintercept = restrictions_day_1, linetype = "dashed") + 
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Time (days)", y = "Incidence", title = "D") +
  ggplot2::scale_fill_manual(name = " ",
                             values = c("incidence" = viridis::viridis(10)[3], 
                                        "incidence_increase_lowest" = viridis::viridis(10)[5],
                                        "incidence_increase_median" = viridis::viridis(10)[7],
                                        "incidence_increase_upper"  = viridis::viridis(10)[9]),
                             labels = c("No increase", 
                                        "0.1% increase",
                                        "1% increase",
                                        "10% increase"))



p5 <- incidence_wider_after_peak %>%
  dplyr::filter(time > 50 & time < 65) %>%
  ggplot2::ggplot(ggplot2::aes(x = time)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = incidence, fill = "incidence"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence, ymax = incidence_increase_lowest, fill = "incidence_increase_lowest"), alpha = 0.7, size = 0.2, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_lowest, ymax = incidence_increase_median, fill = "incidence_increase_median"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_ribbon(ggplot2::aes(ymin = incidence_increase_median, ymax = incidence_increase_upper, fill = "incidence_increase_upper"), alpha = 0.7, size = 0.5, color = "black") + 
  ggplot2::geom_vline(xintercept = restrictions_day_2, linetype = "dashed") + 
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Time (days)", y = "Incidence", title = "E") +
  ggplot2::scale_fill_manual(name = " ",
                             values = c("incidence" = viridis::viridis(10)[3], 
                                        "incidence_increase_lowest" = viridis::viridis(10)[5],
                                        "incidence_increase_median" = viridis::viridis(10)[7],
                                        "incidence_increase_upper"  = viridis::viridis(10)[9]),
                             labels = c("No increase", 
                                        "0.1% increase",
                                        "1% increase",
                                        "10% increase"))

library(patchwork)

layout <- "
    AAAAAAABBBDD
    AAAAAAACCCEE
    "

p_together <- p1 + p2 + p3 + p4 + p5 + plot_layout(guides = "collect", design = layout)

p_together

ggplot2::ggsave("outputs/figure_S6.pdf",
                p_together, 
                width = 30,
                height = 15,
                units = "cm",
                dpi = 500)

