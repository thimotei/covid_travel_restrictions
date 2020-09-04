sirEquations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -r0*gamma*(S*I)/n
    dI <- r0*gamma*(S*I)/n - gamma*I
    dR <- gamma*I
    return(list(c(dS, dI, dR)))
  })
}

meanInfectiousPeriod <- 5 # days
r0Global    <- 3
gammaGlobal <- 1/meanInfectiousPeriod
nGlobal     <- 1000


parametersValues <- c(
  r0 = r0Global,        # average number of secondary infectious (/person)
  gamma  = gammaGlobal, # infectious contact rate (/person/day)
  n = nGlobal - 1
)



initialValues <- c(
  S = nGlobal - 1,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

maxDays <- 35
timeStep <- 0.01
timeValues <- seq(0, maxDays, timeStep) # days


sirValues <- deSolve::ode(
  y = initialValues,
  times = timeValues,
  func = sirEquations,
  parms = parametersValues 
) %>% dplyr::as_tibble()


incidence_wider <- sirValues %>% 
  dplyr::mutate(incidence = (S*I/nGlobal)*gammaGlobal*r0Global) %>%
  dplyr::mutate(incidence_increase_lowest  = incidence*1.001,
                incidence_increase_median  = incidence*1.01,
                incidence_increase_upper = incidence*1.1) %>%
  dplyr::select(-S, -I, -R)

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


restrictions_day_1 <- 16
restrictions_day_2 <- 19

incidence_wider_before_peak <- sirValues %>% 
  dplyr::mutate(incidence = as.numeric((S*I/nGlobal)*gammaGlobal*r0Global)) %>%
  dplyr::mutate(incidence_increase_lowest = dplyr::case_when(time <  restrictions_day_1 ~ incidence*1.001, 
                                                             time >= restrictions_day_1  ~ incidence),
                incidence_increase_median = dplyr::case_when(time <  restrictions_day_1 ~ incidence*1.01,  
                                                             time >= restrictions_day_1 ~ incidence),
                incidence_increase_upper  = dplyr::case_when(time <  restrictions_day_1 ~ incidence*1.1,
                                                             time >= restrictions_day_1 ~ incidence))

incidence_wider_after_peak <- sirValues %>% 
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
  dplyr::filter(time > 14 & time < 18) %>%
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
  dplyr::filter(time > 16 & time < 20) %>%
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

ggplot2::ggsave("outputs/figure_S4.png",
                p_together, 
                width = 30,
                height = 15,
                units = "cm",
                dpi = 500)

