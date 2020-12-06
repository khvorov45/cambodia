cat("clusters summary")

library(tidyverse)

# Functions ===================================================================

trace_plot <- function(data, key) {
  pl <- data %>%
    mutate(
      chain_lbl = factor(chain),
      cluster_lbl = paste0(
        n_clusters, " cluster", if_else(n_clusters == 1, "", "s")
      ) %>%
        factor() %>%
        fct_reorder(n_clusters)
    ) %>%
    ggplot(aes(iteration, estimate, col = chain_lbl)) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "null"),
      strip.background = element_blank()
    ) +
    scale_x_continuous("Iteration") +
    scale_y_continuous("Estimate") +
    facet_grid(parameter ~ cluster_lbl, scales = "free_y") +
    geom_line(alpha = 0.7)
  attr(pl, "key") <- key
  pl
}

save_plot <- function(plot, name, ...) {
  ggdark::ggsave_dark(
    glue::glue("cluster/{name}.pdf"), plot,
    units = "cm", ...
  )
}

# Script ======================================================================

parameters <- read_csv("cluster/parameters.csv", col_types = cols())

# Trace plots

# Split into multiple pages by virus
trace_plots <- parameters %>%
  # filter(virus == first(virus)) %>%
  group_by(virus) %>%
  group_map(trace_plot)

if (!dir.exists("cluster/trace-plots")) dir.create("cluster/trace-plots")
save_trace_plot <- function(trace_pl) {
  key <- attr(trace_pl, "key")
  vir <- key$virus[[1]]
  hor_facets <- parameters %>%
    filter(virus == vir) %>%
    pull(n_clusters) %>%
    unique() %>%
    length()
  ver_facets <- parameters %>%
    filter(virus == vir) %>%
    pull(parameter) %>%
    unique() %>%
    length()
  save_plot(
    trace_pl,
    paste0("trace-plots/", vir),
    width = hor_facets * 5,
    height = ver_facets * 5
  )
}
walk(trace_plots, save_trace_plot)
