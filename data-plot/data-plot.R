cat("plot the data")

library(tidyverse)

# Functions ===================================================================

source("data/read_data.R")

save_plot <- function(plot, name, ...) {
  ggdark::ggsave_dark(
    glue::glue("data-plot/{name}.pdf"), plot,
    units = "cm",
    ...
  )
}

wrap_virus_names <- function(names) {
  names %>% str_replace_all("/", "/\n")
}

titre_plot <- function(data, min_y, max_y) {
  data_mod <- data %>%
    mutate(
      virus_lbl = paste0(virus, "\n", clade) %>% fct_reorder(as.integer(virus))
    )
  sample_sizes <- data_mod %>%
    group_by(study_year, virus_lbl) %>%
    summarise(
      n_ind = length(unique(id)),
      visit = 1,
      titre = 2560,
      .groups = "drop"
    )
  data_mod %>%
    ggplot(aes(visit, titre)) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(min_y, max_y)) +
    scale_y_log10("Titre", breaks = 5 * 2^(0:10)) +
    scale_x_continuous("Visit") +
    facet_grid(study_year ~ virus_lbl, labeller = as_labeller(wrap_virus_names)) +
    geom_line(aes(col = id), alpha = 0.5) +
    geom_point(alpha = 0.5) +
    geom_text(data = sample_sizes, mapping = aes(label = n_ind), hjust = 0)
}

arrange <- function(...) {
  ggdark::lighten_geoms()
  arr <- ggpubr::ggarrange(...)
  ggdark::darken_geoms()
  arr
}

# Script ======================================================================

subject <- read_data("subject")
common_theme <- ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "null")
  )

age_hist <- subject %>%
  ggplot(aes(age_years)) +
  common_theme +
  scale_y_continuous("Count", breaks = 1:100, expand = expansion(c(0, 0.1))) +
  scale_x_continuous("Age") +
  facet_wrap(~study_year) +
  geom_histogram(binwidth = 1)

save_plot(age_hist, "age-hist", width = 15, height = 7)

titre <- read_data("titre")

titre_plots <- titre %>%
  group_split(study_year) %>%
  map(titre_plot, min(titre$titre), max(titre$titre))

titre_plots_arranged <- arrange(plotlist = titre_plots, ncol = 1)

save_plot(titre_plots_arranged, "titre", width = 50, height = 25)
