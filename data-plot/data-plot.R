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

titre_plot <- function(data, min_y = NULL, max_y = NULL) {
  data_mod <- data %>%
    mutate(
      virus_lbl = paste0(virus, "\n", clade) %>% fct_reorder(as.integer(virus)),
      visit = as.character(visit)
    )
  sample_sizes <- data_mod %>%
    group_by(study_year, virus_lbl) %>%
    summarise(
      n_ind = length(unique(id)),
      visit = first(data_mod$visit),
      titre = 2560,
      .groups = "drop"
    )
  data_mod %>%
    ggplot(aes(visit, titre)) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0, "null"),
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(min_y, max_y)) +
    scale_y_log10("Titre", breaks = 5 * 2^(0:10)) +
    scale_x_discrete("Visit") +
    geom_line(aes(group = id, col = id), alpha = 0.5) +
    geom_point(alpha = 0.5) +
    geom_text(data = sample_sizes, mapping = aes(label = n_ind), hjust = 0)
}

facet_year_virus <- function(plot) {
  plot +
    facet_grid(study_year ~ virus_lbl, labeller = as_labeller(wrap_virus_names))
}

arrange_plots <- function(...) {
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
  map(titre_plot, min(titre$titre), max(titre$titre)) %>%
  map(facet_year_virus)

titre_plots_arranged <- arrange_plots(plotlist = titre_plots, ncol = 1)

save_plot(titre_plots_arranged, "titre", width = 50, height = 25)

# Plots for individuals with multiple years data
titre_plot_multiple_years <- titre %>%
  group_by(id, virus) %>%
  filter(length(unique(study_year)) > 1) %>%
  ungroup() %>%
  mutate(visit = glue::glue("{study_year}-{visit}")) %>%
  titre_plot() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~virus_lbl, labeller = as_labeller(wrap_virus_names), nrow = 1)

save_plot(
  titre_plot_multiple_years, "titre-multiple-years",
  width = 50, height = 10
)

# Animal posession

animal_possession <- read_data("animal-possession")

animal_possession_plot <- animal_possession %>%
  ggplot(aes(animal, count)) +
  common_theme +
  scale_y_log10("Count", breaks = c(10^(0:5), 5000)) +
  scale_x_discrete("Animal", labels = as_labeller(tools::toTitleCase)) +
  facet_wrap(~study_year) +
  geom_jitter(height = 0, width = 0.2, alpha = 0.5, shape = 16) +
  geom_boxplot(fill = NA, col = "darkblue", outlier.alpha = 0)

save_plot(animal_possession_plot, "animal-possession", width = 10, height = 7)
