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
  sample_sizes <- data %>%
    group_by(study_year, virus_lbl) %>%
    summarise(
      n_ind = length(unique(id)),
      visit_lbl = first(data$visit_lbl),
      titre = 2560,
      .groups = "drop"
    )
  data %>%
    ggplot(aes(visit_lbl, titre)) +
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

summ_geom_mean <- function(x) {
  x <- na.omit(x)
  logx <- log(x)
  mn_logx <- mean(logx)
  se_mn_logx <- sd(logx) / sqrt(length(logx))
  tibble(
    point = mn_logx,
    low = qnorm(0.025, mn_logx, se_mn_logx),
    high = qnorm(0.975, mn_logx, se_mn_logx),
  ) %>%
    mutate_all(exp)
}

# Script ======================================================================

# Subject characteristics

subject <- read_data("subject")
common_theme <- ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "null")
  )

age_hist <- subject %>%
  ggplot(aes(age_years, fill = gender)) +
  common_theme +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "null")
  ) +
  scale_y_continuous("Count", breaks = 1:100, expand = expansion(c(0, 0.1))) +
  scale_x_continuous("Age") +
  scale_fill_brewer("Gender", type = "qual") +
  facet_wrap(~study_year, ncol = 1) +
  geom_histogram(binwidth = 1)

save_plot(
  age_hist, "age-gender-hist",
  width = 15, height = 5 * length(unique(subject$study_year))
)

# Animal slaughter participation

slaughter_plot <- subject %>%
  mutate_if(is.factor, fct_explicit_na) %>%
  count(study_year, slaughter) %>%
  ggplot(aes(slaughter, n)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  common_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  facet_wrap(~study_year, nrow = 1) +
  scale_x_discrete("Slaughter animals") +
  scale_y_continuous("Count", expand = expansion(c(0, 0.1))) +
  geom_bar(stat = "identity", fill = "gray50") +
  geom_text(aes(label = n, y = 1), vjust = 0)

save_plot(
  slaughter_plot, "slaughter",
  width = 5 * length(unique(subject$study_year)), height = 8
)

# Serology

titre <- read_data("titre")
virus <- read_data("virus")

titre_mod <- inner_join(titre, virus, by = "virus") %>%
  mutate(
    virus_lbl = paste(virus, subtype, sep = "\n") %>%
      fct_reorder(as.integer(virus)),
    visit_lbl = factor(visit)
  )

# All titre data
titre_plots <- titre_mod %>%
  group_split(study_year) %>%
  map(titre_plot, min(titre$titre), max(titre$titre)) %>%
  map(facet_year_virus)

titre_plots_arranged <- arrange_plots(plotlist = titre_plots, ncol = 1)

save_plot(titre_plots_arranged, "titre", width = 50, height = 25)

# Titre summary
titre_summ_scales <- function(xlab = "Virus", ylow_expansion = 0.01) {
  list(
    scale_x_discrete(xlab),
    scale_y_log10(
      "GMT (95% CI)",
      breaks = 5 * 2^(0:10), expand = expansion(mult = c(ylow_expansion, 0.05))
    ),
    scale_fill_brewer("Visit", type = "div"),
    scale_color_brewer("Visit", type = "div"),
    scale_linetype_discrete("Visit"),
    scale_shape_discrete("Visit")
  )
}

titre_summ <- titre_mod %>%
  group_by(visit_lbl, virus, subtype, study_year) %>%
  summarise(summ_geom_mean(titre), .groups = "drop") %>%
  ggplot(aes(virus, point, col = visit_lbl, fill = visit_lbl)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  common_theme +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.position = "bottom",
  ) +
  titre_summ_scales() +
  facet_wrap(~study_year, ncol = 1) +
  geom_pointrange(
    aes(shape = visit_lbl, ymin = low, ymax = high),
    position = position_dodge(width = 0.6)
  ) +
  geom_text(
    aes(y = 2.5, x = virus, label = subtype),
    angle = 90, hjust = 0, vjust = 1.5,
    size = 3, col = "gray50",
    data = virus,
    inherit.aes = FALSE
  )
save_plot(titre_summ, "titre-summary", width = 20, height = 25)

tittre_summ_by_h <- titre_mod %>%
  group_by(id, study_year, visit_lbl, haem) %>%
  summarise(titre = exp(mean(log(titre))), .groups = "drop") %>%
  group_by(visit_lbl, study_year, haem) %>%
  summarise(summ_geom_mean(titre), .groups = "drop") %>%
  ggplot(
    aes(haem, point, ymin = low, ymax = high, col = visit_lbl, fill = visit_lbl)
  ) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  common_theme +
  theme(legend.position = "bottom") +
  titre_summ_scales("Haemagglutinin type", 0.05) +
  facet_wrap(~study_year, ncol = 1) +
  geom_pointrange(
    aes(shape = visit_lbl),
    position = position_dodge(width = 0.6)
  )
save_plot(tittre_summ_by_h, "titre-summary-by-haem", width = 15, height = 20)

# Plots for individuals with multiple years data
titre_plot_multiple_years <- titre_mod %>%
  group_by(id, virus) %>%
  filter(length(unique(study_year)) > 1) %>%
  ungroup() %>%
  mutate(visit_lbl = glue::glue("{study_year}-{visit}")) %>%
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

save_plot(
  animal_possession_plot, "animal-possession",
  width = 5 * length(unique(animal_possession$animal)), height = 7
)

# Animal processsing

animal_sale <- read_data("animal-sale")

animal_sale_plot <- animal_sale %>%
  mutate(from = if_else(from == 0, 0.1, from)) %>%
  ggplot(aes(animal, mid, ymin = from, ymax = to)) +
  common_theme +
  theme(
    axis.title.y = element_blank(),
    strip.placement = "outside"
  ) +
  scale_y_log10(
    breaks = c(0.1, 10^(0:4), 40000), labels = c(0, 10^(0:4), 40000)
  ) +
  scale_x_discrete("Animal", labels = as_labeller(tools::toTitleCase)) +
  facet_grid(
    type ~ study_year,
    switch = "y", labeller = as_labeller(tools::toTitleCase)
  ) +
  geom_errorbar(width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(y = mid), fill = NA, outlier.alpha = 0, col = "darkblue")

save_plot(
  animal_sale_plot, "animal-sale",
  width = 6 * length(unique(animal_sale$animal)), height = 12
)
