cat("plot the data")

# Functions ===================================================================

source("data/read_data.R")

save_plot <- function(plot, name, ...) {
  ggdark::ggsave_dark(
    glue::glue("data-plot/{name}.pdf"), plot,
    units = "cm",
    ...
  )
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
