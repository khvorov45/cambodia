cat("make tables")

library(tidyverse)

# Functions ===================================================================

source("data/read_data.R")

format_decimal <- function(d) {
  signif(d, 2)
}

format_percent <- function(d) {
  paste0(format_decimal(d) * 100, "%")
}

summarise_numeric <- function(nums) {
  glue::glue("{format_decimal(mean(nums))} ± {format_decimal(sd(nums))}")
}

summarise_factor <- function(fac) {
  n <- length(fac)
  one_level <- function(lvl) {
    n_level <- sum(fac == lvl)
    glue::glue("{n_level} ({format_percent(n_level / n)}) {lvl}")
  }
  map(sort(unique(fac)), one_level) %>% paste(collapse = "; ")
}

save_data <- function(data, name) {
  write_csv(data, glue::glue("data-table/{name}.csv"))
  data
}

# Script ======================================================================

subject <- read_data("subject")

subject %>%
  group_by(study_year) %>%
  summarise(
    n = n() %>% as.character(),
    age = summarise_numeric(age_years),
    gender = summarise_factor(gender),
    .groups = "drop"
  ) %>%
  pivot_longer(-study_year, names_to = "stat", values_to = "summ") %>%
  pivot_wider(names_from = "study_year", values_from = "summ") %>%
  save_data("summaries")
