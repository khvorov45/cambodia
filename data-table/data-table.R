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

summarise_binary <- function(bin_vec) {
  total <- length(bin_vec)
  success <- sum(bin_vec)
  failure <- total - success
  point <- success / total
  low <- min(qbeta(0.025, success + 1, failure + 1), point)
  high <- max(qbeta(0.975, success + 1, failure + 1), point)
  glue::glue(
    "{success} / {total} ",
    "{format_percent(point)} ({format_percent(low)}, {format_percent(high)})"
  )
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
  save_data("subject")

titre <- read_data("titre")

summarise_gmt <- function(titres) {
  logtitres <- log(titres)
  logmn <- mean(logtitres)
  logse <- sd(logtitres) / sqrt(length(titres))
  q <- qnorm(0.975)
  glue::glue(
    "{format_decimal(exp(logmn))} ",
    "({format_decimal(exp(logmn - q * logse))}, ",
    "{format_decimal(exp(logmn + q * logse))})",
  )
}

# Summaries that only involve one timepoint

titre %>%
  group_by(study_year, virus, clade, visit) %>%
  summarise(
    n_individuals = length(unique(id)),
    gmt = summarise_gmt(titre),
    titre_above_40 = summarise_binary(titre >= 40),
    .groups = "drop"
  ) %>%
  save_data("titre-one-timepoint")
