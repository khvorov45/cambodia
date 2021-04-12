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
  nums <- na.omit(nums)
  glue::glue("{format_decimal(mean(nums))} ({format_decimal(sd(nums))})")
}

summarise_factor <- function(fac) {
  fac <- na.omit(fac)
  n <- length(fac)
  one_level <- function(lvl) {
    n_level <- sum(fac == lvl)
    glue::glue("{n_level} ({format_percent(n_level / n)}) {lvl}")
  }
  map(sort(unique(fac)), one_level) %>% paste(collapse = "; ")
}

summarise_binary <- function(bin_vec, return_everything = FALSE) {
  bin_vec <- na.omit(bin_vec)
  total <- length(bin_vec)
  success <- sum(bin_vec)
  failure <- total - success
  point <- success / total
  ci <- PropCIs::exactci(success, total, 0.95)$conf.int
  low <- ci[[1]]
  high <- ci[[2]]
  summ <- glue::glue(
    "{success} / {total} ",
    "{format_percent(point)} ({format_percent(low)}, {format_percent(high)})"
  )
  if (return_everything) {
    return(tibble(total, success, failure, point, low, high, summ))
  }
  summ
}

summarise_logmean <- function(titres) {
  titres <- na.omit(titres)
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

summarise_count <- function(count) {
  count <- na.omit(count)
  glue::glue(
    "{median(count)} [{quantile(count, 0.25)}, {quantile(count, 0.75)}] ",
    "{length(count)}"
  )
}

save_data <- function(data, name) {
  write_csv(data, glue::glue("data-summary/{name}.csv"))
  data
}

save_plot <- function(plot, name, ...) {
  ggdark::ggsave_dark(
    glue::glue("data-summary/{name}.pdf"), plot,
    units = "cm",
    ...
  )
}

# Script ======================================================================

subject <- read_data("subject")

subject %>%
  group_by(study_year) %>%
  summarise(
    n = n() %>% as.character(),
    age = summarise_numeric(age_years),
    gender = summarise_factor(gender),
    sector = summarise_factor(sector),
    .groups = "drop"
  ) %>%
  pivot_longer(-study_year, names_to = "stat", values_to = "summ") %>%
  pivot_wider(names_from = "study_year", values_from = "summ") %>%
  save_data("subject")

titre <- read_data("titre")

# Summaries that involve 2 timepoints
timepoint_combos <- tibble(
  t1_lbl = unique(titre$visit),
  t2_lbl = unique(titre$visit)
) %>%
  expand.grid() %>%
  as_tibble() %>%
  filter(t2_lbl > t1_lbl)

summary_one_timepoint_combo <- function(t1_lbl, t2_lbl, data) {
  data_mod <- data %>%
    filter(visit %in% c(t1_lbl, t2_lbl)) %>%
    pivot_wider(names_from = "visit", values_from = titre) %>%
    mutate(
      t1 = !!rlang::sym(as.character(t1_lbl)),
      t2 = !!rlang::sym(as.character(t2_lbl)),
      logt1 = log(t1),
      logt2 = log(t2),
      logdiff = logt1 - logt2,
      ratio = t2 / t1,
    ) %>%
    filter(!is.na(t1), !is.na(t2))
  fit <- lm(logt2 ~ logt1, data_mod)
  b1 <- fit$coef[["logt1"]]
  reference <- log(10)
  data_mod %>%
    mutate(
      logt2_corrected = logt2 - b1 * (logt1 - reference),
      t2_corrected = exp(logt2_corrected),
      ratio_corrected = t2_corrected / 10,
      titre_above_40 = t2 >= 40,
      seroconv = if_else(t1 == 5, t2 >= 40, ratio >= 4),
      timepoints = glue::glue("{t2_lbl} vs {t1_lbl}"),
    )
}
titre_summaries <-
  pmap_dfr(timepoint_combos, summary_one_timepoint_combo, titre)

titre_summaries %>%
  group_by(timepoints, study_year, virus) %>%
  summarise(
    n_individuals = length(unique(id)),
    baseline = summarise_logmean(t1),
    gmt = summarise_logmean(t2),
    gmr = summarise_logmean(ratio),
    gmt_corrected = summarise_logmean(t2_corrected),
    gmr_corrected = summarise_logmean(ratio_corrected),
    titre_above_40_prop = summarise_binary(titre_above_40),
    titre_above_40_below_before_prop =
      summarise_binary(titre_above_40[t1 < 40]),
    seroconv = summarise_binary(seroconv),
    .groups = "drop"
  ) %>%
  select(study_year, timepoints, everything()) %>%
  save_data("titre")

# Infections (as defined by ratio >= 4)
virus <- read_data("virus") %>%
  mutate(lab = glue::glue("{short} ({haem})"))
infection_summ <- titre_summaries %>%
  mutate(infection = ratio >= 4) %>%
  group_by(id, study_year, virus) %>%
  summarise(infection = any(infection), .groups = "drop") %>%
  inner_join(subject, c("id", "study_year")) %>%
  inner_join(virus, "virus") %>%
  group_by(study_year, sector, virus, haem) %>%
  summarise(
    summarise_binary(infection, return_everything = TRUE),
    .groups = "drop"
  )
infection_summ_plot <- infection_summ %>%
  ggplot(aes(virus, point)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(0, "null")
  ) +
  facet_grid(study_year ~ sector, labeller = as_labeller(tools::toTitleCase)) +
  scale_y_continuous("Infections", labels = scales::percent_format(1)) +
  scale_x_discrete(
    "Virus",
    labels = as_labeller(function(labels) {
      virus$lab[virus$virus == labels]
    })
  ) +
  geom_pointrange(aes(ymin = low, ymax = high))
save_plot(infection_summ_plot, "infection-summ", width = 20, height = 20)

infection_summ %>%
  select(sector, study_year, virus, haem, summ) %>%
  pivot_wider(names_from = "sector", values_from = "summ") %>%
  save_data("infection-summ")

# Animal possession

animal_possession <- read_data("animal-possession")

animal_possession %>%
  group_by(study_year, animal) %>%
  summarise(
    summary = summarise_count(count),
    .groups = "drop",
  ) %>%
  pivot_wider(
    names_from = "study_year", values_from = "summary", values_fill = ""
  ) %>%
  save_data("animal-possession")

# Animal processing
animal_sale <- read_data("animal-sale")

head_ids <- animal_sale %>%
  filter(type == "head") %>%
  pull(id)
kg_ids <- animal_sale %>%
  filter(type == "kg") %>%
  pull(id)

setdiff(head_ids, kg_ids)
setdiff(kg_ids, head_ids)
# All heads seem to be in kilograms

animal_sale %>%
  group_by(study_year, animal, type) %>%
  summarise(
    summary = summarise_count(mid),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "type", values_from = "summary") %>%
  save_data("animal-sale")
