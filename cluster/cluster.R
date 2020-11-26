cat("clusters")

library(tidyverse)

# Functions ===================================================================

source("data/read_data.R")

fit_mixak <- function(data, outcome, fixed_formula) {
  y <- data %>%
    select(!!!rlang::enquos(outcome)) %>%
    as.data.frame()
  # Subset the complete model matrix
  x_all <- model.matrix(fixed_formula, data)[, -1]
  outcome_names <- colnames(y)
  names(outcome_names) <- colnames(y)
  x <- map(
    outcome_names,
    function(outcome_name) {
      outcome_formula <- fixed_formula %>%
        paste(collapse = "") %>%
        paste(outcome_name, .) %>%
        as.formula()
      relevant_cols <- model.matrix(outcome_formula, data)[, -1] %>% colnames()
      x_all[, relevant_cols]
    }
  )
  # This produces warnings because someone doesn't know how to work the
  # %in% operator
  fit <- mixAK::GLMM_MCMC(
    # Outcome
    y = y,
    dist = rep("gaussian", ncol(y)),

    # Random effect clusters
    id = data %>% pull(id),

    # Fixed effects
    x = x,

    # Random effects
    z = rep(list("empty"), ncol(y)),

    # Random intercept
    random.intercept = rep(TRUE, ncol(y)),

    prior.b = list(
      # Clusters
      Kmax = 2
    ),

    # MCMC settings
    nMCMC = c(burn = 100, keep = 1000, thin = 10, info = 100),
    parallel = FALSE
  ) %>%
    mixAK::NMixRelabel(type = "stephens", keep.comp.prob = TRUE)
  fit$outcome_names <- colnames(y)
  fit$fixed_names <- map(x, colnames)
  fit
}

# Get the random effects from the horrible fit object
pull_random <- function(fit) {
  map_dfr(
    1:2,
    ~ fit[[.x]]$mixture_b %>%
      as_tibble() %>%
      select(-contains("Corr")) %>%
      rename_with(
        function(x) {
          paste(
            fit$outcome_names, "intercept",
            tolower(x) %>%
              str_replace("^b\\.", "") %>%
              str_replace("\\.\\d$", ""),
            sep = "_"
          )
        }
      ) %>%
      mutate(chain = .x, iteration = row_number())
  )
}

# Get the fixed effects from the horrible fit object
pull_fixed <- function(fit) {
  map_dfr(
    1:2,
    ~ fit[[.x]]$alpha %>%
      as_tibble() %>%
      rename_with(
        function(x) {
          imap(fit$fixed_names, ~ paste(.y, .x, sep = "_")) %>%
            flatten() %>%
            as.character()
        }
      ) %>%
      mutate(chain = .x, iteration = row_number())
  )
}

lengthen_estimates <- function(data) {
  data %>%
    pivot_longer(
      c(-chain, -iteration),
      names_to = c("virus", "parameter"),
      values_to = "estimate",
      names_pattern = "([^_]*)_(.*)"
    )
}

tidy_mixak <- function(fit) {
  bind_rows(
    pull_fixed(fit) %>% lengthen_estimates() %>% mutate(effect = "fixed"),
    pull_random(fit) %>% lengthen_estimates() %>% mutate(effect = "random")
  )
}

# Script ======================================================================

titres <- inner_join(read_data("titre"), read_data("virus"), by = "virus")

titres_wide <- titres %>%
  mutate(
    across(c(study_year, visit), factor),
    logtitremid = if_else(titre == 5, log(5), log(titre) + log(2) / 2)
  ) %>%
  select(id, study_year, visit, short, logtitremid) %>%
  pivot_wider(names_from = "short", values_from = logtitremid) %>%
  mutate(yearvisit = paste(study_year, visit, sep = "v"))

fit <- fit_mixak(titres_wide, c(Mich45, Switz80), ~yearvisit)

fit %>%
  tidy_mixak() %>%
  group_by(virus, parameter, effect) %>%
  summarise(mean_est = mean(estimate), .groups = "drop")
