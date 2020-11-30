cat("clusters")

library(tidyverse)

# Functions ===================================================================

source("data/read_data.R")

fit_mixak <- function(data,
                      outcome, fixed_formula, n_clusters,
                      burn, keep, thin) {
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
      Kmax = n_clusters
    ),

    # MCMC settings
    nMCMC = c(burn = burn, keep = keep, thin = thin, info = 100),
    parallel = TRUE
  ) %>%
    mixAK::NMixRelabel(type = "stephens", keep.comp.prob = TRUE)
  fit$outcome_names <- colnames(y)
  fit$fixed_names <- map(x, colnames)
  fit$ids <- tibble(id = unique(data$id)) %>% mutate(index = row_number())
  fit$n_clusters <- n_clusters
  fit$mcmc_settings <- tibble(burn = burn, keep = keep, thin = thin)
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

cluster_assignment_mixak <- function(fit) {
  map_dfr(1:2, function(c) {
    fit[[c]]$comp.prob %>%
      as_tibble() %>%
      mutate(iteration = row_number()) %>%
      pivot_longer(contains("P"), names_to = "of", values_to = "prob") %>%
      mutate(
        index = str_replace(of, "^P\\((\\d+),\\d+\\)$", "\\1") %>%
          as.integer(),
        cluster = str_replace(of, "^P\\(\\d+,(\\d+)\\)$", "\\1") %>%
          as.integer(),
        chain = c,
      ) %>%
      inner_join(fit$ids, by = "index") %>%
      select(-of, -index)
  })
}

pull_diagnostics <- function(fit) {
  tibble(
    outcome = list(fit$outcome_names),
    fixed = list(fit$fixed_names),
    n_clusters = fit$n_clusters,
    ped = fit$PED[["PED"]],
  ) %>%
    bind_cols(fit$mcmc_settings)
}

save_data <- function(data, name) {
  write_csv(
    data %>%
      mutate(across(where(is.list), ~ map_chr(., jsonlite::toJSON))),
    glue::glue("cluster/{name}.csv")
  )
  data
}

# Script ======================================================================

titres <- inner_join(read_data("titre"), read_data("virus"), by = "virus")

# Find viruses that don't change
titres_no_change <- titres %>%
  group_by(short) %>%
  summarise(unique_measures = length(unique(titre)), .groups = "drop") %>%
  filter(unique_measures == 1) %>%
  pull(short)

titres_wide <- titres %>%
  # Remove viruses that don't change since they provide no information
  filter(!short %in% titres_no_change) %>%
  mutate(
    across(c(study_year, visit), factor),
    logtitremid = if_else(titre == 5, log(5), log(titre) + log(2) / 2)
  ) %>%
  select(id, study_year, visit, short, logtitremid) %>%
  pivot_wider(names_from = "short", values_from = logtitremid) %>%
  # Add yearvisit for convenience
  mutate(yearvisit = paste(study_year, visit, sep = "v"))

# Fit with different clusters
future::plan(future::multisession)
diff_cl <- furrr::future_map(
  1:4,
  ~ fit_mixak(
    titres_wide,
    # Errors occur with the inclusion of CambZ89
    c(
      CambA38, Camb11K, Camb33W, CambA27, Switz971, Cali7,
      Bris60, Phuk30, CambB04, CambB18, Camb9T, Mich45, HKong48, Sing16,
      Switz80, Col6
    ),
    ~yearvisit,
    n_clusters = .x,
    burn = 100, keep = 500, thin = 10
  )
)
diff_cl %>%
  map_dfr(pull_diagnostics) %>%
  save_data("diag")

map_with_cl <- function(.x, .f, ...) {
  map_dfr(.x, ~ .f(.x) %>% mutate(n_clusters = .x$n_clusters), ...)
}
diff_cl %>%
  map_with_cl(tidy_mixak) %>%
  save_data("parameters")

diff_cl %>%
  map_with_cl(cluster_assignment_mixak) %>%
  group_by(cluster, chain, id, n_clusters) %>%
  summarise(
    prob_mn = mean(prob),
    prob_q025 = quantile(prob, 0.025),
    prob_q25 = quantile(prob, 0.25),
    prob_q50 = quantile(prob, 0.5),
    prob_q75 = quantile(prob, 0.75),
    prob_q975 = quantile(prob, 0.975),
    .groups = "drop"
  ) %>%
  save_data("clusters")
