source("cluster/cluster.R")

future::plan(future::multisession)

opt_cl <- furrr::future_map_dfr(
  1:4,
  ~ fit_mixak(
    titres_wide, c(Mich45, Switz80), ~yearvisit,
    n_clusters = .x,
    burn = 1000,
    keep = 5000,
  ) %>%
    pull_diagnostics()
)

save_data(opt_cl, "opt-cluster-num")
