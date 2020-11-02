read_data <- function(name) {
  read_csv(glue::glue("data/{name}.csv"), col_types = cols())
}
