

read_data <- function(name) {
  col_types_common <- list()
  col_types_specific <- list()
  virus_order <- c(
    # A
    ## H1
    "A/California/7/2009-like",
    "A/Michigan/45/2015pdm09-like",
    ## H3
    "A/Switzerland/9715293/2013-like",
    "A/Hong Kong/4801/2014-like",
    "A/Singapore/INFIMH-16-0019/2016-like",
    "A/Switzerland/8060/2017-like",
    # B
    ## Vic
    "B/Brisbane/60/2008-like",
    "B/Colorado/6/2017-like",
    ## Yam
    "B/Phuket/3073/2013-like",
    ## H5
    "A/duck/Cambodia/33W2M3/2013",
    "A/chicken/Cambodia/Z89W11M1/2015",
    "A/chicken/Cambodia/a27W9M1/2016",
    "A/chicken/Cambodia/b0426502/2017",
    "A/chicken/Cambodia/9T-24-1-C4/2018",
    "A/duck/Cambodia/c14T241D4/2019",
    ## H7
    "A/duck/Cambodia/12T-24-1-D3-p1e2/2018",
    ## H9
    "A/chicken/Cambodia/a38W9M1/2016",
    "A/chicken/Cambodia/B18W4M1/2017",
    "A/chicken/Cambodia/11K-22-3-C2/2018"
  )
  if (name == "titre") {
    col_types_specific <- list(
      virus = col_factor(virus_order)
    )
  }
  else if (name == "virus") {
    col_types_specific <- list(
      virus = col_factor(virus_order),
      haem = col_factor(c("H1", "H3", "H5", "H7", "H9", "BVic", "BYam"))
    )
  }
  else if (name == "subject") {
    col_types_specific <- list(
      slaughter = col_factor(c("Never", "Sometimes", "Everyday"))
    )
  }
  col_types <- c(col_types_common, col_types_specific)
  read_csv(glue::glue("data/{name}.csv"), col_types = do.call(cols, col_types))
}
