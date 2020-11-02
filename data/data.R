cat("extract data for analysis")

library(tidyverse)

# Functions ===================================================================

read_raw <- function(name, ...) {
  readxl::read_excel(glue::glue("data-raw/{name}.xlsx"), ...)
}

keep_first_row <- function(data, ...) {
  data %>%
    group_by(...) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

save_data <- function(data, name) {
  write_csv(data, glue::glue("data/{name}.csv"))
  data
}

paste_unique <- function(char_vec) {
  paste(unique(char_vec), collapse = ", ")
}

# Check that each group defined by v1 has one value of v2 in it
check_unique <- function(data, v1, v2) {
  data %>%
    group_by(!!rlang::enquo(v1)) %>%
    filter(length(unique(!!rlang::enquo(v2))) > 1) %>%
    summarise(reps = paste_unique(!!rlang::enquo(v2)), .groups = "drop")
}

check_no_duplicates <- function(data, var_name, group_vars = c()) {
  group_vars_q <- rlang::enquos(group_vars)
  var_name_q <- rlang::enquo(var_name)
  data %>%
    group_by(!!!group_vars_q, !!var_name_q) %>%
    filter(n() != 1) %>%
    group_by(!!!group_vars_q) %>%
    summarise(reps = paste(!!var_name_q, collapse = ", "), .groups = "drop")
}

check_range <- function(data, group_var, num_var, max_range = 1) {
  data %>%
    group_by(!!rlang::enquo(group_var)) %>%
    filter(
      max(!!rlang::enquo(num_var)) - min(!!rlang::enquo(num_var)) > max_range
    ) %>%
    summarise(nums = paste_unique(!!rlang::enquo(num_var)), .groups = "drop")
}

check_ascending <- function(data, group_var, order_var, num_var) {
  num_var_q <- rlang::enquo(num_var)
  data %>%
    group_by(!!rlang::enquo(group_var)) %>%
    mutate(num_var_prev = lag(
      !!num_var_q,
      default = -Inf,
      order_by = !!rlang::enquo(order_var)
    )) %>%
    filter(any(!!num_var_q - num_var_prev < 0)) %>%
    summarise(nums = paste_unique(!!num_var_q), .groups = "drop")
}

lengthen_titres <- function(data) {
  data %>%
    pivot_longer(
      c(contains("A/"), contains("B/")),
      names_to = "virus",
      values_to = "titre"
    )
}

fix_titres <- function(titres) {
  titres %>%
    tolower() %>%
    recode(
      ">=1280" = "2560",
      "no serum" = NA_character_,
      "0" = "5",
      ">=640" = "1280",
      "no serun" = NA_character_,
      "pending" = NA_character_,
    ) %>%
    as.integer()
}

fix_virus_names <- function(viruses) {
  viruses %>%
    str_replace("\\s*\\(.*\\)", "") %>%
    str_replace("/ ", "/") %>%
    str_replace("duck Cambodia", "duck/Cambodia") %>%
    str_replace("-like.*$", "-like")
}

extract_titres <- function(data) {
  data %>%
    select(id, visit, contains("A/"), contains("B/")) %>%
    lengthen_titres() %>%
    mutate(
      titre = fix_titres(titre),
      virus = fix_virus_names(virus)
    ) %>%
    filter(!is.na(titre))
}

extract_virus_names <- function(data, year) {
  data %>%
    filter(study_year == year) %>%
    pull(virus) %>%
    unique()
}

# Script ======================================================================

# 2015 ------------------------------------------------------------------------

serology_2015 <- read_raw(
  "serology",
  sheet = "2015_Result S1-S4", range = "A4:P510"
) %>%
  select(
    case = Case,
    id_visit = `ID Code`,
    age_years = Age,
    gender = Gender,
    visit = `Serum Visit`,
    contains("A/"),
    contains("B/")
  ) %>%
  # Case is a meged cell, only the first entry contains the value, the others
  # are NA, so fill the NA's
  fill(case) %>%
  mutate(
    case = as.character(case),
    # Create an individual id
    id = str_replace(id_visit, "V\\d$", ""),
    # Make visit numeric, warning will be generated if non-numbers are left
    visit = str_replace(visit, "^V", "") %>% as.integer(),
    # Fix age
    age_years = str_replace(age_years, "54/53", "54") %>% as.integer(),
    # Fix gender
    gender = str_replace(gender, "F\\?", "F"),
    # Convert titres to character to be fixed later
    across(c(contains("A/"), contains("B/")), as.character)
  )

# Check that ids and cases correspond
serology_2015 %>% check_unique(case, id)

# Check that age doesn't change by more than a year
serology_2015 %>% check_range(case, age_years)

# Check that age ascends with visit
serology_2015 %>% check_ascending(case, visit, age_years)

# Check that gender doesn't change
serology_2015 %>% check_unique(case, gender)

# Check that gender is either F or M
unique(serology_2015$gender)

# Extract participant info
subjects_2015 <- serology_2015 %>%
  select(id, age_years, gender) %>%
  keep_first_row(id) %>%
  mutate(study_year = 2015)

# Shouldn't be any duplicate ids
subjects_2015 %>% check_no_duplicates(id)

# Check for missing data
subjects_2015 %>% filter(!complete.cases(.))

# Titres
titres_2015 <- serology_2015 %>%
  extract_titres() %>%
  mutate(study_year = 2015)

# Ids should correspond to subjects
titres_2015 %>% filter(!id %in% subjects_2015$id)

# Check that there is no missing data
titres_2015 %>% filter(!complete.cases(.))

# 2017 Onwards ----------------------------------------------------------------

read_serology_2017_plus <- function(sheet, range) {
  read_raw(
    "serology",
    sheet = sheet, range = range
  ) %>%
    select(
      case = Case,
      id_sample = `Sample Code_V1`,
      age_years = Age,
      gender = Gender,
      contains("A/"),
      contains("B/")
    ) %>%
    # Meged cells - fill NA's
    fill(case) %>%
    mutate(
      # Extract visit from id
      id = str_replace(id_sample, "^\\d", "") %>% toupper(),
      visit = str_replace(id_sample, "^(\\d).*", "\\1") %>% as.integer(),
      # Convert titres to character
      across(c(contains("A/"), contains("B/")), as.character),
    )
}

serology_2017 <- read_serology_2017_plus("2017-2018_Results S1-S4", "A4:S756")

# Check that case and id correspond
serology_2017 %>% check_unique(case, id)

# Check that age doesn't change by more than a year
serology_2017 %>% check_range(case, age_years)

# Check that age ascends with visit
serology_2017 %>% check_ascending(case, visit, age_years)

# Check that gender doesn't change
serology_2017 %>% check_unique(case, gender)

# Check that gender is either F or M
unique(serology_2017$gender)

# Check visit
unique(serology_2017$visit)

serology_2018 <- read_serology_2017_plus(
  "2018-2019_Results S1-S4", "A4:U425"
) %>%
  mutate(
    # Make ids consistent (don't left-pad the last number with 0's)
    id_og = id,
    id = str_replace(id, "H0+(\\d+)$", "H\\1"),
    # Loop visit around 1-4, (so 5 -> 1)
    visit_og = visit,
    visit = (visit - 1) %% 4 + 1,
  )

# Check that case and id correspond
serology_2018 %>% check_unique(case, id)

# Check that age doesn't change by more than a year
serology_2018 %>% check_range(case, age_years)

# Check that age ascends with visit
serology_2018 %>% check_ascending(case, visit, age_years)

# Check that gender doesn't change
serology_2018 %>% check_unique(case, gender)

# Check that gender is either F or M
unique(serology_2018$gender)

# Check visit
unique(serology_2018$visit)

# Everyone with visit 5 is supposed to be in 2017 data
serology_2018 %>%
  group_by(case) %>%
  filter(
    any(visit_og > 4),
    !id %in% serology_2017$id,
    id_sample == first(id_sample)
  ) %>%
  pull(id_sample) %>%
  unique()

# Extract 2017+ participants

subjects_2017 <- serology_2017 %>%
  select(id, age_years, gender) %>%
  keep_first_row(id) %>%
  mutate(study_year = 2017)

subjects_2018 <- serology_2018 %>%
  select(id, age_years, gender) %>%
  keep_first_row(id) %>%
  mutate(study_year = 2018)

# Shouldn't be any duplicate ids
subjects_2017 %>% check_no_duplicates(id)
subjects_2018 %>% check_no_duplicates(id)

# Extract 2017+ titres

titres_2017 <- serology_2017 %>%
  extract_titres() %>%
  mutate(study_year = 2017)

titres_2018 <- serology_2018 %>%
  extract_titres() %>%
  mutate(study_year = 2018)

# Ids should correspond to subjects
titres_2017 %>% filter(!id %in% subjects_2017$id)
titres_2018 %>% filter(!id %in% subjects_2018$id)

# Combine ---------------------------------------------------------------------

# Combine all participants
subjects <- bind_rows(list(subjects_2015, subjects_2017, subjects_2018))
# Combine all titres
titres <- bind_rows(list(titres_2015, titres_2017, titres_2018))

# Should be no missing data
subjects %>% filter(!complete.cases(.))
titres %>% filter(!complete.cases(.))

# All titres should correspond to a subject
titres %>% filter(!id %in% subjects$id)

# No duplicate ids in the subject table withing each year
subjects %>% check_no_duplicates(id, study_year)

# Check that there is no inconsistent virus naming between years
viruses_2015 <- extract_virus_names(titres, 2015)
viruses_2017 <- extract_virus_names(titres, 2017)
viruses_2018 <- extract_virus_names(titres, 2018)

setdiff(viruses_2015, viruses_2017)
setdiff(viruses_2017, viruses_2015)

setdiff(viruses_2015, viruses_2018)
setdiff(viruses_2018, viruses_2015)

setdiff(viruses_2017, viruses_2018)
setdiff(viruses_2018, viruses_2018)

# Save

save_data(subjects, "subject")
save_data(titres, "titre")