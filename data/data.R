cat("extract data for analysis")

library(tidyverse)

# Functions ===================================================================

read_raw <- function(name, ext = "xlsx", ...) {
  path <- glue::glue("data-raw/{name}.{ext}")
  if (ext == "xlsx") {
    return(readxl::read_excel(path, ...))
  }
  if (ext == "dta") {
    return(haven::read_dta(path, ...))
  }
  read_csv(path, ...)
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

check_no_duplicates <- function(data, var_name, ...) {
  var_name_q <- rlang::enquo(var_name)
  data %>%
    group_by(!!var_name_q, ...) %>%
    filter(n() != 1) %>%
    group_by(...) %>%
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

fix_clades <- function(clades) {
  clades %>%
    str_replace("^.*\\s+\\((.*)\\s*$", "(\\1") %>%
    str_replace_all("\\(|\\)", "") %>%
    str_replace_all("/|_", " ") %>%
    str_replace("B Vic", "BVic") %>%
    str_replace("B Yam", "BYam")
}

extract_haem <- function(subtype) {
  if_else(
    str_starts(subtype, "H"), str_extract(subtype, "H\\d"), subtype
  )
}

extract_titres <- function(data) {
  data %>%
    select(id, visit, contains("A/"), contains("B/")) %>%
    lengthen_titres() %>%
    mutate(
      titre_og = titre,
      titre = fix_titres(titre_og),
      virus_og = virus,
      virus = fix_virus_names(virus_og),
      clade_og = fix_clades(virus_og),
      subtype = str_split(clade_og, " ") %>% map_chr(1),
      clade = str_split(clade_og, " ") %>%
        map_chr(~ pluck(.x, 2, .default = NA_character_)),
      haem = extract_haem(subtype),
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

# Survey

responses_2015 <- read_raw("responses-2015", "dta") %>%
  select(
    id = idcode2015,
    gender = n12gender,
    age_years = n10ageyear,
    workplace = n8workplac,
    # Number of poultry stored
    contains("n42"),
    contains("n43"),
    # How much do you prepare?
    contains("n32"),
    contains("n33"),
    contains("n34"),
    contains("n35"),
    # How much do you sell?
    contains("n49"),
    contains("n50"),
    contains("n51"),
    contains("n52"),
    # Do you participate in the poultry slaughtering process?
    slaughter = n22doyoupa,
  ) %>%
  mutate(
    across(c(gender, slaughter, workplace), ~ as_factor(.x, levels = "labels"))
  )

# Extract subjects

subjects_2015 <- responses_2015 %>%
  select(id, gender, age_years, slaughter, workplace) %>%
  mutate(
    study_year = 2015,
    # The questionnaire assumes everyone works with poultry
    sector = "poultry"
  )

# Should be no duplicate ids
subjects_2015 %>% check_no_duplicates(id)

# Check gender
subjects_2015$gender %>% unique()

# Check age
subjects_2015$age_years %>% summary()

# Check slaughter
subjects_2015$slaughter %>% unique()

# Check workplace
subjects_2015$workplace %>% unique()

# Should be no missing data
subjects_2015 %>% filter(!complete.cases(.))

# Animal possession

animals_2015 <- responses_2015 %>%
  select(id, contains("n42"), contains("n43")) %>%
  pivot_longer(-id, names_to = "code_og", values_to = "count") %>%
  mutate(
    animal = case_when(
      str_detect(code_og, "n42") ~ "chicken",
      str_detect(code_og, "n43") ~ "duck"
    ),
    bound = if_else(str_length(code_og) == 3, "to", "from")
  ) %>%
  select(-code_og) %>%
  pivot_wider(names_from = "bound", values_from = "count") %>%
  filter(!is.na(to), !is.na(from), to > 0) %>%
  mutate(count = round((from + to) / 2)) %>%
  select(-from, -to) %>%
  mutate(study_year = 2015)

animals_2015 %>% check_no_duplicates(id, animal)
animals_2015 %>% filter(!id %in% subjects_2015$id)

# Animal processing

animal_prepare_2015 <- responses_2015 %>%
  select(
    id,
    contains("n32"),
    contains("n33"),
    contains("n34"),
    contains("n35"),
  ) %>%
  pivot_longer(-id, names_to = "code_og", values_to = "number") %>%
  mutate(
    animal = if_else(
      str_detect(code_og, "n3[2|3]"), "chicken", "duck"
    ),
    type = if_else(str_detect(code_og, "n3[3|5]"), "kg", "head"),
    bound = if_else(str_length(code_og) == 3, "to", "from"),
    study_year = 2015,
  ) %>%
  select(-code_og) %>%
  pivot_wider(names_from = "bound", values_from = "number") %>%
  filter(!is.na(from), !is.na(to), to > 0)

animal_prepare_2015 %>% check_no_duplicates(id, animal, type)
animal_prepare_2015 %>% filter(!id %in% subjects_2015$id)

animal_sale_2015 <- responses_2015 %>%
  select(
    id,
    contains("n49"),
    contains("n50"),
    contains("n51"),
    contains("n52"),
  ) %>%
  pivot_longer(-id, names_to = "code_og", values_to = "number") %>%
  mutate(
    animal = if_else(
      str_detect(code_og, "n5[1|2]"), "duck", "chicken"
    ),
    type = if_else(str_detect(code_og, "n5[0|2]"), "kg", "head"),
    bound = if_else(str_length(code_og) == 3, "to", "from"),
    study_year = 2015,
  ) %>%
  select(-code_og) %>%
  pivot_wider(names_from = "bound", values_from = "number") %>%
  filter(!is.na(from), !is.na(to), to > 0)

animal_sale_2015 %>% check_no_duplicates(id, animal, type)
animal_sale_2015 %>% filter(!id %in% subjects_2015$id)

# Serology

serology_2015 <- read_raw(
  "serology",
  sheet = "2015_Result S1-S4", range = "A4:P509"
) %>%
  select(
    id_visit = `ID Code`,
    visit = `Serum Visit`,
    contains("A/"),
    contains("B/")
  ) %>%
  mutate(
    # Create an individual id
    id = str_replace(id_visit, "V\\d$", ""),
    # Make visit numeric, warning will be generated if non-numbers are left
    visit = str_replace(visit, "^V", "") %>% as.integer(),
    # Convert titres to character to be fixed later
    across(c(contains("A/"), contains("B/")), as.character)
  )

# Check visit
unique(serology_2015$visit)

# Titres
titres_2015 <- serology_2015 %>%
  extract_titres() %>%
  mutate(study_year = 2015)

# Ids should correspond to subjects
titres_2015 %>% filter(!id %in% subjects_2015$id)

# Check that there is no missing data
titres_2015 %>% filter(!complete.cases(.))

# No repeats for visit/virus
titres_2015 %>% check_no_duplicates(id, visit, virus)

# 2017 Onwards ----------------------------------------------------------------

# Survey

responses_2017_plus <- read_raw("responses-2017", "dta") %>%
  select(
    id = idcode,
    age_years = n10ageyear,
    gender = n12gender,
    date_interview = n1intervie,
    workplace = n8workplac,
    sector = n15sector,
    # What animals are present?
    contains("n18"),
    # How many do you sell/process
    contains("n22"),
    # Do you participate in poultry/pig slaughtering process?
    slaughter = n49doyoupa,
  ) %>%
  mutate(
    # Remove visit indicator from id
    id_og = id,
    id = str_replace(id, "^\\d+", ""),
    across(
      c(gender, slaughter, workplace, sector),
      ~ as_factor(.x, levels = "labels")
    ),
    study_year = lubridate::year(date_interview),
    sector = sector %>%
      tolower() %>%
      str_replace("business", "") %>%
      str_trim()
  )

subjects_2017_plus <- responses_2017_plus %>%
  select(id, age_years, gender, slaughter, workplace, sector, study_year)

# Check missing data
subjects_2017_plus %>% filter(!complete.cases(.))

# Check gender
unique(subjects_2017_plus$gender)

# Check age
summary(subjects_2017_plus$age_years)

# Check slaughter
unique(subjects_2017_plus$slaughter)

# Check workplace
responses_2017_plus$workplace %>% unique()

# Check sector
responses_2017_plus$sector %>% unique()

# Check that ids don't repeat within a year
subjects_2017_plus %>% check_no_duplicates(id, study_year)

# Animal posession

animals_preset_2017_plus <- responses_2017_plus %>%
  select(id, study_year, contains("n18")) %>%
  select(-contains("other")) %>%
  pivot_longer(contains("n18"), names_to = "animal", "values_to" = "count") %>%
  filter(!is.na(count), count > 0) %>%
  mutate(
    animal = animal %>%
      str_replace("^n18\\w", "") %>%
      recode("chicke" = "chicken", "buffal" = "buffalo")
  )

animals_preset_2017_plus %>% check_no_duplicates(id, animal, study_year)

animals_other_2017_plus <- responses_2017_plus %>%
  select(id, study_year, contains("n18")) %>%
  select(id, study_year, contains("other")) %>%
  rename(animal = n18fother, count = n18faother) %>%
  filter(!is.na(count), !animal %in% c("", "0"), count > 0)

animals_other_2017_plus %>% check_no_duplicates(id, animal, study_year)

# Animal processing
animal_sale_2017_plus <- responses_2017_plus %>%
  select(id, study_year, contains("n22")) %>%
  pivot_longer(contains("n22"), names_to = "code_og", values_to = "number") %>%
  filter(!is.na(number)) %>%
  mutate(
    animal = case_when(
      str_starts(code_og, "n22[a|b]") ~ "chicken",
      str_starts(code_og, "n22[c|d]") ~ "duck",
      str_starts(code_og, "n22[e|f]") ~ "pig",
    ),
    type = if_else(str_starts(code_og, "n22[a|c|e]"), "head", "kg"),
    bound = if_else(str_length(code_og) == 4, "to", "from"),
  ) %>%
  select(-code_og) %>%
  pivot_wider(names_from = "bound", values_from = "number") %>%
  filter(to > 0)

animal_sale_2017_plus %>% check_no_duplicates(id, study_year, animal, type)
animal_sale_2017_plus %>% filter(to < from)
animal_sale_2017_plus %>% filter(!id %in% subjects_2017_plus$id)

# Serology

read_serology_2017_plus <- function(sheet, range) {
  read_raw(
    "serology",
    sheet = sheet, range = range
  ) %>%
    select(
      id_sample = `Sample Code_V1`,
      contains("A/"),
      contains("B/")
    ) %>%
    mutate(
      # Extract visit from id
      id = str_replace(id_sample, "^\\d", "") %>%
        toupper() %>%
        str_replace_all("-", ""),
      visit = str_replace(id_sample, "^(\\d).*", "\\1") %>% as.integer(),
      # Convert titres to character
      across(c(contains("A/"), contains("B/")), as.character),
    )
}

serology_2017 <- read_serology_2017_plus("2017-2018_Results S1-S4", "A4:S756")

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

# Check visit
unique(serology_2018$visit)

# Extract 2017+ titres

titres_2017 <- serology_2017 %>%
  extract_titres() %>%
  mutate(study_year = 2017)

titres_2018 <- serology_2018 %>%
  extract_titres() %>%
  mutate(study_year = 2018)

# Ids should correspond to subjects
titres_2017 %>% filter(!id %in% subjects_2017_plus$id)
titres_2018 %>% filter(!id %in% subjects_2017_plus$id)

# No repeats for visit/virus
titres_2017 %>% check_no_duplicates(id, visit, virus)
titres_2018 %>% check_no_duplicates(id, visit, virus)

# Combine ---------------------------------------------------------------------

# Combine all participants
subjects <- bind_rows(list(subjects_2015, subjects_2017_plus))
# Combine all titres
titres <- bind_rows(list(titres_2015, titres_2017, titres_2018))

# Check missing data
subjects %>% filter(!complete.cases(.))
titres %>% filter(!complete.cases(.))

# All titres should correspond to a subject
titres %>% filter(!id %in% subjects$id)

# No duplicate ids in the subject table within each year
subjects %>% check_no_duplicates(id, study_year)

# No duplicates for year, visit virus
titres %>% check_no_duplicates(id, study_year, visit, virus)

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

# Extract viruses
viruses <- titres %>%
  select(virus, subtype, clade, haem) %>%
  distinct() %>%
  mutate(
    short = recode(
      virus,
      "A/chicken/Cambodia/Z89W11M1/2015" = "CambZ89",
      "A/chicken/Cambodia/a38W9M1/2016" = "CambA38",
      "A/chicken/Cambodia/11K-22-3-C2/2018" = "Camb11K",
      "A/duck/Cambodia/33W2M3/2013" = "Camb33W",
      "A/chicken/Cambodia/a27W9M1/2016" = "CambA27",
      "A/Switzerland/9715293/2013-like" = "Switz971",
      "A/California/7/2009-like" = "Cali7",
      "B/Brisbane/60/2008-like" = "Bris60",
      "B/Phuket/3073/2013-like" = "Phuk30",
      "A/chicken/Cambodia/b0426502/2017" = "CambB04",
      "A/chicken/Cambodia/B18W4M1/2017" = "CambB18",
      "A/chicken/Cambodia/9T-24-1-C4/2018" = "Camb9T",
      "A/duck/Cambodia/12T-24-1-D3-p1e2/2018" = "Camb12T",
      "A/Michigan/45/2015pdm09-like" = "Mich45",
      "A/Hong Kong/4801/2014-like" = "HKong48",
      "A/Singapore/INFIMH-16-0019/2016-like" = "Sing16",
      "A/duck/Cambodia/c14T241D4/2019" = "CambC14",
      "A/Switzerland/8060/2017-like" = "Switz80",
      "B/Colorado/6/2017-like" = "Col6",
    )
  )

# Same virus can't be in different subtype/clades
viruses %>%
  count(virus, subtype, clade) %>%
  filter(n != 1)

# Survey-derived info

animal_possession <- bind_rows(
  animals_preset_2017_plus, animals_other_2017_plus, animals_2015
)

animal_possession %>% check_no_duplicates(id, animal, study_year)
animal_possession %>% filter(!id %in% subjects$id)

animal_sale <- bind_rows(animal_sale_2017_plus, animal_sale_2015) %>%
  mutate(mid = (from + to) / 2)

animal_sale %>% check_no_duplicates(id, study_year, animal, type)
animal_sale %>% filter(to < from)
animal_sale %>% filter(!id %in% subjects$id)
animal_sale %>% filter(!complete.cases(.))

# Save

save_data(subjects, "subject")
save_data(select(titres, id, study_year, visit, virus, titre), "titre")
save_data(animal_possession, "animal-possession")
save_data(animal_sale, "animal-sale")
save_data(viruses, "virus")
