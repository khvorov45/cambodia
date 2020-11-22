Inconsistent ids (2015 vs 17/18) lead to the necessity to split
tables by year (`study_year`).

# subject

- `id` [chr] Unique ID within `study_year`
- `gender` [chr] "Male" or "Female"
- `age_years` [num] Age in years at visit 1 for that year. Date of birth is also
  present in the survey but more people responded to age, so using this instead.
- `study_year` [num] Study year (e.g. 2015)

# virus

- `virus` [chr] Virus name (e.g. A/chicken/Cambodia/Z89W11M1/2015)
- `subtype` [chr] Virus subtype (e.g. H5N1)
- `clade` [chr] Virus clade (e.g. Cl1.1.2). Wasn't present for all viruses, so
  some are missing

# titre

- `id` [chr] Unique id for that year/visit
- `study_year` [num] Study year (e.g. 2015)
- `visit` [num] Visit number (e.g. 1)
- `virus` [chr] Virus name (e.g. A/chicken/Cambodia/Z89W11M1/2015)
- `titre` [num] Titre (e.g. 20)

# animal-possession

This was asked once a year

- `id` [chr] Unique id for that year
- `study-year` [num] Study year (e.g. 2015)
- `animal` [chr] Animal (e.g. chicken)
- `count` [num] Count (heads)

# animal-sale

Also asked once a year. 2017+ asks "how much do you sell/process".
2015 has 2 questions:
"how much do you sell" and "how much do you prepare".
More people answered "sell" so I'm using that.

- `id` [chr] Unique id for that year
- `study-year` [num] Study year (e.g. 2015)
- `animal` [chr] Animal (e.g. chicken)
- `type` [chr] head/kg
- `from` [num] From (input was a range)
- `to` [num] To (input was a range)
- `mid` [num] (`from` + `to`) / 2
