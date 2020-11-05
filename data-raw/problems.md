# Serology 2015

- Case 11 changes id:

R1105SV1
R1101SV2
R1101SV3
R1101SV4

- ID R1105S is present in serology but not in the survey

- ID R1906W has 2 entries for visit 3

# Survey 2017+

- A number of subjects are under 18 (as low as 1).
  Don't know if this is how it's meant to be.

# Serology 2017

- Cases 19 and 20 repeat (i.e. it goes ..., 17, 18, 19, 20, 19, 20, 21, ...)

- ID K112H5 has 2 entries for visit 1

# Serology 2018

- Everyone with visit 5+ is implied to have had visits 1-4 in 2017. The
  following ids have visits labelled as 5+ but no corresponding entry in 2017
  serology data:

"5K-24-1-H08" "5K-24-1-H09" "5K-24-1-H10" "5K-24-1-H11" "5T-24-3-H18" "5T-24-3-H19" "5T-24-3-H20"
"5T-24-3-H21" "5T-24-3-H22" "5T-24-3-H23" "5T-24-3-H24" "5T-24-3-H25" "5T-24-3-H26" "5T-24-3-H27"
"5T-24-3-H28" "5T-24-3-H29" "5T-24-3-H30" "5T-24-7-H01"

# Serology in general

- Age and gender columns are not necessary because they repeat themeselves and
  they repeat information that's already in the survey.
  Their presense in serology
  introduces the possibility of conflicts due to data entry errors and is
  therefore detrimental to data integrity.

- Virus names are coded inconsistently between different years, for example:

A/duck Cambodia/33W2M3/2013 (2015 data)
A/duck/ Cambodia/33W2M3/2013 (2017 data)
A/duck Cambodia/33W2M3/2013 (2018 data)

B/Phuket/3073/2013-like virus (2015 data)
B/Phuket/3073/2013-like (2017 data)
B/Phuket/3073/2013-like virus (2018 data)

- "No serum" is sometimes "No serun" (e.g. in 2017 data)

- ID's have the last number left-padded with zeros in the 2018 data but not
  in the 2017 data nor in the survey
  (e.g. 1K-22-1-H1 (2017) -> 1K-22-1-H01 (2018, survey))
