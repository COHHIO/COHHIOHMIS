# Destinations Groups (FY2020) --------------------------------------------

destinations = list(
  perm = c(3, 10, 11, 19:23, 26, 28, 31, 33, 34, 36),
  temp = c(1, 2, 12, 13, 14, 16, 18, 27, 32, 35),
  institutional = c(4:7, 15, 25, 27, 29),
  other = c(8, 9, 17, 24, 30, 37, 99)
)

# Project Groupings -------------------------------------------------------
#TODO
# GPD_project_ids <- c(751, 776, 749, 1229, 127, 550)
#
# fake_projects <- c(1027, 1849, 1028, 1033, 1032, 1029, 1931, 1030, 1031, 1317)

col_cats = list(Client = list(
  gender = c(
    "Female",
    "Male",
    "NoSingleGender",
    "Transgender",
    "Questioning",
    "GenderNone"
  ),
  race = c(
    "AmIndAKNative",
    "Asian",
    "BlackAfAmerican",
    "NativeHIPacific",
    "White",
    "RaceNone",
    "Ethnicity"
  )
))

# Project Type Groupings --------------------------------------------------

project_types = list(
  es = 1,
  th = 2,
  sh = 8,
  lh = c(1, 2, 8),
  lh_hp = c(1, 2, 8, 12),
  lh_so_hp = c(1, 2, 4, 8, 12),
  lh_at_entry = c(1, 2, 3, 4, 8, 9, 13),
  lh_ph_hp = c(1, 2, 3, 4, 8, 9, 12, 13),
  ph = c(3, 9, 13),
  psh = 3,
  rrh = 13,
  coc_funded = c(2, 3, 13),
  w_beds = c(1, 2, 3, 8, 9),
  ap = c(14)
)
