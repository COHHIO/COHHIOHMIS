




# Project Groupings -------------------------------------------------------

# GPD_project_ids <- c(751, 776, 749, 1229, 127, 550)
#
# fake_projects <- c(1027, 1849, 1028, 1033, 1032, 1029, 1931, 1030, 1031, 1317)

#' @title Categories of HUD CSV Columns
#' @description Currently only groupings of Client columns
#' @export
col_cats = list(Client = list(
  gender = c(
    "Woman",
    "Man",
    "NonBinary",
    "CulturallySpecific",
    "Transgender",
    "Questioning",
    "DifferentIdentity",
    "GenderNone",
    "DifferentIdentityText"
  ),
  race = c(
    "AmIndAKNative",
    "Asian",
    "BlackAfAmerican",
    "HispanicLatinaeo",
    "MidEastNAfrican",
    "NativeHIPacific",
    "White",
    "RaceNone",
    "AdditionalRaceEthnicity"
  )
))

#' @title FY 2022 HUD CSV Data groupings
#' @description This list provides common groupings of specific data elements in the HUD CSV for ease of reference
#' @seealso destinations
#' @export

data_types = list(Project = list(ProjectType = list( # formerly project_types
  es = 1,
  th = 2,
  sh = 8,
  so = 6,
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
  ap = 14,
  ce = 14
)),
CurrentLivingSituation = list(CurrentLivingSituation = list( #formerly living_situations
  homeless = c(16, 1, 18),
  likely_homeless = c(16, 1, 18, 17, 7),
  institutional = c(15, 6, 7, 25, 4, 5),
  temp_psh = c(29, 14, 2, 32, 13, 36, 12, 22, 35, 23, 26, 27, 28, 19, 13, 31, 33, 34, 10, 20, 21, 11),
  other = c(30, 17, 24, 37, 8, 9, 99)
)),
Exit = list(Destination = list( # formerly destinations
  perm = c(3, 10, 11, 19:23, 26, 28, 31, 33, 34, 36),
  temp = c(1, 2, 12, 13, 14, 16, 18, 27, 32, 35),
  institutional = c(4:7, 15, 25, 27, 29),
  other = c(8, 9, 17, 24, 30, 37, 99)
)))

#' @inherit data_types title description
#' @export
project_types <- data_types$Project$ProjectType

#' @inherit data_types title description
#' @export
living_situations <- data_types$CurrentLivingSituation$CurrentLivingSituation

#' @inherit data_types title description
#' @export
destinations <- data_types$Exit$Destination

#' Colors for the various level of housing status used in the prioritization report
#' @export
prioritization_colors <- c(
  "Housed",
  "Entered RRH",
  "Permanent Housing Track",
  "Follow-up needed",
  "No current Entry",
  "Not referred",
  "No Entry"
) |>
  {\(x) {rlang::set_names(grDevices::colorRampPalette(c("#45d63e", "#ff2516"), space = "Lab")(length(x)), x)}}()




