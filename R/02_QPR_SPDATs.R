# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

# this script uses the HMIS data to compare the avg SPDAT score of
# clients served in a county during the reporting period to the avg
# SPDAt score of those who enrolled into a PSH or RRH project during the
# reporting period.
qpr_spdats <- function(Enrollment_extra_Client_Exit_HH_CL_AaE,
                       Project,
                       clarity_api = get_clarity_api(e = rlang::caller_env()),
                       app_env = get_app_env(e = rlang::caller_env())

) {
if (is_app_env(app_env))
  app_env$set_parent(missing_fmls())

# more paring down, only taking what variables I need from Enrollment
enrollment_small <- Enrollment_extra_Client_Exit_HH_CL_AaE %>%
  dplyr::left_join(Project, by = c("ProjectType", "ProjectID", "ProjectName")) %>%
  dplyr::select(
    EnrollmentID,
    UniqueID,
    PersonalID,
    ProjectID,
    ProjectType,
    ProjectName,
    OperatingStartDate,
    OperatingEndDate,
    EntryDate,
    ExitDate,
    RelationshipToHoH,
    CountyServed
  )
# Entries will give us all the times a hh has an Entry into a PH project
Entries <- enrollment_small %>%
  dplyr::filter(ProjectType %in% data_types$Project$ProjectType$ph)
qpr_note <- list()
qpr_note$served_county <- "The horizontal lines represent the average scores of Heads of Household who were served in the County in a ES, TH, SH, or Outreach project during the reporting period and who were scored. If a Head of Household was served in a County outside the Balance of State or if that data was missing, they are not being counted. When there are multiple project entries for the same client, this only counts the most recent entry. When there are multiple scores, this only counts the most recent score. There should not be more than 1 score on the same day, but if there are it is counting the highest score."

# this object is used in the app to create the plot. it has date variables
# included so the numbers can be filtered by date range in the app. it takes
# long to run.

qpr_spdats_county <-
  dplyr::left_join(enrollment_small, Scores, by = UU::common_names(enrollment_small, Scores)) %>%
  dplyr::filter(
    ProjectType %in% c(1, 2, 4, 8) &
      RelationshipToHoH == 1 &
      ScoreDate <= EntryDate &
      !CountyServed %in% c(
        "Montgomery",
        "Cuyahoga",
        "Mahoning",
        "Lucas",
        "Stark",
        "Summit",
        "Hamilton",
        "Franklin",
        "--Outside of Ohio--"
      ) &
      !is.na(CountyServed)
  ) %>%
  dplyr::select(
    EnrollmentID,
    UniqueID,
    PersonalID,
    ProjectName,
    EntryDate,
    ExitDate,
    CountyServed,
    ScoreDate,
    Score
  ) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::slice_max(EntryDate) %>% # most recent EE
  dplyr::slice_max(ScoreDate) %>% # most recent score
  dplyr::slice_max(Score) %>% # highest score
  dplyr::ungroup() %>%
  dplyr::select(PersonalID,
                UniqueID,
         ProjectName,
         CountyServed,
         Score,
         EntryDate,
         ExitDate)

qpr_note$housed_county <- "The triangle represents the average score of each household entering into a permanent housing project in a County during the reporting period. This will necessarily leave out households coming from Domestic Violence shelters since they are not scored. Any Heads of Household who entered a permanent housing project without a score will be counted as having a score of 0."

qpr_note$dq_community_need <- "It is very important that your Duplicate Entry Exits and your Household Data Quality tabs are totally clear for this report to be accurate. It is also important that your VI-SPDAT scores are ON THE HEAD OF HOUSEHOLD'S RECORD. Any scores recorded on non-HoHs will not be counted here.  Also if a HoH is missing their County data or they were served in a County outside the Ohio Balance of State, they will also not show here."

# this pulls all entries into PSH or RRH
entry_scores <- dplyr::left_join(Entries, Scores, by = UU::common_names(Entries, Scores))

qpr_spdats_project <- entry_scores %>%
  dplyr::select(-ProjectType,
         -OperatingStartDate,
         -OperatingEndDate) %>%
  dplyr::filter(
    RelationshipToHoH == 1 &
      (ScoreDate <= EntryDate | is.na(ScoreDate)) &
      !CountyServed %in% c(
        "Montgomery",
        "Cuyahoga",
        "Mahoning",
        "Lucas",
        "Stark",
        "Summit",
        "Hamilton",
        "Franklin",
        "Outside of Ohio"
      ) &
      !is.na(CountyServed)
  ) %>%
  dplyr::mutate(
    ScoreAdjusted = dplyr::if_else(is.na(Score), 0, Score),
    ScoreDateAdjusted = dplyr::if_else(is.na(ScoreDate), lubridate::today(), ScoreDate)
  ) %>%
  dplyr::group_by(EnrollmentID) %>%
  dplyr::slice_max(ScoreDateAdjusted) %>%
  dplyr::slice_max(ScoreAdjusted) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::select(-ScoreDateAdjusted)

# If you have clients here, you should either verify the scores saved here are
# valid or the correct client is marked as the Head of Household.

SPDATsOnNonHoHs <- entry_scores %>%
  HMIS::served_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
  dplyr::filter(RelationshipToHoH != 1 &
           !is.na(Score)) %>%
  dplyr::select(ProjectName, PersonalID, UniqueID, EntryDate, ExitDate, Score) %>%
  dplyr::arrange(ProjectName)

rm(Entries, enrollment_small, SPDATsOnNonHoHs)
# WARNING save.image does not save the environment properly, save must be used.
app_env$gather_deps(ls(pattern = "(?:^qpr)"))

}
