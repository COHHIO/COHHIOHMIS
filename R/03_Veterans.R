# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
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

vets <- function(Client,
                 clarity_api,
                 app_env,
                 e = rlang::caller_env()

) {
if (missing(clarity_api))
  clarity_api <- get_clarity_api(e = e)
if (missing(app_env))
  app_env <- get_app_env(e = e)
Rm_env$set_parent(missing_fmls())

# getting all the veterans
Veterans <- Client %>%
  dplyr::filter(VeteranStatus == 1) %>%
  dplyr::select(PersonalID, dplyr::all_of(c(col_cats$Client$gender, col_cats$Client$race)))

# getting all the EE data of all the veterans
VeteranHHs <- Veterans %>%
  dplyr::left_join(Enrollment_extra_Client_Exit_HH_CL_AaE, by = c("PersonalID")) %>%
  dplyr::select(
    PersonalID,
    UniqueID,
    ProjectID,
    EnrollmentID,
    EntryDate,
    HouseholdID,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    DateOfEngagement,
    MoveInDate,
    VAMCStation,
    CountyServed,
    CountyPrior,
    ExitDate,
    Destination,
    OtherDestination,
    ExitAdjust,
    AgeAtEntry
  )

# adding in all the provider data
VeteranHHs <- Project %>%
  dplyr::select(
    ProjectID,
    OrganizationName,
    OperatingStartDate,
    OperatingEndDate,
    ProjectType,
    GrantType,
    ProjectName,
    ProjectRegion
  ) %>%
  dplyr::right_join(VeteranHHs, by = "ProjectID")

VeteranHHs <- VeteranHHs %>%
  dplyr::left_join(VeteranCE, by = c("PersonalID", "EnrollmentID", "UniqueID"))

CurrentVeterans <- VeteranHHs %>%
  dplyr::filter((ProjectType %in% c(1, 2, 4, 8, 12) & (
    EntryDate <= Sys.Date() &
      (is.na(ExitDate) | ExitDate > Sys.Date())
  )) |
    (ProjectType %in% c(3, 9, 13) & (
      MoveInDate <= Sys.Date() &
        (is.na(ExitDate) |
           ExitDate > Sys.Date())
    )))

CurrentVeteranCounts <- CurrentVeterans %>%
  dplyr::filter(ProjectType %in% c(1, 2, 4, 8)) %>%
  dplyr::mutate(ProjectRegion = dplyr::if_else(is.na(ProjectRegion),
                                 "Balance of State",
                                 ProjectRegion)) %>%
  dplyr::group_by(ProjectName, ProjectRegion) %>%
  dplyr::summarise(Veterans = dplyr::n()) %>%
  dplyr::ungroup()

VeteranEngagement <- CurrentVeterans %>%
  dplyr::filter(ProjectType %in% c(1, 2, 4, 8)) %>%
  dplyr::mutate(
    EngagementStatus = dplyr::case_when(
      !is.na(PHTrack) & PHTrack != "None" &
        ExpectedPHDate >= Sys.Date() ~ "Has Current Housing Plan",
      is.na(PHTrack) | PHTrack == "None" |
        (!is.na(PHTrack) & (
          ExpectedPHDate < Sys.Date() |
            is.na(ExpectedPHDate)
        )) ~ "No Current Housing Plan"
    ),
    ProjectRegion = dplyr::if_else(is.na(ProjectRegion),
                            "Balance of State",
                            ProjectRegion)
  ) %>%
  dplyr::select(ProjectName, ProjectType, ProjectRegion, PersonalID, PHTrack,
         ExpectedPHDate, EngagementStatus)

veteran_current_in_project <- VeteranEngagement %>%
  dplyr::group_by(ProjectName, ProjectType, ProjectRegion, EngagementStatus) %>%
  dplyr::summarise(CurrentVeteranCount = dplyr::n()) %>%
  dplyr::ungroup() %>%
  tidyr::spread(key = EngagementStatus, value = CurrentVeteranCount) %>%
  dplyr::rename(HasCurrentHousingPlan = `Has Current Housing Plan`,
         NoCurrentHousingPlan = `No Current Housing Plan`)

veteran_current_in_project[is.na(veteran_current_in_project)] <- 0

veteran_current_in_project <- veteran_current_in_project %>%
  dplyr::mutate(
    Summary =
      dplyr::case_when(
        HasCurrentHousingPlan == 0 &
          NoCurrentHousingPlan == 1 ~
          "This veteran has no current Housing Plan",
        HasCurrentHousingPlan == 0 &
          NoCurrentHousingPlan > 1  ~
          "None of these veterans have current Housing Plans",
        HasCurrentHousingPlan == 1 &
          NoCurrentHousingPlan == 0 ~
          "This veteran has a current Housing Plan!",
        HasCurrentHousingPlan > 1 &
          NoCurrentHousingPlan == 0  ~
          "All veterans in this project have current Housing Plans!",
        HasCurrentHousingPlan == 1 &
          NoCurrentHousingPlan > 0 ~
          paste(HasCurrentHousingPlan,
                "of these veterans has a current Housing Plan"),
        HasCurrentHousingPlan > 1 &
          NoCurrentHousingPlan > 0 ~
          paste(HasCurrentHousingPlan,
                "of these veterans have current Housing Plans")
      )
  ) %>%
  dplyr::left_join(CurrentVeteranCounts, by = c("ProjectName", "ProjectRegion")) %>%
  dplyr::ungroup()

current_tay_hohs <- tay %>%
  dplyr::filter(RelationshipToHoH == 1 &
           is.na(ExitDate) &
           ProjectType %in% c(1, 2, 4, 8)) %>%
  dplyr::group_by(ProjectName, ProjectType) %>%
  dplyr::summarise(TAYHHs = sum(TAY)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(tay, by = c("ProjectName", "ProjectType")) %>%
  dplyr::filter(RelationshipToHoH == 1 &
           is.na(ExitDate) &
           ProjectType %in% c(1, 2, 4, 8)) %>%
  dplyr::select(PersonalID,
         EnrollmentID,
         ProjectName,
         ProjectType,
         TAYHHs) %>%
  dplyr::left_join(
    VeteranCE %>%
      dplyr::select(PersonalID,
             EnrollmentID,
             PHTrack,
             ExpectedPHDate),
    by = c("PersonalID", "EnrollmentID")
  ) %>%
  dplyr::mutate(HasPlan = dplyr::if_else(
    !is.na(PHTrack) & PHTrack != "None" &
      !is.na(ExpectedPHDate) &
      ExpectedPHDate >= Sys.Date(),
    1,
    0
  )) %>%
  dplyr::group_by(ProjectName, ProjectType, TAYHHs, HasPlan) %>%
  dplyr::summarise(HasPlan = sum(HasPlan)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Summary =
      dplyr::case_when(
        HasPlan == 0 &
          TAYHHs == 1 ~
          "This Transition Aged Youth household has no current Housing Plan",
        HasPlan == 0 &
          TAYHHs > 1  ~
          "None of these Transition Aged Youth households have current Housing Plans",
        HasPlan == 1 &
          TAYHHs == 1 ~
          "This Transition Aged Youth household has a current Housing Plan!",
        HasPlan == TAYHHs &
          HasPlan > 0 ~
          "All Transition Aged Youth households in this project have current Housing Plans!",
        HasPlan == 1 &
          HasPlan != TAYHHs ~
          paste(
            HasPlan,
            "of these Transition Aged Youth households has a current Housing Plan"
          ),
        HasPlan > 1 &
          HasPlan != TAYHHs ~
          paste(
            HasPlan,
            "of these Transition Aged Youth households have current Housing Plans"
          )
      )
  )

rm(Veterans, CurrentVeterans, VeteranEngagement, VeteranHHs, CurrentVeteranCounts)

app_env$gather_deps()
app_env
}
