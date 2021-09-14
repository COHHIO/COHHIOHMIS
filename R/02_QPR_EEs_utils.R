qpr_project_small <- function(Project, rm_dates, app_env = get_app_env(e = rlang::caller_env())) {
  Project |>
    dplyr::select(ProjectID,
                  OrganizationName,
                  OperatingStartDate,
                  OperatingEndDate,
                  ProjectName,
                  ProjectType,
                  HMISParticipatingProject,
                  GrantType,
                  ProjectCounty,
                  ProjectRegion) |>
    HMIS::operating_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(HMISParticipatingProject == 1 &
                    !is.na(ProjectRegion) &
                    ProjectType %in% c(1:4, 8:9, 12:14)) |>
    dplyr::mutate(
      FriendlyProjectName = ProjectName)
}

qpr_enrollment_small <- function(Enrollment_extra_Exit_HH_CL_AaE, app_env = get_app_env(e = rlang::caller_env())) {
  Enrollment_extra_Exit_HH_CL_AaE |>
    dplyr::select(
      EnrollmentID,
      PersonalID,
      HouseholdID,
      ProjectID,
      RelationshipToHoH,
      CountyServed,
      EntryDate,
      MoveInDate,
      ExitDate,
      EntryAdjust,
      MoveInDateAdjust,
      ExitAdjust,
      LivingSituation,
      Destination,
      DateCreated
    )
}



qpr_validation <- function(project_small, enrollment_small, app_env = get_app_env(e = rlang::caller_env())) {
  project_small |>
    dplyr::left_join(enrollment_small, by = "ProjectID") |>
    dplyr::select(
      ProjectID,
      ProjectName,
      ProjectType,
      CountyServed,
      EnrollmentID,
      PersonalID,
      HouseholdID,
      RelationshipToHoH,
      EntryDate,
      EntryAdjust,
      MoveInDate,
      MoveInDateAdjust,
      ExitDate,
      LivingSituation,
      Destination,
      DateCreated
    ) |>
    dplyr::filter(!is.na(EntryDate))
}

