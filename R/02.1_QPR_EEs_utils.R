qpr_project_small <- function(Project, rm_dates, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
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
                    ProjectType %in% c(1:4, 8:9, 12:14))
}

qpr_enrollment_small <- function(Enrollment_extra_Client_Exit_HH_CL_AaE, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  Enrollment_extra_Client_Exit_HH_CL_AaE |>
    dplyr::select(
      dplyr::all_of(c(
        "CountyServed",
        "DateCreated",
        "Destination",
        "EnrollmentID",
        "EntryAdjust",
        "EntryDate",
        "ExitAdjust",
        "ExitDate",
        "HouseholdID",
        "LivingSituation",
        "MoveInDate",
        "MoveInDateAdjust",
        "PersonalID",
        "ProjectID",
        "RelationshipToHoH",
        "UniqueID"
      ))
    )
}



qpr_validation <- function(project_small, enrollment_small, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
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
      DateCreated,
      UniqueID
    ) |>
    dplyr::filter(!is.na(EntryDate))
}

qpr_mental_health <- function(validation, Disabilities, app_env = get_app_env(e = rlang::caller_env())) {
  out <- dplyr::left_join(validation, dplyr::select(Disabilities, dplyr::ends_with("ID"), DisabilityType, DateUpdated), by = c("PersonalID", "EnrollmentID")) |>
    dplyr::filter(DisabilityType %in% c(9, # Mental Health
                                        10 # Substance Abuse
                                        ) &
                  CountyServed %in% c("Lake", "Lorain", "Trumbull") &
                  is.na(ExitDate) & # Assumed actively enrolled
                  LivingSituation %in% data_types$CurrentLivingSituation$CurrentLivingSituation$homeless) |>
    dplyr::distinct(UniqueID, PersonalID, LivingSituation, DisabilityType)

}


