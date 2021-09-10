is_clarity <- function() {
  getOption("HMIS")$Clarity
}

is_sp <- function() {
  getOption("HMIS")$ServicePoint
}

get_null_names <- function(fmls = rlang::fn_fmls(), e = rlang::caller_env()) {
  browser()
  names(fmls)[purrr::imap_lgl(fmls, ~exists(.y, e, mode = "NULL"))]
}

#' @title Funder_VA
#' @description This filters for VA Funders
#' @param x \code{(data.frame)} Funder Export Object
#' @param ids \code{(numeric)} All VA Related IDs
#'
#' @return \code{(data.frame)} with Project ID
#'
#TODO need to update IDs of VA associated projects
Funder_VA_ProjectID <- function(x, ids = c(27, 30, 33, 37:42, 45)) {
  x %>%
    dplyr::filter(Funder %in% ids) %>%
    dplyr::select(ProjectID)
}


#' @title Filter for Current HMIS participating projects
#'
#' @param Project
#' @param Inventory
#'
#' @return \code{(data.frame)}

projects_current_hmis <- function (Project,
                                   Inventory,
                                   calc,
                                   meta_HUDCSV,
                                   app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  Project %>%
    dplyr::left_join(Inventory, by = "ProjectID") |>
    HMIS::operating_between(calc$data_goes_back_to, meta_HUDCSV$Export_End) |>
    dplyr::filter(HMISParticipatingProject == 1 &
                    (GrantType != "HOPWA" | is.na(GrantType))) |>
    dplyr::select(
      ProjectID,
      OrganizationID,
      OperatingStartDate,
      OperatingEndDate,
      ProjectType,
      GrantType,
      ProjectName,
      OrganizationName,
      ProjectCounty,
      ProjectRegion
    ) |>  unique()
}

#' @title Create the data.frame of Clients to Check `served_in_date_range`
#'
#' @param projects_current_hmis \code{(data.frame)} of Providers to check. See `projects_current_hmis`
#' @param Enrollment_extra_Exit_HH_CL_AaE \code{(data.frame)} Enrollment with all additions from `load_export`
#' @param Client \code{(data.frame)} Client with all additions from `load_export`
#' @param Project \code{(data.frame)} Project with extras including Regions and GrantType see `Pe_add_regions` & `Pe_add_GrantType`.
#' @param Inventory \code{(data.frame)} Inventory
#' @param calc \code{(list)} of dates from `dates`
#' @param meta_HUDCSV \code{(list)} of dates from `dates`
#' @param app_env \code{(data.frame)} Instead of providing all arguments with NULL defaults,`app_env` with all arguments saved internally can be provided.
#'
#' @return \code{(data.frame)}


served_in_date_range <- function(projects_current_hmis, Enrollment_extra_Exit_HH_CL_AaE = NULL, Client = NULL, Project = NULL, Inventory = NULL, HealthAndDV = NULL, calc = NULL, meta_HUDCSV = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  Enrollment_extra_Exit_HH_CL_AaE  |>
    HMIS::served_between(calc$data_goes_back_to, meta_HUDCSV$Export_End)  |>
    dplyr::left_join(Client  |>
                       dplyr::select(-DateCreated), by = "PersonalID") |>
    dplyr::select(
      PersonalID,
      FirstName,
      NameDataQuality,
      SSN,
      SSNDataQuality,
      DOB,
      DOBDataQuality,
      AmIndAKNative,
      Asian,
      BlackAfAmerican,
      NativeHIOtherPacific,
      White,
      RaceNone,
      Ethnicity,
      Gender,
      VeteranStatus,
      EnrollmentID,
      ProjectID,
      EntryDate,
      HouseholdID,
      RelationshipToHoH,
      LivingSituation,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH,
      DateToStreetESSH,
      TimesHomelessPastThreeYears,
      AgeAtEntry,
      MonthsHomelessPastThreeYears,
      DisablingCondition,
      DateOfEngagement,
      MoveInDate,
      MoveInDateAdjust,
      CountyServed,
      CountyPrior,
      ExitDate,
      Destination,
      ExitAdjust,
      DateCreated,
      UserCreating,
      ClientEnrolledInPATH,
      LengthOfStay,
      DateOfPATHStatus,
      ReasonNotEnrolled,
      ClientLocation,
      PHTrack,
      ExpectedPHDate
      #, EEType # Deprecated SP logic
    )  |>
    dplyr::inner_join(projects_current_hmis, by = "ProjectID") |>
    dplyr::left_join(
      HealthAndDV  |>
        dplyr::filter(DataCollectionStage == 1)  |>
        dplyr::select(
          EnrollmentID,
          DomesticViolenceVictim,
          WhenOccurred,
          CurrentlyFleeing
        ),
      by = "EnrollmentID"
    )


}

#' Filter for Enrollments in a Specific Project Type
#'
#' @param served_in_date_range \code{(data.frame)} See `served_in_date_range`
#' @param type \code{(numeric)} ProjectType. For full project type names see `hud.extract::hud_translations$[["2.02.6 ProjectType"]](table = TRUE)`
#'
#' @return \code{(data.frame)} with `PersonalID` for all Enrollees in the ProjectType, their `MoveInDateAdjust`, the `TimeInterval` for which they were in the Project, and the `ProjectName`
#' @export

enrolled_in <-
  function(served_in_date_range,
           type = c(
             ES = 1,
             TH = 2,
             PSH = 3,
             SO = 4,
             ServicesOnly = 6,
             Other = 7,
             SH = 8,
             PHHO = 9,
             PHHS = 10,
             DS = 11,
             HP = 12,
             RRH = 13,
             CE = 14
           )[13],
           has_movein = FALSE) {
    f_expr <- rlang::expr(ProjectType %in% type)
    if (has_movein)
      f_expr <- rlang::expr(!!f_expr & !is.na(MoveInDateAdjust))
    served_in_date_range %>%
      dplyr::filter(!!f_expr) %>%
      dplyr::mutate(TimeInterval = lubridate::interval(EntryDate, ExitAdjust - lubridate::days(1))) %>%
      dplyr::select(PersonalID,
                    MoveInDateAdjust,
                    TimeInterval,
                    ProjectName)
  }

#' @title Data Quality report on Missing First Names
#'
#' @param served_in_date_range \code{(data.frame)} See `served_in_date_range`
#' @param guidance \code{(list)}
#' @param vars \code{(list)}
#' @param app_env \code{(app_env)} Object containing dependencies. If all arguments to this function are saved in the `app_env`, then they will be called from there and arguments do not need to be specified.
#'
#' @return \code{(data.frame)} `vars$we_want` and `Issue` (Issue Name), `Type` (Error or Warning), and `Guidance` (How to correct the issue)

dq_name <- function(served_in_date_range, guidance = NULL, vars = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range  |>
    dplyr::mutate(
      Issue = dplyr::case_when(
        FirstName == "Missing" ~
          "Missing Name Data Quality",
        FirstName %in% c("DKR", "Partial") ~
          "Incomplete or Don't Know/Refused Name"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Name Data Quality" ~ "Error",
        Issue == "Incomplete or Don't Know/Refused Name" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_pii)
    )  |>
    dplyr::filter(!is.na(Issue)) |>
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Data quality report on Missing/Incorrect DOB
#'
#' @inherit dq_name params return


dq_dob <- function(served_in_date_range, guidance = NULL, vars = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(DOB) & DOBDataQuality %in% c(1, 2) ~ "Missing DOB",
        DOBDataQuality == 99 ~ "Missing Date of Birth Data Quality",
        DOBDataQuality %in% c(2, 8, 9) ~ "Don't Know/Refused or Approx. Date of Birth",
        AgeAtEntry < 0 |
          AgeAtEntry > 95 ~ "Incorrect Date of Birth or Entry Date"
      ),
      Type = dplyr::case_when(
        Issue %in% c(
          "Missing DOB",
          "Incorrect Date of Birth or Entry Date",
          "Missing Date of Birth Data Quality"
        ) ~ "Error",
        Issue ==  "Don't Know/Refused or Approx. Date of Birth" ~ "Warning"
      ),
      Guidance = dplyr::case_when(
        Issue == "Incorrect Date of Birth or Entry Date" ~
          "The HMIS data is indicating the client entered the project PRIOR to
      being born. Correct either the Date of Birth or the Entry Date, whichever
      is incorrect.",
      Issue %in% c("Missing DOB", "Missing Date of Birth Data Quality") ~
        guidance$missing_at_entry,
      Issue == "Don't Know/Refused or Approx. Date of Birth" ~
        guidance$dkr_data
      )
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Data quality report on SSN Validity
#' @inherit dq_name params return


dq_ssn <- function(served_in_date_range, guidance = NULL, vars = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        SSN == "Missing" ~ "Missing SSN",
        SSN == "Invalid" ~ "Invalid SSN",
        SSN == "DKR" ~ "Don't Know/Refused SSN",
        SSN == "Incomplete" ~ "Invalid SSN"
      ),
      Type = dplyr::case_when(
        Issue %in% c("Missing SSN", "Invalid SSN") ~ "Error",
        Issue == "Don't Know/Refused SSN" ~ "Warning"
      ),
      Guidance = dplyr::case_when(
        Issue == "Don't Know/Refused SSN" ~ guidance$dkr_data,
        Issue == "Missing SSN" ~ guidance$missing_pii,
        Issue == "Invalid SSN" ~ "The Social Security Number does not conform with
      standards set by the Social Security Administration. This includes rules
      like every SSN is exactly 9 digits and cannot have certain number patterns.
      Correct by navigating to the client's record, then clicking the Client
      Profile tab, then click into the Client Record pencil to correct the data."
      )
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Data quality report on Race data
#' @inherit dq_name params return

dq_race <- function(served_in_date_range, guidance = NULL, vars = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
   served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        RaceNone == 99 ~ "Missing Race",
        RaceNone %in% c(8, 9) ~ "Don't Know/Refused Race"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Race" ~ "Error",
        Issue == "Don't Know/Refused Race" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_at_entry)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Data quality report on Ethnicity data
#' @inherit dq_name params return

dq_ethnicity <- function(served_in_date_range, guidance = NULL, vars = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        Ethnicity == 99 ~ "Missing Ethnicity",
        Ethnicity %in% c(8, 9) ~ "Don't Know/Refused Ethnicity"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Ethnicity" ~ "Error",
        Issue == "Don't Know/Refused Ethnicity" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_at_entry)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Data quality report on Gender Data
#' @inherit dq_name params return
#TODO Change for FY 2022
dq_gender <- function(served_in_date_range, guidance = NULL, vars = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        Gender == 99 ~ "Missing Gender",
        Gender %in% c(8, 9) ~ "Don't Know/Refused Gender"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Gender" ~ "Error",
        Issue == "Don't Know/Refused Gender" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_at_entry)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Data quality report on Veteran Status
#' @inherit dq_name params return

dq_veteran <- function(served_in_date_range, guidance = NULL, vars = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          VeteranStatus == 99 ~ "Missing Veteran Status",
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          VeteranStatus %in% c(8, 9) ~ "Don't Know/Refused Veteran Status",
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          RelationshipToHoH == 1 &
          VeteranStatus == 0 &
          Destination %in% c(19, 28) ~ "Check Veteran Status for Accuracy"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Veteran Status" ~ "Error",
        Issue %in% c(
          "Don't Know/Refused Veteran Status",
          "Check Veteran Status for Accuracy"
        ) ~ "Warning"
      ),
      Guidance = dplyr::case_when(
        Issue == "Check Veteran Status for Accuracy" ~ "You have indicated the
      household exited to a destination that only veterans are eligible for, but
      the head of household appears to be not a veteran. Either the Veteran
      Status is incorrect or the Destination is incorrect.",
      Issue == "Missing Veteran Status" ~ guidance$missing_pii,
      Issue == "Don't Know/Refused Veteran Status" ~ guidance$dkr_data)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Data quality report on Race data
#' @param doses \code{(data.frame)} See the `[["HUD Extras"]]$Client_Doses_extra` method in the instantiated clarity_api object
#' @inherit dq_name params return
#' @inheritParams served_in_date_range

dq_missing_vaccine_exited <- function(served_in_date_range, dose_counts, vars, mahoning_projects = NULL, doses = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  # TODO Representation of C19ConsentToVaccine is likely different in Clarity. Once this data is populated this will need to be updated.
  if (!missing(app_env))
    app_env$merge_deps_to_env("mahoning_projects", "doses", "hc")
  served_in_date_range |>
    HMIS::served_between(hc$bos_start_vaccine_data, lubridate::today()) %>%
    dplyr::left_join(doses[c("PersonalID", "C19ConsentToVaccine", "C19VaccineConcerns")],
                     by = "PersonalID") %>%
    dplyr::left_join(dose_counts,
                     by = "PersonalID") %>%
    dplyr::filter(
      !ProjectID %in% c(mahoning_projects) &
        !is.na(ExitDate) &
        (is.na(ExitDate) |
           ExitDate >= hc$bos_start_vaccine_data) &
        (
          C19ConsentToVaccine == "Data not collected (HUD)" |
            is.na(C19ConsentToVaccine)
        ) &
        is.na(Doses) &
        (ProjectType %in% c(1, 2, 4, 8) |
           (
             ProjectType %in% c(3, 9, 13) &
               is.na(MoveInDateAdjust)
           ))
    ) |>
    dplyr::mutate(
      Type = "Warning",
      Issue = "Vaccine data not collected and client has exited",
      Guidance = "Client was literally homeless on Feb 5th, 2021 or later and
         is missing their vaccine data, and the client has exited the project.
         If you are unable to follow up with the client, leave the client as is.
         Please see the guidance
         <a href = \"https://cohhio.org/boscoc/covid19/\" target = \"blank\">
         for more information</a>."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' Find Missing vaccine data for enrolled clients
#'
#' @inheritParams dq_missing_vaccine_exited
#' @param served_in_date_range \code{(data.frame)}
#' @param mahoning_projects \code{(numeric)} from `load_export`
#' @param hc \code{(list)} from `dates`
#' @param app_env
#'
#' @export
#' @inherit dq_name params return
dq_missing_vaccine_current <- function(served_in_date_range, vars, dose_counts, doses = NULL, mahoning_projects = NULL, hc = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())

  # TODO Representation of C19ConsentToVaccine is likely different in Clarity. Once this data is populated this will need to be updated.
  served_in_date_range  |>
    dplyr::left_join(doses[c("PersonalID", "C19ConsentToVaccine", "C19VaccineConcerns")],
                     by = "PersonalID") %>%
    dplyr::left_join(dose_counts, by = "PersonalID") %>%
    dplyr::filter(
      !ProjectID %in% c(mahoning_projects) &
        is.na(ExitDate) &
        ProjectID != 1695 &
        (
          is.na(ExitDate) |
            ExitDate >= hc$bos_start_vaccine_data
        ) &
        (
          C19ConsentToVaccine == "Data not collected (HUD)" |
            is.na(C19ConsentToVaccine)
        ) &
        is.na(Doses) &
        (ProjectType %in% c(1, 2, 4, 8) |
           (
             ProjectType %in% c(3, 9, 13) &
               is.na(MoveInDateAdjust)
           ))
    ) %>%
    dplyr::mutate(
      Type = "Error",
      Issue = "Vaccine data not collected on current client",
      Guidance = "Client was literally homeless on Feb 5th, 2021 or later and is
    missing their vaccine data. Because the client has not exited the project,
    this data can still be collected. Please see
    <a href = \"https://cohhio.org/boscoc/covid19/\" target = \"blank\">
    for more information</a>."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' Find Dose Date Errors
#'
#' @param doses
#' @param served_in_date_range
#' @param hc
#' @inherit dq_name params return
#' @return
#' @export

dq_dose_date_error <- function(served_in_date_range, vars, doses, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  doses %>%
    dplyr::filter(C19DoseDate < hc$first_vaccine_administered_in_us) %>%
    dplyr::left_join(HMIS::served_between(served_in_date_range, hc$bos_start_vaccine_data, lubridate::today()),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Error",
                  Issue = "Vaccine Date Incorrect",
                  Guidance = "Vaccination date precedes the vaccine being available in the US.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find missing client locations
#' @inherit dq_name params return
#' @export
dq_missing_client_location <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::filter(is.na(ClientLocation),
                  RelationshipToHoH == 1) %>%
    dplyr::mutate(Type = "High Priority",
                  Issue = "Missing Client Location",
                  Guidance = "If Client Location is missing, this household will be
         excluded from all HUD reporting.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Households without adults
#' @inherit dq_name params return
#' @export
dq_hh_children_only <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(GrantType != "RHY" |
                    is.na(GrantType)) %>% # not checking for children-only hhs for RHY
    dplyr::group_by(HouseholdID) %>%
    dplyr::summarise(
      hhMembers = dplyr::n(),
      maxAge = max(AgeAtEntry),
      PersonalID = min(PersonalID)
    ) %>%
    dplyr::filter(maxAge < 18) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
    dplyr::mutate(Issue = "Children Only Household",
                  Type = "High Priority",
                  Guidance = "Unless your project serves youth younger than 18
         exclusively, every household should have at least one adult in it. If
         you are not sure how to correct this, please contact the HMIS team for
         help.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Date Homeless
#' @inherit dq_name params return
#' @export
dq_missing_approx_date_homeless <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  missing_approx_date_homeless <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      EnrollmentID,
      ProjectID,
      AgeAtEntry,
      RelationshipToHoH,
      LOSUnderThreshold,
      DateToStreetESSH,
      PreviousStreetESSH
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    is.na(DateToStreetESSH) &
                    LOSUnderThreshold == 1 &
                    PreviousStreetESSH == 1
    ) %>%
    dplyr::mutate(Issue = "Missing Approximate Date Homeless",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Length of Time Homeless questions for Emergency Shelters and Safe Havens
#' @inherit dq_name params return
#' @export
dq_missing_previous_street_ESSH <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  missing_previous_street_ESSH <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      DateToStreetESSH,
      PreviousStreetESSH,
      LOSUnderThreshold
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    is.na(PreviousStreetESSH) &
                    LOSUnderThreshold == 1
    ) %>%
    dplyr::mutate(Issue = "Missing Previously From Street, ES, or SH (Length of Time Homeless questions)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Prior Residence
#' @inherit dq_name params return
#' @export
dq_missing_residence_prior <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  missing_residence_prior <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LivingSituation) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    (is.na(LivingSituation) | LivingSituation == 99)) %>%
    dplyr::mutate(Issue = "Missing Residence Prior",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


}

#' @title Find Don't Know/Refused Prior Residence
#' @inherit dq_name params return
#' @export
dq_dkr_residence_prior <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  dkr_residence_prior <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LivingSituation) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    LivingSituation %in% c(8, 9)) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Residence Prior",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Length of Stay
#' @inherit dq_name params return
#' @export
dq_missing_LoS <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  missing_LoS <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LengthOfStay) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
    dplyr::mutate(Issue = "Missing Length of Stay",
                  Type = "Error",
                  Guidance = "This data element may be answered with an old value or it
         may simply be missing. If the value selected is \"One week or less (HUD)\",
         you will need to change that value to either \"One night or less (HUD)\"
         or \"Two to six nights (HUD)\".") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Don't Know/Refused Length of Stay
#' @inherit dq_name params return
#' @export
dq_dkr_LoS <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LengthOfStay) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    LengthOfStay %in% c(8, 9)) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Residence Prior",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Months or Times Homeless
#' @inherit dq_name params return
#' @export
dq_missing_months_times_homeless <- function(served_in_date_range, vars, guidance = NULL, hc = NULL, app_env = get_app_env(e = rlang::caller_env())) {

  if (is_app_env(app_env))
    app_env$merge_deps_to_env(dq_missing_fmls())

  missing_months_times_homeless <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    ProjectType %in% c(1, 4, 8) &
                    (
                      is.na(MonthsHomelessPastThreeYears) |
                        is.na(TimesHomelessPastThreeYears) |
                        MonthsHomelessPastThreeYears == 99 |
                        TimesHomelessPastThreeYears == 99
                    )
    ) %>%
    dplyr::mutate(Issue = "Missing Months or Times Homeless",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Don't Know/Refused Months or Times Homeless
#' @inherit dq_name params return
#' @export
dq_dkr_months_times_homeless <- function(served_in_date_range, vars, hc = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    (
                      MonthsHomelessPastThreeYears %in% c(8, 9) |
                        TimesHomelessPastThreeYears %in% c(8, 9)
                    )
    ) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Months or Times Homeless",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Invalid Months or Times Homeless Entries
#' @inherit dq_name params return
#' @export
dq_invalid_months_times_homeless <- function(served_in_date_range, vars, hc = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears,
      DateToStreetESSH
    ) %>%
    dplyr::filter(
      ProjectType != 12 &
        (RelationshipToHoH == 1 | AgeAtEntry > 17) &
        EntryDate >= hc$prior_living_situation_required &
        TimesHomelessPastThreeYears == 1 &
        !is.na(DateToStreetESSH)
    ) %>%
    dplyr::mutate(
      MonthHomelessnessBegan = lubridate::floor_date(DateToStreetESSH, "month"),
      MonthEnteredProgram = lubridate::floor_date(EntryDate, "month"),
      MonthDiff = lubridate::interval(MonthHomelessnessBegan, MonthEnteredProgram) %/% months(1) + 1,
      MonthDiff = dplyr::if_else(MonthDiff >= 13, 13, MonthDiff),
      DateMonthsMismatch = dplyr::if_else(MonthsHomelessPastThreeYears - MonthDiff != 100, 1, 0),
      Issue = dplyr::case_when(
        MonthDiff <= 0 ~
          "Homelessness Start Date Later Than Entry",
        MonthsHomelessPastThreeYears < 100 ~
          "Number of Months Homeless Can Be Determined",
        DateMonthsMismatch == 1 ~
          "Invalid Homelessness Start Date/Number of Months Homeless"),
      Type = "Warning",
      Guidance = dplyr::case_when(
        MonthDiff <= 0 ~
          "This client has an Approximate Date Homeless in their Entry that is after
          their Entry Date. The information in the Entry should reflect the
          client's situation at the point of Entry, so this date may have been
          incorrectly entered.",
        MonthsHomelessPastThreeYears < 100 ~
          "According to this client's entry, they experienced a single episode of
          homelessness in the three years prior to their entry and the approximate
          start date of their homelessness is known, but there was no response
          entered for the number of months they experienced homelessness prior to
          this entry. It should be possible to determine and enter the number of
          months homeless based on the Approximate Date Homeless and the Entry Date.",
        DateMonthsMismatch == 1 ~
          "According to this client's entry, they experienced a single episode of
          homelessness in the three years prior to their entry and the approximate
          start date of their homelessness is known, but the recorded number of
          months they experienced homelessness prior to this entry is inconsistent
          with the given dates. Please double-check this information for
          consistency and accuracy.")) %>%
    dplyr::filter(!is.na(Guidance)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Living Situation
#' @inherit dq_name params return
#' @export
dq_missing_living_situation <- function(served_in_date_range, vars, hc = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      LivingSituation,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH,
      DateToStreetESSH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    # not req'd prior to this
                    ProjectType %in% c(2, 3, 6, 9, 10, 12, 13) &
                    (
                      (
                        LivingSituation %in% c(15, 6, 7, 24, 4, 5) &
                          LengthOfStay %in% c(2, 3, 10, 11) &
                          (is.na(LOSUnderThreshold) |
                             is.na(PreviousStreetESSH))
                      ) |
                        (
                          LivingSituation %in% c(2, 3, 12, 13, 14, 15, 19,
                                                 20, 21, 22, 23, 25, 26) &
                            LengthOfStay %in% c(10, 11) &
                            (is.na(LOSUnderThreshold) |
                               is.na(PreviousStreetESSH))
                        )
                    )
    ) %>%
    dplyr::mutate(Issue = "Incomplete Living Situation Data",
                  Type = "Error",
                  Guidance = "When responding to the Living Situation questions in your
           Entry Assessment, users must answer questions about some clients'
           situation prior to the \"Residence Prior\" that are important to help
           determine that client's Chronicity. Please answer these questions to
           the best of your knowledge.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


#' @title Find Don't Know/Refused Living Situation
#' @inherit dq_name params return
#' @export
dq_dkr_living_situation <- function(served_in_date_range, vars, hc = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(
      PersonalID,
      HouseholdID,
      EnrollmentID,
      ProjectID,
      ProjectType,
      ProjectName,
      ProjectRegion,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      AgeAtEntry,
      CountyServed,
      RelationshipToHoH,
      LivingSituation,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH,
      DateToStreetESSH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears,
      UserCreating
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate > hc$prior_living_situation_required &
                    (
                      MonthsHomelessPastThreeYears %in% c(8, 9) |
                        TimesHomelessPastThreeYears %in% c(8, 9) |
                        LivingSituation %in% c(8, 9)
                    )
    ) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Living Situation",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Disabilities and Conflicting Disability of Long Duration
#' @inherit dq_name params return
#' @export
dq_detail_missing_disabilities <- function(served_in_date_range, Disabilities, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {

if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())

  detail_missing_disabilities <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  DisablingCondition) %>%
    dplyr::filter(DisablingCondition == 99 |
                    is.na(DisablingCondition)) %>%
    dplyr::mutate(Issue = "Missing Disabling Condition",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry)

  missing_disabilities <- detail_missing_disabilities %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  smallDisabilities <- Disabilities %>%
    dplyr::filter(DataCollectionStage == 1 &
                    ((DisabilityType == 10 &
                        DisabilityResponse %in% c(1:3)) |
                       (DisabilityType != 10 & DisabilityResponse == 1)
                    )) %>%
    dplyr::mutate(
      IndefiniteAndImpairs =
        dplyr::case_when(
          DisabilityType %in% c(6L, 8L) ~ 1L,
          TRUE ~ IndefiniteAndImpairs)
    ) %>%
    dplyr::select(
      PersonalID,
      DisabilitiesID,
      EnrollmentID,
      InformationDate,
      DisabilityType,
      IndefiniteAndImpairs
    )

  # Developmental & HIV/AIDS get automatically IndefiniteAndImpairs = 1 per FY2020

  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  AgeAtEntry,
                  RelationshipToHoH,
                  DisablingCondition) %>%
    dplyr::left_join(
      smallDisabilities %>%
        dplyr::filter(IndefiniteAndImpairs == 1L),
      by = c("PersonalID", "EnrollmentID")
    ) %>%
    dplyr::filter((DisablingCondition == 0 & !is.na(DisabilitiesID)) |
                    (DisablingCondition == 1 & is.na(DisabilitiesID))) %>%
    dplyr::mutate(
      Issue = "Conflicting Disability of Long Duration yes/no",
      Type = "Error",
      Guidance = "If the user answered \"Yes\" to the \"Does the client have a
      disabling condition?\", then there should be a disability subassessment that
      indicates the disability determination is Yes *and* the \"If yes,... long
      duration\" question is Yes. Similarly if the user answered \"No\", the
      client should not have any disability subassessments that indicate that they
      do have a Disabling Condition."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Clients in Mahoning with 60 Days elapsed in Coordinated Entry
#' @inherit dq_name params return
#' @export

dq_mahoning_ce_60_days <- function(served_in_date_range, mahoning_projects, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  mahoning_ce <- mahoning_projects[stringr::str_detect(names(mahoning_projects), "Coordinated Entry")]
  served_in_date_range %>%
    dplyr::filter(ProjectID == mahoning_ce &
                    EntryDate <= lubridate::today() - lubridate::days(60) &
                    is.na(ExitDate)) %>%
    dplyr::mutate(
      Issue = "60 Days in Mahoning Coordinated Entry",
      Type = "Warning",
      Guidance = "If this household is \"unreachable\" as defined in the Mahoning County
      Coordinated Entry Policies and Procedures, they should be exited."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Clients with Extremely Long Stays
#' @inherit dq_name params return
#' @export
dq_th_stayers_bos <- function(served_in_date_range, mahoning_projects, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  th_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate))) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 2 &
                    !ProjectID %in% c(mahoning_projects))

  th_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate))) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 2 &
                    ProjectID %in% c(mahoning_projects))

  Top2_TH_bos <- subset(th_stayers_bos, Days > stats::quantile(Days, prob = 1 - 2 / 100))
  Top2_TH_mah <- subset(th_stayers_mah, Days > stats::quantile(Days, prob = 1 - 2 / 100))

  rrh_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 13 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  rrh_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 13 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top2_RRH_bos <- subset(rrh_stayers_bos, Days > stats::quantile(Days, prob = 1 - 2 / 100))
  Top2_RRH_mah <- subset(rrh_stayers_mah, Days > stats::quantile(Days, prob = 1 - 2 / 100))

  es_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 1 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  es_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 1 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top2_ES_bos <- subset(es_stayers_bos, Days > stats::quantile(Days, prob = 1 - 2 / 100))
  Top2_ES_mah <- subset(es_stayers_mah, Days > stats::quantile(Days, prob = 1 - 2 / 100))

  psh_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 3 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  psh_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 3 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top1_PSH_bos <- subset(psh_stayers_bos, Days > stats::quantile(Days, prob = 1 - 1 / 100))
  Top1_PSH_mah <- subset(psh_stayers_mah, Days > stats::quantile(Days, prob = 1 - 1 / 100))

  hp_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 12 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  hp_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 12 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top5_HP_bos <- subset(hp_stayers_bos, Days > stats::quantile(Days, prob = 1 - 5 / 100))
  Top10_HP_mah <- subset(hp_stayers_mah, Days > stats::quantile(Days, prob = 90 / 100))

  rbind(Top1_PSH_bos,
        Top2_ES_bos,
        Top2_RRH_bos,
        Top2_TH_bos,
        Top5_HP_bos,
        Top1_PSH_mah,
        Top2_ES_mah,
        Top2_RRH_mah,
        Top2_TH_mah,
        Top10_HP_mah) %>%
    dplyr::mutate(
      Issue = "Extremely Long Stayer",
      Type = "Warning",
      Guidance = paste(
        "This client is showing as an outlier for Length of Stay for this project
        type in the",
        dplyr::if_else(
          ProjectID %in% c(mahoning_projects),
          "Mahoning County",
          "Balance of State"
        ),
        "CoC. Please verify that
           this client is still in your project. If they are, be sure there are no
           alternative permanent housing solutions for this client. If the client
           is no longer in your project, please enter their Exit Date as the
           closest estimation of the day they left your project."
      )
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

# Incorrect Destination ---------------------------------------------------

#' @title Find Incorrect Exits in RRH
#' @inherit dq_name params return
#' @export
dq_rrh_check_exit_destination <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())

  enrolled_in_type <- enrolled_in(served_in_date_range, type = 13, has_movein = TRUE)

  served_in_date_range %>%
    dplyr::left_join(enrolled_in_type, by = "PersonalID", suffix = c("", "_rrh")) %>%
    dplyr::filter(ProjectType != 13 &
                    ExitDate == MoveInDateAdjust_rrh &
                    Destination != 31) %>%
    dplyr::mutate(
      Issue = "Maybe Incorrect Exit Destination (did you mean \"Rental by client, with RRH...\"?)",
      Type = "Warning",
      Guidance = "This household has a Move-In Date into an RRH project that
      matches their Exit from your project, but the Exit Destination from your
      project does not indicate that the household exited to Rapid Rehousing. If
      the household exited to a Destination that was not \"Rental by client\", but
      it is a permanent destination attained through a Rapid Rehousing project,
      then there is no change needed. If this is not the case, then the Destination
      should be \"Rental by client, with RRH or equivalent subsidy\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Possibly Incorrect Exits in PSH
#' @inherit dq_name params return
#' @export
dq_psh_check_exit_destination <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  enrolled_in_type <- enrolled_in(served_in_date_range, type = c(3,9), TRUE)

  served_in_date_range %>%
    dplyr::left_join(enrolled_in_type, by = "PersonalID", suffix = c("", "_psh")) %>%
    dplyr::filter(!ProjectType %in% c(3, 9) &
                    lubridate::`%within%`(ExitAdjust, TimeInterval)  &
                    !Destination %in% c(3, 19, 26)) %>%
    dplyr::mutate(
      Issue = "Check Exit Destination (may be \"Permanent housing (other
      than RRH)...\")",
      Type = "Warning",
      Guidance = "This household appears to have an Entry into a PSH project that
      overlaps their Exit from your project. Typically this means the client moved
      into a Permanent Supportive Housing unit after their stay with you. If that
      is true, the Destination should be
      \"Permanent housing (other than RRH) for formerly homeless persons\". If you
      are sure the current Destination is accurate, then please leave it the way
      it is."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Incorrect Exits in PSH
#' @inherit dq_name params return
#' @export
dq_psh_incorrect_destination <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  enrolled_in_type <- enrolled_in(served_in_date_range, type = c(3,9), TRUE)

  served_in_date_range %>%
    dplyr::left_join(enrolled_in_type, by = "PersonalID", suffix = c("", "_psh")) %>%
    dplyr::filter(!ProjectType %in% c(3, 9) &
                    ExitDate == MoveInDateAdjust_psh  &
                    !Destination %in% c(3, 19, 26)) %>%
    dplyr::mutate(
      Issue = "Incorrect Exit Destination (should be \"Permanent housing (other
    than RRH)...\")",
    Type = "Error",
    Guidance = "This household appears to have a Move-In Date into a PSH project
    that matches their Exit from your project, but the Exit Destination from your
    project does not indicate that the household exited to PSH. The correct
    Destination for households entering PSH from your project is
    \"Permanent housing (other than RRH) for formerly homeless persons\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Incorrect Exits in TH
#' @inherit dq_name params return
#' @export

dq_th_check_exit_destination <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  enrolled_in_type <- enrolled_in(served_in_date_range, type = 2)
  served_in_date_range %>%
    dplyr::left_join(enrolled_in_type, by = "PersonalID", suffix = c("", "_th")) %>%
    dplyr::filter(ProjectType != 2 &
                    lubridate::`%within%`(ExitAdjust, TimeInterval) &
                    Destination != 2) %>%
    dplyr::mutate(
      Issue = "Incorrect Exit Destination (should be \"Transitional housing...\")",
      Type = "Error",
      Guidance = "This household appears to have an Entry into a Transitional
      Housing project that overlaps their Exit from your project, but the Exit
      Destination from your project does not indicate that the household exited to
      Transitional Housing. The correct Destination for households entering TH from
      your project is \"Transitional housing for homeless persons (including
      homeless youth)\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Incorrect Exits in SH
#' @inherit dq_name params return
#' @export

dq_sh_check_exit_destination <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  enrolled_in_type <- enrolled_in(served_in_date_range, type = 8)
  served_in_date_range %>%
    dplyr::left_join(enrolled_in_type, by = "PersonalID", suffix = c("", "_sh")) %>%
    dplyr::filter(ProjectType != 8 &
                    lubridate::`%within%`(ExitAdjust, TimeInterval) &
                    Destination != 18) %>%
    dplyr::mutate(
      Issue = "Incorrect Exit Destination (should be \"Safe Haven\")",
      Type = "Error",
      Guidance = "This household appears to have an Entry into a Safe Haven that
      overlaps their Exit from your project, but the Exit Destination from your
      project does not indicate that the household exited to a Safe Haven. The
      correct Destination for households entering SH from your project is
      \"Safe Haven\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

# Missing Project Stay or Incorrect Destination ---------------------------
#' @title Find Missing Project Stay or Incorrect Destination for RRH
#' @inherit dq_name params return
#' @export

dq_rrh_missing_project_stay <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::filter(Destination == 31) %>%
    dplyr::anti_join(enrolled_in(served_in_date_range, type = 13), by = "PersonalID") %>%
    dplyr::mutate(
      Issue = "Missing RRH Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
      to Rapid Rehousing, but there is no RRH project stay on the client. If the
      RRH project the household exited to is outside of the Balance of State or
      Mahoning County CoCs, then no correction is necessary. If they received RRH services
      in the Balance of State CoC or Mahoning County CoC, then this household is missing
      their RRH project stay. If they did not actually receive RRH services at all,
      the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Project Stay or Incorrect Destination for PSH
#' @inherit dq_name params return
#' @export
dq_psh_missing_project_stay <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(Destination == 3) %>%
    dplyr::anti_join(enrolled_in(served_in_date_range, type = c(3,9), TRUE), by = "PersonalID", suffix = c("", "_psh")) %>%
    dplyr::mutate(
      Issue = "Missing PSH Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
      to Permanent Supportive Housing, but there is no PSH project stay on the
      client. If the PSH project the household exited to is outside of the Balance
      of State CoC or Mahoning County CoC, then no correction is necessary. If they
      entered PSH in the Balance of State CoC or Mahoning County CoC, then this household
      is missing their PSH project stay. If they did not actually enter PSH at all,
      the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Project Stay or Incorrect Destination for TH
#' @inherit dq_name params return
#' @export

dq_th_missing_project_stay <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(Destination == 2) %>%
    dplyr::anti_join(enrolled_in(served_in_date_range, type = 2), by = "PersonalID") %>%
    dplyr::mutate(
      Issue = "Missing TH Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
      to Transitional Housing, but there is no TH project stay on the client. If the
      TH project that the household exited to is outside of the Balance of State
      CoC or Mahoning County CoC, then no correction is necessary. If they went into a TH
      project in the Balance of State CoC or Mahoning County CoC, then this household is
      missing their TH project stay. If they did not actually enter Transitional
      Housing at all, the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Project Stay or Incorrect Destination for SH
#' @inherit dq_name params return
#' @export
dq_sh_missing_project_stay <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(Destination == 18) %>%
    dplyr::anti_join(enrolled_in(served_in_date_range, type = 8), by = "PersonalID", suffix = c("", "_sh")) %>%
    dplyr::mutate(
      Issue = "Missing Safe Haven Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
      to a Safe Haven, but there is no Entry in HMIS into a Safe Haven. Keep in
      mind that there is only one Safe Haven in the Balance of State and they are
      no longer operating as of 1/1/2021. If you meant to indicate that the household
      exited to a Domestic Violence shelter, please select \"Emergency shelter, ...\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

dq_sp_incorrect_ee_type <- function(server_in_date_range) {
  if (!is_sp())
    rlang::abort(match.call()[[1]], " is a ServicePoint specific data quality check.")
  served_in_date_range %>%
    dplyr::filter(
      (
        is.na(GrantType) &
          !grepl("GPD", ProjectName) &
          !grepl("HCHV", ProjectName) &
          !grepl("VET", ProjectName) &
          !grepl("Veterans", ProjectName) &
          ProjectID != 1695 &
          EEType != "HUD"
      ) |
        ((
          GrantType == "SSVF" |
            grepl("GPD", ProjectName) |
            grepl("HCHV", ProjectName) |
            grepl("Veterans", ProjectName) |
            grepl("VET", ProjectName) |
            grepl("VASH", ProjectName)
        ) &
          EEType != "VA"
        ) |
        (GrantType == "RHY" &
           !grepl("YHDP", ProjectName) &
           !grepl("ODH", ProjectName) &
           EEType != "RHY") |
        (GrantType == "RHY" &
           grepl("YHDP", ProjectName) &
           grepl("ODH", ProjectName) &
           EEType != "HUD") |
        (GrantType == "PATH" & EEType != "PATH") |
        (ProjectID == 1695 & EEType != "Standard")
    ) %>%
    dplyr::mutate(Issue = "Incorrect Entry Exit Type",
                  Type = "High Priority",
                  Guidance = "The user selected the wrong Entry Exit Type. To correct,
             click the Entry pencil and Save & Continue. The Entry Exit Type at
             the top can then be changed. Click \"Update\" to make this change
             take effect.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

dq_sp_stray_services <- function(stray_services) {
  if (!is_sp())
    rlang::abort(match.call()[[1]], " is a ServicePoint specific data quality check.")
    stray_services %>%
    dplyr::mutate(Issue = "Service Not Attached to an Entry Exit",
                  Type = "Warning",
                  Guidance = "This Service does not fall between any project stay,
             so it will not show in any reporting.") %>%
    dplyr::select(PersonalID, ServiceProvider, ServiceStartDate, Issue, Type)
}

dq_sp_referrals_on_hh_members <- function(served_in_date_range, vars) {
  if (!is_sp())
    rlang::abort(match.call()[[1]], " is a ServicePoint specific data quality check.")
  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  RelationshipToHoH,
                  EnrollmentID,
                  GrantType) %>%
    dplyr::filter(RelationshipToHoH != 1 &
                    (GrantType != "SSVF"  | is.na(GrantType))) %>%
    dplyr::semi_join(Referrals,
                     by = c("PersonalID", "ProjectName" = "ProviderCreating")) %>%
    dplyr::mutate(
      Issue = "Referral on a Non Head of Household",
      Type = "Warning",
      Guidance = guidance$referral_on_non_hoh
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

dq_sp_referrals_on_hh_members_ssvf <- function(served_in_date_range, vars) {
  if (!is_sp())
    rlang::abort(match.call()[[1]], " is a ServicePoint specific data quality check.")
  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  RelationshipToHoH,
                  EnrollmentID,
                  GrantType) %>%
    dplyr::filter(RelationshipToHoH != 1 &
                    GrantType == "SSVF") %>%
    dplyr::semi_join(Referrals, by = c("PersonalID")) %>%
    dplyr::mutate(Issue = "Referral on a Non Head of Household (SSVF)",
                  Type = "Error",
                  Guidance = guidance$referral_on_non_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


dq_internal_old_outstanding_referrals <- function(served_in_date_range, Referrals, vars) {
  served_in_date_range %>%
    dplyr::semi_join(Referrals,
                     by = c("PersonalID")) %>%
    dplyr::left_join(Referrals,
                     by = c("PersonalID")) %>%
    dplyr::filter(ReferringProjectID == ProjectName &
                    ProjectID != 1695) %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  ReferringProjectID,
                  ReferralDate,
                  ReferralOutcome,
                  EnrollmentID) %>%
    dplyr::filter(is.na(ReferralOutcome) &
                    ReferralDate < lubridate::today() - lubridate::days(14)) %>%
    dplyr::mutate(
      ProjectName = ReferringProjectID,
      Issue = "Old Outstanding Referral",
      Type = "Warning",
      Guidance = "Referrals should be closed in about 2 weeks. Please be sure you are
      following up with any referrals and helping the client to find permanent
      housing. Once a Referral is made, the receiving agency should be saving
      the \"Referral Outcome\" once it is known. If you have Referrals that are
      legitimately still open after 2 weeks because there is a lot of follow
      up going on, no action is needed since the HMIS data is accurate."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}
