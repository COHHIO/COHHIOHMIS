#' @include 04_Guidance.R

#' @title Is this instance using Clarity
#' @description Set an option in `.Rprofile` using `usethis::edit_r_profile('project')` called HMIS which is a list containing two logical values:
#' \itemize{
#'   \item{\code{Clarity}}{ A logical to indicate whether Clarity is (or has been) used by this CoC}
#'   \item{\code{ServicePoint}}{ A logical to indicate whether Servicepoint is (or has been) used by this CoC}
#' }
#' @return \code{(logical)}
is_clarity <- function() {
  getOption("HMIS")$Clarity
}

#' @title Is this instance using ServicePoint
#' @inherit is_clarity description return

is_sp <- function() {
  getOption("HMIS")$ServicePoint
}

#' @title This instance must be using ServicePoint, otherwise throw an error.
#' @inherit is_clarity description return

must_sp <- function(.call = match.call()[[1]]) {
  if (!is_sp())
    rlang::abort(.call, " is a ServicePoint specific data quality check.")
  TRUE
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
#' @inheritParams data_quality_tables
#'
#' @return \code{(data.frame)}

projects_current_hmis <- function (Project,
                                   Inventory,
                                   rm_dates,
                                   app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  Project %>%
    dplyr::left_join(Inventory, by = "ProjectID") |>
    HMIS::operating_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
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


served_in_date_range <- function(projects_current_hmis, Enrollment_extra_Exit_HH_CL_AaE = NULL, Client = NULL, Project = NULL, Inventory = NULL, HealthAndDV = NULL, vars, rm_dates = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  Enrollment_extra_Exit_HH_CL_AaE  |>
    HMIS::served_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End)  |>
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
      #, EEType # Deprecated SP logic
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

      ) |>
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

# Missing UDEs ------------------------------------------------------------

#' @title Data Quality report on Missing First Names
#' @family Clarity Checks
#' @family DQ: Missing UDEs
#' @describeIn data_quality_tables
#' @inherit data_quality_tables params return

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
#' @family Clarity Checks
#' @family DQ: Missing UDEs
#' @describeIn data_quality_tables
#' @inherit data_quality_tables params return
# TODO Check to ensure missing DOB are not present in imported.

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
#' @family Clarity Checks
#' @family DQ: Missing UDEs
#' @describeIn data_quality_tables
#' @inherit data_quality_tables params return


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
#' @family Clarity Checks
#' @family DQ: Missing UDEs
#' @describeIn data_quality_tables
#' @inherit data_quality_tables params return

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
#' @family Clarity Checks
#' @family DQ: Missing UDEs
#' @describeIn data_quality_tables
#' @inherit data_quality_tables params return

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
#' @family Clarity Checks
#' @family DQ: Missing UDEs
#' @describeIn data_quality_tables
#' @inherit data_quality_tables params return
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
#' @family Clarity Checks
#' @family DQ: Missing UDEs
#' @describeIn data_quality_tables
#' @inherit data_quality_tables params return

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

# Missing Vaccine data ----------------------------------------------------
#' @title Data quality report on Race data
#' @param doses \code{(data.frame)} See the `[["HUD Extras"]]$Client_Doses_extra` method in the instantiated clarity_api object
#' @family Clarity Checks
#' @family DQ: Vaccines
#' @param dose_counts \code{(data.frame)} Count of doses per client
#' @inherit data_quality_tables params return
#' @describeIn data_quality_tables
#' @inheritParams served_in_date_range

dq_missing_vaccine_exited <- function(served_in_date_range, dose_counts, vars, mahoning_projects = NULL, doses = NULL, rm_dates = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  # TODO Representation of C19ConsentToVaccine is likely different in Clarity. Once this data is populated this will need to be updated.
  if (!missing(app_env))
    app_env$merge_deps_to_env("mahoning_projects", "doses", "hc")
  served_in_date_range |>
    HMIS::served_between(rm_dates$hc$bos_start_vaccine_data, lubridate::today()) %>%
    dplyr::left_join(doses[c("PersonalID", "C19ConsentToVaccine", "C19VaccineConcerns")],
                     by = "PersonalID") %>%
    dplyr::left_join(dose_counts,
                     by = "PersonalID") %>%
    dplyr::filter(
      !ProjectID %in% c(mahoning_projects) &
        !is.na(ExitDate) &
        (is.na(ExitDate) |
           ExitDate >= rm_dates$hc$bos_start_vaccine_data) &
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
#' @family Clarity Checks
#' @family DQ: Vaccines
#' @export
#' @inherit data_quality_tables params return
dq_missing_vaccine_current <- function(served_in_date_range, vars, dose_counts, doses = NULL, mahoning_projects = NULL, rm_dates = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
            ExitDate >= rm_dates$hc$bos_start_vaccine_data
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
#' @family Clarity Checks
#' @inherit data_quality_tables params return
#' @family DQ: Vaccines
#' @export
dq_dose_date_error <- function(served_in_date_range, vars, doses, guidance = NULL, rm_dates = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
		app_env$merge_deps_to_env(missing_fmls())
  doses %>%
    dplyr::filter(C19DoseDate < rm_dates$hc$first_vaccine_administered_in_us) %>%
    dplyr::left_join(HMIS::served_between(served_in_date_range, rm_dates$hc$bos_start_vaccine_data, lubridate::today()),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Error",
                  Issue = "Vaccine Date Incorrect",
                  Guidance = "Vaccination date precedes the vaccine being available in the US.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find missing client locations
#' @family Clarity Checks
#' @inherit data_quality_tables params return
#' @export
dq_missing_client_location <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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


# Household Issues --------------------------------------------------------
#' @title Find Households without adults
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Household Checks
#' @export
dq_hh_children_only <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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

#' @title Find Households with no Head of Household
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Household Checks
#' @export
dq_hh_no_oh <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::summarise(hasHoH = dplyr::if_else(min(RelationshipToHoH) != 1,
                                             FALSE,
                                             TRUE),
                     PersonalID = min(PersonalID)) %>%
    dplyr::filter(hasHoH == FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
    dplyr::mutate(
      Issue = "No Head of Household",
      Type = "High Priority",
      Guidance = "Please be sure all members of the household are included in the program
        stay, and that each household member's birthdate is correct. If those
        things are both true, or the client is a single, check inside the Entry
        pencil to be sure each household member has \"Relationship to Head of
        Household\" answered and that one of them says Self (head of household).
        Singles are always Self (head of household)."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Households with Too Many Head of Household
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Household Checks
#' @export
dq_hh_too_many_hohs <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1) %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::summarise(HoHsinHousehold = dplyr::n(),
                     PersonalID = min(PersonalID)) %>%
    dplyr::filter(HoHsinHousehold > 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
    dplyr::mutate(Issue = "Too Many Heads of Household",
                  Type = "High Priority",
                  Guidance = "Check inside the Entry pencil to be sure each household member has
        \"Relationship to Head of Household\" answered and that only one of
        them says \"Self (head of household)\".") %>%
    dplyr::select(dplyr::all_of(vars$we_want))


}

#' @title Find Households with Missing Relationship to Head of Household
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Household Checks
#' @export
dq_hh_missing_rel_to_hoh <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 99) %>%
    dplyr::anti_join(dq_hh_no_hoh()["HouseholdID"], by = "HouseholdID") %>%
    dplyr::mutate(Issue = "Missing Relationship to Head of Household",
                  Type = "High Priority",
                  Guidance = "Check inside the Entry pencil to be sure each household member has
          \"Relationship to Head of Household\" answered and that only one of
          them says \"Self (head of household)\".") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


# Missing Data at Entry ---------------------------------------------------
#' @title Find Missing Date Homeless
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_missing_approx_date_homeless <- function(served_in_date_range, vars, guidance = NULL, rm_dates = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
                    EntryDate >= rm_dates$hc$prior_living_situation_required &
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
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_missing_previous_street_ESSH <- function(served_in_date_range, vars, guidance = NULL, rm_dates = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
                    EntryDate >= rm_dates$hc$prior_living_situation_required &
                    is.na(PreviousStreetESSH) &
                    LOSUnderThreshold == 1
    ) %>%
    dplyr::mutate(Issue = "Missing Previously From Street, ES, or SH (Length of Time Homeless questions)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Prior Residence
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_missing_residence_prior <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_dkr_residence_prior <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_missing_LoS <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_dkr_LoS <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
#' @inherit data_quality_tables params return
#' @family DQ: Missing Data at Entry
#' @export
dq_missing_months_times_homeless <- function(served_in_date_range, vars, guidance = NULL, rm_dates = NULL, app_env = get_app_env(e = rlang::caller_env())) {

  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  missing_months_times_homeless <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= rm_dates$hc$prior_living_situation_required &
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
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_dkr_months_times_homeless <- function(served_in_date_range, vars, rm_dates = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
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
                    EntryDate >= rm_dates$hc$prior_living_situation_required &
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
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_invalid_months_times_homeless <- function(served_in_date_range, vars, rm_dates = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
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
        EntryDate >= rm_dates$hc$prior_living_situation_required &
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
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_missing_living_situation <- function(served_in_date_range, vars, rm_dates = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
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
                    EntryDate >= rm_dates$hc$prior_living_situation_required &
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
                  Guidance = "When responding to the Living Situation questions in your Entry Assessment, users must answer questions about some clients' situation prior to the 'Residence Prior' that are important to help determine that client's Chronicity. Please answer these questions to the best of your knowledge."
                  ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


#' @title Find Don't Know/Refused Living Situation
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_dkr_living_situation <- function(served_in_date_range, vars, rm_dates = NULL, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
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
                    EntryDate > rm_dates$hc$prior_living_situation_required &
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
#' @family Clarity Checks
#' @inherit data_quality_tables params return
#' @family DQ: Missing Data at Entry
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
      Guidance = "If the user answered 'Yes' to the 'Does the client have a disabling condition?', then there should be a disability subassessment that indicates the disability determination is Yes *and* the 'If yes,... long duration' question is Yes. Similarly if the user answered 'No', the client should not have any disability subassessments that indicate that they do have a Disabling Condition."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Clients in Mahoning with 60 Days elapsed in Coordinated Entry
#' @family Clarity Checks
#' @inherit data_quality_tables params return
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
      Guidance = "If this household is 'unreachable' as defined in the Mahoning County Coordinated Entry Policies and Procedures, they should be exited."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Clients with Extremely Long Stays
#' @family Clarity Checks
#' @inherit data_quality_tables params return
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
      Guidance =  "This client is showing as an outlier for Length of Stay for this project type in your CoC. Please verify that this client is still in your project. If they are, be sure there are no alternative permanent housing solutions for this client. If the client is no longer in your project, please enter their Exit Date as the closest estimation of the day they left your project."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

# Incorrect Destination ---------------------------------------------------

#' @title Find Incorrect Exits in RRH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Issue = "Maybe Incorrect Exit Destination (did you mean 'Rental by client, with RRH...'?)",
      Type = "Warning",
      Guidance = "This household has a Move-In Date into an RRH project that matches their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to Rapid Rehousing. If the household exited to a Destination that was not 'Rental by client', but it is a permanent destination attained through a Rapid Rehousing project, then there is no change needed. If this is not the case, then the Destination should be 'Rental by client, with RRH or equivalent subsidy'."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Possibly Incorrect Exits in PSH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Guidance = "This household appears to have an Entry into a PSH project that overlaps their Exit from your project. Typically this means the client moved into a Permanent Supportive Housing unit after their stay with you. If that is true, the Destination should be 'Permanent housing (other than RRH) for formerly homeless persons'. If you are sure the current Destination is accurate, then please leave it the way it is."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Incorrect Exits in PSH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
    Guidance = "This household appears to have a Move-In Date into a PSH project that matches their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to PSH. The correct Destination for households entering PSH from your project is 'Permanent housing (other than RRH) for formerly homeless persons'."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Incorrect Exits in TH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Guidance = "This household appears to have an Entry into a Transitional Housing project that overlaps their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to Transitional Housing. The correct Destination for households entering TH from your project is 'Transitional housing for homeless persons (including homeless youth)."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Incorrect Exits in SH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Guidance = "This household appears to have an Entry into a Safe Haven that overlaps their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to a Safe Haven. The correct Destination for households entering SH from your project is 'Safe Haven'."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

# Missing Project Stay or Incorrect Destination ---------------------------
#' @title Find Missing Project Stay or Incorrect Destination for RRH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Guidance = "The Exit Destination for this household indicates that they exited to Rapid Rehousing, but there is no RRH project stay on the client. If the RRH project the household exited to is outside of the Balance of State or Mahoning County CoCs, then no correction is necessary. If they received RRH services in the Balance of State CoC or Mahoning County CoC, then this household is missing their RRH project stay. If they did not actually receive RRH services at all, the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Project Stay or Incorrect Destination for PSH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Guidance = "The Exit Destination for this household indicates that they exited to Permanent Supportive Housing, but there is no PSH project stay on the client. If the PSH project the household exited to is outside of the Balance of State CoC or Mahoning County CoC, then no correction is necessary. If they entered PSH in the Balance of State CoC or Mahoning County CoC, then this household is missing their PSH project stay. If they did not actually enter PSH at all, the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Project Stay or Incorrect Destination for TH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Guidance = "The Exit Destination for this household indicates that they exited to Transitional Housing, but there is no TH project stay on the client. If the TH project that the household exited to is outside of the Balance of State CoC or Mahoning County CoC, then no correction is necessary. If they went into a TH project in the Balance of State CoC or Mahoning County CoC, then this household is missing their TH project stay. If they did not actually enter Transitional Housing at all, the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Project Stay or Incorrect Destination for SH
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Incorrect Destinations
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
      Guidance = "The Exit Destination for this household indicates that they exited to a Safe Haven, but there is no Entry in HMIS into a Safe Haven. Keep in mind that there is only one Safe Haven in the Balance of State and they are no longer operating as of 1/1/2021. If you meant to indicate that the household exited to a Domestic Violence shelter, please select 'Emergency shelter'."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing County Served
#' @family Clarity Checks
#' @inherit data_quality_tables params return
#' @export

dq_missing_county_served <- function(served_in_date_range, mahoning_projects, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(is.na(CountyServed) & !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(
      Issue = "Missing County Served",
      Type = "Error",
      Guidance = "County Served must be collected at Entry for all clients. County is very important so that the client is prioritized into the correct service areas for various housing solutions. This can be corrected through the Entry pencil."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing County Prior
#' @family Clarity Checks
#' @inherit data_quality_tables params return
#' @export

dq_missing_county_prior <- function(served_in_date_range, mahoning_projects, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(is.na(CountyPrior) & !ProjectID %in% c(mahoning_projects) &
                    (AgeAtEntry > 17 |
                       is.na(AgeAtEntry))) %>%
    dplyr::mutate(Issue = "Missing County of Prior Residence",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

# Check Eligibility, Project Type, Residence Prior ------------------------
#' @title Check Eligibility
#' @description The Residence Prior may suggest that the project is serving ineligible households, the household was entered into the wrong project, or the Residence Prior at Entry is incorrect.
#' @family Clarity Checks
#' @family DQ: Check Eligibility
#' @inherit data_quality_tables params return
#' @param detail \code{(logical)} Whether to return eligibility detail
#'
#' @export
dq_check_eligibility <- function(served_in_date_range, mahoning_projects, vars, rm_dates, app_env = get_app_env(e = rlang::caller_env()), detail = FALSE) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  check_eligibility <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      ProjectID,
      AgeAtEntry,
      RelationshipToHoH,
      LivingSituation,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH,
      GrantType
    ) %>%
    dplyr::filter(
      RelationshipToHoH == 1 &
        AgeAtEntry > 17 &
        EntryDate > rm_dates$hc$check_eligibility_back_to &
        (ProjectType %in% c(3, 4, 8, 9, 10, 12, 13) |
           (ProjectType == 2 & (is.na(GrantType) | GrantType != "RHY"))) &
        (
          (ProjectType %in% c(2, 3, 9, 10, 13) &
             # PTCs that require LH status
             (
               is.na(LivingSituation) |
                 (
                   LivingSituation %in% c(4:7, 15, 25:27, 29) & # institution
                     (
                       !LengthOfStay %in% c(2, 3, 10, 11) | # <90 days
                         is.na(LengthOfStay) |
                         PreviousStreetESSH == 0 | # LH prior
                         is.na(PreviousStreetESSH)
                     )
                 ) |
                 (
                   LivingSituation %in% c(3, 10, 11, 14, 19:23, 28, 31, 35, 36) &
                     # not homeless
                     (
                       !LengthOfStay %in% c(10, 11) |  # <1 week
                         is.na(LengthOfStay) |
                         PreviousStreetESSH == 0 | # LH prior
                         is.na(PreviousStreetESSH)
                     )
                 )
             )) |
            (
              ProjectType == 12 &
                (!LivingSituation %in% c(3, 10, 11, 14, 19:23, 28, 31, 35, 36) |
                   PreviousStreetESSH != 0 )
            ) |
            (ProjectType %in% c(8, 4) & # Safe Haven and Outreach
               LivingSituation != 16) # unsheltered only
        )
    )

  if (detail) {
    out <- check_eligibility %>%
      dplyr::select(
        PersonalID,
        ProjectName,
        ProjectType,
        LivingSituation,
        EntryDate,
        ExitDate,
        LengthOfStay,
        LOSUnderThreshold,
        PreviousStreetESSH
      ) %>%
      dplyr::mutate(
        ResidencePrior =
          living_situation(LivingSituation),
        LengthOfStay = dplyr::case_when(
          LengthOfStay == 2 ~ "One week or more but less than one month",
          LengthOfStay == 3 ~ "One month or more but less than 90 days",
          LengthOfStay == 4 ~ "90 days or more but less than one year",
          LengthOfStay == 5 ~ "One year or longer",
          LengthOfStay == 8 ~ "Client doesn't know",
          LengthOfStay == 9 ~ "Client refused",
          LengthOfStay == 10 ~ "One night or less",
          LengthOfStay == 11 ~ "Two to six nights",
          LengthOfStay == 99 ~ "Data not collected"
        )
      )
  } else {
    out <- check_eligibility %>%
      dplyr::mutate(
        Issue = "Check Eligibility",
        Type = "Warning",
        Guidance = "Your Residence Prior data suggests that this project is either serving ineligible households, the household was entered into the wrong project, or the Residence Prior data at Entry is incorrect. Please check the terms of your grant or speak with your CoC Team Coordinator if you are unsure of eligibility criteria for your project type."
      ) %>%
      dplyr::select(dplyr::all_of(vars$we_want))
  }
  return(out)
}

#' @title Rent Payment Made, No Move-In Date
#' @description This client does not have a valid Move-In Date, but there is at least one rent/deposit payment Service Transaction recorded for this program.  Until a Move-In Date is entered, this client will continue to be counted as literally homeless while in your program. Move-in dates must be on or after the Entry Date. If a client is housed then returns to homelessness while in your program, they need to be exited from their original Entry and re-entered in a new one that has no Move-In Date until they are re-housed.
#' @inherit data_quality_tables params return
#' @family Clarity Checks
#' @family DQ: Check Eligibility
#' @export
dq_rent_paid_no_move_in <- function(served_in_date_range, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(is.na(MoveInDateAdjust) &
                    RelationshipToHoH == 1 &
                    ProjectType %in% c(3, 9, 13)) %>%
    dplyr::inner_join(Services %>%
                        dplyr::filter(
                          Description %in% c(
                            "Rent Payment Assistance",
                            "Utility Deposit Assistance",
                            "Rental Deposit Assistance"
                          )
                        ) %>%
                        dplyr::select(-PersonalID),
                      by = "EnrollmentID") %>%
    dplyr::mutate(
      Issue = "Rent Payment Made, No Move-In Date",
      Type = "Error",
      Guidance = "This client does not have a valid Move-In Date, but there is at least one rent/deposit payment Service Transaction recorded for this program. Until a Move-In Date is entered, this client will continue to be counted as literally homeless while in your program. Move-in dates must be on or after the Entry Date. If a client is housed then returns to homelessness while in your program, they need to be exited from their original Entry and re-entered in a new one that has no Move-In Date until they are re-housed."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Missing Destination
#' @description It is widely understood that not every client will complete an exit interview, especially for high-volume emergency shelters. A few warnings for Missing Destination is no cause for concern, but if there is a large number this will surface these errors.
#' @family Clarity Checks
#' @family DQ: Check Eligibility
#' @inherit data_quality_tables params return
#' @export

dq_missing_destination <- function(served_in_date_range,  mahoning_projects, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(!is.na(ExitDate) &
                    (is.na(Destination) | Destination %in% c(99, 30))) %>%
    dplyr::mutate(
      Issue = "Missing Destination",
      Type = "Warning",
      Guidance = "It is widely understood that not every client will complete an exit interview, especially for high-volume emergency shelters. A few warnings for Missing Destination is no cause for concern, but if there is a large number, please contact your CoC Team Coordinator"
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Don't Know Refused Destination
#' @family Clarity Checks
#' @family DQ: Check Eligibility
#' @inherit data_quality_tables params return
#' @export
dq_dkr_destination <- function(served_in_date_range,
                               vars,
                               app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(Destination %in% c(8, 9)) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Destination",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

# Missing PATH Data -------------------------------------------------------


#' @title Return a subset of Project data
#' @param Project \code{(data.frame)} of Project with additional features added in `load_export`
#' @family Clarity Checks
#' @family DQ: Path Checks
#' @return \code{(data.frame)} with ProjectID, ProjectName, ProjectCounty

dq_project_small <- function(Projec) {
  Project |> dplyr::select(ProjectID,
                           ProjectName,
                           ProjectCounty)
}



#' @title PATH: Missing Residence Prior Length of Stay
#' @inherit data_quality_tables params return
#' @family DQ: Path Checks
#' @inheritParams dq_project_small
#' @export
dq_path_missing_los_res_prior <- function(served_in_date_range, Project, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      ProjectID,
      AgeAtEntry,
      ClientEnrolledInPATH,
      LengthOfStay
    ) %>%
    dplyr::left_join(dq_project_small(Project), by = c("ProjectID", "ProjectName")) %>%
    dplyr::filter(AgeAtEntry > 17 &
                    ClientEnrolledInPATH == 1 &
                    (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
    dplyr::mutate(Issue = "Missing Residence Prior Length of Stay (PATH)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


#' @title PATH: Status at Exit Missing or Incomplete
#' @details Engagement at Exit & adult, PATH-enrolled, Date of Engagement is null -> error
#' @family Clarity Checks
#' @family DQ: Path Checks
#' @inherit data_quality_tables params return
#' @inheritParams dq_project_small
#' @export
dq_path_no_status_at_exit <- function(served_in_date_range, vars, Project, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      ClientEnrolledInPATH,
      DateOfPATHStatus,
      ReasonNotEnrolled
    ) %>%
    dplyr::left_join(dq_project_small(Project), by = "ProjectName") %>%
    dplyr::filter(!is.na(ExitDate) &
                    AgeAtEntry > 17 &
                    (
                      is.na(ClientEnrolledInPATH) |
                        is.na(DateOfPATHStatus) |
                        (ClientEnrolledInPATH == 0 &
                           is.na(ReasonNotEnrolled))
                    )) %>%
    dplyr::mutate(Issue = "PATH Status at Exit Missing or Incomplete",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}



#' @title PATH: Missing Date of PATH Status
#' @description Users must indicate the PATH Status Date for any adult enrolled in PATH.
#' @family DQ: Path Checks
#' @family Clarity Checks
#' @inherit data_quality_tables params return
#' @inheritParams dq_project_small
#' @details Status Determination at Exit &adult, PATH-Enrolled is not null & Date of Status is null -> error
#' @export

dq_path_status_determination <- function(served_in_date_range, Project, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  ClientEnrolledInPATH,
                  DateOfPATHStatus) %>%
    dplyr::left_join(dq_project_small(Project), by = "ProjectName") %>%
    dplyr::filter(AgeAtEntry > 17 &
                    !is.na(ClientEnrolledInPATH) &
                    is.na(DateOfPATHStatus)
    ) %>%
    dplyr::mutate(Issue = "Missing Date of PATH Status",
                  Type = "Error",
                  Guidance = "Users must indicate the PATH Status Date for any adult enrolled in PATH.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}



#' @title PATH: Missing PATH Enrollment at Exit
#' @description Users must indicate the PATH Enrollment Date at Entry, Exit when creating an Interim
#' @family Clarity Checks
#' @family DQ: Path Checks
#' @inherit data_quality_tables params return
#' @inheritParams dq_project_small
#' @details PATH Enrolled at Exit & adult & PATH Enrolled null or DNC -> error
#' @export
dq_path_enrolled_missing <- function(served_in_date_range, Project, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), AgeAtEntry, ClientEnrolledInPATH) %>%
    dplyr::left_join(dq_project_small(Project), by = "ProjectName") %>%
    dplyr::filter(!is.na(ExitDate) &
                    AgeAtEntry > 17 &
                    (ClientEnrolledInPATH == 99 |
                       is.na(ClientEnrolledInPATH))
    ) %>%
    dplyr::mutate(
      Issue = "Missing PATH Enrollment at Exit",
      Type = "Error",
      Guidance = guidance$path_enrolled_missing
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}



#' @title PATH: Missing Reason Not PATH Enrolled
#' @description The user has indicated the household was not enrolled into PATH, but no reason was selected.
#' @family Clarity Checks
#' @family DQ: Path Checks
#' @inherit data_quality_tables params return
#' @inheritParams dq_project_small
#' @details adult & PATH Enrolled = No & Reason is null -> error
#' @export
dq_path_reason_missing <- function(served_in_date_range, Project, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      ClientEnrolledInPATH,
      ReasonNotEnrolled,
      ProjectType
    ) %>%
    dplyr::left_join(dq_project_small(Project), by = "ProjectName") %>%
    dplyr::filter(AgeAtEntry > 17 &
                    ClientEnrolledInPATH == 0 &
                    is.na(ReasonNotEnrolled)) %>%
    dplyr::mutate(
      Issue = "Missing Reason Not PATH Enrolled",
      Type = "Error",
      Guidance = guidance$path_reason_missing
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}



#' @title PATH: Missing Connection with SOAR at Exit
#' @family Clarity Checks
#' @family DQ: Path Checks
#' @inherit data_quality_tables params return
#' @inheritParams dq_project_small
#' @details adult & Connection w/ SOAR is null or DNC -> error
#' @export
dq_path_SOAR_missing_at_exit <- function(served_in_date_range, Project, IncomeBenefits, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  smallIncomeSOAR <- IncomeBenefits %>%
    dplyr::select(PersonalID,
                  EnrollmentID,
                  ConnectionWithSOAR,
                  DataCollectionStage) %>%
    dplyr::filter(DataCollectionStage == 3)

  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  AgeAtEntry,
                  ClientEnrolledInPATH) %>%
    dplyr::left_join(dq_project_small(Project), by = "ProjectName") %>%
    dplyr::left_join(smallIncomeSOAR, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::filter(AgeAtEntry > 17 &
                    DataCollectionStage == 3 &
                    is.na(ConnectionWithSOAR)) %>%
    dplyr::mutate(Issue = "Missing Connection with SOAR at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


#' @title PATH: Missing PATH Contact
#' @family Clarity Checks
#' @family DQ: Path Checks
#' @description  Every adult or Head of Household must have a Living Situation contact record. If you see a record there but there is no Date of Contact, saving the Date of Contact will correct this issue. This is a high priority DQ issue.
#' @inherit data_quality_tables params return
#' @inheritParams dq_project_small
#' @param Contacts \code{(data.frame)} From the HUD CSV Export
#' @details client is adult/hoh and has no contact record in the EE -> error
#' @export
dq_missing_path_contact <- function(served_in_date_range, Contacts, rm_dates, vars, app_env = get_app_env(e = rlang::caller_env())) {
  ## this is a high priority data quality issue
  ## if the contact was an "Outreach" record after 10/1/2019, it is being
  ## filtered out because they should be using CLS subs past that date.
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  small_contacts <-  Contacts %>%
    dplyr::left_join(served_in_date_range, by = "PersonalID") %>%
    dplyr::filter(
      ContactDate >= EntryDate &
        ContactDate <= ExitAdjust &
        ContactDate < rm_dates$hc$outreach_to_cls
    ) %>%
    dplyr::group_by(PersonalID, ProjectName, EntryDate, ExitDate) %>%
    dplyr::summarise(ContactCount = dplyr::n()) %>%
    dplyr::ungroup()

  served_in_date_range %>%
    dplyr::filter(GrantType == "PATH" &
                    (AgeAtEntry > 17 |
                       RelationshipToHoH == 1)) %>%
    dplyr::select(dplyr::all_of(vars$prep)) %>%
    dplyr::left_join(small_contacts,
                     by = c("PersonalID",
                            "ProjectName",
                            "EntryDate",
                            "ExitDate")) %>%

    dplyr::filter(is.na(ContactCount)) %>%
    dplyr::mutate(
      Issue = "Missing PATH Contact",
      Type = "High Priority",
      Guidance = guidance$missing_path_contact
    )  |>
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title PATH: Incorrect PATH Contact Date
#' @family Clarity Checks
#' @family DQ: Path Checks
#' @description Every adult or head of household should have a Living Situation contact record where the Contact Date matches the Entry Date. This would represent the initial contact made with the client.
#' @inherit data_quality_tables params return
#' @inheritParams dq_project_small
#' @param Contacts \code{(data.frame)} From the HUD CSV Export
#' @details client is adult/hoh, has a contact record, and the first record in the EE does not equal the Entry Date ->  error
#' @export

dq_incorrect_path_contact_date <- function(served_in_date_range, Contacts, rm_dates, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  first_contact <- Contacts %>%
    dplyr::filter(ContactDate < rm_dates$hc$outreach_to_cls) %>%
    dplyr::left_join(served_in_date_range, by = "PersonalID") %>%
    dplyr::select(PersonalID, EntryDate, ExitAdjust, ExitDate, ContactDate, ProjectName,
                  EntryDate, ExitAdjust) %>%
    dplyr::filter(ContactDate >= EntryDate &
                    ContactDate <= ExitAdjust) %>%
    dplyr::group_by(PersonalID, ProjectName, EntryDate, ExitDate) %>%
    dplyr::arrange(ContactDate) %>%
    dplyr::slice(1L)

  incorrect_path_contact_date <- served_in_date_range %>%
    dplyr::filter(GrantType == "PATH" &
                    (AgeAtEntry > 17 |
                       RelationshipToHoH == 1)) %>%
    dplyr::select(dplyr::all_of(vars$prep)) %>%
    dplyr::inner_join(first_contact, by = c("PersonalID",
                                            "ProjectName",
                                            "EntryDate",
                                            "ExitDate")) %>%
    dplyr::filter(ContactDate != EntryDate) %>%
    dplyr::mutate(
      Issue = "No PATH Contact Entered at Entry",
      Type = "Error",
      Guidance = guidance$incorrect_path_contact_date
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

}

# Entry Exits ------------------------------------------------------

#' @title Find Duplicate EEs
#' @family Clarity Checks
#' @family DQ: EE Checks
#' @description Users sometimes create this error when they forget to click into a program stay by using the Entry pencil, and instead they click \"Add Entry/Exit\" each time. To correct, EDA to the project the Entry/Exit belongs to, navigate to the Entry/Exit tab and delete the program stay that was accidentally added for each household member.
#' @inherit data_quality_tables params return
#' @export
dq_duplicate_ees <- function(served_in_date_range, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  janitor::get_dupes(served_in_date_range, PersonalID, ProjectID, EntryDate) %>%
    dplyr::mutate(
      Issue = "Duplicate Entry Exits",
      Type = "High Priority",
      Guidance = guidance$duplicate_ees
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Future EEs
#' @family Clarity Checks
#' @family DQ: EE Checks
#' @description Users should not be entering a client into a project on a date in the future. If the Entry Date is correct, there is no action needed, but going forward, please be sure that your data entry workflow is correct according to your project type.
#' @inherit data_quality_tables params return
#' @export

dq_future_ees <- function(served_in_date_range, rm_dates, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::filter(EntryDate > DateCreated &
                    (ProjectType %in% c(1, 2, 4, 8, 13) |
                       (
                         ProjectType %in% c(3, 9) &
                           EntryDate >= rm_dates$hc$psh_started_collecting_move_in_date
                       )))  %>%
    dplyr::mutate(
      Issue = "Future Entry Date",
      Type = "Warning",
      Guidance = guidance$future_ees
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


}

#' @title Find Future Exits
#' @family Clarity Checks
#' @family DQ: EE Checks
#' @description This client's Exit Date is a date in the future. Please enter the exact date the client left your program. If this client has not yet exited, delete the Exit and then enter the Exit Date once the client is no longer in your program.
#' @inherit data_quality_tables params return
#' @export
dq_future_exits <- function(served_in_date_range, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
  dplyr::filter(ExitDate > lubridate::today()) %>%
  dplyr::mutate(
    Issue = "Future Exit Date",
    Type = "Error",
    Guidance = guidance$future_exits
  ) %>%
  dplyr::select(dplyr::all_of(vars$we_want))
}

# HoHs Entering PH without SPDATs -----------------------------------------

#' @title Find Non-DV HoHs Entering PH or TH without SPDAT, HoHs in shelter for 8+ days without SPDAT, and SPDAT Created on a Non-HoH
#' @family Clarity Checks
#' @family DQ: SPDAT Checks
#' @description This checks for three warning types:
#' \itemize{
#'   \item{Non-DV HoHs Entering PH or TH without SPDAT}{ Every household (besides those fleeing domestic violence) must have a VI-SPDAT score to aid with prioritization into a Transitional Housing or Permanent Housing (RRH or PSH) project.}
#'   \item{HoHs in shelter for 8+ days without SPDAT}{ Any household who has been in shelter or a Safe Haven for over 8 days should be assessed with the VI-SPDAT so that they can be prioritized for Permanent Housing (RRH or PSH).}
#'   \item{SPDAT Created on a Non-Head-of-Household}{ It is very important to be sure that the VI-SPDAT score goes on the Head of Household of a given program stay because otherwise that score may not pull into any reporting. It is possible a Non Head of Household was a Head of Household in a past program stay, and in that situation, this should not be corrected unless the Head of Household of your program stay is missing their score. To correct this, you would need to completely re-enter the score on the correct client's record.}
#' }
#' @inherit data_quality_tables params return
#' @export

dq_ph_without_spdats <- function(served_in_date_range, Funder, rm_dates, vars, app_env = get_app_env(e = rlang::caller_env()), unsh = FALSE) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  va_funded <- Funder |>
    Funder_VA_ProjectID()


  ees_with_spdats <- served_in_date_range %>%
    dplyr::anti_join(va_funded, by = "ProjectID") %>%
    dplyr::left_join(Scores, by = "PersonalID") %>%
    dplyr::ungroup() %>%
    dplyr::select(PersonalID,
                  EnrollmentID,
                  RelationshipToHoH,
                  EntryDate,
                  ExitAdjust,
                  ScoreDate,
                  Score) %>%
    dplyr::filter(ScoreDate + lubridate::days(365) > EntryDate &
                    # score is < 1 yr old
                    ScoreDate < ExitAdjust) %>%  # score is prior to Exit
    dplyr::group_by(EnrollmentID) %>%
    dplyr::slice_max(ScoreDate) %>%
    dplyr::slice_max(Score) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ScoreAdjusted = dplyr::if_else(is.na(Score), 0, Score))

  entered_ph_without_spdat <-
    dplyr::anti_join(served_in_date_range, ees_with_spdats, by = "EnrollmentID") %>%
    dplyr::filter(
      ProjectType %in% c(2, 3, 9, 13) &
        EntryDate > rm_dates$hc$began_requiring_spdats &
        # only looking at 1/1/2019 forward
        RelationshipToHoH == 1 &
        (CurrentlyFleeing != 1 |
           is.na(CurrentlyFleeing) |
           !WhenOccurred %in% c(1:3))
    ) %>%
    dplyr::mutate(
      Issue = "Non-DV HoHs Entering PH or TH without SPDAT",
      Type = "Warning",
      Guidance = guidance$ph_without_spdats
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # HoHs in Shelter without a SPDAT -----------------------------------------

  lh_without_spdat <- served_in_date_range %>%
    dplyr::filter(is.na(PHTrack) | PHTrack != "Self Resolve" |
                    ExpectedPHDate < lubridate::today()) %>%
    dplyr::anti_join(ees_with_spdats, by = "EnrollmentID") %>%
    dplyr::filter(
      ProjectType %in% c(1, 4, 8) &
        VeteranStatus != 1 &
        RelationshipToHoH == 1 &
        EntryDate < lubridate::today() - lubridate::days(8) &
        is.na(ExitDate) &
        EntryDate > rm_dates$hc$began_requiring_spdats
    ) %>%
    dplyr::mutate(
      Issue = "HoHs in shelter for 8+ days without SPDAT",
      Type = "Warning",
      Guidance = guidance$lh_without_spdats
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  spdat_on_non_hoh <- ees_with_spdats %>%
    dplyr::left_join(
      served_in_date_range,
      by = c(
        "PersonalID",
        "EnrollmentID",
        "RelationshipToHoH",
        "EntryDate",
        "ExitAdjust"
      )
    ) %>%
    dplyr::filter(RelationshipToHoH != 1) %>%
    dplyr::mutate(
      Issue = "SPDAT Created on a Non-Head-of-Household",
      Type = "Warning",
      Guidance = guidance$spdat_on_non_hoh
      ) %>%
      dplyr::select(dplyr::all_of(vars$we_want))


  out <- dplyr::bind_rows(spdat_on_non_hoh, lh_without_spdat, entered_ph_without_spdat, va_funded)
  if (unsh && must_sp())
    out <- dplyr::bind_rows(lh_without_spdat, spdat_on_non_hoh)

  out
}

# Income Checks ----
# Thu Sep 16 17:25:10 2021

#' @title Find Conflicting Income yes/no at Entry or Exit
#' @family DQ: Income Checks
#' @family Clarity Checks
#' @family DQ: EE Checks
#' @description If the user answered Yes to Income from any source, then  there should be an income sub-assessment where it indicates which type of income the client is receiving. Similarly if the user answered No, there should not be any income records that say the client is receiving that type of income.
#' @inherit data_quality_tables params return
#' @export


dq_conflicting_income <- function(served_in_date_range, IncomeBenefits, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  # Not calculating Conflicting Income Amounts bc they're calculating the TMI from the
  # subs instead of using the field itself. Understandable but that means I would
  # have to pull the TMI data in through RMisc OR we kill TMI altogether. (We
  # decided to kill TMI altogether.)
  smallIncome <- IncomeBenefits %>%
    dplyr::select(
      PersonalID,
      EnrollmentID,
      Earned,
      Unemployment,
      SSI,
      SSDI,
      VADisabilityService,
      VADisabilityNonService,
      PrivateDisability,
      WorkersComp,
      TANF,
      GA,
      SocSecRetirement,
      Pension,
      ChildSupport,
      Alimony,
      OtherIncomeSource,
      DataCollectionStage
    )

  smallIncome[is.na(smallIncome)] <- 0

  smallIncome <-
    smallIncome %>% dplyr::full_join(IncomeBenefits[c(
      "PersonalID",
      "EnrollmentID",
      "DataCollectionStage",
      "TotalMonthlyIncome",
      "IncomeFromAnySource"
    )],
    by = c("PersonalID",
           "EnrollmentID",
           "DataCollectionStage"))

  income_subs <- served_in_date_range[c("EnrollmentID",
                                        "AgeAtEntry",
                                        vars$prep)] %>%
    dplyr::left_join(smallIncome, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::mutate(
      IncomeCount =
        Earned +
        Unemployment +
        SSI +
        SSDI +
        VADisabilityService +
        VADisabilityNonService +
        PrivateDisability +
        WorkersComp +
        TANF +
        GA +
        SocSecRetirement +
        Pension +
        ChildSupport +
        Alimony +
        OtherIncomeSource
    )


  conflicting_income_entry <- income_subs %>%
    dplyr::filter(DataCollectionStage == 1 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((IncomeFromAnySource == 1 &
                        IncomeCount == 0) |
                       (IncomeFromAnySource == 0 &
                          IncomeCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Income yes/no at Entry",
                  Type = "Error",
                  Guidance = guidance$conflicting_income) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  conflicting_income_exit <- income_subs %>%
    dplyr::filter(DataCollectionStage == 3 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((IncomeFromAnySource == 1 &
                        IncomeCount == 0) |
                       (IncomeFromAnySource == 0 &
                          IncomeCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Income yes/no at Exit",
                  Type = "Error",
                  Guidance = guidance$conflicting_income) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dplyr::bind_rows(conflicting_income_exit, conflicting_income_exit)

}

#' @title Find Conflicting Income yes/no at Entry or Exit
#' @family DQ: Income Checks
#' @family Clarity Checks
#' @family DQ: EE Checks
#' @description Please enter the data for this item by clicking into the Entry or Exit pencil on the given Client ID on the appropriate program stay.
#' @inherit data_quality_tables params return
#' @export

dq_missing_income <- function(served_in_date_range, IncomeBenefits, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  missing_income_entry <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      DataCollectionStage,
      TotalMonthlyIncome,
      IncomeFromAnySource
    ) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    (AgeAtEntry > 17 |
                       is.na(AgeAtEntry)) &
                    (IncomeFromAnySource == 99 |
                       is.na(IncomeFromAnySource))) %>%
    dplyr::mutate(Issue = "Income Missing at Entry",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_income_exit <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      DataCollectionStage,
      TotalMonthlyIncome,
      IncomeFromAnySource,
      UserCreating
    ) %>%
    dplyr::filter(DataCollectionStage == 3 &
                    (AgeAtEntry > 17 |
                       is.na(AgeAtEntry)) &
                    (IncomeFromAnySource == 99 |
                       is.na(IncomeFromAnySource))) %>%
    dplyr::mutate(Issue = "Income Missing at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
  dplyr::bind_rows(missing_income_entry, missing_income_exit)

}

# Overlapping Enrollment/Move In Dates ------------------------------------

#' @title Find Overlapping Project Stays
#' @family Clarity Checks
#' @family ServicePoint Checks
#' @family DQ: Overlapping Enrollment/Move-In Dates
#' @description A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not. Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the 'Previous Provider's' mistake, but if you are seeing clients here, it means your project stay was entered last. If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected or send an email to \href{mailto:hmis\@cohhio.org}{hmis\@cohhio.org} if you cannot get it resolved. These clients will NOT show on their Data Quality app. If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed.
#' @inherit data_quality_tables params return
#' @export

dq_overlaps <- function(served_in_date_range, Users, vars, guidance, app_env = get_app_env(e = rlang::caller_env()), unsh = FALSE) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  dq_overlaps <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      EntryAdjust = dplyr::case_when(
        #for PSH and RRH, EntryAdjust = MoveInDate
        ProjectType %in% c(1, 2, 8, 12) |
          ProjectName == "Unsheltered Clients - OUTREACH" ~ EntryDate,
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
        ProjectType %in% c(3, 9, 13) &
          is.na(MoveInDateAdjust) ~ EntryDate
      ),
      ExitAdjust = ExitAdjust - lubridate::days(1),
      # bc a client can exit&enter same day
      LiterallyInProject = dplyr::if_else(
        ProjectType %in% c(3, 9, 13),
        lubridate::interval(MoveInDateAdjust, ExitAdjust),
        lubridate::interval(EntryAdjust, ExitAdjust)
      ),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = guidance$project_stays
    ) %>%
    dplyr::filter(!is.na(LiterallyInProject) &
                    lubridate::int_length(LiterallyInProject) > 0) |>
    janitor::get_dupes(PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryAdjust) %>%
    dplyr::mutate(
      PreviousEntryAdjust = dplyr::lag(EntryAdjust),
      PreviousExitAdjust = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(!is.na(PreviousEntryAdjust)) %>%
    dplyr::ungroup() |>
    dplyr::mutate(
      PreviousStay = lubridate::interval(PreviousEntryAdjust, PreviousExitAdjust),
      Overlap = lubridate::int_overlaps(LiterallyInProject, PreviousStay)
    ) %>%
    dplyr::filter(Overlap == TRUE) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  if (unsh && must_sp()) {
    dq_overlaps <- dq_overlaps %>%
      dplyr::filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
      dplyr::left_join(Users, by = "UserCreating") %>%
      dplyr::select(PersonalID,
                    DefaultProvider,
                    EntryDate,
                    ExitDate,
                    PreviousProject)
  }
  dq_overlaps
}

#' @title Find Overlapping Project Stays on the Same Day
#' @family Clarity Checks
#' @family DQ: Overlapping Enrollment/Move-In Dates
#' @inherit dq_overlaps params return description
#' @export
dq_overlaps_same_day <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::filter((ProjectType == 13 & MoveInDateAdjust == ExitDate) |
                    ProjectType != 13) %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      EntryAdjust = dplyr::case_when(
        #for PSH and RRH, EntryAdjust = MoveInDate
        ProjectType %in% c(1, 2, 8, 12) |
          ProjectName == "Unsheltered Clients - OUTREACH" ~ EntryDate,
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
        ProjectType %in% c(3, 9, 13) &
          is.na(MoveInDateAdjust) ~ EntryDate
      ),
      LiterallyInProject = dplyr::case_when(
        ProjectType %in% c(3, 9) ~ lubridate::interval(MoveInDateAdjust, ExitAdjust),
        ProjectType %in% c(1, 2, 4, 8, 12) ~ lubridate::interval(EntryAdjust, ExitAdjust)
      ),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = guidance$project_stays
    ) %>%
    dplyr::filter((!is.na(LiterallyInProject) & ProjectType != 13) |
                    ProjectType == 13) %>%
    janitor::get_dupes(PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryAdjust) %>%
    dplyr::mutate(
      PreviousEntryAdjust = dplyr::lag(EntryAdjust),
      PreviousExitAdjust = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(ExitDate > PreviousEntryAdjust &
                    ExitDate < PreviousExitAdjust) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(vars$we_want), PreviousProject)
}

#' @title Find Overlapping Project Stays for RRH
#' @family Clarity Checks
#' @family DQ: Overlapping Enrollment/Move-In Dates
#' @inherit dq_overlaps params return description
#' @export
dq_overlaps_rrh <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      ExitAdjust = ExitAdjust - lubridate::days(1),
      # bc a client can exit&enter same day
      InProject = lubridate::interval(EntryDate, ExitAdjust),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = guidance$project_stays
    ) %>%
    dplyr::filter(ProjectType == 13) |>
    janitor::get_dupes(PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryDate) %>%
    dplyr::mutate(
      PreviousEntry = dplyr::lag(EntryDate),
      PreviousExit = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(!is.na(PreviousEntry)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      PreviousStay = lubridate::interval(PreviousEntry, PreviousExit),
      Overlap = lubridate::int_overlaps(InProject, PreviousStay)
    ) %>%
    dplyr::filter(Overlap == TRUE) %>%
    dplyr::select(dplyr::all_of(vars$we_want), PreviousProject)
}

#' @title Find Overlapping Project Stays for PSH
#' @family Clarity Checks
#' @family DQ: Overlapping Enrollment/Move-In Dates
#' @inherit dq_overlaps params return description
#' @export
dq_overlaps_psh <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      ExitAdjust = ExitAdjust - lubridate::days(1),
      # bc a client can exit&enter same day
      InProject = lubridate::interval(EntryDate, ExitAdjust),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = guidance$project_stay
    ) %>%
    dplyr::filter(ProjectType == 3) |>
    janitor::get_dupes(PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryDate) %>%
    dplyr::mutate(
      PreviousEntry = dplyr::lag(EntryDate),
      PreviousExit = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(!is.na(PreviousEntry)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      PreviousStay = lubridate::interval(PreviousEntry, PreviousExit),
      Overlap = lubridate::int_overlaps(InProject, PreviousStay)
    ) %>%
    dplyr::filter(Overlap == TRUE) %>%
    dplyr::select(dplyr::all_of(vars$we_want), PreviousProject)
}

# Missing Health Ins ------------------------------------------------------
#' @title Find Missing Health Insurance at Entry
#' @family Clarity Checks
#' @family DQ: Health Insurance Checks
#' @description This data element is required to be collected at project Entry or Exit. Please click into the client's Entry/Exit pencil to save this data to HMIS.
#' @inherit data_quality_tables params return
#' @export


dq_missing_hi_entry <- function(served_in_date_range,  IncomeBenefits, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  DataCollectionStage,
                  InsuranceFromAnySource) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    (InsuranceFromAnySource == 99 |
                       is.na(InsuranceFromAnySource))) %>%
    dplyr::mutate(Issue = "Health Insurance Missing at Entry",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Health Insurance at Exit
#' @family Clarity Checks
#' @family DQ: Health Insurance Checks
#' @description This data element is required to be collected at project Entry or Exit. Please click into the client's Entry/Exit pencil to save this data to HMIS.
#' @inherit data_quality_tables params return
#' @export
dq_missing_hi_exit <- function(served_in_date_range,  IncomeBenefits, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  InsuranceFromAnySource) %>%
    dplyr::filter(DataCollectionStage == 3 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    (InsuranceFromAnySource == 99 |
                       is.na(InsuranceFromAnySource))) %>%
    dplyr::mutate(Issue = "Health Insurance Missing at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Conflicting Health Insurance at Entry/Exit
#' @family Clarity Checks
#' @family DQ: Health Insurance Checks
#' @description If the user answered 'Yes' to 'Covered by Health Insurance?', then there should be a Health Insurance subassessment where it indicates which type of health insurance the client is receiving. Similarly if the user answered 'No', there should not be any Health Insurance records that say the client is receiving that type of Health Insurance.
#' @inherit data_quality_tables params return
#' @export


dq_conflicting_hi_ee <- function(served_in_date_range,  IncomeBenefits, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  hi_subs <-
    served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      DataCollectionStage,
      InsuranceFromAnySource,
      Medicaid,
      Medicare,
      SCHIP,
      VAMedicalServices,
      EmployerProvided,
      COBRA,
      PrivatePay,
      StateHealthIns,
      IndianHealthServices,
      OtherInsurance,
      HIVAIDSAssistance,
      ADAP,
      UserCreating
    ) %>%
    dplyr::mutate(
      SourceCount = Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
        COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
        OtherInsurance + Medicare
    )

  conflicting_hi_entry <- hi_subs |>
    dplyr::filter(DataCollectionStage == 1 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    ((InsuranceFromAnySource == 1 &
                        SourceCount == 0) |
                       (InsuranceFromAnySource == 0 &
                          SourceCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Health Insurance yes/no at Entry",
                  Type = "Error",
                  Guidance = guidance$conflicting_hi) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  conflicting_hi_exit <- hi_subs %>%
    dplyr::filter(DataCollectionStage == 3 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    ((InsuranceFromAnySource == 1 &
                        SourceCount == 0) |
                       (InsuranceFromAnySource == 0 &
                          SourceCount > 0)
                    )) %>%
    dplyr::mutate(
      Issue = "Conflicting Health Insurance yes/no at Exit",
      Type = "Error",
      Guidance = guidance$conflicting_hi
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
  dplyr::bind_rows(conflicting_hi_entry, conflicting_hi_exit)
}

#' @title Find Missing Non-Cash Benefits (NCBS) at Entry/Exit
#' @family Clarity Checks
#' @family DQ: Non-Cash Benefit Checks
#' @description This data element is required to be collected at project Entry or Exit. Please click into the client's Entry/Exit pencil to save this data to HMIS.
#' @inherit data_quality_tables params return
#' @export

dq_missing_ncbs <- function(served_in_date_range, IncomeBenefits, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  missing_ncbs_entry <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(AgeAtEntry,
                  dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  BenefitsFromAnySource) %>%
    dplyr::filter(
      DataCollectionStage == 1 &
        (AgeAtEntry > 17 |
           is.na(AgeAtEntry)) &
        (BenefitsFromAnySource == 99 |
           is.na(BenefitsFromAnySource))
    ) %>%
    dplyr::mutate(Issue = "Non-cash Benefits Missing at Entry",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_ncbs_exit <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(AgeAtEntry,
                  dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  BenefitsFromAnySource) %>%
    dplyr::filter(
      DataCollectionStage == 3 &
        (AgeAtEntry > 17 |
           is.na(AgeAtEntry)) &
        (BenefitsFromAnySource == 99 |
           is.na(BenefitsFromAnySource))
    ) %>%
    dplyr::mutate(Issue = "Non-cash Benefits Missing at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
  dplyr::bind_rows(missing_ncbs_exit, missing_ncbs_entry)

}

#' @title Find Conflicting or Unlikely Non-Cash Benefits (NCBS) at Entry/Exit
#' @family DQ: Non-Cash Benefit Checks
#' @family Clarity Checks
#' @description
#' \itemize{
#'   \item{Conflicting NCBs}{  If the user answered 'Yes' to 'Non-cash benefits from any source', then there should be a Non-cash benefits subassessment where it indicates which type of income the client is receiving. Similarly if the user answered 'No', then there should not be any non-cash records that say the client is receiving that type of benefit}
#'   \item{Unlikeley NCBs}{ This client has every single Non-Cash Benefit, according to HMIS, which is highly unlikely. Please correct (unless it's actually true).}
#' }
#' @inherit data_quality_tables params return
#' @export

dq_conflicting_unlikely_ncbs <- function(served_in_date_range, IncomeBenefits, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  ncb_subs <- IncomeBenefits %>%
    dplyr::select(
      PersonalID,
      EnrollmentID,
      DataCollectionStage,
      SNAP,
      WIC,
      TANFChildCare,
      TANFTransportation,
      OtherTANF,
      OtherBenefitsSource
    )

  ncb_subs[is.na(ncb_subs)] <- 0

  ncb_subs <- ncb_subs %>%
    dplyr::full_join(IncomeBenefits[c("PersonalID",
                                      "EnrollmentID",
                                      "DataCollectionStage",
                                      "BenefitsFromAnySource")],
                     by = c("PersonalID",
                            "EnrollmentID",
                            "DataCollectionStage"))

  ncb_subs <- served_in_date_range %>%
    dplyr::filter(ProjectName != "Unsheltered Clients - OUTREACH") %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      PersonalID,
      EnrollmentID,
      HouseholdID,
      AgeAtEntry,
      ProjectName,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      ProjectType,
      DataCollectionStage,
      BenefitsFromAnySource,
      SNAP,
      WIC,
      TANFChildCare,
      TANFTransportation,
      OtherTANF,
      OtherBenefitsSource,
      UserCreating
    ) %>%
    dplyr::mutate(
      BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation +
        OtherTANF + OtherBenefitsSource
    ) %>%
    dplyr::select(PersonalID,
                  EnrollmentID,
                  DataCollectionStage,
                  BenefitsFromAnySource,
                  BenefitCount) %>%
    unique()

  unlikely_ncbs_entry <- served_in_date_range %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      AgeAtEntry,
      dplyr::all_of(vars$prep),
      DataCollectionStage,
      BenefitsFromAnySource,
      BenefitCount
    ) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    (BenefitCount == 6)) %>%
    dplyr::mutate(Issue = "Client has ALL SIX Non-cash Benefits at Entry",
                  Type = "Warning",
                  Guidance = guidance$unlikely_ncbs) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  conflicting_ncbs_entry <- served_in_date_range %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(AgeAtEntry,
                  dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  BenefitsFromAnySource,
                  BenefitCount) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((BenefitsFromAnySource == 1 &
                        BenefitCount == 0) |
                       (BenefitsFromAnySource == 0 &
                          BenefitCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Non-cash Benefits yes/no at Entry",
                  Type = "Error",
                  Guidance = guidance$conflicting_ncbs) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  conflicting_ncbs_exit <- served_in_date_range %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      AgeAtEntry,
      dplyr::all_of(vars$prep),
      DataCollectionStage,
      BenefitsFromAnySource,
      BenefitCount
    ) %>%
    dplyr::filter(DataCollectionStage == 3 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((BenefitsFromAnySource == 1 &
                        BenefitCount == 0) |
                       (BenefitsFromAnySource == 0 &
                          BenefitCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Non-cash Benefits yes/no at Exit",
                  Type = "Error",
                  Guidance = guidance$conflicting_ncbs) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dplyr::bind_rows(unlikely_ncbs_entry, conflicting_ncbs_exit, conflicting_ncbs_entry)

}

#' @title Find SSI/SSDI but no Disability
#' @family Clarity Checks
#' @description `r guidance$check_disability_ssi`
#' @inherit data_quality_tables params return
#' @export

dq_check_disability_ssi <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  AgeAtEntry,
                  DisablingCondition) %>%
    dplyr::left_join(IncomeBenefits %>%
                       dplyr::select(EnrollmentID, PersonalID, SSI, SSDI), by = c("EnrollmentID", "PersonalID")) %>%
    dplyr::mutate(SSI = dplyr::if_else(is.na(SSI), 0, SSI),
                  SSDI = dplyr::if_else(is.na(SSDI), 0, SSDI)) %>%
    dplyr::filter(SSI + SSDI > 0 &
                    DisablingCondition == 0 & AgeAtEntry > 17) %>%
    dplyr::select(-DisablingCondition, -SSI, -SSDI, -AgeAtEntry) %>%
    unique() %>%
    dplyr::mutate(
      Issue = "Client with No Disability Receiving SSI/SSDI (could be ok)",
      Type = "Warning",
      Guidance = guidance$check_disability_ssi
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Services on Household Members
#' @family Clarity Checks
#' @description `r guidance$services_on_non_hoh`
#' @family DQ: Household Checks
#' @inherit data_quality_tables params return
#' @export

dq_services_on_non_hoh <- function(served_in_date_range, vars, rm_dates, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  RelationshipToHoH,
                  GrantType) %>%
    dplyr::filter(
      RelationshipToHoH != 1 &
        EntryDate >= rm_dates$hc$no_more_svcs_on_hh_members &
        (GrantType != "SSVF" | is.na(GrantType))
    ) %>%
    dplyr::semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::mutate(Issue = "Service Transaction on a Non Head of Household",
                  Type = "Warning",
                  Guidance = guidance$services_on_non_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Services on Household Members in SSVF
#' @family Clarity Checks
#' @family DQ: Household Checks
#' @inherit data_quality_tables params return
#' @export

dq_services_on_hh_members_ssvf <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  RelationshipToHoH,
                  GrantType) %>%
    dplyr::filter(RelationshipToHoH != 1 &
                    GrantType == "SSVF") %>%
    dplyr::semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::mutate(Issue = "Service Transaction on a Non Head of Household (SSVF)",
                  Type = "Error",
                  Guidance = guidance$services_on_non_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

}

#' @title Find Referrals on Household Members in SSVF
#' @family Clarity Checks
#' @family DQ: Household Checks
#' @inherit data_quality_tables params return
#' @export

dq_referrals_on_hh_members_ssvf <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

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

#' @title Find Access Points with Entrys/Exits
#' @family Clarity Checks
#' @family DQ: EE Checks
#' @inherit data_quality_tables params return
#' @export

dq_aps_with_ees <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(ProjectType == 14) %>% # not incl Mah CE
    dplyr::mutate(
      Issue = "Access Point with Entry Exits",
      Type = "High Priority",
      Guidance = guidance$aps_with_ees
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}



# SSVF --------------------------------------------------------------------

#' @title Create `ssvf_served_in_date_range` for Clients receiving grants from SSVF
#'
#' @inherit served_in_date_range params return
#' @export

ssvf_served_in_date_range <- function(Enrollment, served_in_date_range, Client, app_env = get_app_env(e = rlang::caller_env())) {
    if (is_app_env(app_env))
      app_env$merge_deps_to_env(missing_fmls())

    Enrollment %>%
      dplyr::select(
        EnrollmentID,
        HouseholdID,
        PersonalID,
        ProjectName,
        ProjectType,
        EntryDate,
        MoveInDateAdjust,
        ExitDate,
        UserCreating,
        RelationshipToHoH,
        PercentAMI,
        LastPermanentStreet,
        LastPermanentCity,
        LastPermanentState,
        LastPermanentZIP,
        AddressDataQuality,
        VAMCStation,
        HPScreeningScore,
        ThresholdScore,
        IraqAfghanistan,
        FemVet
      ) %>%
      dplyr::right_join(
        served_in_date_range %>%
          dplyr::filter(GrantType == "SSVF") %>%
          dplyr::select(PersonalID, EnrollmentID, HouseholdID, ProjectRegion),
        by = c("PersonalID", "EnrollmentID", "HouseholdID")
      ) %>%
      dplyr::left_join(
        Client %>%
          dplyr::select(
            PersonalID,
            VeteranStatus,
            YearEnteredService,
            YearSeparated,
            WorldWarII,
            KoreanWar,
            VietnamWar,
            DesertStorm,
            AfghanistanOEF,
            IraqOIF,
            IraqOND,
            OtherTheater,
            MilitaryBranch,
            DischargeStatus
          ),
        by = "PersonalID"
      )
  }

#' @title Check for missing Year Entered on clients who are Veterans
#'
#' @param ssvf_served_in_date_range See \code{ssvf_served_in_date_range}
#' @inherit data_quality_tables params return
#' @family DQ: EE Checks
#' @family DQ: SSVF Checks
#' @export

dq_veteran_missing_year_entered <- function(ssvf_served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(YearEnteredService) ~ "Missing Year Entered Service",
        YearEnteredService > lubridate::year(lubridate::today()) ~ "Incorrect Year Entered Service"),
      Type = "Error",
      Guidance = guidance$missing_at_entry
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


#' @title Check for missing Year Separated on clients who are Veterans
#'
#' @inherit dq_veteran_missing_year_entered params return
#' @family DQ: SSVF Checks
#' @family DQ: EE Checks
#' @export

dq_veteran_missing_year_separated <- function(ssvf_served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(YearSeparated) ~ "Missing Year Separated",
        YearSeparated > lubridate::year(lubridate::today()) ~ "Incorrect Year Separated"),
      Type = "Error",
      Guidance = guidance$missing_at_entry
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_wars <- ssvf_served_in_date_range %>%
    dplyr::filter(
      VeteranStatus == 1 &
        (
          is.na(WorldWarII) | WorldWarII == 99 |
            is.na(KoreanWar) | KoreanWar == 99 |
            is.na(VietnamWar) | VietnamWar == 99 |
            is.na(DesertStorm) | DesertStorm == 99 |
            is.na(AfghanistanOEF) | AfghanistanOEF == 99 |
            is.na(IraqOIF) | IraqOIF == 99 |
            is.na(IraqOND) | IraqOND == 99 |
            is.na(OtherTheater) |
            OtherTheater == 99
        )
    ) %>%
    dplyr::mutate(Issue = "Missing War(s)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


#' @title Check for missing Branch on clients who are Veterans
#'
#' @inherit dq_veteran_missing_year_entered params return
#' @family DQ: SSVF Checks
#' @family DQ: EE Checks
#' @export

dq_veteran_missing_branch <- function(ssvf_served_in_date_range, guidance, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1 &
                    is.na(MilitaryBranch)) %>%
    dplyr::mutate(Issue = "Missing Military Branch",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Check for missing Discharge Status on clients who are Veterans
#'
#' @inherit dq_veteran_missing_year_entered params return
#' @family DQ: SSVF Checks
#' @family DQ: EE Checks
#' @export

dq_veteran_missing_discharge_status <- function(ssvf_served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1 & is.na(DischargeStatus)) %>%
    dplyr::mutate(Issue = "Missing Discharge Status",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Check for Dont Know/Refused Wars/Branch/Discharge on clients who are Veterans
#'
#' @inherit dq_veteran_missing_year_entered params return
#' @family DQ: SSVF Checks
#' @family DQ: EE Checks
#' @export

dq_dkr_client_veteran_info <- function(ssvf_served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        WorldWarII %in% c(8, 9) |
          KoreanWar %in% c(8, 9) |
          VietnamWar %in% c(8, 9) |
          DesertStorm  %in% c(8, 9) |
          AfghanistanOEF %in% c(8, 9) |
          IraqOIF %in% c(8, 9) |
          IraqOND %in% c(8, 9) |
          OtherTheater  %in% c(8, 9)  ~ "Don't Know/Refused War(s)",
        MilitaryBranch %in% c(8, 9) ~ "Missing Military Branch",
        DischargeStatus %in% c(8, 9) ~ "Missing Discharge Status"
      ),
      Type = "Warning",
      Guidance = guidance$dkr_data
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}


#' @title Check for missing Percent AMI on clients who are Veterans
#'
#' @inherit dq_veteran_missing_year_entered params return
#' @family DQ: SSVF Checks
#' @family DQ: EE Checks
#' @export

dq_ssvf_missing_percent_ami <- function(ssvf_served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())
  ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    is.na(PercentAMI)) %>%
    dplyr::mutate(Issue = "Missing Percent AMI",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  ssvf_missing_vamc <- ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    is.na(VAMCStation)) %>%
    dplyr::mutate(Issue = "Missing VAMC Station Number",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

}

#' @title Check for missing address on clients who are Veterans
#'
#' @inherit dq_veteran_missing_year_entered params return
#' @family DQ: SSVF Checks
#' @family DQ: EE Checks
#' @export

dq_ssvf_missing_address <-
  function(ssvf_served_in_date_range,
           vars,
           guidance,
           app_env = get_app_env(e = rlang::caller_env())) {
    if (is_app_env(app_env))
      app_env$merge_deps_to_env(missing_fmls())

    ssvf_served_in_date_range %>%
      dplyr::filter(RelationshipToHoH == 1 &
                      (
                        is.na(LastPermanentStreet) |
                          is.na(LastPermanentCity) |
                          # is.na(LastPermanentState) | # still not fixed in export
                          is.na(LastPermanentZIP)
                      )) %>%
      dplyr::mutate(
        Issue = "Missing Some or All of Last Permanent Address",
        Type = "Error",
        Guidance = guidance$missing_at_entry
      ) %>%
      dplyr::select(dplyr::all_of(vars$we_want))
  }

# AP No Recent Referrals --------------------------------------------------

dq_aps <- function(Project, Referrals, data = FALSE, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  co_APs <- Project %>%
    dplyr::filter(ProjectType == 14) %>% # not incl Mah CE
    dplyr::select(
      ProjectID,
      OperatingStartDate,
      OperatingEndDate,
      ProjectName,
      HMISParticipatingProject,
      ProjectCounty
    )

  aps_no_referrals <- Referrals %>%
    dplyr::right_join(co_APs, by = c("ReferringProjectID" = "ProjectID")) %>%
    dplyr::filter(is.na(PersonalID)) %>%
    dplyr::select(ReferringProjectID) %>%
    unique()

  aps_with_referrals <- Referrals %>%
    dplyr::right_join(co_APs, by = c("ReferringProjectID" = "ProjectID")) %>%
    dplyr::filter(!is.na(PersonalID)) %>%
    dplyr::select(ReferringProjectID) %>%
    unique()

  if (!data)
    return(aps_no_referrals)

  data_APs <- dplyr::data.frame(
    category = c("No Referrals", "Has Created Referrals"),
    count = c(nrow(aps_no_referrals), nrow(aps_with_referrals)),
    providertype = rep("Access Points"),
    total = rep(c(
      nrow(aps_no_referrals) + nrow(aps_with_referrals)
    )),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(percent = count / total,
                  prettypercent = scales::percent(count / total))
  data_APs

}


#' @title Make a Clarity Profile link using the UniqueID and PersonalID
#' @description If used in a \link[DT]{datatable}, set `escape = FALSE`
#' @param pid \code{(character/data.frame)} Either the `PersonalID` column, or the \code{data.frame} with it.
#' @param uid \code{(character)} The `UniqueID` column, unnecessary to specific if \code{data.frame} supplied to PersonalID
#' @param chr \code{(logical)} Whether to output a character or a `shiny.tag` if `FALSE`. **Default** TRUE
#'
#' @return \code{(character/data.frame/shiny.tag)} If `PersonalID` is a character vector (IE nested in a mutate), and `chr = TRUE` a character vector, if `chr = FALSE` a `shiny.tag`. If `PersonalID` is a `data.frame` a `data.frame` with the `UniqueID` column replaced with the link to the profile and the `UniqueID` as the text
#' @export
#'
#' @examples
#' data.frame(a = letters, b = seq_along(letters)) |> dplyr::rowwise() |>  dplyr::mutate(a = make_profile_link(a, b)) |> DT::datatable(escape = FALSE)

make_profile_link <- function(pid, uid, chr) {
  href <- httr::parse_url(clarity_url)
  href$path <- c("client",pid, "profile")
  out <- htmltools::tags$a(href = httr::build_url(href), uid, target = "_blank")
  if (chr)
    out <- as.character(out)
}



#' @title Make UniqueID into a Clarity client profile link
#' @param x \code{(data.frame)} must have `PersonalID` & `UniqueID` columns.
#'
#' @return \code{(data.frame)} With `UniqueID` as a profile link and PersonalID removed.
#' @export
#'
#' @examples
#' data.frame(a = letters, b = seq_along(letters)) |> make_profile_link() |>
#' DT::datatable(escape = FALSE)
make_profile_link_df <- function(x) {
  x |>
    dplyr::rowwise() |>
    dplyr::mutate(UniqueID = make_profile_link(PersonalID, UniqueID, chr = TRUE)) |>
    dplyr::select( - PersonalID)
}

read_roxygen <- function(file = file.path("R","04_DataQuality_utils.R"), tag = "family") {
  readLines(file) |>
    stringr::str_extract(paste0("(?<=\\#\\'\\s{1,2}\\@", tag, "\\s).*")) |>
    na.omit() |>
    as.character() |>
    unique()
}


# Plot utils ----
# Mon Sep 20 11:16:46 2021

dq_plot_theme_labs <- function(g, x = NULL, y = NULL) {
  .labs <- list()
  if (!missing(x))
    .labs <- list(x = x)
  if (!missing(y))
    .labs <- list(x = y)
  g +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    do.call(ggplot2::labs, .labs) +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)
}
