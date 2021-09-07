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



#' @title Filter for Current HMIS participating projects
#'
#' @param Project
#' @param Inventory
#'
#' @return \code{(data.frame)}

projects_current_hmis <- function (Project, Inventory) {
  Project %>%
    dplyr::left_join(Inventory, by = "ProjectID") |>
    HMIS::operating_between(calc$data_goes_back_to, meta_HUDCSV$Export_End) |>
    dplyr::filter(HMISParticipatingProject == 1 &
        (GrantType != "HOPWA" | is.na(GrantType))
    ) |>
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


served_in_date_range <- function(projects_current_hmis, Enrollment_extra_Exit_HH_CL_AaE = NULL, Client = NULL, Project = NULL, Inventory = NULL, calc = NULL, meta_HUDCSV = NULL, app_env) {
  if (!missing(app_env))
    app_env$merge_deps_to_env("Enrollment_extra_Exit_HH_CL_AaE", "Client", "Project", "Inventory", "calc", "meta_HUDCSV")
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

#' @title Data Quality report on Missing First Names
#'
#' @param served_in_date_range \code{(data.frame)} See `served_in_date_range`
#' @param guidance \code{(list)}
#' @param vars \code{(list)}
#'
#' @return \code{(data.frame)} with `Issue` (Issue Name), `Type` (Error or Warning), and `Guidance` (How to correct the issue)

dq_name <- function(served_in_date_range, guidance = NULL, vars = NULL) {
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


dq_dob <- function(served_in_date_range, guidance = NULL, vars = NULL) {
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


dq_ssn <- function(served_in_date_range, guidance = NULL, vars = NULL) {
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

dq_race <- function(served_in_date_range, guidance = NULL, vars = NULL) {
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

dq_ethnicity <- function(served_in_date_range, guidance = NULL, vars = NULL) {
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
dq_gender <- function(served_in_date_range, guidance = NULL, vars = NULL) {
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

dq_veteran <- function(served_in_date_range, guidance = NULL, vars = NULL) {
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
#' @inherit dq_name return
#' @inheritParams dq_name
#' @inheritParams served_in_date_range

dq_missing_vaccine_exited <- function(served_in_date_range, dose_counts, vars, mahoning_projects = NULL, doses = NULL, hc = NULL, app_env) {
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

dq_missing_vaccine_current <- function(served_in_date_range,  dose_counts, vars, doses = NULL, mahoning_projects = NULL, hc = NULL, app_env) {
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

dq_incorrect_ee_type <- function(server_in_date_range) {
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

dq_stray_services <- function(stray_services) {
  if (!is_sp())
    rlang::abort(match.call()[[1]], " is a ServicePoint specific data quality check.")
    stray_services %>%
    dplyr::mutate(Issue = "Service Not Attached to an Entry Exit",
                  Type = "Warning",
                  Guidance = "This Service does not fall between any project stay,
             so it will not show in any reporting.") %>%
    dplyr::select(PersonalID, ServiceProvider, ServiceStartDate, Issue, Type)
}

dq_referrals_on_hh_members <- function(served_in_date_range, vars) {
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

dq_referrals_on_hh_members_ssvf <- function(served_in_date_range, vars) {
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
  served_in_date_range%>%
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
