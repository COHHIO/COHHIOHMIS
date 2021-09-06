is_clarity <- function() {
  getOption("HMIS")$Clarity
}

is_sp <- function() {
  getOption("HMIS")$ServicePoint
}

projects_current_hmis <- function (Project, Inventory) {

  Project %>%
    dplyr::left_join(Inventory, by = "ProjectID") %>%
    dplyr::filter(HMISParticipatingProject == 1 &
        HMIS::operating_between(., calc$data_goes_back_to, meta_HUDCSV$Export_End) &
        (GrantType != "HOPWA" | is.na(GrantType))
    ) %>%
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
    ) %>% unique()
}

#' Create the data.frame of Clients to Check `served_in_date_range`
#'
#' @param projects_current_hmis \code{(data.frame)} of Providers to check. See `projects_current_hmis`
#' @param Enrollment_extra_Exit_HH_CL_AaE \code{(data.frame)} Enrollment with all additions from `load_export`
#' @param Client \code{(data.frame)} Client with all additions from `load_export`
#' @param Project \code{(data.frame)} Project with extras including Regions and GrantType see `Pe_add_regions` & `Pe_add_GrantType`.
#' @param Inventory \code{(data.frame)} Inventory
#' @param calc \code{(list)} of dates from `dates`
#' @param meta_HUDCSV \code{(list)} of dates from `dates`
#' @param app_env \code{(data.frame)} Instead of providing all `Enrollment`, `Client`, `Project`, `Inventory`, `calc` & `meta_HUDCSV`, `app_env` with all inputs saved internally can be provided.
#'
#' @return \code{data.frame}
#' @export

served_in_date_range <- function(projects_current_hmis, Enrollment_extra_Exit_HH_CL_AaE, Client, Project, Inventory, calc, meta_HUDCSV, app_env) {
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
      EEType,
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
    )  |>
    dplyr::inner_join(projects_current_hmis, by = "ProjectID")

  DV <- HealthAndDV  |>
    dplyr::filter(DataCollectionStage == 1)  |>
    dplyr::select(EnrollmentID, DomesticViolenceVictim, WhenOccurred, CurrentlyFleeing)

  served_in_date_range |>
    dplyr::left_join(DV, by = "EnrollmentID")

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

dq_stray_services <- function(stray_services) {
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
