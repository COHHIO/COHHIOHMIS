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
