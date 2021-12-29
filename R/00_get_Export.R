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

# PLEASE NOTE THIS SCRIPT OVERWRITES THE CLIENT.CSV FILE ON YOUR HARD DRIVE!
# IT REPLACES THE NAMES AND SSNS WITH DATA QUALITY SIGNIFIERS!
# IT CAN BE RUN ON A CLEAN CLIENT.CSV FILE OR ONE THAT'S BEEN OVERWRITTEN.

#' @title Load the HUD Export and join extras.
#'
#' @inheritParams R6Classes
#' @param error \code{(logical)} whether to error or send a message via pushbullet when data checks fail
#'
#' @return `app_env` with data dependencies
#' @export


load_export <- function(
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env()),
  error = FALSE
) {

  force(clarity_api)

  # Public data
  app_env <- load_public()
  # Client
  app_env <- load_client()

  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())

  # ProjectCoC
  ProjectCoC <-
    clarity_api$ProjectCoC()
  # Project
  app_env <- load_project(ProjectCoC = ProjectCoC, app_env = app_env)


  # Affiliation -------------------------------------------------------------

  Affiliation <- clarity_api$Affiliation()



  # Disabilities ------------------------------------------------------------

  Disabilities <- clarity_api$Disabilities()


  # EmploymentEducation -----------------------------------------------------

  EmploymentEducation <- clarity_api$EmploymentEducation()



  # EnrollmentCoC -----------------------------------------------------------

  EnrollmentCoC <- clarity_api$EnrollmentCoC() |>
    EnrollmentCoC_RemoveCoCCodes()



  # Enrollment --------------------------------------------------------------
  # getting EE-related data, joining both to En
  Enrollment_extras <- clarity_api$`HUD Extras`$Enrollment_extras()
  Enrollment <- clarity_api$Enrollment()
  Enrollment_extra_Client_Exit_HH_CL_AaE <- dplyr::left_join(Enrollment, Enrollment_extras, by = UU::common_names(Enrollment, Enrollment_extras)) |>
    # Add Exit
    Enrollment_add_Exit(clarity_api$Exit()) |>
    # Add Households
    Enrollment_add_Household(Project) |>
    # Add Veteran Coordinated Entry
    Enrollment_add_VeteranCE(VeteranCE = VeteranCE) |>
    # Add Client Location from EnrollmentCoC
    Enrollment_add_ClientLocation(EnrollmentCoC) |>
    # Add Client AgeAtEntry
    Enrollment_add_AgeAtEntry_UniqueID(Client) |>
    dplyr::left_join(dplyr::select(Client,-dplyr::all_of(
      c(
        "DateCreated",
        "DateUpdated",
        "UserID",
        "DateDeleted",
        "ExportID"
      )
    )),
    by = c("PersonalID", "UniqueID"))

  UU::join_check(Enrollment, Enrollment_extra_Client_Exit_HH_CL_AaE)


  # Funder ------------------------------------------------------------------

  Funder <-
    clarity_api$Funder()

  # HealthAndDV -------------------------------------------------------------

  HealthAndDV <-
    clarity_api$HealthAndDV()

  # IncomeBenefits ----------------------------------------------------------

  IncomeBenefits <-
    clarity_api$IncomeBenefits() |>
    dplyr::mutate(dplyr::across(c(tidyselect::contains("Amount"), tidyselect::all_of("TotalMonthlyIncome")), as.numeric))

  # Inventory ---------------------------------------------------------------

  Inventory <-
    clarity_api$Inventory()

  # Organization ------------------------------------------------------------

  Organization <-
    clarity_api$Organization()


  # Contacts ----------------------------------------------------------------
  # only pulling in contacts made between an Entry Date and an Exit Date

  Contacts <- clarity_api$`HUD Extras`$Contact_extras()

  # Scores ------------------------------------------------------------------

  Scores <-  clarity_api$`HUD Extras`$Client_SPDAT_extras()

  # Offers -----------------------------------------------------------------

  Offers <- clarity_api$`HUD Extras`$Client_Offer_extras()




  Doses <- clarity_api$`HUD Extras`$Client_Doses_extras()


  # Users ----
  # Thu Sep 23 14:38:19 2021
  Users <- clarity_api$User()

  # Services ----------------------------------------------------------------

  Services <- clarity_api$Services()
  Services_extras <- clarity_api$`HUD Extras`$Services_extras()
  Services_enroll_extras  <- dplyr::left_join(Services,
                     Services_extras,
                     by = UU::common_names(Services, Services_extras)) |>
    dplyr::left_join(dplyr::select(Enrollment_extra_Client_Exit_HH_CL_AaE, dplyr::all_of(
      c(
        "EnrollmentID",
        "PersonalID",
        "ProjectName",
        "EntryDate",
        "ExitAdjust",
        "ProjectID",
        "ProjectName"
      )
    )),
                     by = c("PersonalID", "EnrollmentID")) |>
    unique() |>
    dplyr::mutate(
      ServiceEndAdjust = dplyr::if_else(
        is.na(ServiceEndDate) |
          ServiceEndDate > Sys.Date(),
        Sys.Date(),
        ServiceEndDate
      ),
      service_interval = lubridate::interval(start = ServiceStartDate, end = ServiceEndAdjust),
      ee_interval = lubridate::interval(start = EntryDate, end = ExitAdjust),
      intersect_tf = lubridate::int_overlaps(service_interval, ee_interval),
      stray_service = is.na(intersect_tf) |
        intersect_tf == FALSE
    ) |>
    dplyr::select(
      UniqueID,
      PersonalID,
      ServiceID,
      EnrollmentID,
      ProjectName,
      HouseholdID,
      ServiceStartDate,
      ServiceEndDate,
      RecordType,
      ServiceItemName,
      FundName,
      FundingSourceID,
      ServiceAmount,
      stray_service
    )

  stray_services <- Services_enroll_extras |>
    dplyr::filter(stray_service) |>
    dplyr::select(-stray_service)

  Services_enroll_extras <- Services_enroll_extras |>
    dplyr::filter(!stray_service) |>
    dplyr::select(-stray_service)



  # Referrals ---------------------------------------------------------------


  Referrals <- clarity_api$`HUD Extras`$CE_Referrals_extras(col_types = list(ReferralConnectedPTC = "c", DeniedByType = "c")) |>
    dplyr::rename_with(.cols = - dplyr::matches("(?:^PersonalID)|^(?:^UniqueID)"), rlang::as_function(~paste0("R_",.x))) |>
    dplyr::mutate(R_ReferralConnectedPTC = stringr::str_remove(R_ReferralConnectedPTC, "\\s\\(disability required\\)$"),
                  R_ReferralConnectedPTC = dplyr::if_else(R_ReferralConnectedPTC == "Homeless Prevention", "Homelessness Prevention", R_ReferralConnectedPTC),
                  R_ReferralConnectedPTC = HMIS::hud_translations$`2.02.6 ProjectType`(R_ReferralConnectedPTC))

  # Full needed for dqu_aps
  Referrals_full <- Referrals

  referrals_expr <- rlang::exprs(
    housed1 = R_RemovedFromQueueSubreason %in% c(
      "Housed with Community Inventory",
      "Housed with Community Inventory - Not with CE",
      "Permanently Living with Family/Friends",
      "Return To Prior Residence",
      "Rental By Client"
    ),
    housed2 = !is.na(R_ReferralConnectedMoveInDate),
    housed3 = R_ExitHoused == "Housed",
    is_last = R_IsLastReferral == "Yes",
    is_last_enroll = R_IsLastEnrollment == "Yes",
    is_active = R_ActiveInProject == "Yes",
    accepted = stringr::str_detect(R_ReferralResult, "accepted$"),
    coq = R_ReferralCurrentlyOnQueue == "Yes"
  )
  referral_result_summarize <- purrr::map(referrals_expr, ~rlang::expr(isTRUE(any(!!.x, na.rm = TRUE))))


  Referrals <- Referrals |>
    filter_dupe_soft(!!referrals_expr$is_last_enroll,
                     !!referrals_expr$is_last,
                     !!referrals_expr$is_active,
                     !is.na(R_ReferralResult),
                     !!referrals_expr$housed3 & !!referrals_expr$accepted,
                     key = PersonalID) |>
    filter_dupe_last_EnrollmentID(key = PersonalID, R_ReferredEnrollmentID) |>
    dplyr::arrange(dplyr::desc(R_ReferredEnrollmentID)) |>
    dplyr::distinct(dplyr::across(-R_ReferredEnrollmentID), .keep_all = TRUE)



  app_env$gather_deps("everything")

}



#' @title Load public data pre-requisites for `load_export`
#'
#' @inheritParams data_quality_tables
#' @inherit load_export return
#' @export

load_public <- function(app_env = get_app_env(e = rlang::caller_env())) {
  # Public -----------------------------------------------------------
  ServiceAreas <- clarity.looker::hud_load("ServiceAreas.feather", dirs$public)
  Regions <- clarity.looker::hud_load("Regions", dirs$public)
  app_env$gather_deps("everything")
}

#' @title Load Client data & extras pre-requisites for `load_export`
#'
#' @inheritParams data_quality_tables
#' @inherit load_export return
#' @export

load_client <- function(clarity_api = get_clarity_api(e = rlang::caller_env()),
                 app_env = get_app_env(e = rlang::caller_env())) {
  Client <- clarity_api$Client()
  # this saves Client as a feather file with redacted PII as a security measure.
  if (!all(Client$SSN %in% c("ok" ,"Invalid", "DKR", "Missing"))) {
    Client <- Client_redact(Client)
    clarity.looker::hud_feather(Client, dirs$export)
  }
  # Veteran Client_extras ----
  VeteranCE <- clarity_api$`HUD Extras`$Client_extras()

  Client <- Client_add_UniqueID(Client, clarity_api$`HUD Extras`$Client_UniqueID_extras())
  app_env$gather_deps("everything")
}

#' @title Load Project & extras pre-requisites for `load_export`
#'
#' @inheritParams data_quality_tables
#' @param Regions From public data. See `load_public`
#' @inherit load_export return
#' @export

load_project <- function(Regions, ProjectCoC, clarity_api = get_clarity_api(e = rlang::caller_env()),
                         app_env = get_app_env(e = rlang::caller_env())) {

  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  # Project_extras -----------------------------------------------------------------
  # provider_extras
  # Thu Aug 12 14:23:50 2021


  provider_extras <- clarity_api$`HUD Extras`$Project_extras()
  provider_extras <- pe_add_ProjectType(provider_extras) |>
    pe_add_regions(Regions, dirs = dirs) |>
    pe_add_GrantType()

  # Rminor: Coordinated Entry Access Points [CEAP]
  APs <- pe_create_APs(provider_extras, ProjectCoC, dirs = dirs)



  Project <- clarity_api$Project()
  Project <- Project |>
    dplyr::select(-ProjectCommonName) |>
    {\(x) {dplyr::left_join(x, provider_extras |> dplyr::select(- dplyr::matches("FundingSourceID")) |> dplyr::distinct(ProjectID, .keep_all = TRUE), by = UU::common_names(x, provider_extras))}}()
  UU::join_check(clarity_api$Project(), Project)

  mahoning_projects <- dplyr::filter(ProjectCoC, CoCCode %in% "OH-504") |>
    dplyr::select(ProjectID) |>
    {\(x) {
      dplyr::left_join(x, dplyr::select(Project, ProjectID, ProjectTypeCode, ProjectName), by = "ProjectID") |>
        Project_rm_zz() |>
        dplyr::distinct(ProjectID, .keep_all = TRUE) |>
        {\(y) {rlang::set_names(y$ProjectID, dplyr::pull(y, ProjectTypeCode))}}()
    }}()
  app_env$gather_deps("everything")
}

