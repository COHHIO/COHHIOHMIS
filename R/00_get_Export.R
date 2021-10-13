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

load_export <- function(
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env()),
  error = FALSE
) {

  force(clarity_api)
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())

  # Service Areas -----------------------------------------------------------
  ServiceAreas <- clarity.looker::hud_load("ServiceAreas.feather", dirs$public)

  # Affiliation -------------------------------------------------------------

  Affiliation <- cl_api$Affiliation()

  # Client ------------------------------------------------------------------

  Client <- cl_api$Client()
  # this saves Client as a feather file with redacted PII as a security measure.
  if(ncol(Client) == 36) {
    Client <- Client_redact(Client)
    clarity.looker::hud_feather(Client, dirs$export)
  }
  # Veteran Client_extras ----
  VeteranCE <- cl_api$`HUD Extras`$Client_extras()

  Client <- Client_add_UniqueID(Client, VeteranCE)

  # Disabilities ------------------------------------------------------------

  Disabilities <- cl_api$Disabilities()


  # EmploymentEducation -----------------------------------------------------

  EmploymentEducation <- cl_api$EmploymentEducation()

  # ProjectCoC --------------------------------------------------------------

  ProjectCoC <-
    cl_api$ProjectCoC()


  app_env$merge_deps_to_env("dirs")
  force(dirs)

  # Project_extras -----------------------------------------------------------------
  # provider_extras
  # Thu Aug 12 14:23:50 2021
  provider_extras <- cl_api$`HUD Extras`$Project_extras()
  provider_extras <- pe_add_ProjectType(provider_extras)
  provider_extras <- pe_add_regions(provider_extras, dirs = dirs)
  provider_extras <- pe_add_GrantType(provider_extras)

  # Rminor: Coordinated Entry Access Points [CEAP]
  APs <- pe_create_APs(provider_extras, ProjectCoC, dirs = dirs)

Project <- cl_api$Project() |>
  dplyr::select(-ProjectCommonName) |>
  {\(x) {dplyr::left_join(x, provider_extras, by = UU::common_names(x, provider_extras))}}()

mahoning_projects <- dplyr::filter(ProjectCoC, CoCCode %in% "OH-504") |>
  dplyr::select(ProjectID) |>
  {\(x) {
    dplyr::left_join(x, dplyr::select(Project, ProjectID, ProjectTypeCode, ProjectName), by = "ProjectID") |>
      dplyr::filter(stringr::str_detect(ProjectName, "^zz", negate = TRUE)) |>
      dplyr::distinct(ProjectID, .keep_all = TRUE) |>
      {\(y) {rlang::set_names(y$ProjectID, dplyr::pull(y, ProjectTypeCode))}}()
  }}()


  # EnrollmentCoC -----------------------------------------------------------

  EnrollmentCoC <-
    cl_api$EnrollmentCoC()




  # Enrollment --------------------------------------------------------------

  # from sheets 1 and 2, getting EE-related data, joining both to En
  Enrollment_extras <- cl_api$`HUD Extras`$Enrollment_extras()
  Enrollment <- cl_api$Enrollment()
  Enrollment_extra_Exit_HH_CL_AaE <- dplyr::inner_join(Enrollment, Enrollment_extras, by = UU::common_names(Enrollment, Enrollment_extras)) |>
    # Add Exit
    Enrollment_add_Exit(cl_api$Exit()) |>
    # Add Households
    Enrollment_add_Household(Project, app_env$.__enclos_env__$rm_dates) |>
    # Add Veteran Coordinated Entry
    Enrollment_add_VeteranCE(VeteranCE) |>
    # Add Client Location from EnrollmentCoC
    Enrollment_add_ClientLocation(EnrollmentCoC) |>
    # Add Client AgeAtEntry
    Enrollment_add_AgeAtEntry_UniqueID(Client)


  # Funder ------------------------------------------------------------------

  Funder <-
    cl_api$Funder()

  # HealthAndDV -------------------------------------------------------------

  HealthAndDV <-
    cl_api$HealthAndDV()

  # IncomeBenefits ----------------------------------------------------------

  IncomeBenefits <-
    cl_api$IncomeBenefits()

  # Inventory ---------------------------------------------------------------

  Inventory <-
    cl_api$Inventory()

  # Organization ------------------------------------------------------------

  Organization <-
    cl_api$Organization()


  # Contacts ----------------------------------------------------------------
  # only pulling in contacts made between an Entry Date and an Exit Date

  Contacts <- cl_api$`HUD Extras`$Contact_extras()

  # Scores ------------------------------------------------------------------

  Scores <-  cl_api$`HUD Extras`$Client_SPDAT_extras()

  # Offers -----------------------------------------------------------------

  # TODO Used in Veterans Active List
  # Offers <- cl_api$`HUD Extras`$Client_Offer_extras()
  # Offers <-
  #   readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 7) %>%
  #   dplyr::mutate(AcceptDeclineDate = lubridate::ymd(as.Date(AcceptDeclineDate, origin = "1899-12-30")),
  #                 OfferDate = lubridate::ymd(as.Date(OfferDate, origin = "1899-12-30")))




  doses <- cl_api$`HUD Extras`$Client_Doses_extras()


  # Users ----
  # Thu Sep 23 14:38:19 2021
  Users <- cl_api$User()

  # Services ----------------------------------------------------------------

  # services_funds <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 9)
  # TODO To get the Total RRH (Which should be 75% of all ESG funding spent on Services)
  # Rme - QPR - RRH Spending
  # Rm - QPR - RRH vs HP
  # Services_extras$ServiceAmount[Services_extras$FundName |>
  #                              stringr::str_detect("RRH") |>
  #                              which()]
  # Services <- cl_api$Services()
  # raw_services <- cl_api$`HUD Extras`$Services_extras() |>
  #   dplyr::left_join(Enrollment_extra_Exit_HH_CL_AaE[c("EnrollmentID",
  #                                 "PersonalID",
  #                                 "ProjectName",
  #                                 "EntryDate",
  #                                 "ExitAdjust")],
  #                    by = c("PersonalID", "EnrollmentID")) %>%
  #   unique() %>%
  #   dplyr::left_join(services_funds, by = "ServiceID") %>%
  #   dplyr::mutate(
  #     ServiceEndAdjust = dplyr::if_else(is.na(ServiceEndDate) | ServiceEndDate > Sys.Date(), Sys.Date(), ServiceEndDate),
  #     service_interval = lubridate::interval(start = ServiceStartDate, end = ServiceEndAdjust),
  #     ee_interval = lubridate::interval(start = EntryDate, end = ExitAdjust),
  #     intersect_tf = lubridate::int_overlaps(service_interval, ee_interval),
  #     stray_service = is.na(intersect_tf) | intersect_tf == FALSE | ServiceProvider != ProjectName
  #   ) %>%
  #   dplyr::select(PersonalID, ServiceID, EnrollmentID, ServiceProvider, ServiceHHID,
  #                 ServiceStartDate, ServiceEndDate, Code, Description, ProviderCreating,
  #                 Fund, Amount, stray_service)
  #
  # stray_services <- Services %>%
  #   dplyr::filter(stray_service) %>%
  #   dplyr::select(-stray_service)
  #
  # Services <- Services %>%
  #   dplyr::filter(!stray_service) %>%
  #   dplyr::select(-stray_service)
  #
  # rm(raw_services, services_funds)

  # Referrals ---------------------------------------------------------------


  Referrals <- cl_api$`HUD Extras`$CE_Referrals_extras()
  # TODO ReferralOutcome must be replaced by a Clarity element (or derived from multiple) for dq_internal_old_outstanding_referrals


  # HUD CSV Specs -----------------------------------------------------------
  #TODO hud.extract Data element coercion functions
  HUD_specs <- clarity.looker::hud_load("HUD_specs", dirs$public)


  app_env$gather_deps("everything")

}




