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
  # Project
  app_env <- load_project(ProjectCoC = clarity_api$ProjectCoC(), app_env = app_env)


  # Affiliation -------------------------------------------------------------
  #Unused
  #Affiliation <- clarity_api$Affiliation()


  # Disabilities ------------------------------------------------------------

  Disabilities <- clarity_api$Disabilities()


  # EmploymentEducation -----------------------------------------------------
  #Unused
  #EmploymentEducation <- clarity_api$EmploymentEducation()



  # EnrollmentCoC -----------------------------------------------------------

  # Used in load_enrollment


  # Referrals ---------------------------------------------------------------
  app_env <- load_referrals(Referrals = clarity_api$CE_Referrals_new_extras(col_types = list(ReferringPTC = "c", DeniedByType = "c")))


  # Enrollment --------------------------------------------------------------


  app_env <- load_enrollment(Enrollment = clarity_api$Enrollment(),
                             Enrollment_extras = clarity_api$Enrollment_extras() |>
                               dplyr::mutate_all(as.character),
                             Exit = clarity_api$Exit())

  # Funder ------------------------------------------------------------------

  Funder <-
    clarity_api$Funder()

  # HealthAndDV -------------------------------------------------------------

  HealthAndDV <-
    clarity_api$HealthAndDV()

  # IncomeBenefits ----------------------------------------------------------

  IncomeBenefits <-
    clarity_api$IncomeBenefits() |>
    dplyr::mutate(dplyr::across(c(tidyselect::contains("Amount"), tidyselect::all_of("TotalMonthlyIncome")), as.numeric)) |>
    dplyr::mutate(TotalMonthlyIncome = dplyr::if_else(IncomeFromAnySource == 0 & is.na(TotalMonthlyIncome),
                                                      0, TotalMonthlyIncome))

  # Inventory ---------------------------------------------------------------

  Inventory <- clarity_api$Inventory()


  # Organization ------------------------------------------------------------

  # Only used in PE - load moved there to unburden RAM


  # Contacts ----------------------------------------------------------------
  # only pulling in contacts made between an Entry Date and an Exit Date

  Contacts <- clarity_api$Contact_extras()

  # Scores ------------------------------------------------------------------

  Scores <-  clarity_api$Client_SPDAT_extras() |>
    dplyr::filter(Deleted == "No") |>
    dplyr::mutate(Score = dplyr::if_else(is.na(Score), CustomScore, Score),
                  CustomScore = NULL)

  # Offers -----------------------------------------------------------------
# Only used in vet_active. Moved There

  Doses <- clarity_api$Client_Doses_extras() |>
    dplyr::filter(Deleted == "No")


  # Users ----
  # Thu Sep 23 14:38:19 2021
  Users <- clarity_api$User()
  Users_link <- clarity_api$UserNamesIDs_extras() |>
    dplyr::mutate(UserCreated = as.character(UserCreated))
  Users <- dplyr::left_join(Users, Users_link, by = c(UserID = "UserCreated"))

  # Services ----------------------------------------------------------------
  app_env <- load_services(Services = clarity_api$Services(),
                Services_extras = clarity_api$Services_extras()
                )

  program_lookup <- load_program_lookup(clarity_api$Program_lookup_extras())




  app_env$gather_deps("everything")
}


