# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
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

# Some definitions:
# PH = PSH + RRH
# household = one or more people who present for housing/homeless services
# served = the Entry to Exit Date range crosses the Report Date range
# entered = the Entry Date is inside the Report Date range
# served_leaver = (regardless of Move-In) the Exit Date is inside the Report
#     Date range
# moved_in_leaver = a subset of served_leaver, these stays include a Move-In Date
#     where that's relevant (PH projects)
# moved_in = any stay in a non-PH project where the Entry to Exit Date range
#     crosses the Report Date range PLUS any stay in a PH project where the
#     Move In Date to the Exit Date crosses the Report Date range
# hohs = heads of household
# adults = all adults in a household
# clients = all members of the household
#' @include app_dependencies.R init_R6.R

cohorts <- function(
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
) {

  if (is_app_env(app_env))
    app_env$set_parent()
  vars <- list(we_want = c(
    "PersonalID",
    "EnrollmentID",
    "CountyServed",
    "ProjectName",
    "ProjectID",
    "ProjectType",
    "HouseholdID",
    "AgeAtEntry",
    "RelationshipToHoH",
    "VeteranStatus",
    "EntryDate",
    "EntryAdjust",
    "MoveInDate",
    "MoveInDateAdjust",
    "ExitDate",
    "ExitAdjust",
    "Destination"
  ))


  # Transition Aged Youth

  tay <- Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want)) %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::mutate(
      TAY = dplyr::if_else(max(AgeAtEntry) < 25 & max(AgeAtEntry) >= 16, 1, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(TAY == 1 & !is.na(ProjectName))

  # Leaver and Stayer HoHs who were served during the reporting period
  co_hohs_served <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::served_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(RelationshipToHoH == 1) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))



  # Leaver HoHs served during the reporting period
  co_hohs_served_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::exited_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(RelationshipToHoH == 1) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))


  #	Leavers	who were Served During Reporting Period	Deaths
  co_hohs_served_leavers_died <- Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::exited_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(
      RelationshipToHoH == 1,
      Destination == 24
    ) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers and Stayers	who were Served During Reporting Period	All
  co_clients_served <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::served_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers and Stayers	who were Served During Reporting Period	Adults
  co_adults_served <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::served_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers and Stayers	who	Entered During Reporting Period	Adults

  co_adults_entered <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::entered_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers and Stayers	who	Entered During Reporting Period	HoHs
  co_hohs_entered <- Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::entered_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(RelationshipToHoH == 1) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	All
  co_clients_moved_in <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::stayed_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	Adults
  co_adults_moved_in <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    HMIS::stayed_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers	who were Served During Reporting Period (and Moved In)	All
  co_clients_moved_in_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::exited_between(., rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) &
                    HMIS::stayed_between(., rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End)) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leaver hohs	who were Served (and Moved In) During Reporting Period	HoHs
  co_hohs_moved_in_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::stayed_between(., rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) &
                    HMIS::exited_between(., rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) &
                    RelationshipToHoH == 1) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  #	Leavers	who were Served During Reporting Period (and Moved In)	Adults
  co_adults_moved_in_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::exited_between(., rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) &
                    HMIS::stayed_between(., rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) &
                    AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary <- hoh_count(co_clients_served) %>%
    dplyr::full_join(hoh_count(co_clients_moved_in), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_hohs_moved_in_leavers), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_adults_served), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_adults_moved_in), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_clients_moved_in_leavers), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_adults_moved_in_leavers), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_hohs_served), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_hohs_entered), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_hohs_served_leavers), by= "ProjectName") %>%
    dplyr::full_join(hoh_count(co_adults_entered), by = "ProjectName") %>%
    dplyr::full_join(hoh_count(co_hohs_served_leavers_died), by = "ProjectName")





  # PIT Counts --------------------------------------------------------------

  BoS_PIT <- dplyr::tribble(
    ~Population, ~January2020Count, ~January2021Count,
    "Total", 3577, 2334,
    "Sheltered", 2591, 2334,
    "Veterans", 162, 87,
    "Chronic", 192, 196
  )

  Mah_PIT <- dplyr::tribble(
    ~Population, ~January2020Count, ~January2021Count,
    "Total", 100, 62,
    "Sheltered", 78, 62,
    "Veterans", 2, 6,
    "Chronic", 6, 3
  )


  # Counties ----------------------------------------------------------------

  bos_counties <- ServiceAreas %>%
    dplyr::filter(CoC == "OH-507 Balance of State") %>%
    dplyr::pull(County)

  # Destinations Groups (FY2020) --------------------------------------------

  perm_destinations <- c(3, 10, 11, 19:23, 26, 28, 31, 33, 34, 36)

  temp_destinations <-  c(1, 2, 12, 13, 14, 16, 18, 27, 32, 35)

  institutional_destinations <- c(4:7, 15, 25, 27, 29)

  other_destinations <- c(8, 9, 17, 24, 30, 37, 99)

  # Project Groupings -------------------------------------------------------
  #TODO
  # GPD_project_ids <- c(751, 776, 749, 1229, 127, 550)
  #
  # fake_projects <- c(1027, 1849, 1028, 1033, 1032, 1029, 1931, 1030, 1031, 1317)



  # Project Type Groupings --------------------------------------------------

  lh_project_types <- c(1, 2, 8)

  ph_project_types <- c(3, 9, 13)

  lh_at_entry_project_types <- c(1, 2, 3, 4, 8, 9, 13)

  lh_ph_hp_project_types <- c(1, 2, 3, 4, 8, 9, 12, 13)

  coc_funded_project_types <- c(2, 3, 13)

  project_types_w_beds <- c(1, 2, 3, 8, 9)



  app_env$gather_deps("everything")

}


