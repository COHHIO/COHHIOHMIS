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
#

cohorts <- function(
  clarity_api,
  app_env,
  e = rlang::caller_env()
  ) {
  if (missing(clarity_api))
    clarity_api <- get_clarity_api(e = e)
  if (missing(app_env))
    app_env <- get_app_env(e = e)
  vars <- list()
  vars$we_want <- c(
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
  )

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
    dplyr::filter(HMIS::served_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    RelationshipToHoH == 1) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_hohs_served <- co_hohs_served %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(hohs_served = dplyr::n())

  # Leaver HoHs served during the reporting period
  co_hohs_served_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(
      HMIS::exited_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
        RelationshipToHoH == 1
    ) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_hohs_served_leavers <- co_hohs_served_leavers %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(hohs_served_leavers = dplyr::n())

  #	Leavers	who were Served During Reporting Period	Deaths
  co_hohs_served_leavers_died <- Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(
      HMIS::exited_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
        RelationshipToHoH == 1,
      Destination == 24
    ) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_hohs_served_leavers_died  <- co_hohs_served_leavers_died  %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(hohs_served_leavers_died = dplyr::n())

  #	Leavers and Stayers	who were Served During Reporting Period	All
  co_clients_served <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::served_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End)) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_clients_served <- co_clients_served %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(clients_served = dplyr::n())

  #	Leavers and Stayers	who were Served During Reporting Period	Adults
  co_adults_served <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::served_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_adults_served <- co_adults_served %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(adults_served = dplyr::n())

  #	Leavers and Stayers	who	Entered During Reporting Period	Adults

  co_adults_entered <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::entered_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_adults_entered <- co_adults_entered %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(adults_entered = dplyr::n())

  #	Leavers and Stayers	who	Entered During Reporting Period	HoHs
  co_hohs_entered <- Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(
      HMIS::entered_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
        RelationshipToHoH == 1
    ) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_hohs_entered <- co_hohs_entered %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(hohs_entered = dplyr::n())

  #	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	All
  co_clients_moved_in <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(
      HMIS::stayed_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End)
    ) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_clients_moved_in <- co_clients_moved_in %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(clients_moved_in = dplyr::n())

  #	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	Adults
  co_adults_moved_in <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::stayed_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_adults_moved_in <- co_adults_moved_in %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(adults_moved_in = dplyr::n())

  #	Leavers	who were Served During Reporting Period (and Moved In)	All
  co_clients_moved_in_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::exited_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    HMIS::stayed_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End)) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_clients_moved_in_leavers <- co_clients_moved_in_leavers %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(clients_moved_in_leavers = dplyr::n())

  #	Leaver hohs	who were Served (and Moved In) During Reporting Period	HoHs
  co_hohs_moved_in_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::stayed_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    HMIS::exited_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    RelationshipToHoH == 1) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_hohs_moved_in_leavers <- co_hohs_moved_in_leavers %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(hohs_moved_in_leavers = dplyr::n())

  #	Leavers	who were Served During Reporting Period (and Moved In)	Adults
  co_adults_moved_in_leavers <-  Enrollment_extra_Exit_HH_CL_AaE %>%
    dplyr::filter(HMIS::exited_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    HMIS::stayed_between(., lubridate::ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
                    AgeAtEntry > 17) %>%
    dplyr::left_join(Client, by = "PersonalID") %>%
    dplyr::select(tidyselect::all_of(vars$we_want))

  summary_adults_moved_in_leavers <- co_adults_moved_in_leavers %>%
    dplyr::distinct(PersonalID, ProjectName) %>%
    dplyr::group_by(ProjectName) %>%
    dplyr::summarise(adults_moved_in_leavers = dplyr::n())

  summary <- summary_clients_served %>%
    dplyr::full_join(summary_clients_moved_in, by = "ProjectName") %>%
    dplyr::full_join(summary_hohs_moved_in_leavers, by = "ProjectName") %>%
    dplyr::full_join(summary_adults_served, by = "ProjectName") %>%
    dplyr::full_join(summary_adults_moved_in, by = "ProjectName") %>%
    dplyr::full_join(summary_clients_moved_in_leavers, by = "ProjectName") %>%
    dplyr::full_join(summary_adults_moved_in_leavers, by = "ProjectName") %>%
    dplyr::full_join(summary_hohs_served, by = "ProjectName") %>%
    dplyr::full_join(summary_hohs_entered, by = "ProjectName") %>%
    dplyr::full_join(summary_hohs_served_leavers, by= "ProjectName") %>%
    dplyr::full_join(summary_adults_entered, by = "ProjectName") %>%
    dplyr::full_join(summary_hohs_served_leavers_died, by = "ProjectName")

  rm(vars$we_want)

  # APs ---------------------------------------------------------------------

  project_addresses <- ProjectCoC %>%
    dplyr::select(ProjectID, CoCCode, Address1, Address2, City, State, ZIP)

  APs <- Project %>%
    dplyr::inner_join(provider_geo, by = c("ProjectID", "ProjectName")) %>%
    dplyr::filter(ProjectType == 14) %>%
    dplyr::left_join(provider_services, by = "ProjectID") %>%
    dplyr::select(
      ProjectID,
      ProjectAKA,
      OrganizationName,
      ProjectName,
      TargetPop,
      CountiesServed,
      ProjectAreaServed,
      ProjectHours,
      ProjectWebsite,
      ProjectTelNo
    ) %>%
    unique() %>%
    dplyr::mutate(OrgLink = dplyr::if_else(!is.na(ProjectWebsite), paste0(
      "<a href='",
      ProjectWebsite,
      "' target='_blank'>",
      ProjectAKA,
      "</a><small> (#",
      ProjectID,
      ")</small>"
    ), paste0(ProjectAKA,
              "<small> (#",
              ProjectID,
              ")</small>"))) %>%
    dplyr::left_join(project_addresses, by = "ProjectID") %>%
    dplyr::mutate(
      City = paste0(City, ", ", State, " ", ZIP),
      Addresses = dplyr::coalesce(Address1, Address2)
    ) %>%
    dplyr::select(ProjectID, ProjectAKA, OrganizationName, ProjectName, TargetPop,
                  "ProjectCountyServed" = CountiesServed, ProjectAreaServed,
                  ProjectHours, ProjectTelNo, OrgLink, CoCCode, Addresses, City)

  readr::write_csv(APs, "public_data/aps.csv")

  rm(list = ls(pattern = "summary_"))


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



  # Save it out -------------------------------------------------------------
  # WARNING save.image does not save the environment properly, save must be used.
  save(list = ls(), file = "images/cohorts.RData")
}


