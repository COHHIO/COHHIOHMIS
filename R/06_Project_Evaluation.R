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


# loading old data to freeze data as of the deadline
Project_Evaluation <- function(
             clarity_api,
             app_env,
             e = rlang::caller_env()
            ) {
if (missing(clarity_api))
  clarity_api <- UU::find_by_class("clarity_api", e)
if (missing(app_env))
  app_env <- UU::find_by_class("app_env", e)

  # TODO update file names when PE Freeze happens
  load("pe_dataset_final/images/COHHIOHMIS.RData")
  load("pe_dataset_final/images/Data_Quality.RData")
  load("pe_dataset_final/images/cohorts.RData")
  # hc_project_eval_start <- mdy("01012019") # for comparison purposes
  # hc_project_eval_end <- mdy("12312019")
  # rlang::env_binding_lock(environment(), ls())

# loading in scoring rubric

scoring_rubric <- readxl::read_excel(file.path(dirs$public, "scoring_rubric.xlsx")) %>%
  dplyr::mutate(maximum = as.double(maximum),
         minimum = as.double(minimum))

# Staging -----------------------------------------------------------------

keepers <- c(15, 1353, 1566, 2068)
retired <- c(1774, 390, 1579, 2069)

coc_funded <- Funder %>%
  dplyr::filter(Funder %in% c(1:7) &
           ProjectID != 2408 & # project too new
           (ProjectID %in% c(keepers, retired) |
              (
                lubridate::ymd(StartDate) <= lubridate::ymd(hc_project_eval_end) &
                  (is.na(EndDate) |
                     lubridate::ymd(EndDate) >= lubridate::ymd(hc_project_eval_end))
              ))) %>%
  dplyr::select(ProjectID, Funder, StartDate, EndDate) %>%
  dplyr::left_join(Project[c("ProjectID",
                      "ProjectName",
                      "ProjectType",
                      "HMISParticipatingProject",
                      "ProjectRegion")], by = "ProjectID") %>%
  dplyr::filter(HMISParticipatingProject == 1 &
           ProjectRegion != "Mahoning County CoC") %>%
  dplyr::select(ProjectType,
         ProjectName,
         ProjectID)

# consolidated projects

consolidations <- coc_funded %>%
  dplyr::filter(ProjectID %in% c(keepers, retired)) %>%
  dplyr::mutate(
    AltProjectID = dplyr::case_when(
      ProjectID %in% c(1353, 390) ~ 3000,
      ProjectID %in% c(1774, 15) ~ 3001,
      ProjectID %in% c(1566, 1579) ~ 3002,
      ProjectID %in% c(2068, 2069) ~ 3003
    ),
    AltProjectName = dplyr::case_when(
      ProjectID %in% c(1353, 390) ~ "Springfield SPC 1 Combined (1353, 390)",
      ProjectID %in% c(1774, 15) ~ "GLCAP PSH Combined (1774, 15)",
      ProjectID %in% c(1566, 1579) ~ "One Eighty PSH Plus Care Combined (1566, 1579)",
      ProjectID %in% c(2068, 2069) ~ "Licking - Region 9 RRH"
    )
  ) %>%
  dplyr::select(ProjectID, ProjectName, AltProjectID, AltProjectName)

# filter to only CoC-funded projects (leaving out the SSO)

pe_coc_funded <- coc_funded %>%
  dplyr::left_join(consolidations, by = c("ProjectID", "ProjectName")) %>%
  dplyr::mutate(
    AltProjectID = dplyr::if_else(is.na(AltProjectID), ProjectID, AltProjectID),
    AltProjectName = dplyr::if_else(is.na(AltProjectName), ProjectName, AltProjectName)
  )

vars_we_want <- c(
  "PersonalID",
  "ProjectType",
  "AltProjectID",
  "VeteranStatus",
  "EnrollmentID",
  "AltProjectName",
  "EntryDate",
  "HouseholdID",
  "RelationshipToHoH",
  "LivingSituation",
  "LengthOfStay",
  "LOSUnderThreshold",
  "PreviousStreetESSH",
  "DateToStreetESSH",
  "TimesHomelessPastThreeYears",
  "AgeAtEntry",
  "MonthsHomelessPastThreeYears",
  "DisablingCondition",
  "MoveInDate",
  "MoveInDateAdjust",
  "ExitDate",
  "Destination",
  "EntryAdjust",
  "ExitAdjust"
)

vars_to_the_apps <- c(
  "ProjectType",
  "AltProjectName",
  "PersonalID",
  "EnrollmentID",
  "HouseholdID",
  "EntryDate",
  "MoveInDateAdjust",
  "ExitDate",
  "MeetsObjective"
)


# Project Evaluation cohorts ----------------------------------------------

# pe_[cohort]: uses cohort objects to narrow down data to coc-funded projects'
# data to the 'vars_we_want', then dedupes in case there are multiple stays in
# that project during the date range.

# clients served during date range

pe_clients_served <-  co_clients_served %>%
  dplyr::filter(HMIS::served_between(., hc_project_eval_start, hc_project_eval_end)) %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate)) %>%
  dplyr::distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project
# several measures will use this

# Checking for deceased hohs for points adjustments

hoh_exits_to_deceased <- pe_clients_served %>%
  dplyr::filter(Destination == 24 &
           RelationshipToHoH == 1 &
           HMIS::exited_between(., hc_project_eval_start, hc_project_eval_end)) %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(HoHDeaths = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID")

hoh_exits_to_deceased[is.na(hoh_exits_to_deceased)] <- 0

# Adults who entered during date range

pe_adults_entered <-  co_adults_served %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID, -DateCreated, -DateUpdated, -DateDeleted, -ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::mutate(HHEntryDate = min(EntryDate)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(HMIS::entered_between(., hc_project_eval_start, hc_project_eval_end) &
           EntryDate == HHEntryDate) %>%
  # group_by(PersonalID, AltProjectID) %>%
  # arrange(desc(ymd(EntryDate))) %>%
  # slice_head() %>%
  # ungroup() %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate))

# counts each client's entry

## for vispdat measure

pe_hohs_entered <-  co_hohs_entered %>%
  dplyr::filter(HMIS::entered_between(., hc_project_eval_start, hc_project_eval_end)) %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID, -DateCreated, -DateUpdated, -DateDeleted, -ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  # group_by(PersonalID, AltProjectID) %>%
  # slice_min(order_by = ymd(EntryDate), with_ties = FALSE) %>%
  # ungroup() %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate))

# for ncb logic
# Adults who moved in and exited during date range

pe_adults_moved_in_leavers <-  co_adults_moved_in_leavers %>%
  dplyr::filter(
    HMIS::stayed_between(., hc_project_eval_start, hc_project_eval_end) &
      HMIS::exited_between(., hc_project_eval_start, hc_project_eval_end)
  ) %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate)) %>%
  dplyr::distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# increase income
#Adults who moved in and were served during date range

# pe_adults_moved_in <-  co_adults_moved_in %>%
#   filter(stayed_between(., hc_project_eval_start, hc_project_eval_end)) %>%
#   select("PersonalID", "ProjectID", "EnrollmentID") %>%
#   inner_join(pe_coc_funded, by = "ProjectID") %>%
#   left_join(Client, by = "PersonalID") %>%
#   left_join(
#     Enrollment %>%
#       select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
#     by = c(
#       "PersonalID",
#       "EnrollmentID",
#       "ProjectID",
#       "ProjectType",
#       "ProjectName"
#     )
#   ) %>%
#   select(all_of(vars_we_want)) %>%
#   arrange(PersonalID, AltProjectID, desc(EntryDate)) %>%
#   distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# health insurance
# Clients who moved in and exited during date range

pe_clients_moved_in_leavers <-  co_clients_moved_in_leavers %>%
  dplyr::filter(HMIS::stayed_between(., hc_project_eval_start, hc_project_eval_end) &
           HMIS::exited_between(., hc_project_eval_start, hc_project_eval_end)) %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate)) %>%
  dplyr::distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# exits to PH, but needs an added filter of only mover-inners
# Heads of Household who were served during date range

pe_hohs_served <- co_hohs_served %>%
  dplyr::filter(HMIS::served_between(., hc_project_eval_start, hc_project_eval_end)) %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate)) %>%
  dplyr::distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

pe_hohs_served_leavers <- co_hohs_served %>%
  dplyr::filter(HMIS::served_between(., hc_project_eval_start, hc_project_eval_end) &
           HMIS::exited_between(., hc_project_eval_start, hc_project_eval_end)) %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate)) %>%
  dplyr::distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# own housing and LoS
# Heads of Household who moved in and exited during date range

pe_hohs_moved_in_leavers <-  co_hohs_moved_in_leavers %>%
  dplyr::filter(HMIS::stayed_between(., hc_project_eval_start, hc_project_eval_end) &
           HMIS::exited_between(., hc_project_eval_start, hc_project_eval_end)) %>%
  dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
  dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
  dplyr::left_join(Client, by = "PersonalID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(-UserID, -DateCreated, -DateUpdated, -DateDeleted, -ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  dplyr::select(dplyr::all_of(vars_we_want)) %>%
  dplyr::arrange(PersonalID, AltProjectID, dplyr::desc(EntryDate)) %>%
  dplyr::distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# Create Validation Summary -----------------------------------------------

# summary_pe_[cohort] - takes client-level pe_[cohort], calculates # of total
# clients in the cohort at the alt-project level

summary_pe_hohs_moved_in_leavers <- pe_hohs_moved_in_leavers %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(HoHsMovedInLeavers = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(HoHsMovedInLeavers = dplyr::if_else(is.na(HoHsMovedInLeavers),
                                      as.integer(0),
                                      HoHsMovedInLeavers))

summary_pe_adults_moved_in_leavers <- pe_adults_moved_in_leavers %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(AdultMovedInLeavers = dplyr::n()) %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(AdultMovedInLeavers = dplyr::if_else(is.na(AdultMovedInLeavers),
                                       as.integer(0),
                                       AdultMovedInLeavers))

# summary_pe_adults_moved_in <- pe_adults_moved_in %>%
#   group_by(AltProjectID) %>%
#   summarise(AdultsMovedIn = n()) %>%
#   ungroup() %>%
#   right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
#   mutate(AdultsMovedIn = if_else(is.na(AdultsMovedIn),
#                                  as.integer(0),
#                                  AdultsMovedIn))

summary_pe_clients_moved_in_leavers <- pe_clients_moved_in_leavers %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(ClientsMovedInLeavers = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(ClientsMovedInLeavers = dplyr::if_else(is.na(ClientsMovedInLeavers),
                                         as.integer(0),
                                         ClientsMovedInLeavers))

summary_pe_hohs_served <- pe_hohs_served %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(HoHsServed = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(HoHsServed = dplyr::if_else(is.na(HoHsServed),
                              as.integer(0),
                              HoHsServed))

summary_pe_hohs_served_leavers <- pe_hohs_served_leavers %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(HoHsServedLeavers = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(HoHsServedLeavers = dplyr::if_else(is.na(HoHsServedLeavers),
                              as.integer(0),
                              HoHsServedLeavers))

summary_pe_clients_served <- pe_clients_served %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(ClientsServed = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(ClientsServed = dplyr::if_else(is.na(ClientsServed),
                                 as.integer(0),
                                 ClientsServed))

summary_pe_adults_entered <- pe_adults_entered %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(AdultsEntered = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(AdultsEntered = dplyr::if_else(is.na(AdultsEntered),
                                 as.integer(0),
                                 AdultsEntered))

summary_pe_hohs_entered <- pe_hohs_entered %>%
  dplyr::group_by(AltProjectID) %>%
  dplyr::summarise(HoHsEntered = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  dplyr::mutate(HoHsEntered = dplyr::if_else(is.na(HoHsEntered),
                               as.integer(0),
                               HoHsEntered))

# joins all summary_pe_[cohort]s into one object so now you have all the cohort
# totals at the alt-project level

pe_validation_summary <- summary_pe_adults_entered %>%
  # full_join(summary_pe_adults_moved_in, by = "AltProjectID") %>%
  dplyr::full_join(summary_pe_hohs_served_leavers, by = "AltProjectID") %>%
  dplyr::full_join(summary_pe_adults_moved_in_leavers, by = "AltProjectID") %>%
  dplyr::full_join(summary_pe_clients_served, by = "AltProjectID") %>%
  dplyr::full_join(summary_pe_clients_moved_in_leavers, by = "AltProjectID") %>%
  dplyr::full_join(summary_pe_hohs_moved_in_leavers, by = "AltProjectID") %>%
  dplyr::full_join(summary_pe_hohs_served, by = "AltProjectID") %>%
  dplyr::full_join(summary_pe_hohs_entered, by = "AltProjectID") %>%
  dplyr::left_join(pe_coc_funded %>%
              dplyr::select(AltProjectID, ProjectType, AltProjectName) %>%
              unique(), by = c("AltProjectID")) %>%
  dplyr::left_join(hoh_exits_to_deceased, by = "AltProjectID") %>%
  dplyr::select(
    ProjectType,
    AltProjectID,
    AltProjectName,
    ClientsServed,
    HoHsEntered,
    HoHsServed,
    HoHsServedLeavers,
    HoHDeaths,
    # AdultsMovedIn,
    AdultsEntered,
    ClientsMovedInLeavers,
    AdultMovedInLeavers,
    HoHsMovedInLeavers
  )

rm(list = ls(pattern = "summary_"))

# Finalizing DQ Flags -----------------------------------------------------

# calculates how many clients have a qualifying error of whatever type. only
# returns the providers with any qualifying errors.

dq_flags_staging <- dq_for_pe %>%
  dplyr::right_join(pe_coc_funded, by = c("ProjectType", "ProjectID", "ProjectName")) %>%
  dplyr::mutate(
    GeneralFlag =
      dplyr::if_else(
        Issue %in% c(
          "Duplicate Entry Exits",
          "Incorrect Entry Exit Type",
          "Children Only Household",
          "No Head of Household",
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household"
        ),
        1,
        0
      ),
    BenefitsFlag =
      dplyr::if_else(
        Issue %in% c(
          "Non-cash Benefits Missing at Entry",
          "Conflicting Non-cash Benefits yes/no at Entry"
        ),
        1,
        0
      ),
    IncomeFlag =
      dplyr::if_else(
        Issue %in% c("Income Missing at Entry",
                     "Conflicting Income yes/no at Entry"),
        1,
        0
      ),
    LoTHFlag =
      dplyr::if_else(
        Issue %in% c("Missing Residence Prior",
                     "Missing Months or Times Homeless",
                     "Incomplete Living Situation Data"),
        1,
        0
      )
  ) %>%
  dplyr::select(AltProjectName,
         PersonalID,
         HouseholdID,
         GeneralFlag,
         BenefitsFlag,
         IncomeFlag,
         LoTHFlag) %>%
  dplyr::filter(
    GeneralFlag + BenefitsFlag + IncomeFlag + LoTHFlag > 0
  ) %>%
  dplyr::group_by(AltProjectName) %>%
  dplyr::summarise(GeneralFlagTotal = sum(GeneralFlag),
            BenefitsFlagTotal = sum(BenefitsFlag),
            IncomeFlagTotal = sum(IncomeFlag),
            LoTHFlagTotal = sum(LoTHFlag))

# calculates whether the # of errors of whatever type actually throws a flag.
# includes all alt-projects regardless of if they have errors

data_quality_flags_detail <- pe_validation_summary %>%
  dplyr::left_join(dq_flags_staging, by = "AltProjectName") %>%
  dplyr::mutate(General_DQ = dplyr::if_else(GeneralFlagTotal/ClientsServed >= .02, 1, 0),
         Benefits_DQ = dplyr::if_else(BenefitsFlagTotal/AdultsEntered >= .02, 1, 0),
         Income_DQ = dplyr::if_else(IncomeFlagTotal/AdultsEntered >= .02, 1, 0),
         LoTH_DQ = dplyr::if_else(LoTHFlagTotal/HoHsServed >= .02, 1, 0))

data_quality_flags_detail[is.na(data_quality_flags_detail)] <- 0

# writing out a file to help notify flagged projects toward end of process

users_eda_groups <- readxl::read_xlsx("data/RMisc2.xlsx",
                             sheet = 15) %>%
  dplyr::select(UserID, UserEmail, EDAGroupName)

eda_groups_providers <- readxl::read_xlsx("data/RMisc2.xlsx",
                                  sheet = 16) %>%
  dplyr::select(ProjectID, EDAGroup)

providers_users <- users_eda_groups %>%
  dplyr::left_join(eda_groups_providers, by = c("EDAGroupName" = "EDAGroup")) %>%
  dplyr::filter(!is.na(ProjectID) &
           !UserID %in% c(COHHIO_admin_user_ids))

notify_about_dq <- data_quality_flags_detail %>%
  dplyr::filter(GeneralFlagTotal > 0 |
           BenefitsFlagTotal > 0 |
           IncomeFlagTotal > 0 |
           LoTHFlagTotal > 0) %>%
  dplyr::left_join(consolidations %>%
              dplyr::select(ProjectID, AltProjectID), by = "AltProjectID") %>%
  dplyr::mutate(ProjectID = dplyr::if_else(is.na(ProjectID), AltProjectID, ProjectID)) %>%
  dplyr::left_join(providers_users, by = "ProjectID")


readr::write_csv(notify_about_dq, "Reports/notify.csv")

# this file ^^ is used by Reports/CoC_Competition/Notify_DQ.Rmd to produce
# emails to all users attached to any of the providers with DQ flags.

# displays flags thrown at the alt-project level

data_quality_flags <- data_quality_flags_detail %>%
  dplyr::select(AltProjectName, General_DQ, Benefits_DQ, Income_DQ, LoTH_DQ)

# CoC Scoring -------------------------------------------------------------

summary_pe_coc_scoring <- pe_coc_funded %>%
  dplyr::left_join(Project, by = c("ProjectType", "ProjectName", "ProjectID")) %>%
  dplyr::select(
    ProjectType,
    ProjectID,
    AltProjectID,
    AltProjectName,
    DateReceivedPPDocs,
    HousingFirstScore,
    ChronicPrioritizationScore,
    PrioritizationWorkgroupScore
  ) %>%
  dplyr::filter(!ProjectID %in% retired) %>%
  dplyr::mutate(
    PrioritizationWorkgroupScore = tidyr::replace_na(PrioritizationWorkgroupScore, 0),
    PrioritizationWorkgroupPossible = 5,
    PrioritizationWorkgroupMath = dplyr::case_when(
      lubridate::today() <= lubridate::ymd(hc_project_eval_docs_due) &
        is.na(DateReceivedPPDocs) ~
        paste0(
          "Documents either not yet received or not yet processed. They are due ",
          format(hc_project_eval_docs_due, "%A %b %e, %Y"),
          "."
        ),
      lubridate::today() > lubridate::ymd(hc_project_eval_docs_due) &
        is.na(DateReceivedPPDocs) ~
        paste0(
          "Documentation either not yet received or not yet processed by the
               CoC Team. They were due ",
          format(hc_project_eval_docs_due, "%A %b %e, %Y"),
          "."
        ),
      lubridate::ymd(DateReceivedPPDocs) > lubridate::ymd(hc_project_eval_docs_due) ~
        "Documentation received past deadline.",
      lubridate::ymd(DateReceivedPPDocs) <= lubridate::ymd(hc_project_eval_docs_due) ~
        "Your documentation was reviewed by the CoC team and scored. Please contact
      ohioboscoc@cohhio.org if you have questions about your scoring."
    ),
    HousingFirstPossible = 15,
    HousingFirstDQ = dplyr::case_when(
      lubridate::ymd(DateReceivedPPDocs) <= lubridate::ymd(hc_project_eval_docs_due) &
        is.na(HousingFirstScore) ~ 3,
      is.na(DateReceivedPPDocs) &
        is.na(HousingFirstScore) ~ 2,
      is.na(DateReceivedPPDocs) &
        !is.na(HousingFirstScore) ~ 4,
      lubridate::ymd(DateReceivedPPDocs) > lubridate::ymd(hc_project_eval_docs_due) ~ 5
    ),
    HousingFirstScore = dplyr::case_when(
      is.na(DateReceivedPPDocs) |
        is.na(HousingFirstScore) ~ -10,
      lubridate::ymd(DateReceivedPPDocs) > lubridate::ymd(hc_project_eval_docs_due) ~ -10,
      lubridate::ymd(DateReceivedPPDocs) <= lubridate::ymd(hc_project_eval_docs_due) ~ HousingFirstScore
    ),
    HousingFirstMath = dplyr::case_when(
      lubridate::today() <= lubridate::ymd(hc_project_eval_docs_due) &
        is.na(DateReceivedPPDocs) ~
        paste0(
          "Documents either not yet received or not yet processed. They are
               due ",
          format(hc_project_eval_docs_due, "%A %b %e, %Y"),
          "."
        ),
      lubridate::today() > lubridate::ymd(hc_project_eval_docs_due) &
        is.na(DateReceivedPPDocs) ~
        paste0(
          "Documentation either not yet received or not yet processed by the
               CoC Team. They were due ",
          format(hc_project_eval_docs_due, "%A %b %e, %Y"),
          "."
        ),
      lubridate::ymd(DateReceivedPPDocs) > lubridate::ymd(hc_project_eval_docs_due) ~
        "Documentation received past deadline.",
      lubridate::ymd(DateReceivedPPDocs) <= lubridate::ymd(hc_project_eval_docs_due) ~
        "Your documentation was reviewed by the CoC team and scored. Please contact
      ohioboscoc@cohhio.org if you have questions about your scoring."
    ),
    ChronicPrioritizationDQ = dplyr::case_when(
      lubridate::ymd(DateReceivedPPDocs) <= lubridate::ymd(hc_project_eval_docs_due) &
        is.na(ChronicPrioritizationScore) ~ 3,
      is.na(DateReceivedPPDocs) &
        is.na(ChronicPrioritizationScore) ~ 2,
      is.na(DateReceivedPPDocs) &
        !is.na(ChronicPrioritizationScore) ~ 4,
      lubridate::ymd(DateReceivedPPDocs) > lubridate::ymd(hc_project_eval_docs_due) ~ 5
    ),
    ChronicPrioritizationPossible = dplyr::if_else(ProjectType == 3, 10, NULL),
    ChronicPrioritizationScore =
      dplyr::case_when(
        lubridate::ymd(DateReceivedPPDocs) <= lubridate::ymd(hc_project_eval_docs_due) &
          ProjectType == 3 &
          !is.na(ChronicPrioritizationScore) ~ ChronicPrioritizationScore,
        is.na(DateReceivedPPDocs) &
          ProjectType == 3 &
          is.na(ChronicPrioritizationScore) ~ -5,
        lubridate::ymd(DateReceivedPPDocs) > lubridate::ymd(hc_project_eval_docs_due) &
          ProjectType == 3 ~ -5
      ),
    ChronicPrioritizationMath = dplyr::case_when(
      lubridate::today() <= lubridate::ymd(hc_project_eval_docs_due) &
        is.na(DateReceivedPPDocs) ~
        paste0(
          "Documents either not yet received or not yet processed. They are due ",
          format(hc_project_eval_docs_due, "%A %b %e, %Y"),
          "."
        ),
      lubridate::today() > lubridate::ymd(hc_project_eval_docs_due) &
        is.na(DateReceivedPPDocs) ~
        paste0(
          "Documentation either not yet received or not yet processed by the
               CoC Team. They were due ",
          format(hc_project_eval_docs_due, "%A %b %e, %Y"),
          "."
        ),
      lubridate::ymd(DateReceivedPPDocs) > lubridate::ymd(hc_project_eval_docs_due) ~
        "Documentation received past deadline.",
      lubridate::ymd(DateReceivedPPDocs) <= lubridate::ymd(hc_project_eval_docs_due) ~
        "Your documentation was reviewed by the CoC team and scored. Please contact
      ohioboscoc@cohhio.org if you have questions about your scoring."
    )
  )

pt_adjustments_after_freeze <- summary_pe_coc_scoring %>%
  dplyr::mutate(
    PrioritizationWorkgroupScore = dplyr::case_when(
      AltProjectID %in% c(1088, 730) ~ 1,
      TRUE ~ PrioritizationWorkgroupScore
    ),
    ChronicPrioritizationScore = dplyr::case_when(
      AltProjectID == 1673 ~ 6,
      AltProjectID == 719 ~ 10,
      TRUE ~ ChronicPrioritizationScore
    )
  )

summary_pe_coc_scoring <- pt_adjustments_after_freeze

# 2 = Documents not yet received
# 3 = Docs received, not yet scored
# 4 = CoC Error
# 5 = Docs received past the due date

# Housing Stability: Exits to PH ------------------------------------------

# pe_[measure] - client-level dataset of all clients counted in the measure
# along with whether each one meets the objective
# summary_pe_[measure] - uses pe_[measure] to smush to alt-project level and
# adds a score

# PSH (includes stayers tho), TH, SH, RRH

pe_exits_to_ph <- pe_hohs_served %>%
  dplyr::right_join(pe_coc_funded %>%
               dplyr::select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(),
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  dplyr::left_join(data_quality_flags, by = "AltProjectName") %>%
  dplyr::filter((ProjectType %in% c(2, 8, 13) &
            HMIS::exited_between(., hc_project_eval_start, hc_project_eval_end)) |
           ProjectType == 3) %>% # filtering out non-PSH stayers
  dplyr::mutate(
    DestinationGroup = dplyr::case_when(
      is.na(Destination) | lubridate::ymd(ExitAdjust) > lubridate::ymd(hc_project_eval_end) ~
        "Still in Program at Report End Date",
      Destination %in% c(temp_destinations) ~ "Temporary",
      Destination %in% c(perm_destinations) ~ "Permanent",
      Destination %in% c(institutional_destinations) ~ "Institutional",
      Destination == 24 ~ "Deceased (not counted)",
      Destination %in% c(other_destinations) ~ "Other"
    ),
    ExitsToPHDQ = dplyr::case_when(
      General_DQ == 1 ~ 1,
      TRUE ~ 0
    ),
    MeetsObjective =
      dplyr::case_when(
        DestinationGroup == "Permanent" |
          (ProjectType == 3 &
             DestinationGroup == "Still in Program at Report End Date") ~ 1,
        TRUE ~ 0
      ),
    PersonalID = as.character(PersonalID)
  ) %>%
  dplyr::select(dplyr::all_of(vars_to_the_apps), ExitsToPHDQ, Destination, DestinationGroup)

summary_pe_exits_to_ph <- pe_exits_to_ph %>%
  dplyr::group_by(ProjectType, AltProjectName, ExitsToPHDQ) %>%
  dplyr::summarise(ExitsToPH = sum(MeetsObjective)) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::mutate(
    ExitsToPHCohort = dplyr::if_else(ProjectType == 3, "HoHsServed", "HoHsServedLeavers"),
    HoHsServedLeavers = HoHsServedLeavers - HoHDeaths,
    HoHsServed = HoHsServed - HoHDeaths,
    ExitsToPH = dplyr::if_else(is.na(ExitsToPH), 0, ExitsToPH),
    ExitsToPHPercent = dplyr::if_else(
      ProjectType == 3,
      ExitsToPH / HoHsServed,
      ExitsToPH / HoHsServedLeavers
    ),
    ExitsToPHPercentJoin = dplyr::if_else(is.na(ExitsToPHPercent), 0, ExitsToPHPercent)) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "exits_to_ph"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(ExitsToPHPossible = max(points)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= ExitsToPHPercentJoin &
                   maximum > ExitsToPHPercentJoin,
                 minimum < ExitsToPHPercentJoin &
                   maximum >= ExitsToPHPercentJoin)) %>%
  dplyr::mutate(ExitsToPHMath = dplyr::case_when(
    ProjectType == 3 & HoHsServed != 0 ~
      paste(
        ExitsToPH,
        "exits to permanent housing or retention in PSH /",
        HoHsServed,
        "heads of household =",
        scales::percent(ExitsToPHPercent, accuracy = 1)
      ),
    ProjectType != 3 & HoHsServedLeavers != 0 ~
      paste(
        ExitsToPH,
        "exits to permanent housing /",
        HoHsServedLeavers,
        "heads of household leavers =",
        scales::percent(ExitsToPHPercent, accuracy = 1)
      )
  ),
  ExitsToPHPoints = dplyr::if_else(
    (ProjectType == 3 &
       HoHsServed == 0) |
      (ProjectType != 3 &
         HoHsServedLeavers == 0),
    ExitsToPHPossible, points
  ),
  ExitsToPHPoints = dplyr::if_else(
    ExitsToPHDQ == 0 | is.na(ExitsToPHDQ),
    ExitsToPHPoints,
    0
  )
  ) %>%
  dplyr::select(
    ProjectType,
    AltProjectName,
    ExitsToPH,
    ExitsToPHMath,
    ExitsToPHPercent,
    ExitsToPHPoints,
    ExitsToPHPossible,
    ExitsToPHDQ,
    ExitsToPHCohort
  )

# # Housing Stability: Moved into Own Housing -------------------------------
# # TH, SH, RRH
#
# pe_own_housing <- pe_hohs_moved_in_leavers %>%
#   right_join(pe_coc_funded %>%
#                select(ProjectType, AltProjectID, AltProjectName) %>%
#                unique(),
#              by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
#   left_join(data_quality_flags, by = "AltProjectName") %>%
#   filter(ProjectType != 3) %>%
#   mutate(
#     MeetsObjective = case_when(
#       Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ 1,
#       !Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ 0
#     ),
#     OwnHousingDQ = case_when(
#       General_DQ == 1 ~ 1,
#       TRUE ~ 0
#     ),
#     DestinationGroup = case_when(
#       is.na(Destination) | ymd(ExitAdjust) > ymd(hc_project_eval_end) ~
#         "Still in Program at Report End Date",
#       Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
#       Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ "Household's Own Housing",
#       Destination %in% c(22:23) ~ "Shared Housing",
#       Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
#       Destination %in% c(8, 9, 17, 30, 99, 32) ~ "Other",
#       Destination == 24 ~ "Deceased"
#     ),
#     PersonalID = as.character(PersonalID)
#   ) %>%
#   select(all_of(vars_to_the_apps), OwnHousingDQ, Destination, DestinationGroup)
#
# summary_pe_own_housing <- pe_own_housing %>%
#   group_by(ProjectType, AltProjectName, OwnHousingDQ) %>%
#   summarise(OwnHousing = sum(MeetsObjective)) %>%
#   ungroup() %>%
#   right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
#   mutate(
#     HoHsMovedInLeavers = HoHsMovedInLeavers - HoHDeaths,
#     OwnHousing = if_else(is.na(OwnHousing), 0, OwnHousing),
#     Structure = if_else(ProjectType != 3, "72_80_5", NULL),
#     OwnHousingPercent = if_else(ProjectType != 3,
#                                 OwnHousing / HoHsMovedInLeavers,
#                                 NULL),
#     OwnHousingMath = case_when(
#       HoHsMovedInLeavers == 0 &
#         ProjectType != 3 ~
#         "All points granted because this project had 0 Heads of Household Leavers who Moved into Housing",
#       ProjectType == 3 &
#         (HoHsMovedInLeavers == 0 | HoHsMovedInLeavers != 0) ~ "",
#       HoHsMovedInLeavers != 0 & ProjectType != 3 ~ paste(
#         OwnHousing,
#         "exited to their own permanent housing /",
#         HoHsMovedInLeavers,
#         "heads of household leavers who moved into housing =",
#         percent(OwnHousingPercent, accuracy = 1)
#       )
#     ),
#     OwnHousingPoints = if_else(
#       HoHsMovedInLeavers == 0 & ProjectType != 3,
#       10,
#       pe_score(Structure, OwnHousingPercent)
#     ),
#     OwnHousingPoints = if_else(is.nan(OwnHousingPercent) &
#                                  ProjectType != 3, 5, OwnHousingPoints),
#     OwnHousingPoints = case_when(OwnHousingDQ == 1 ~ 0,
#                                  is.na(OwnHousingDQ) |
#                                    OwnHousingDQ == 0 ~ OwnHousingPoints),
#     OwnHousingPoints = if_else(is.na(OwnHousingPoints), 0, OwnHousingPoints),
#     OwnHousingPossible = if_else(ProjectType != 3, 5, NULL),
#     OwnHousingCohort = "HoHsMovedInLeavers"
#   ) %>%
#   select(ProjectType,
#          AltProjectName,
#          OwnHousingCohort,
#          OwnHousing,
#          OwnHousingMath,
#          OwnHousingPercent,
#          OwnHousingPoints,
#          OwnHousingPossible,
#          OwnHousingDQ)

# Accessing Mainstream Resources: Benefits -----------------------------------
# PSH, TH, SH, RRH

pe_benefits_at_exit <- pe_adults_moved_in_leavers %>%
  dplyr::right_join(pe_coc_funded %>%
               dplyr::select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(),
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  dplyr::left_join(data_quality_flags, by = "AltProjectName") %>%
  dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  dplyr::select(
    PersonalID,
    AltProjectName,
    EnrollmentID,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    VeteranStatus,
    EntryDate,
    MoveInDateAdjust,
    AgeAtEntry,
    ExitDate,
    ExitAdjust,
    BenefitsFromAnySource,
    InsuranceFromAnySource,
    DataCollectionStage,
    General_DQ,
    Benefits_DQ
  ) %>%
  dplyr::filter(DataCollectionStage == 3) %>%
  dplyr::mutate(
    MeetsObjective =
      dplyr::case_when(
        (BenefitsFromAnySource == 1 |
           InsuranceFromAnySource == 1) ~ 1,
        TRUE ~ 0
      ),
    BenefitsAtExitDQ = dplyr::if_else(General_DQ == 1 |
                                 Benefits_DQ == 1, 1, 0),
    PersonalID = as.character(PersonalID)
  ) %>%
  dplyr::select(
    dplyr::all_of(vars_to_the_apps),
    BenefitsAtExitDQ,
    BenefitsFromAnySource,
    InsuranceFromAnySource
  )

summary_pe_benefits_at_exit <- pe_benefits_at_exit %>%
  dplyr::group_by(ProjectType, AltProjectName, BenefitsAtExitDQ) %>%
  dplyr::summarise(BenefitsAtExit = sum(MeetsObjective)) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::mutate(
    BenefitsAtExit = dplyr::if_else(is.na(BenefitsAtExit), 0, BenefitsAtExit),
    BenefitsAtExitPercent = BenefitsAtExit / AdultMovedInLeavers,
    BenefitsAtExitPercentJoin = dplyr::if_else(is.na(BenefitsAtExitPercent), 0, BenefitsAtExitPercent)) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "benefits_at_exit"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(BenefitsAtExitPossible = max(points)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= BenefitsAtExitPercentJoin &
                   maximum > BenefitsAtExitPercentJoin,
                 minimum < BenefitsAtExitPercentJoin &
                   maximum >= BenefitsAtExitPercentJoin)) %>%
  dplyr::mutate(BenefitsAtExitMath = dplyr::if_else(
    AdultMovedInLeavers == 0,
    "All points granted because this project had no adult leavers who moved into the project's housing",
    paste(
      BenefitsAtExit,
      "exited with benefits or health insurance /",
      AdultMovedInLeavers,
      "adult leavers who moved into the project's housing =",
      scales::percent(BenefitsAtExitPercent, accuracy = 1)
    )
  ),
  BenefitsAtExitDQ = dplyr::if_else(is.na(BenefitsAtExitDQ), 0, BenefitsAtExitDQ),
  BenefitsAtExitPoints = dplyr::case_when(
    AdultMovedInLeavers == 0 ~ BenefitsAtExitPossible,
    TRUE ~ points
  ),
  BenefitsAtExitPoints = dplyr::case_when(
    BenefitsAtExitDQ == 1 ~ 0,
    is.na(BenefitsAtExitDQ) |
      BenefitsAtExitDQ == 0 ~ BenefitsAtExitPoints
  ),
  BenefitsAtExitCohort = "AdultMovedInLeavers"
  ) %>%
  dplyr::select(
    ProjectType,
    AltProjectName,
    BenefitsAtExitCohort,
    BenefitsAtExit,
    BenefitsAtExitMath,
    BenefitsAtExitPercent,
    BenefitsAtExitPoints,
    BenefitsAtExitPossible,
    BenefitsAtExitDQ
  )

# Accessing Mainstream Resources: Increase Total Income -------------------
# PSH, TH, SH, RRH

# income_staging2 <-  pe_adults_moved_in %>%
#   right_join(pe_coc_funded %>%
#                select(ProjectType, AltProjectID, AltProjectName) %>%
#                unique(),
#              by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
#   left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
#   select(PersonalID,
#          EnrollmentID,
#          EntryDate,
#          ExitDate,
#          TotalMonthlyIncome,
#          DateCreated,
#          DataCollectionStage) %>%
#   mutate(
#     DataCollectionStage = case_when(
#       DataCollectionStage == 1 ~ "Entry",
#       DataCollectionStage == 2 ~ "Update",
#       DataCollectionStage == 3 ~ "Exit",
#       DataCollectionStage == 5 ~ "Annual"
#     )
#   )
#
# income_staging_fixed <- income_staging2 %>%
#   filter(DataCollectionStage == "Entry")
#
# income_staging_variable <- income_staging2 %>%
#   filter(DataCollectionStage %in% c("Update", "Annual", "Exit")) %>%
#   group_by(EnrollmentID) %>%
#   mutate(MaxUpdate = max(ymd_hms(DateCreated))) %>%
#   filter(MaxUpdate == DateCreated) %>%
#   select(-MaxUpdate) %>%
#   distinct() %>%
#   ungroup()
#
# income_staging <- rbind(income_staging_fixed, income_staging_variable) %>%
#   select(PersonalID, EnrollmentID, TotalMonthlyIncome, DataCollectionStage) %>%
#   unique()
#
# pe_increase_income <- income_staging %>%
#   pivot_wider(names_from = DataCollectionStage,
#               values_from = TotalMonthlyIncome) %>%
#   left_join(pe_adults_moved_in, by = c("PersonalID", "EnrollmentID")) %>%
#   left_join(data_quality_flags, by = "AltProjectName") %>%
#   mutate(
#     MostRecentIncome = case_when(
#       !is.na(Exit) ~ Exit,
#       !is.na(Update) ~ Update,
#       !is.na(Annual) ~ Annual
#     ),
#     IncomeAtEntry = if_else(is.na(Entry), 0, Entry),
#     IncomeMostRecent = if_else(is.na(MostRecentIncome),
#                                IncomeAtEntry,
#                                MostRecentIncome),
#     MeetsObjective = case_when(
#       IncomeMostRecent > IncomeAtEntry ~ 1,
#       IncomeMostRecent <= IncomeAtEntry ~ 0),
#     IncreasedIncomeDQ = if_else(General_DQ == 1 |
#                                   Income_DQ == 1, 1, 0),
#     PersonalID = as.character(PersonalID)
#   ) %>%
#   select(
#     all_of(vars_to_the_apps),
#     IncreasedIncomeDQ,
#     IncomeAtEntry,
#     IncomeMostRecent
#   )
#
# rm(list = ls(pattern = "income_staging"))
#
# summary_pe_increase_income <- pe_increase_income %>%
#   group_by(ProjectType, AltProjectName, IncreasedIncomeDQ) %>%
#   summarise(IncreasedIncome = sum(MeetsObjective)) %>%
#   ungroup() %>%
#   right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
#   mutate(
#     IncreasedIncome = if_else(is.na(IncreasedIncome), 0, IncreasedIncome),
#     Structure = case_when(
#       ProjectType == 3 ~ "24_30_11",
#       ProjectType == 2 ~ "22_28_10",
#       ProjectType == 8 ~ "16_20_10",
#       ProjectType == 13 ~ "14_18_10"
#     ),
#     IncreasedIncomePercent = IncreasedIncome / AdultsMovedIn,
#     IncreasedIncomeMath = if_else(
#       AdultsMovedIn != 0,
#       paste(
#         IncreasedIncome,
#         "increased income during their stay /",
#         AdultsMovedIn,
#         "adults who moved into the project's housing =",
#         percent(IncreasedIncomePercent, accuracy = 1)
#       ),
#       "All points granted because 0 adults moved into the project's housing"
#     ),
#     IncreasedIncomePoints = case_when(
#       IncreasedIncomeDQ == 1 ~ 0,
#       AdultsMovedIn > 0 ~ pe_score(Structure, IncreasedIncomePercent),
#       ProjectType == 3 &
#       AdultsMovedIn == 0 &
#         (IncreasedIncomeDQ == 0 | is.na(IncreasedIncomeDQ)) ~ 11,
#       ProjectType != 3 &
#         AdultsMovedIn == 0 &
#         (IncreasedIncomeDQ == 0 | is.na(IncreasedIncomeDQ)) ~ 10
#     ),
#     IncreasedIncomePossible = if_else(ProjectType == 3, 11, 10),
#     IncreasedIncomeCohort = "AdultsMovedIn"
#   ) %>%
#   select(
#     ProjectType,
#     AltProjectName,
#     IncreasedIncome,
#     IncreasedIncomeCohort,
#     IncreasedIncomeMath,
#     IncreasedIncomePercent,
#     IncreasedIncomePoints,
#     IncreasedIncomePossible,
#     IncreasedIncomeDQ
#   )

# Housing Stability: Length of Time Homeless ------------------------------
# TH, SH, RRH

pe_length_of_stay <- pe_hohs_moved_in_leavers %>%
  dplyr::right_join(pe_coc_funded %>%
               dplyr::select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(),
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  dplyr::left_join(data_quality_flags, by = "AltProjectName") %>%
  dplyr::mutate(DaysInProject = difftime(lubridate::ymd(ExitAdjust), lubridate::ymd(EntryDate), units = "days"),
         PersonalID = as.character(PersonalID)) %>%
  dplyr::select(ProjectType,
         AltProjectName,
         General_DQ,
         EntryDate,
         EntryAdjust,
         MoveInDateAdjust,
         ExitDate,
         DaysInProject,
         PersonalID,
         EnrollmentID,
         HouseholdID)

summary_pe_length_of_stay <- pe_length_of_stay %>%
  dplyr::group_by(ProjectType, AltProjectName, General_DQ) %>%
  dplyr::summarise(
    AverageDays = as.numeric(mean(DaysInProject))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::mutate(
    AverageDaysJoin = dplyr::if_else(is.na(AverageDays), 0, AverageDays)) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "length_of_stay"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(AverageLoSPossible = max(points)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= AverageDaysJoin &
                   maximum > AverageDaysJoin,
                 minimum < AverageDaysJoin &
                   maximum >= AverageDaysJoin)) %>%
  dplyr::mutate(
    AverageLoSPoints = dplyr::case_when(
      ClientsMovedInLeavers == 0 &
        ProjectType != 3 ~ AverageLoSPossible,
      TRUE ~ points
    ),
    AverageLoSMath = dplyr::if_else(
      ClientsMovedInLeavers == 0,
      "All points granted because this project had 0 leavers who moved into the project's housing",
      paste(as.integer(AverageDays), "average days")
    ),
    AverageLoSDQ = dplyr::case_when(
      ProjectType %in% c(2, 8, 13) ~ General_DQ),
    AverageLoSPoints = dplyr::case_when(
      AverageLoSDQ == 1 ~ 0,
      AverageLoSDQ == 0 | is.na(AverageLoSDQ) ~ AverageLoSPoints),
    AverageLoSPoints = dplyr::if_else(is.na(AverageLoSPoints), 0, AverageLoSPoints),
    AverageLoSCohort = "ClientsMovedInLeavers"
  ) %>%
  dplyr::select(ProjectType, AltProjectName, AverageLoSMath, AverageLoSCohort,
         AverageLoSPoints, AverageLoSPossible, AverageLoSDQ)

# Community Need: Res Prior = Streets or ESSH -----------------------------
# PSH, TH, SH (Street only), RRH

pe_res_prior <- pe_adults_entered %>%
  dplyr::right_join(pe_coc_funded %>%
               dplyr::select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(),
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  dplyr::left_join(data_quality_flags, by = "AltProjectName") %>%
  dplyr::filter(ProjectType %in% c(2, 3, 13, 8)) %>%
  dplyr::mutate(LHResPriorDQ = dplyr::if_else(General_DQ == 1, 1, 0),
         MeetsObjective = dplyr::if_else(
           (ProjectType %in% c(2, 3, 13) &
              LivingSituation %in% c(1, 16, 18)) |
             (ProjectType == 8 &
                LivingSituation == 16),
           1,
           0
         ),
         PersonalID = as.character(PersonalID)) %>%
  dplyr::select(dplyr::all_of(vars_to_the_apps), LivingSituation, LHResPriorDQ)

summary_pe_res_prior <- pe_res_prior %>%
  dplyr::group_by(ProjectType, AltProjectName, LHResPriorDQ) %>%
  dplyr::summarise(LHResPrior = sum(MeetsObjective)) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary,
             by = c("ProjectType",
                    "AltProjectName")) %>%
  dplyr::mutate(
    LHResPrior = dplyr::if_else(is.na(LHResPrior), 0, LHResPrior),
    LHResPriorPercent = LHResPrior / AdultsEntered,
    LHResPriorPercentJoin = dplyr::if_else(is.na(LHResPriorPercent), 0, LHResPriorPercent)) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "res_prior"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(LHResPriorPossible = max(points)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= LHResPriorPercentJoin &
                   maximum > LHResPriorPercentJoin,
                 minimum < LHResPriorPercentJoin &
                   maximum >= LHResPriorPercentJoin)) %>%
  dplyr::mutate(LHResPriorMath = dplyr::if_else(
    AdultsEntered == 0,
    "All points granted because this project has 0 adults who entered the project",
    paste(
      LHResPrior,
      "coming from shelter or streets (unsheltered) /",
      AdultsEntered,
      "adults who entered the project during the reporting period =",
      scales::percent(LHResPriorPercent, accuracy = 1)
    )
  ),
  LHResPriorDQ = dplyr::if_else(is.na(LHResPriorDQ), 0, LHResPriorDQ),
  LHResPriorPoints = dplyr::case_when(
    AdultsEntered == 0 ~ LHResPriorPossible,
    TRUE ~ points),
  LHResPriorPoints = dplyr::case_when(
    LHResPriorDQ == 1 ~ 0,
    LHResPriorDQ == 0 | is.na(LHResPriorDQ) ~ LHResPriorPoints),
  LHResPriorCohort = "AdultsEntered"
  ) %>%
  dplyr::select(
    ProjectType,
    AltProjectName,
    LHResPrior,
    LHResPriorCohort,
    LHResPriorMath,
    LHResPriorPercent,
    LHResPriorPoints,
    LHResPriorPossible,
    LHResPriorDQ
  )

# Community Need: Entries with No Income ----------------------------------
# PSH, TH, SH, RRH

pe_entries_no_income <- pe_adults_entered %>%
  dplyr::right_join(pe_coc_funded %>%
               dplyr::select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(),
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  dplyr::left_join(data_quality_flags, by = "AltProjectName") %>%
  dplyr::filter(ProjectType %in% c(2, 3, 13, 8)) %>%
  dplyr::left_join(IncomeBenefits %>%
              dplyr::select(EnrollmentID,
                     InformationDate,
                     IncomeFromAnySource) %>%
              unique(),
            by = c("EnrollmentID", "EntryDate" = "InformationDate")) %>%
  dplyr::mutate(
    PersonalID = as.character(PersonalID),
    IncomeFromAnySource = dplyr::if_else(is.na(IncomeFromAnySource),
                                  99,
                                  IncomeFromAnySource),
    MeetsObjective = dplyr::if_else(IncomeFromAnySource == 0, 1, 0),
    NoIncomeAtEntryDQ = dplyr::if_else(General_DQ == 1 |
                                  Income_DQ == 1, 1, 0)
  ) %>%
  dplyr::select(dplyr::all_of(vars_to_the_apps), IncomeFromAnySource, NoIncomeAtEntryDQ)

summary_pe_entries_no_income <- pe_entries_no_income %>%
  dplyr::group_by(ProjectType, AltProjectName, NoIncomeAtEntryDQ) %>%
  dplyr::summarise(NoIncomeAtEntry = sum(MeetsObjective)) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::mutate(
    NoIncomeAtEntry = dplyr::if_else(is.na(NoIncomeAtEntry),
                              0,
                              NoIncomeAtEntry),
    NoIncomeAtEntryDQ = dplyr::if_else(is.na(NoIncomeAtEntryDQ), 0, NoIncomeAtEntryDQ),
    NoIncomeAtEntryPercent = NoIncomeAtEntry / AdultsEntered,
    NoIncomeAtEntryPercentJoin = dplyr::if_else(is.na(NoIncomeAtEntryPercent), 0, NoIncomeAtEntryPercent)) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "entries_no_income"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(NoIncomeAtEntryPossible = max(points)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= NoIncomeAtEntryPercentJoin &
                   maximum > NoIncomeAtEntryPercentJoin,
                 minimum < NoIncomeAtEntryPercentJoin &
                   maximum >= NoIncomeAtEntryPercentJoin)) %>%
  dplyr::mutate(
    NoIncomeAtEntryMath = dplyr::if_else(
      AdultsEntered == 0,
      "All points granted because 0 adults entered this project during the reporting period",
      paste(
        NoIncomeAtEntry,
        "had no income at entry /",
        AdultsEntered,
        "adults who entered the project during the reporting period =",
        scales::percent(NoIncomeAtEntryPercent, accuracy = 1)
      )
    ),
    NoIncomeAtEntryPoints = dplyr::case_when(
      AdultsEntered == 0 ~ NoIncomeAtEntryPossible,
      TRUE ~ points),
    NoIncomeAtEntryPoints = dplyr::case_when(
      NoIncomeAtEntryDQ == 1 ~ 0,
      NoIncomeAtEntryDQ == 0 |
        is.na(NoIncomeAtEntryDQ) ~ NoIncomeAtEntryPoints
    ),
    NoIncomeAtEntryCohort = "AdultsEntered"
  ) %>%
  dplyr::select(
    ProjectType,
    AltProjectName,
    NoIncomeAtEntry,
    NoIncomeAtEntryCohort,
    NoIncomeAtEntryMath,
    NoIncomeAtEntryPercent,
    NoIncomeAtEntryPoints,
    NoIncomeAtEntryPossible,
    NoIncomeAtEntryDQ
  )

# Community Need: Homeless History Index ----------------------------------
# PSH, TH, SH, RRH

score_matrix <- as.data.frame(matrix(
  c(0, 1, 1, 2,
    1, 1, 2, 2,
    2, 2, 2, 3,
    3, 3, 4, 4,
    4, 5, 5, 6,
    5, 6, 6, 7),
  nrow = 6, ncol = 4, byrow = TRUE))

pe_homeless_history_index <- pe_adults_entered %>%
  dplyr::select(
    ProjectType,
    AltProjectName,
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    VeteranStatus,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears
  ) %>%
  dplyr::left_join(data_quality_flags, by = "AltProjectName") %>%
  dplyr::mutate(
    DaysHomelessAtEntry = dplyr::if_else(
      lubridate::ymd(EntryDate) >= lubridate::ymd(DateToStreetESSH),
      difftime(EntryDate,
               DateToStreetESSH,
               units = "days"),
      NULL
    ),
    NumMonthsLevel = dplyr::case_when(
      is.na(MonthsHomelessPastThreeYears) ~ 1,
      MonthsHomelessPastThreeYears == 101 ~ 2,
      MonthsHomelessPastThreeYears %in% c(102, 103, 104) ~ 3,
      MonthsHomelessPastThreeYears %in% c(105, 106, 107, 108) ~ 4,
      MonthsHomelessPastThreeYears %in% c(109, 110, 111) ~ 5,
      MonthsHomelessPastThreeYears %in% c(112, 113) ~ 6
    ),
    TimesLevel = dplyr::case_when(
      is.na(TimesHomelessPastThreeYears) ~ 1,
      TimesHomelessPastThreeYears == 1 ~ 2,
      TimesHomelessPastThreeYears %in% c(2, 3) ~ 3,
      TimesHomelessPastThreeYears == 4 ~ 4
    ),
    HHI = dplyr::if_else(DaysHomelessAtEntry >= 365, 7,
                  score_matrix[cbind(NumMonthsLevel, TimesLevel)]),
    HHI = tidyr::replace_na(HHI, 0), # when null I'm seeing client wasn't even eligible
    PersonalID = as.character(PersonalID)
  ) %>%
  dplyr::select(-NumMonthsLevel, -TimesLevel)

summary_pe_homeless_history_index <- pe_homeless_history_index %>%
  dplyr::group_by(ProjectType, AltProjectName, General_DQ) %>%
  dplyr::summarise(MedHHI = stats::median(HHI)) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "homeless_history_index"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(MedianHHIPossible = max(points),
         MedHHIJoin = dplyr::if_else(is.na(MedHHI), 0, MedHHI)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= MedHHIJoin &
                   maximum > MedHHIJoin,
                 minimum < MedHHIJoin &
                   maximum >= MedHHIJoin)) %>%
  dplyr::mutate(
    MedianHHIMath = dplyr::if_else(
      AdultsEntered == 0,
      "All points granted since 0 adults entered this project during the reporting period",
      paste("Median Homeless History Index = ", MedHHI)
    ),
    MedianHHIPoints = dplyr::if_else(AdultsEntered == 0, MedianHHIPossible,
                              points),
    MedianHHIDQ = dplyr::if_else(General_DQ == 1, 1, 0),
    MedianHHIDQ = dplyr::if_else(is.na(MedianHHIDQ), 0, MedianHHIDQ),
    MedianHHIPoints = dplyr::case_when(MedianHHIDQ == 1 ~ 0,
                                MedianHHIDQ == 0 | is.na(MedianHHIDQ) ~ MedianHHIPoints),
    MedianHHICohort = "AdultsEntered"
  ) %>%
  dplyr::select(ProjectType,
         AltProjectName,
         MedHHI,
         MedianHHIMath,
         MedianHHICohort,
         MedianHHIPoints,
         MedianHHIPossible,
         MedianHHIDQ)

# HMIS Data Quality -------------------------------------------------------
# PSH, TH, SH, RRH

pe_dq <- dq_for_pe %>%
  dplyr::filter(Type %in% c("Error", "High Priority") &
           ProjectType %in% c(2, 3, 13, 8)) %>%
  dplyr::inner_join(pe_coc_funded, by = c("ProjectName", "ProjectID", "ProjectType"))

summary_pe_dq <- pe_dq %>%
  dplyr::group_by(AltProjectName, ProjectType) %>%
  dplyr::count() %>%
  dplyr::ungroup()

summary_pe_dq <- pe_validation_summary %>%
  dplyr::select(AltProjectName, ProjectType, ClientsServed) %>%
  dplyr::left_join(summary_pe_dq, by = c("ProjectType", "AltProjectName"))

summary_pe_dq[is.na(summary_pe_dq)] <- 0

summary_pe_dq <- summary_pe_dq %>%
  dplyr::mutate(DQPercent = n / ClientsServed,
         DQMath = paste(n,
                      "errors /",
                      ClientsServed,
                      "clients served =",
                      scales::percent(DQPercent, accuracy = 1)),
         DQPoints = dplyr::case_when(
           n == 0 ~ 5,
           DQPercent > 0 & DQPercent <= .02 ~ 4,
           DQPercent > .02 & DQPercent <= .05 ~ 3,
           DQPercent > .05 & DQPercent <= .08 ~ 2,
           DQPercent > .08 & DQPercent <= .1 ~ 1,
           DQPercent > .1 ~ 0
           ),
         DQPossible = 5,
         DQCohort = "ClientsServed"
         ) %>%
  dplyr::select(AltProjectName, ProjectType, "DQIssues" = n, DQCohort, DQPercent,
         DQPoints, DQMath, DQPossible)

# Community Need: Long Term Homeless Households ---------------------------
# PSH
# Decided in Feb meeting that we're going to use Adults Entered for this one

pe_long_term_homeless <- pe_adults_entered %>%
  dplyr::right_join(pe_coc_funded %>%
               dplyr::select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(),
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  dplyr::left_join(data_quality_flags, by = "AltProjectName") %>%
  dplyr::mutate(
    CurrentHomelessDuration = difftime(lubridate::ymd(EntryDate), lubridate::ymd(DateToStreetESSH),
                                       units = "days"),
    MeetsObjective = dplyr::if_else((
        CurrentHomelessDuration >= 365 &
          !is.na(CurrentHomelessDuration)
      ) |
        (
          TimesHomelessPastThreeYears == 4 &
            MonthsHomelessPastThreeYears %in% c(112, 113) &
            !is.na(TimesHomelessPastThreeYears) &
            !is.na(MonthsHomelessPastThreeYears)
        ),
      1,
      0
    ),
    LTHomelessDQ = dplyr::if_else(ProjectType == 3 & General_DQ == 1, 1, 0),
    PersonalID = as.character(PersonalID)
  ) %>%
  dplyr::select(dplyr::all_of(vars_to_the_apps), DateToStreetESSH,
         CurrentHomelessDuration, MonthsHomelessPastThreeYears,
         TimesHomelessPastThreeYears, LTHomelessDQ)

summary_pe_long_term_homeless <- pe_long_term_homeless %>%
  dplyr::group_by(ProjectType, AltProjectName, LTHomelessDQ) %>%
  dplyr::summarise(LongTermHomeless = sum(MeetsObjective)) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::mutate(
    LongTermHomeless = dplyr::if_else(is.na(LongTermHomeless),
                               0,
                               LongTermHomeless),
    LongTermHomelessPercent = dplyr::if_else(AdultsEntered > 0,
                                      LongTermHomeless / AdultsEntered,
                                      NULL),
    LongTermHomelessPercentJoin = dplyr::if_else(is.na(LongTermHomelessPercent), 0, LongTermHomelessPercent)) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "long_term_homeless"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(LongTermHomelessPossible = max(points)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= LongTermHomelessPercentJoin &
                   maximum > LongTermHomelessPercentJoin,
                 minimum < LongTermHomelessPercentJoin &
                   maximum >= LongTermHomelessPercentJoin)) %>%
  dplyr::mutate(
    LongTermHomelessMath = dplyr::if_else(
      AdultsEntered == 0,
      "All points granted because 0 adults entered this project during the reporting period",
      paste(
        LongTermHomeless,
        "considered to be long-term homeless /",
        AdultsEntered,
        "adults entered the project during the reporting period =",
        scales::percent(LongTermHomelessPercent, accuracy = 1)
      )
    ),
    LongTermHomelessPoints = dplyr::if_else(AdultsEntered == 0 &
                                       ProjectType == 3, LongTermHomelessPossible,
                                     points),
    LongTermHomelessPoints = dplyr::case_when(LTHomelessDQ == 0 |
                                         is.na(LTHomelessDQ) ~ LongTermHomelessPoints,
                                       LTHomelessDQ == 1 ~ 0),
    LongTermHomelessPoints = dplyr::if_else(is.na(LongTermHomelessPoints), 0,
                                     LongTermHomelessPoints),
    LongTermhomelessCohort = "AdultsEntered"
  ) %>%
  dplyr::select(
    ProjectType,
    AltProjectName,
    LongTermHomeless,
    LongTermHomelessPercent,
    LongTermHomelessPoints,
    LongTermHomelessMath,
    LongTermhomelessCohort,
    LongTermHomelessPossible,
    LTHomelessDQ
  )

# VISPDATs at Entry into PH -----------------------------------------------

pe_scored_at_ph_entry <- pe_hohs_entered %>%
  dplyr::right_join(pe_coc_funded %>%
               dplyr::select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(),
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  dplyr::left_join(data_quality_flags, by = c("AltProjectName")) %>%
  dplyr::left_join(
    dq_for_pe %>%
      dplyr::filter(Issue == "Non-DV HoHs Entering PH or TH without SPDAT") %>%
      dplyr::select("PersonalID", "HouseholdID", "Issue"),
    by = c("PersonalID", "HouseholdID")
  ) %>%
  dplyr::filter(ProjectType != 8) %>%
  dplyr::mutate(
    MeetsObjective = dplyr::case_when(
      !is.na(PersonalID) & is.na(Issue) & ProjectType %in% c(2, 3, 13) ~ 1,
      !is.na(PersonalID) & !is.na(Issue) & ProjectType %in% c(2, 3, 13) ~ 0,
      is.na(PersonalID) & is.na(Issue) & ProjectType %in% c(2, 3, 13) ~ 1),
    ScoredAtEntryDQ = dplyr::case_when(
      ProjectType %in% c(2, 3, 13) & General_DQ == 1 ~ 1,
      ProjectType %in% c(2, 3, 13) & General_DQ == 0 ~ 0),
    PersonalID = as.character(PersonalID)
  ) %>%
  dplyr::select(dplyr::all_of(vars_to_the_apps), ScoredAtEntryDQ)

summary_pe_scored_at_ph_entry <- pe_scored_at_ph_entry %>%
  dplyr::group_by(ProjectType, AltProjectName, ScoredAtEntryDQ) %>%
  dplyr::summarise(ScoredAtEntry = sum(MeetsObjective)) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::mutate(
    ScoredAtEntry = dplyr::if_else(is.na(ScoredAtEntry),
                            0,
                            ScoredAtEntry),
    ScoredAtEntryPercent = dplyr::if_else(HoHsEntered > 0,
                                   ScoredAtEntry / HoHsEntered,
                                   NULL),
    ScoredAtEntryPercentJoin = dplyr::if_else(is.na(ScoredAtEntryPercent), 0, ScoredAtEntryPercent)) %>%
  dplyr::right_join(scoring_rubric %>%
               dplyr::filter(metric == "scored_at_ph_entry"),
             by = "ProjectType") %>%
  dplyr::group_by(ProjectType) %>%
  dplyr::mutate(ScoredAtEntryPossible = max(points)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::if_else(goal_type == "max",
                 minimum <= ScoredAtEntryPercentJoin &
                   maximum > ScoredAtEntryPercentJoin,
                 minimum < ScoredAtEntryPercentJoin &
                   maximum >= ScoredAtEntryPercentJoin)) %>%
  dplyr::mutate(
    ScoredAtEntryMath = dplyr::if_else(
      HoHsEntered == 0,
      "All points granted because 0 households entered the project during the reporting period",
      paste(
        ScoredAtEntry,
        "had a VI-SPDAT score at entry /",
        HoHsEntered,
        "heads of household who entered the project during the reporting period =",
        scales::percent(ScoredAtEntryPercent, accuracy = 1)
      )
    ),
    ScoredAtEntryPoints = dplyr::case_when(
      HoHsEntered == 0 ~ ScoredAtEntryPossible,
      HoHsEntered > 0 ~ points),
    ScoredAtEntryPoints = dplyr::case_when(
      ScoredAtEntryDQ == 0 ~ ScoredAtEntryPoints,
      ScoredAtEntryDQ == 1 ~ 0,
      is.na(ScoredAtEntryDQ) ~ ScoredAtEntryPoints),
    ScoredAtEntryPoints = dplyr::if_else(is.na(ScoredAtEntryPoints),
                                  0,
                                  ScoredAtEntryPoints),
    ScoredAtEntryCohort = "HoHsEntered"
  ) %>%
  dplyr::select(
    ProjectType,
    AltProjectName,
    ScoredAtEntry,
    ScoredAtEntryMath,
    ScoredAtEntryPercent,
    ScoredAtEntryPoints,
    ScoredAtEntryCohort,
    ScoredAtEntryPossible,
    ScoredAtEntryDQ
  )

# Final Scoring -----------------------------------------------------------

# all the alt-projects & score details & totals

summary_pe_final_scoring <-
  pe_coc_funded[c("ProjectType", "AltProjectName")] %>%
  unique() %>%
  dplyr::left_join(summary_pe_dq, by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_entries_no_income,
            by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_exits_to_ph,
            by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_scored_at_ph_entry,
            by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_homeless_history_index,
            by = c("ProjectType", "AltProjectName")) %>%
  # left_join(summary_pe_increase_income,
  #           by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_length_of_stay,
            by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_long_term_homeless,
            by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_benefits_at_exit,
            by = c("ProjectType", "AltProjectName")) %>%
  # left_join(summary_pe_own_housing,
  #           by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_res_prior,
            by = c("ProjectType", "AltProjectName")) %>%
  dplyr::left_join(summary_pe_coc_scoring, by = c("ProjectType", "AltProjectName"))

pe_final_scores <- summary_pe_final_scoring

pe_final_scores$HousingFirstScore[is.na(pe_final_scores$HousingFirstScore)] <- 0
pe_final_scores$ChronicPrioritizationScore[is.na(pe_final_scores$ChronicPrioritizationScore)] <- 0
pe_final_scores$PrioritizationWorkgroupScore[is.na(pe_final_scores$PrioritizationWorkgroupScore)] <- 0
pe_final_scores$AverageLoSPoints[is.na(pe_final_scores$AverageLoSPoints)] <- 0
pe_final_scores$LongTermHomelessPoints[is.na(pe_final_scores$LongTermHomelessPoints)] <- 0

pe_final_scores <- pe_final_scores %>%
  dplyr::mutate(
    TotalScore = DQPoints +
      NoIncomeAtEntryPoints +
      ExitsToPHPoints +
      ScoredAtEntryPoints +
      MedianHHIPoints +
      # IncreasedIncomePoints +
      AverageLoSPoints +
      LongTermHomelessPoints +
      BenefitsAtExitPoints +
      # OwnHousingPoints +
      LHResPriorPoints +
      HousingFirstScore +
      ChronicPrioritizationScore +
      PrioritizationWorkgroupScore
  ) %>%
  dplyr::select(ProjectType,
         AltProjectName,
         dplyr::ends_with("Points"),
         dplyr::ends_with("Score"),
         dplyr::ends_with("Scoring"),
         TotalScore)

# adding in Organization Name for publishing the final ranking
# Org Names for the combined projects have to be done manually

project_and_alt_project <- pe_coc_funded %>%
  dplyr::left_join(Project[c("ProjectID", "OrganizationID")], by = "ProjectID") %>%
  dplyr::left_join(Organization[c("OrganizationID", "OrganizationName")],
            by = "OrganizationID")

final_scores <- pe_final_scores %>%
  dplyr::select(AltProjectName, TotalScore) %>%
  dplyr::left_join(project_and_alt_project, by = c("AltProjectName" = "ProjectName")) %>%
  dplyr::select(OrganizationName, AltProjectName, TotalScore) %>%
  dplyr::arrange(dplyr::desc(TotalScore))

# Clean the House ---------------------------------------------------------

rm(list = ls()[!(ls() %in% c(
  'pe_benefits_at_exit',
  'pe_coc_scoring',
  'pe_dq_by_provider',
  'pe_entries_no_income',
  'pe_exits_to_ph',
  'pe_final_scores',
  'pe_homeless_history_index',
  # 'pe_increase_income',
  'pe_length_of_stay',
  'pe_long_term_homeless',
  # 'pe_own_housing',
  'pe_res_prior',
  'pe_scored_at_ph_entry',
  'pe_validation_summary',
  'summary_pe_benefits_at_exit',
  'summary_pe_dq_by_provider',
  'summary_pe_entries_no_income',
  'summary_pe_exits_to_ph',
  'summary_pe_homeless_history_index',
  # 'summary_pe_increase_income',
  'summary_pe_length_of_stay',
  'summary_pe_long_term_homeless',
  # 'summary_pe_own_housing',
  'summary_pe_res_prior',
  'summary_pe_scored_at_ph_entry',
  'summary_pe_utilization',
  'summary_pe_final_scoring',
  'final_scores'
))])
# commenting all this out since we don't want to overwrite these files after
# the deadline

zero_divisors <- pe_validation_summary %>%
  dplyr::filter(ClientsServed == 0 |
           HoHsEntered == 0 |
           HoHsServed == 0 |
           HoHsServedLeavers == 0 |
           # AdultsMovedIn == 0 |
           AdultsEntered == 0 |
           ClientsMovedInLeavers == 0 |
           AdultMovedInLeavers == 0 |
           HoHsMovedInLeavers == 0) %>%
  dplyr::select(-HoHDeaths)

readr::write_csv(zero_divisors, "random_data/zero_divisors.csv")

readr::write_csv(final_scores %>%
            dplyr::select(OrganizationName,
                   AltProjectName,
                   TotalScore), "random_data/pe_final.csv")

readr::write_csv(pe_final_scores, "random_data/pe_final_all.csv")

# saving old data to "current" image so it all carries to the apps
app_env$gather_deps()
app_env
}
