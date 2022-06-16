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

# this script uses the HMIS data to populate the QPR.

qpr_ees <- function(
  Enrollment_extra_Client_Exit_HH_CL_AaE,
  Services_enroll_extras,
  enrollment_small,
  project_small,
  dirs,
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  # TODO To get the Total RRH (Which should be 75% of all ESG funding spent on Services)
  # Rme - QPR - RRH Spending
  # Rm - QPR - RRH vs HP
  # Services_extras$ServiceAmount[Services_extras$FundName |>
  #                              stringr::str_detect("RRH") |>
  #                              which()]

  # decided to continue to use a separate file for Goals (instead of building it
  # in a tribble) because this way the CoC team can review it more easily.

  # goals <- readr::read_csv(file.path(dirs$public, "BoSGoals.csv"), col_types = "cccdddddddd")

  goals <- clarity.looker::hud_load("goals", dirs$public) |>
    tidyr::pivot_longer(- tidyselect::all_of(c("SummaryMeasure", "Measure", "Operator")),  names_to = "ProjectType",
                        values_to = "Goal") |>
    dplyr::mutate(ProjectType = as.numeric(ProjectType)) |>
    dplyr::filter(!is.na(Goal))


  # Building qpr_leavers ----------------------------------------------------

  enrollment_small <- enrollment_small |>
    {
      \(x) {
        dplyr::filter(
          x,
          HouseholdID %in% (
            x |> dplyr::group_by(HouseholdID) |> dplyr::summarise(N = dplyr::n()) |> dplyr::filter(N == 1) |> dplyr::pull(HouseholdID)
          ) |
            RelationshipToHoH == 1
        )
      }
    }() #<- only pulls in hohs and singles

  # captures all leavers PLUS stayers in either HP or PSH because we include those
  # stayers in Permanent Destinations. This is used for LoS and Exits to PH.
  project_enrollment_small <- project_small |>
    dplyr::left_join(enrollment_small, by = "ProjectID")

  qpr_leavers <- project_enrollment_small |>
    HMIS::served_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter((!is.na(ExitDate) | ProjectType %in% c(1:4, 8:9, 12:13)) &  RelationshipToHoH == 1) |>
    dplyr::mutate(
      DestinationGroup = dplyr::case_when(
        Destination %in% destinations$temp ~ "Temporary",
        Destination %in% destinations$perm ~ "Permanent",
        Destination %in% destinations$institutional ~ "Institutional",
        Destination %in% destinations$other ~ "Other",
        is.na(Destination) ~ "Still in Program"
      ),
      DaysinProject = as.numeric(difftime(ExitAdjust, EntryDate, units = "days"))
    ) |>
    HMIS::stayed_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::arrange(ProjectName)

  qpr_rrh_enterers <- project_enrollment_small |>
    HMIS::entered_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(ProjectType == 13 & RelationshipToHoH == 1) |>
    dplyr::mutate(
      DaysToHouse = as.numeric(difftime(MoveInDateAdjust, EntryDate, units = "days")),
      DaysinProject = as.numeric(difftime(ExitAdjust, EntryAdjust, units = "days"))
    )

  smallMainstreamBenefits <- IncomeBenefits |>
    dplyr::select(InsuranceFromAnySource, BenefitsFromAnySource,
           DataCollectionStage, EnrollmentID, InformationDate) |>
    dplyr::group_by(EnrollmentID) |>
    dplyr::slice(which.max(InformationDate)) |> # most recent answer per Enrollment
    dplyr::ungroup()


  qpr_benefits <- project_enrollment_small |>
    HMIS::exited_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(RelationshipToHoH == 1) |>
    dplyr::left_join(smallMainstreamBenefits, by = "EnrollmentID") |>
    dplyr::select(
      ProjectName,
      UniqueID,
      PersonalID,
      HouseholdID,
      EntryDate,
      EntryAdjust,
      MoveInDate,
      MoveInDateAdjust,
      ExitDate,
      ExitAdjust,
      InsuranceFromAnySource,
      BenefitsFromAnySource,
      DataCollectionStage,
      InformationDate,
      ProjectRegion,
      ProjectCounty,
      ProjectType
    ) |>
    dplyr::mutate(ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)) |>
    dplyr::arrange(ProjectName, HouseholdID)

  incomeMostRecent <- IncomeBenefits |>
    dplyr::select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage,
           EnrollmentID, InformationDate) |>
    dplyr::group_by(EnrollmentID) |>
    dplyr::arrange(EnrollmentID, InformationDate) |>
    tidyr::fill(TotalMonthlyIncome, .direction = "down") |>
    dplyr::slice(which.max(InformationDate)) |>
    dplyr::ungroup() |>
    dplyr::mutate(RecentIncome = TotalMonthlyIncome) |>
    dplyr::select(EnrollmentID, RecentIncome)

  incomeAtEntry <- IncomeBenefits |>
    dplyr::select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage,
           EnrollmentID, InformationDate) |>
    dplyr::group_by(EnrollmentID) |>
    dplyr::slice(which.min(InformationDate)) |>
    dplyr::ungroup() |>
    dplyr::mutate(EntryIncome = TotalMonthlyIncome) |>
    dplyr::select(EnrollmentID, EntryIncome)

  smallIncomeDiff <-
    dplyr::full_join(incomeAtEntry, incomeMostRecent, by = "EnrollmentID")

  qpr_income <- project_enrollment_small |>
    HMIS::served_between(rm_dates$calc$data_goes_back_to, rm_dates$meta_HUDCSV$Export_End) |>
    dplyr::filter(RelationshipToHoH == 1) |>
    dplyr::left_join(smallIncomeDiff, by = "EnrollmentID") |>
    dplyr::select(
      ProjectName,
      UniqueID,
      PersonalID,
      HouseholdID,
      EntryDate,
      EntryAdjust,
      MoveInDate,
      MoveInDateAdjust,
      ExitDate,
      ExitAdjust,
      EntryIncome,
      RecentIncome,
      ProjectRegion,
      ProjectCounty,
      ProjectType
    ) |>
    dplyr::mutate(
      Difference = RecentIncome - EntryIncome,
      ProjectType = HMIS::hud_translations$`2.02.6 ProjectType`(ProjectType)
    ) |>
    dplyr::arrange(ProjectName, HouseholdID)

  qpr_spending <- Services_enroll_extras |>
    dplyr::filter(!is.na(ServiceAmount)) |>
    dplyr::distinct(ServiceID, PersonalID, EnrollmentID, ServiceStartDate, ServiceEndDate, FundName, ServiceAmount, .keep_all = TRUE) |>
    dplyr::left_join(Enrollment_extra_Client_Exit_HH_CL_AaE,
              by = UU::common_names(Services_enroll_extras, Enrollment_extra_Client_Exit_HH_CL_AaE)) |>
    dplyr::left_join(project_small, by = c("ProjectID", "ProjectType", "ProjectName")) |>
    dplyr::select(
      UniqueID,
      PersonalID,
      OrganizationName,
      ProjectName,
      ProjectRegion,
      ProjectType,
      ServiceAmount,
      ServiceItemName,
      RelationshipToHoH,
      ServiceStartDate,
      EntryDate,
      MoveInDateAdjust,
      ExitDate
    ) |>
    dplyr::filter(ProjectType %in% c(13, 12) &
             RelationshipToHoH == 1 &
             !is.na(ServiceAmount)) |>
    dplyr::select(-RelationshipToHoH)




  # Save it out -------------------------------------------------------------
  # WARNING save.image does not save the environment properly, save must be used.
  app_env$gather_deps(ls(pattern = "^qpr"))


}
