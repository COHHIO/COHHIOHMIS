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



#' @title Bed Utilizations
#'
#' @inheritParams load_export
#'
#' @export
#'
#' @include 01_Bed_Unit_Utilization_utils.R
bed_unit_utilization <- function(
  Project,
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
) {

  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())


# despite the fact we're pulling in usually more than 2 years of data, the
# utilization reporting will only go back 2 years. (decision based on lack of
# a need to go back further and time to code all that.)




# Creating Beds table -----------------------------------------------------

small_project <- Project |>
  HMIS::operating_between(rm_dates$calc$two_yrs_prior_start, rm_dates$calc$two_yrs_prior_end) |>
  dplyr::filter(ProjectType %in% data_types$Project$ProjectType$w_beds &
           is.na(GrantType) &
           HMISParticipationType == 1) |>
  dplyr::select(ProjectID,
                ProjectName,
                ProjectType,
                HMISParticipationType)

small_inventory <- Inventory |>
  dplyr::select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate
    )  |>
  dplyr::filter((
    InventoryStartDate <= rm_dates$calc$two_yrs_prior_end &
      (
        InventoryEndDate >= rm_dates$calc$two_yrs_prior_start |
          is.na(InventoryEndDate)
      )
  ) &
    Inventory$CoCCode %in% c("OH-507", "OH-504"))

utilization_beds <- dplyr::inner_join(small_project, small_inventory, by = "ProjectID")

# Creating Utilizers table ------------------------------------------------

small_enrollment <- Enrollment_extra_Client_Exit_HH_CL_AaE |>
  dplyr::select(
    UniqueID,
    PersonalID,
    EnrollmentID,
    ProjectID,
    EntryDate,
    EntryAdjust,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust,
    HouseholdID,
    RelationshipToHoH,
    MoveInDate
  ) |>
  HMIS::served_between(rm_dates$calc$two_yrs_prior_start,
                       rm_dates$calc$two_yrs_prior_end)

Utilizers <- dplyr::semi_join(small_enrollment, utilization_beds, by = "ProjectID")

Utilizers <- dplyr::left_join(Utilizers, small_project, by = "ProjectID") |>
  dplyr::select(
    UniqueID,
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectName,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    EntryAdjust,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust
  )

# Client Utilization of Beds ----------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the
# fake training providers
utilization_clients <- Utilizers |>
  dplyr::mutate(StayWindow = lubridate::interval(EntryAdjust, ExitAdjust)) |>
  dplyr::filter(
    lubridate::int_overlaps(StayWindow, rm_dates$calc$two_yrs_prior_range) &
      (
    (
      ProjectType %in% c(3, 9) &
        !is.na(EntryAdjust) &
        MoveInDateAdjust >= EntryDate &
        MoveInDateAdjust < ExitAdjust
    ) |
      ProjectType %in% c(1, 2, 8)
  ))

# filtering Beds object to exclude any providers that served 0 hhs in date range

utilization_beds <- dplyr::right_join(utilization_beds, utilization_clients |>
               dplyr::select(ProjectID) |>
               unique(), by = "ProjectID")




utilization_clients <- bu_add_month_counts(utilization_clients) |>
  dplyr::select(
    ProjectName,
    UniqueID,
    ProjectID,
    ProjectType,
    PersonalID,
    EnrollmentID,
    EntryDate,
    MoveInDateAdjust,
    EntryAdjust,
    ExitDate,
    tidyselect::matches("\\w{3}\\d{4}")
  )







# making granularity by provider instead of by enrollment id
BedNights <- utilization_clients  |>
  bu_sum_months("_BN")

# Bed Capacity ------------------------------------------------------------

BedCapacity <- utilization_beds |>
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) |>
  dplyr::mutate(InventoryEndAdjust = dplyr::if_else(is.na(InventoryEndDate),
                                      rm_dates$calc$two_yrs_prior_end,
                                      InventoryEndDate),
         InventoryStartAdjust = dplyr::if_else(InventoryStartDate >= rm_dates$calc$two_yrs_prior_start,
                                        InventoryStartDate,
                                        rm_dates$calc$two_yrs_prior_start),
         AvailableWindow = lubridate::interval(InventoryStartAdjust,
                                    InventoryEndAdjust))



BedCapacity <- bu_add_month_counts(BedCapacity, fn = unit_capacity)  |>
  dplyr::select(
    -InventoryStartDate,
    -InventoryEndDate,
    -InventoryEndAdjust,-BedInventory,
    -InventoryStartAdjust,
    -AvailableWindow
  )


BedCapacity <- bu_sum_months(BedCapacity, "_BC")


# Bed Utilization ---------------------------------------------------------

utilization_bed <- bu_month_proportion(BedNights, BedCapacity)


# Inf means there were no beds but there were clients served.
# %NaN means there were no beds and no clients served that month.

rm(BedCapacity, BedNights)



# HH Utilization of Units -------------------------------------------------


HHUtilizers <- Utilizers |>
  dplyr::mutate(
    EntryAdjust = dplyr::case_when(
      ProjectType %in% c(data_types$Project$ProjectType$lh) ~ EntryDate,
      ProjectType %in% c(3, 9) ~ MoveInDateAdjust
    ),
    ExitAdjust = dplyr::if_else(
      is.na(ExitDate) & EntryAdjust <= rm_dates$calc$two_yrs_prior_end,
      rm_dates$calc$two_yrs_prior_end,
      ExitDate
    ),
    StayWindow = lubridate::interval(EntryAdjust, ExitAdjust)
  ) |>
  dplyr::filter(
    (HouseholdID %in% {
      Utilizers |>
        dplyr::group_by(HouseholdID) |>
        dplyr::summarise(N = dplyr::n(), .groups = "drop") |>
        dplyr::filter(N == 1) |>
        dplyr::pull(HouseholdID)
      } | RelationshipToHoH == 1) &
      lubridate::int_overlaps(StayWindow, rm_dates$calc$two_yrs_prior_range) &
      (
        (
          ProjectType %in% c(3, 9) &
            !is.na(EntryAdjust) &
            MoveInDateAdjust >= EntryDate &
            MoveInDateAdjust <= ExitAdjust
        ) |
          ProjectType %in% data_types$Project$ProjectType$lh
      )
  ) |>
  dplyr::select(-EntryDate,-MoveInDateAdjust,-HouseholdID,-RelationshipToHoH)

HHUtilizers <- bu_add_month_counts(HHUtilizers) |>
  dplyr::mutate(
    dplyr::across(tidyselect::matches("\\w{3}\\d{4}"), ~dplyr::if_else(is.na(.x), 0, .x))
  )

# making granularity by provider instead of by enrollment id
HHNights <- bu_sum_months(HHUtilizers, "_HN")

# leaving this one ^^ because the client-level
# detail should be good enough for R minor elevated

rm(HHUtilizers)


# Unit Capacity -----------------------------------------------------------

UnitCapacity <- utilization_beds |>
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) |>
  dplyr::mutate(InventoryEndAdjust = dplyr::if_else(is.na(InventoryEndDate),
                                      rm_dates$calc$two_yrs_prior_end,
                                      InventoryEndDate),
         InventoryStartAdjust = dplyr::if_else(InventoryStartDate >= rm_dates$calc$two_yrs_prior_start,
                                        InventoryStartDate,
                                        rm_dates$calc$two_yrs_prior_start),
         AvailableWindow = lubridate::interval(InventoryStartAdjust,
                                    InventoryEndAdjust),
         UnitCount = dplyr::if_else(HouseholdType == 3,
                             UnitInventory, BedInventory))



UnitCapacity <- bu_add_month_counts(UnitCapacity, fn = unit_capacity, multiplier_col = "UnitCount")

UnitCapacity <- bu_sum_months(UnitCapacity, "_UC")

# Unit Utilization --------------------------------------------------------

utilization_unit <- bu_month_proportion(HHNights,
                                        UnitCapacity)


rm(UnitCapacity, HHNights, Utilizers)


small_project <- Project |>
  dplyr::filter(ProjectType %in% c(data_types$Project$ProjectType$w_beds) &
           OperatingStartDate <= lubridate::today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= lubridate::today()) &
           is.na(Project$GrantType)) |>
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         OrganizationName,
         HMISParticipationType)

# Current Bed Utilization -------------------------------------------------

small_inventory <- Inventory |>
  dplyr::filter((InventoryStartDate <= lubridate::today() &
            (
              InventoryEndDate >= lubridate::today() |
                is.na(InventoryEndDate)
            )) &
           Inventory$CoCCode %in% c("OH-507", "OH-504")) |>
  dplyr::select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate
  )

small_inventory <- dplyr::inner_join(small_project, small_inventory, by = "ProjectID")

Capacity <- small_inventory |>
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         OrganizationName,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) |>
  dplyr::mutate(UnitCount = dplyr::if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) |>
  dplyr::group_by(ProjectID, ProjectName, ProjectType, OrganizationName) |>
  dplyr::summarise(UnitCount = sum(UnitCount),
            BedCount = sum(BedInventory),
            .groups = "drop")

providerids <- Capacity |>
  dplyr::select(ProjectID, ProjectName, OrganizationName, ProjectType) |>
  dplyr::arrange(ProjectName)
#here is where you could add a left join to the Regions object and add in Region

Clients <- Enrollment_extra_Client_Exit_HH_CL_AaE  |>
  dplyr::left_join(providerids, by = c("ProjectID", "ProjectName")) |>
  dplyr::filter(is.na(ExitDate)) |>
  dplyr::group_by(ProjectID, ProjectName) |>
  dplyr::summarise(Clients = dplyr::n_distinct(PersonalID),
                   .groups = "drop")

Households <- Enrollment_extra_Client_Exit_HH_CL_AaE |>
  dplyr::left_join(providerids, by = c("ProjectID", "ProjectName")) |>
  dplyr::filter(is.na(ExitDate)) |>
  dplyr::group_by(ProjectID, ProjectName) |>
  dplyr::summarise(Households = dplyr::n_distinct(HouseholdID),
                   .groups = "drop")

utilization <-
  dplyr::left_join(Capacity, Clients,
            by = c("ProjectID", "ProjectName"))  |>
  dplyr::left_join(Households,
            by = c("ProjectID", "ProjectName")) |>
  dplyr::filter(ProjectType %in% c(data_types$Project$ProjectType$w_beds)) |>
  dplyr::mutate(BedUtilization = scales::percent(Clients/BedCount, accuracy = 1),
         UnitUtilization = scales::percent(Households/UnitCount, accuracy = 1))


rm(Households, Clients, Capacity, small_inventory, small_project, providerids)

# notes moved to Rminor: notes.R



# removing all the Value objects we created as those are not used in the apps



# Find Outliers for HIC Purposes ------------------------------------------

# utilization_unit_overall <- utilization_unit |>
#   select(ProjectID, ProjectName, ProjectType, rm_dates$calc$two_yrs_prior_range)
#
# outliers_hi <- subset(utilization_unit_overall,
#                       rm_dates$calc$two_yrs_prior_range > quantile(rm_dates$calc$two_yrs_prior_range, prob = 0.90))
#
# outliers_lo <- subset(utilization_unit_overall,
#                       rm_dates$calc$two_yrs_prior_range < quantile(rm_dates$calc$two_yrs_prior_range, prob = 0.03))
#
# outliers <- rbind(outliers_hi, outliers_lo)

# WARNING save.image does not save the environment properly, save must be used.
app_env$gather_deps(utilization, utilization_unit, utilization_bed, utilization_beds)

}

