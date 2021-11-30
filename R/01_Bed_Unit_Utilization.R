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
  Inventory,
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
  dplyr::filter(ProjectType %in% project_types$w_beds &
           is.na(GrantType) &
           HMISParticipatingProject == 1) |>
  dplyr::select(ProjectID,
                ProjectName,
                ProjectType,
                HMISParticipatingProject)

small_inventory <- Inventory %>%
  dplyr::select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate
    )  %>%
  dplyr::filter((
    InventoryStartDate <= rm_dates$calc$two_yrs_prior_end &
      (
        InventoryEndDate >= rm_dates$calc$two_yrs_prior_start |
          is.na(InventoryEndDate)
      )
  ) &
    Inventory$CoCCode %in% c("OH-507", "OH-504"))

Beds <- dplyr::inner_join(small_project, small_inventory, by = "ProjectID")

# Creating Utilizers table ------------------------------------------------

small_enrollment <- Enrollment_extra_Exit_HH_CL_AaE %>%
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

Utilizers <- dplyr::semi_join(small_enrollment, Beds, by = "ProjectID")

Utilizers <- dplyr::left_join(Utilizers, small_project, by = "ProjectID") %>%
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
utilization_clients <- Utilizers %>%
  dplyr::mutate(StayWindow = lubridate::interval(EntryAdjust, ExitAdjust)) %>%
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

Beds <- dplyr::right_join(Beds, utilization_clients %>%
               dplyr::select(ProjectID) %>%
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

BedCapacity <- Beds %>%
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
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


HHUtilizers <- Utilizers %>%
  dplyr::mutate(
    EntryAdjust = dplyr::case_when(
      ProjectType %in% c(project_types$lh) ~ EntryDate,
      ProjectType %in% c(3, 9) ~ MoveInDateAdjust
    ),
    ExitAdjust = dplyr::if_else(
      is.na(ExitDate) & EntryAdjust <= rm_dates$calc$two_yrs_prior_end,
      rm_dates$calc$two_yrs_prior_end,
      ExitDate
    ),
    StayWindow = lubridate::interval(EntryAdjust, ExitAdjust)
  ) %>%
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
          ProjectType %in% project_types$lh
      )
  ) %>%
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

UnitCapacity <- Beds %>%
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
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


small_project <- Project %>%
  dplyr::filter(ProjectType %in% c(project_types$w_beds) &
           OperatingStartDate <= lubridate::today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= lubridate::today()) &
           is.na(Project$GrantType)) %>%
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         OrganizationName,
         HMISParticipatingProject)

# Current Bed Utilization -------------------------------------------------

small_inventory <- Inventory %>%
  dplyr::filter((InventoryStartDate <= lubridate::today() &
            (
              InventoryEndDate >= lubridate::today() |
                is.na(InventoryEndDate)
            )) &
           Inventory$CoCCode %in% c("OH-507", "OH-504")) %>%
  dplyr::select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate
  )

small_inventory <- dplyr::inner_join(small_project, small_inventory, by = "ProjectID")

Capacity <- small_inventory %>%
  dplyr::select(ProjectID,
         ProjectName,
         ProjectType,
         OrganizationName,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  dplyr::mutate(UnitCount = dplyr::if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) %>%
  dplyr::group_by(ProjectID, ProjectName, ProjectType, OrganizationName) %>%
  dplyr::summarise(UnitCount = sum(UnitCount),
            BedCount = sum(BedInventory),
            .groups = "drop")

providerids <- Capacity %>%
  dplyr::select(ProjectID, ProjectName, OrganizationName, ProjectType) %>%
  dplyr::arrange(ProjectName)
#here is where you could add a left join to the Regions object and add in Region

Clients <- Enrollment_extra_Exit_HH_CL_AaE  |>
  dplyr::left_join(providerids, by = c("ProjectID", "ProjectName")) %>%
  dplyr::filter(is.na(ExitDate)) %>%
  dplyr::group_by(ProjectID, ProjectName) %>%
  dplyr::summarise(Clients = dplyr::n_distinct(PersonalID),
                   .groups = "drop")

Households <- Enrollment_extra_Exit_HH_CL_AaE |>
  dplyr::left_join(providerids, by = c("ProjectID", "ProjectName")) %>%
  dplyr::filter(is.na(ExitDate)) %>%
  dplyr::group_by(ProjectID, ProjectName) %>%
  dplyr::summarise(Households = dplyr::n_distinct(HouseholdID),
                   .groups = "drop")

utilization <-
  dplyr::left_join(Capacity, Clients,
            by = c("ProjectID", "ProjectName"))  |>
  dplyr::left_join(Households,
            by = c("ProjectID", "ProjectName")) %>%
  dplyr::filter(ProjectType %in% c(project_types$w_beds)) %>%
  dplyr::mutate(BedUtilization = scales::percent(Clients/BedCount, accuracy = 1),
         UnitUtilization = scales::percent(Households/UnitCount, accuracy = 1))


rm(Households, Clients, Capacity, small_inventory, small_project, providerids)

note_bed_utilization <- "Bed Utilization is the percentage of a project's available beds being populated by individual clients."

note_unit_utilization <- "Unit Utilization is the percentage of a project's available units being populated by households. A household can be a single individual or multiple clients presenting together for housing."

note_calculation_utilization <- "Bed Utilization = bed nights* served / total possible bed nights** in a month.
Unit Utilization = unit nights* served / total possible unit nights in a
month.

* A bed night is a single night in a bed.
* A unit night is a single night in a unit.
** Total possible bed/unit nights = number of beds/units a project has multiplied by how many days are in the given month.

Example A: Client A enters a shelter on May 1 and exits on May 5. They spent four nights in the shelter, so that was 4 bed nights from that client alone in the month of May for that shelter.

Example B: PSH Project A served 10 people every single night in the month of June. Each client was served 30 bed nights during that month, and since there were 10 clients, that PSH project served a total of 300 bed nights for the month of June.

Example C: PSH Project B has 5 beds. That project's total possible bed
nights for the month of April (which has 30 days in it) is 30 x 5, which is 150.

Example D: Using what we know from Example B of PSH Project A's total bed nights for the month of June, let's calculate what their bed utilization was for that month. They have 11 beds and June has 30 days so since 11 × 30 = 330 possible bed nights. Their bed utilization is bed nights (300) divided by possible bed nights (330), which is: 91%!"

# removing all the Value objects we created as those are not used in the apps



# Find Outliers for HIC Purposes ------------------------------------------

# utilization_unit_overall <- utilization_unit %>%
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
app_env$gather_deps("everything")
app_env

}

