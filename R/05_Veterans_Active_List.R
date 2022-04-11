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

vet_active <- function(
  ServiceAreas,
  co_clients_served,
  data_types,
  Enrollment,
  Project,
  VeteranCE,
  Contacts,
  bos_counties,
  clarity_api,
  app_env,
  e = rlang::caller_env()
) {
  if (missing(clarity_api))
    clarity_api <- RmData::get_clarity_api(e = e)
  if (missing(app_env))
    app_env <- RmData::get_app_env(e = e)
  app_env$set_parent(missing_fmls())


  Offers <- clarity_api$`HUD Extras`$Client_Offer_extras()
  # Get all veterans and associated hh members ------------------------------

  responsible_providers <- ServiceAreas |>
    dplyr::select(County, SSVFServiceArea)
  .vet_ees_cols <-
    c(
      "AgeAtEntry",
      "AnnualPercentAMI",
      "ClientLocation",
      "County",
      "DateToStreetESSH",
      "DateVeteranIdentified",
      "Destination",
      "DisablingCondition",
      "EnrollmentID",
      "EntryAdjust",
      "EntryDate",
      "ExitAdjust",
      "ExitDate",
      "ExpectedPHDate",
      "HOMESID",
      "HouseholdID",
      "HousingStatus",
      "LengthOfStay",
      "ListStatus",
      "LivingSituation",
      "LOSUnderThreshold",
      "MonthsHomelessPastThreeYears",
      "MoveInDateAdjust",
      "OtherDestination",
      "PersonalID",
      "PHTrack",
      "PreviousStreetESSH",
      "ProjectCounty",
      "ProjectID",
      "ProjectName",
      "ProjectType",
      "RelationshipToHoH",
      "SSVFIneligible",
      "TimesHomelessPastThreeYears",
      "UniqueID",
      "UserCreating",
      "VAEligible",
      "VAMCStation",
      "VeteranStatus"
    )

  vet_ees <- co_clients_served |>
    dplyr::filter(ProjectType %in% c(data_types$Project$ProjectType$lh_at_entry, data_types$Project$ProjectType$ap)) |>
    dplyr::mutate(VeteranStatus = dplyr::if_else(VeteranStatus == 1, 1, 0)) |>
    dplyr::group_by(HouseholdID) |> # pulling in all Veterans & non-veteran hh members
    dplyr::summarise(VetCount = sum(VeteranStatus, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(VetCount > 0) |>
    dplyr::left_join(dplyr::select(Enrollment_extra_Client_Exit_HH_CL_AaE, dplyr::any_of(c(
      .vet_ees_cols, "CountyServed"
    ))),
    by = "HouseholdID") |>
    dplyr::left_join(Project[c("ProjectID", "ProjectCounty")], by = "ProjectID") |>
    dplyr::left_join(VeteranCE,
                     by = c("PersonalID", "UniqueID", "EnrollmentID", "ExpectedPHDate", "PHTrack")) |>

    dplyr::mutate(County = dplyr::if_else(is.na(CountyServed), ProjectCounty, CountyServed)) |>
    dplyr::filter(County %in% bos_counties |
                     County == "Mahoning") |>
    dplyr::select(dplyr::all_of(.vet_ees_cols))

  # Currently in PSH/RRH ----------------------------------------------------

  # RRH PSH stays with no Exit but a valid Move-In Date

  currently_housed_in_psh_rrh <- vet_ees |>
    {\(x) {HMIS::stayed_between(x, start = min(x$EntryAdjust, na.rm = TRUE),
                               end = Sys.Date())}}() |>
    dplyr::filter(ProjectType %in% data_types$Project$ProjectType$ph &
                    VeteranStatus == 1) |>
    dplyr::pull(PersonalID)

  # If they're in RRH or PSH and have MoveInDates they should not appear on the active list
  # browser()

  # Declined  ---------------------------------------------------------------
  most_recent_offer <- Offers |>
    dplyr::filter(!is.na(AcceptDeclineDate) &
                    !is.na(OfferAccepted) &
                    !is.na(PHTypeOffered)) |>
    dplyr::group_by(PersonalID) |>
    dplyr::slice_max(OfferDate) |> # same date
    dplyr::slice_max(OfferAccepted) |> # both rejected/accepted
    dplyr::slice(1) |> # pick 1, doesn't matter if those ^ are the same
    dplyr::ungroup() |>
    unique()

  declined <- vet_ees |>
    dplyr::left_join(most_recent_offer, by = "PersonalID") |>
    dplyr::filter(OfferAccepted == "No" &
                    OfferDate >= lubridate::today() - lubridate::days(14) &
                    VeteranStatus == 1) |>
    unique()

  # Notes -------------------------------------------------------------------

  small_CLS <- Contacts |>
    dplyr::group_by(PersonalID) |>
    dplyr::arrange(dplyr::desc(ContactDate)) |>
    dplyr::mutate(Notes = paste0(glue_skip_NA(ContactDate, CurrentLivingSituation, str_expr = "{ContactDate} - CLS: {stringr::str_remove(CurrentLivingSituation, '\\\\(.*\\\\)')}") , glue_skip_NA(LocationDetails, str_expr = "{paste0('\nDetails: ', LocationDetails)}"))) |>
    dplyr::select(PersonalID, Notes) |>
    dplyr::summarise(Notes = paste0(Notes, collapse = "\n"), .groups = "drop")


  # Entry Exits -------------------------------------------------------------

  small_ees <- vet_ees |>
    dplyr::filter(!PersonalID %in% currently_housed_in_psh_rrh &
                    VeteranStatus == 1 &
                    (is.na(ExitDate) |
                       (
                         !Destination %in% destinations$perm &
                           ExitDate >= lubridate::today() - lubridate::days(90)
                       ))) |>
    dplyr::select(
      PersonalID,
      EnrollmentID,
      ProjectID,
      ProjectType,
      ProjectName,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      Destination
    ) |>
    unique() |>
    dplyr::group_by(PersonalID) |>
    dplyr::arrange(dplyr::desc(EntryDate)) |>
    dplyr::mutate(
      Entries = paste(
        "Entered",
        ProjectName,
        "on",
        EntryDate,
        dplyr::case_when(
          is.na(MoveInDateAdjust) & is.na(ExitDate) ~  dplyr::if_else(
            ProjectType %in% data_types$Project$ProjectType$lh, "to present", "awaiting housing"),
          !is.na(MoveInDateAdjust) & !is.na(ExitDate) ~
            paste(
              "Moved In on",
              MoveInDateAdjust,
              "and Exited on",
              ExitDate,
              "to",
              HMIS::hud_translations$`3.12.1 Living Situation Option List`(Destination) |> stringr::str_remove("\\(.*\\)")
            ),
          !is.na(MoveInDateAdjust) & is.na(ExitDate) ~ # should never happen but eh
            paste("Moved In on",
                  MoveInDateAdjust,
                  "and is current"),
          is.na(MoveInDateAdjust) & !is.na(ExitDate) ~
            paste("Exited on", ExitDate,
                  "to", HMIS::hud_translations$`3.12.1 Living Situation Option List`(Destination) |> stringr::str_remove("\\(.*\\)"))
        )
      )
    ) |>
    dplyr::summarise(Entries = paste0(Entries, collapse = "\n"), .groups = "drop")

  # Active List -------------------------------------------------------------

  # stayers & people who exited in the past 90 days to a temp destination

  vet_active <- vet_ees |>
    dplyr::filter(!HousingStatus %in% c("Housed", "Likely housed"))


  hh_size <- vet_active |>
    dplyr::select(HouseholdID, PersonalID) |>
    unique() |>
    dplyr::count(HouseholdID)

  veteran_active_list_enrollments <- vet_active |>
    dplyr::filter(VeteranStatus == 1) |>
    dplyr::left_join(hh_size, by = "HouseholdID") |>
    dplyr::rename("HouseholdSize" = n) |>
    dplyr::mutate(EnrollType = dplyr::case_when(
      ProjectType %in% data_types$Project$ProjectType$lh ~ 1,
      ProjectType %in% data_types$Project$ProjectType$ph ~ 2,
      TRUE ~ 3
    )) |>
    dplyr::group_by(PersonalID, EnrollType) |>
    dplyr::arrange(dplyr::desc(EntryDate)) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  non_hoh_vets <- veteran_active_list_enrollments |>
    dplyr::filter(RelationshipToHoH != 1) |>
    dplyr::select(PersonalID, HouseholdID, RelationshipToHoH)

  hoh_chronicity <- non_hoh_vets |>
    dplyr::inner_join(vet_ees |>
                        dplyr::filter(RelationshipToHoH == 1 &
                                        HouseholdID %in% non_hoh_vets$HouseholdID) |>
                        dplyr::distinct() |>
                        chronic_determination() |>
                        dplyr::rename(HoHChronicStatus = ChronicStatus),
                      by = c("HouseholdID"), suffix = c("", "_V")) |>
    dplyr::select(PersonalID, HoHChronicStatus) |>
    dplyr::arrange(HoHChronicStatus) |>
    dplyr::group_by(PersonalID) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  enrollments_to_use <- veteran_active_list_enrollments |>
    dplyr::mutate(ProjectName = dplyr::if_else(ProjectName == "Unsheltered Clients - OUTREACH",
                                               paste("Unsheltered in", County, "County"),
                                               ProjectName),
                  TimeInProject = dplyr::if_else(
                    is.na(ExitDate),
                    paste("Since", format(EntryDate, "%m-%d-%Y")),
                    paste(
                      format(EntryDate, "%m-%d-%Y"),
                      "to",
                      format(ExitDate, "%m-%d-%Y")
                    )
                  )) |>
    dplyr::select(PersonalID, ProjectName, TimeInProject, ProjectType, EntryDate)

  combined <- enrollments_to_use |>
    dplyr::filter(ProjectType %in% data_types$Project$ProjectType$lh) |>
    dplyr::rename_with(.cols = - PersonalID, .fn = ~{paste0(.x,"_LH")}) |>

    dplyr::mutate(transitional_housing_entry =
                    dplyr::case_when(ProjectType_LH == 2 &
                                       grepl("Since", TimeInProject_LH) ~ EntryDate_LH)) |>
    dplyr::full_join(enrollments_to_use |>
                       dplyr::filter(ProjectType %in% data_types$Project$ProjectType$ph),
                     by = "PersonalID") |>
    dplyr::rename_with(.cols = c(- PersonalID, - tidyselect::ends_with("_LH")), .fn = ~{paste0(.x,"_PH")}) |>
    dplyr::full_join(enrollments_to_use |>
                       dplyr::filter(!ProjectType %in% data_types$Project$ProjectType$lh &
                                       !ProjectType %in% data_types$Project$ProjectType$ph) |>
                       dplyr::rename_with(.cols = - PersonalID, .fn = ~{paste0(.x,"_O")}),
                     by = "PersonalID") |>
    dplyr::select(!dplyr::contains(c("ProjectType", "EntryDate")))


    veteran_active_list <- veteran_active_list_enrollments |>
      dplyr::select(
        PersonalID,
        UniqueID,
        DateVeteranIdentified,
        VAEligible,
        SSVFIneligible,
        PHTrack,
        ExpectedPHDate,
        County,
        HOMESID,
        ListStatus,
        EntryDate,
        AgeAtEntry,
        DisablingCondition,
        DateToStreetESSH,
        TimesHomelessPastThreeYears,
        MonthsHomelessPastThreeYears,
        ExitAdjust,
        ProjectType
      ) |>
      dplyr::group_by(PersonalID, County) |>
      dplyr::arrange(dplyr::desc(EntryDate)) |>
      dplyr::slice(1L) |>
      dplyr::ungroup() |>
      chronic_determination() |>
      long_term_homeless_determination() |>
      dplyr::mutate(
        ActiveDate = dplyr::case_when(
          is.na(DateVeteranIdentified) ~ EntryDate,
          DateVeteranIdentified < EntryDate ~ DateVeteranIdentified,
          TRUE ~ EntryDate
        )
      ) |>
      dplyr::select(
        -c(
          DateToStreetESSH,
          TimesHomelessPastThreeYears,
          MonthsHomelessPastThreeYears,
          ExitAdjust,
          ProjectType
        )
      ) |>
      dplyr::left_join(combined, by = "PersonalID") |>
      dplyr::left_join(dplyr::select(most_recent_offer, - UniqueID), by = "PersonalID") |>
      dplyr::left_join(small_CLS, by = "PersonalID") |>
      dplyr::left_join(hoh_chronicity, by = "PersonalID") |>
      dplyr::mutate(
        ChronicStatus = dplyr::if_else(
          !is.na(HoHChronicStatus) &
            HoHChronicStatus < ChronicStatus,
          HoHChronicStatus,
          ChronicStatus
        ),
        ActiveDateDisplay = paste0(
          ActiveDate,
          "<br>(",
          difftime(lubridate::today(), ActiveDate),
          " days)"
        ),
        DaysActive = difftime(lubridate::today(), ActiveDate),
        Eligibility =
          dplyr::if_else(
            is.na(VAEligible) & is.na(SSVFIneligible),
            "Unknown",
            paste(
              "VA Eligibility:",
              VAEligible,
              "<br><br>SSVF Eligibility:",
              SSVFIneligible
            )
          ),
        MostRecentOffer = dplyr::if_else(
          is.na(AcceptDeclineDate),
          "None",
          paste(
            "Offer of",
            PHTypeOffered,
            "on",
            OfferDate,
            "was",
            dplyr::if_else(OfferAccepted == "Yes", "accepted", "declined"),
            "on",
            AcceptDeclineDate
          )
        ),
        HousingPlan =
          dplyr::case_when(
            !is.na(PHTrack) & !is.na(ExpectedPHDate) ~ paste(
              PHTrack,
              "by",
              ExpectedPHDate),
            !is.na(PHTrack) & is.na(ExpectedPHDate) ~
              paste(
                PHTrack,
                "by",
                "unknown date"),
            is.na(PHTrack) & !is.na(ExpectedPHDate) ~
              "Expected housed by ExpectedPHDate",
              TRUE ~ "No Housing plan"),
        ListStatus = dplyr::case_when(
          !is.na(ProjectName_PH) & stringr::str_detect(ProjectName_PH, "VASH") ~ "Inactive (Permanently Housed)",
          stringr::str_detect(TimeInProject_LH, "Since") ~ "Active - ES/TH",

          is.na(ListStatus) ~ "No Status Set",
          TRUE ~ ListStatus
        )
      ) |>
      dplyr::left_join(responsible_providers, by = "County") |>
      unique() |>
      dplyr::mutate(PH = dplyr::if_else(!is.na(ProjectName_PH) & !is.na(TimeInProject_PH), paste0(
        "<span style='background-color:lavenderblush;'>",
        ProjectName_PH,
        ": ",
        TimeInProject_PH,
        "</span><br>"
      ), "", ""),
      LH = dplyr::if_else(!is.na(ProjectName_LH) & !is.na(TimeInProject_LH), paste0(
        "<span style='background-color:lightgoldenrodyellow;'>",
        ProjectName_LH,
        ": ",
        TimeInProject_LH,
        "</span><br>"
      ), "", ""),
      O = dplyr::if_else(!is.na(ProjectName_O) & !is.na(TimeInProject_O), paste0(
        "<span style='background-color:paleturquoise;'>",
        ProjectName_O,
        ": ",
        TimeInProject_O,
        "</span><br>"
      ), "", ""),
      Enrollments = paste0(PH, LH, O))

  # Currently Homeless Vets -------------------------------------------------

  # same as Active List except it only includes stayers and leaves out households
  # that have exited to a temporary destination. Not sure we'll need this actually
  # because we can just make it a widget on the report, to exclude those.


  # Veterans Missing Veteran Assessment -------------------------------------



  # Chronic ---------------------------------------------------------

  # thinking of moving the code I already wrote for this in the Active List
  # up to cohorts.R so I can get this easily from there instead of having to
  # copy that code to here

  # actually maybe not because the chronic code in the active_list.R looks at
  # an entire household's chronic status and then marks otherwise-non-chronic
  # clients as chronic if they're in a household, but this report only looks at
  # veterans. BUT maybe it shouldn't. Like it would make more sense to calculate
  # chronicity the same from one report to the other and take into account a
  # veteran's household's chronic status as well.

  # ON THE OTHER HAND, it's very specific to the way the Active List is written
  # because that script is untangling household data quality issues first and THEN
  # calculating it, but I'm not planning to untangle household dq issues in this
  # report. Maybe I should untangle household dq issues in cohorts too. AAaaa

  # I think it will be best to move the chronic code to cohorts, and the Returns
  # code can go there too.

  # Offers ------------------------------------------------------------------

  # checking to be sure I'm not using "Most Recent Offer ..." data anywhere
  # since I should be able to just use the subs in Rm/Rme and eliminate those
  # redundant data elements once this is all done.

  # Exited to PH ------------------------------------------------------------

  vets_permanently_housed <- vet_ees |>
    dplyr::filter(VeteranStatus == 1 &
                    Destination %in% c(destinations$perm) &
                    ExitDate >= lubridate::today() - lubridate::days(90)) |>
    dplyr::mutate(EntryAdj = dplyr::if_else(
      !is.na(DateVeteranIdentified) & DateVeteranIdentified < EntryDate,
      DateVeteranIdentified, EntryDate),
      time_to_house = difftime(lubridate::floor_date(ExitDate, unit = "day"),
                               lubridate::floor_date(EntryAdj, unit = "day"),
                               units = "days")) |>
    dplyr::select(
      PersonalID,
      EntryAdj,
      ExitDate,
      County,
      time_to_house
    ) |>
    unique()

  # Entered in Past 90 Days -------------------------------------------------

  vets_entered_past_90_days <- vet_ees |>
      {\(x) {
        dplyr::filter(x, (ProjectType %in% data_types$Project$ProjectType$lh |
                         (ProjectType %in% data_types$Project$ProjectType$ph &
                            is.na(MoveInDateAdjust))) &
                        (HMIS::entered_between(x, start = lubridate::today() - lubridate::days(90),
                                               end = lubridate::today(), lgl = TRUE) |
                           DateVeteranIdentified >= lubridate::today() - lubridate::days(90)))
      }}() |>
    dplyr::select(
      PersonalID, County
    ) |>
    unique() |>
    dplyr::mutate(housed_in_last_90 = dplyr::if_else(
      PersonalID %in% vets_permanently_housed$PersonalID, 1, 0
    ))

  # New GPD ----------------------------------------------------

  vets_new_gpd <- vet_ees |>
      HMIS::entered_between(lubridate::today() - lubridate::days(90),
                            end = Sys.Date()) |>
    dplyr::filter(VeteranStatus == 1 &
                    stringr::str_detect(ProjectName, "GPD")) |>
    dplyr::select(
      PersonalID, County
    ) |>
    unique()

    app_env$gather_deps(veteran_active_list)
}
