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

Veterans_Active <- function(
             clarity_api,
             app_env,
             e = rlang::caller_env()
            ) {
if (missing(clarity_api))
  clarity_api <- UU::find_by_class("clarity_api", e)
if (missing(app_env))
  app_env <- UU::find_by_class("app_env", e)


# Get all veterans and associated hh members ------------------------------

responsible_providers <- ServiceAreas %>%
  dplyr::select(County, SSVFServiceArea)

vet_ees <- co_clients_served %>%
  dplyr::filter(ProjectType %in% c(lh_at_entry_project_types)) %>%
  dplyr::mutate(VeteranStatus = dplyr::if_else(VeteranStatus == 1, 1, 0)) %>%
  dplyr::group_by(HouseholdID) %>% # pulling in all Veterans & non-veteran hh members
  dplyr::summarise(VetCount = sum(VeteranStatus)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(VetCount > 0) %>%
  dplyr::left_join(Enrollment, by = "HouseholdID") %>%
  dplyr::left_join(co_clients_served[c("PersonalID", "VeteranStatus")], by = "PersonalID") %>%
  dplyr::left_join(Project[c("ProjectID", "ProjectCounty")], by = "ProjectID") %>%
  dplyr::left_join(VeteranCE,
            by = c("PersonalID", "EnrollmentID", "ExpectedPHDate", "PHTrack")) %>%

  dplyr::mutate(
    County = dplyr::if_else(is.na(CountyServed), ProjectCounty, CountyServed)
  ) %>%
  dplyr::filter((County %in% c(bos_counties) |
            County == "Mahoning") &
           !ProjectID %in% c(1282)) %>% # i don't remember why i'm excluding this?
  dplyr::select(
    HouseholdID,
    EnrollmentID,
    PersonalID,
    HOMESID,
    ProjectID,
    ProjectType,
    ProjectName,
    ProjectCounty,
    DateVeteranIdentified,
    EntryDate,
    EntryAdjust,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust,
    RelationshipToHoH,
    LivingSituation,
    ListStatus,
    VAEligible,
    SSVFIneligible,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    AnnualPercentAMI,
    VAMCStation,
    UserCreating,
    County,
    PHTrack,
    ExpectedPHDate,
    Destination,
    OtherDestination,
    ClientLocation,
    AgeAtEntry,
    VeteranStatus
  )

# Currently in PSH/RRH ----------------------------------------------------

# RRH PSH stays with no Exit but a valid Move-In Date

currently_housed_in_psh_rrh <- vet_ees %>%
  dplyr::filter(HMIS::stayed_between(., start = format(lubridate::today(), "%m%d%Y"),
                        end = format(lubridate::today(), "%m%d%Y")) &
           ProjectType %in% c(ph_project_types) &
           VeteranStatus == 1) %>%
  dplyr::pull(PersonalID)

# Declined  ---------------------------------------------------------------

most_recent_offer <- Offers %>%
  dplyr::filter(!is.na(AcceptDeclineDate) &
           !is.na(OfferAccepted) &
           !is.na(PHTypeOffered)) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::slice_max(lubridate::ymd(OfferDate)) %>% # same date
  dplyr::slice_max(OfferAccepted) %>% # both rejected/accepted
  dplyr::slice(1) %>% # pick 1, doesn't matter if those ^ are the same
  dplyr::ungroup() %>%
  unique()

declined <- vet_ees %>%
  dplyr::left_join(most_recent_offer, by = "PersonalID") %>%
  dplyr::filter(OfferAccepted == "No" &
           lubridate::ymd(OfferDate) >= lubridate::today() - lubridate::days(14) &
           VeteranStatus == 1) %>%
  unique()

# Notes -------------------------------------------------------------------

small_CLS <- Contacts %>%
  dplyr::filter(RecordType == "CLS") %>%
  dplyr::mutate(Notes = stringr::str_remove_all(Notes, "<"),
         Notes = stringr::str_remove_all(Notes, ">")) %>% # in case there's html in the notes
  tidyr::unite("Notes", ContactDate, Notes, sep = ": ") %>%
  dplyr::select(PersonalID, Notes) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::arrange(dplyr::desc(Notes)) %>%
  dplyr::summarise(Notes = list(Notes)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Notes = as.character(Notes),
    Notes = dplyr::if_else(stringr::str_starts(Notes, "c"),
                    stringr::str_replace_all(Notes, "\", \"", "<br>"),
                    Notes),
    Notes = gsub("c\\(\"", "", Notes),
    Notes = gsub("\"\\)", "", Notes)
  )

# Entry Exits -------------------------------------------------------------

small_ees <- vet_ees %>%
  dplyr::filter(!PersonalID %in% c(currently_housed_in_psh_rrh) &
           VeteranStatus == 1 &
           (is.na(ExitDate) |
              (
                !Destination %in% c(perm_destinations) &
                  lubridate::ymd(ExitDate) >= lubridate::today() - lubridate::days(90)
              ))) %>%
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
  ) %>%
  unique() %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::arrange(dplyr::desc(EntryDate)) %>%
  dplyr::mutate(
    EntryDate = format.Date(EntryDate, "%m-%d-%Y"),
    MoveInDateAdjust = format.Date(MoveInDateAdjust, "%m-%d-%Y"),
    ExitDate = format.Date(ExitDate, "%m-%d-%Y"),
    Entries = paste(
      "Entered",
      ProjectName,
      "on",
      EntryDate,
      dplyr::case_when(
        is.na(MoveInDateAdjust) & is.na(ExitDate) ~  dplyr::if_else(
          ProjectType %in% c(lh_project_types), "to present", "awaiting housing"),
        !is.na(MoveInDateAdjust) & !is.na(ExitDate) ~
          paste(
            "Moved In on",
            MoveInDateAdjust,
            "and Exited on",
            ExitDate,
            "to",
            living_situation(Destination)
          ),
        !is.na(MoveInDateAdjust) & is.na(ExitDate) ~ # should never happen but eh
          paste("Moved In on",
                MoveInDateAdjust,
                "and is current"),
        is.na(MoveInDateAdjust) & !is.na(ExitDate) ~
          paste("Exited on", ExitDate,
                "to", living_situation(Destination))
      )
    )
  ) %>%
  dplyr::summarise(Entries = list(Entries)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Entries = as.character(Entries),
    Entries = dplyr::if_else(stringr::str_starts(Entries, "c"),
                    stringr::str_replace_all(Entries, "\", \"", "<br>"),
                    Entries),
    Entries = gsub("c\\(\"", "", Entries),
    Entries = gsub("\"\\)", "", Entries)
  )

# Active List -------------------------------------------------------------

# stayers & people who exited in the past 90 days to a temp destination

vet_active <- vet_ees %>%
  dplyr::filter(!PersonalID %in% c(currently_housed_in_psh_rrh) &
           (is.na(ExitDate) |
              (
                !Destination %in% c(perm_destinations) &
                  lubridate::ymd(ExitDate) >= lubridate::today() - lubridate::days(90)
              )))

hh_size <- vet_active %>%
  dplyr::select(HouseholdID, PersonalID) %>%
  unique() %>%
  dplyr::count(HouseholdID)

veteran_active_list_enrollments <- vet_active %>%
  dplyr::filter(VeteranStatus == 1) %>%
  dplyr::left_join(hh_size, by = "HouseholdID") %>%
  dplyr::rename("HouseholdSize" = n) %>%
  dplyr::mutate(EnrollType = dplyr::case_when(
    ProjectType %in% lh_project_types ~ 1,
    ProjectType %in% ph_project_types ~ 2,
    TRUE ~ 3
  )) %>%
  dplyr::group_by(PersonalID, EnrollType) %>%
  dplyr::arrange(dplyr::desc(EntryDate)) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup()

non_hoh_vets <- veteran_active_list_enrollments %>%
  dplyr::filter(RelationshipToHoH != 1) %>%
  dplyr::select(PersonalID, HouseholdID, RelationshipToHoH)

hoh_chronicity <- non_hoh_vets %>%
  stats::setNames(paste0("V_", names(.))) %>%
  dplyr::inner_join(vet_ees %>%
               dplyr::filter(RelationshipToHoH == 1 &
                        HouseholdID %in% non_hoh_vets$HouseholdID) %>%
               dplyr::distinct() %>%
               chronic_determination() %>%
               dplyr::rename(HoHChronicStatus = ChronicStatus),
             by = c("V_HouseholdID" = "HouseholdID")) %>%
  dplyr::select(V_PersonalID, HoHChronicStatus) %>%
  dplyr::rename(PersonalID = V_PersonalID) %>%
  dplyr::arrange(HoHChronicStatus) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup()

enrollments_to_use <- veteran_active_list_enrollments %>%
  dplyr::mutate(ProjectName = dplyr::if_else(ProjectName == "Unsheltered Clients - OUTREACH",
                               paste("Unsheltered in", County, "County"),
                               ProjectName),
    TimeInProject = dplyr::if_else(
           is.na(ExitDate),
           paste("Since", format(lubridate::ymd(EntryDate), "%m-%d-%Y")),
           paste(
             format(lubridate::ymd(EntryDate), "%m-%d-%Y"),
             "to",
             format(lubridate::ymd(ExitDate), "%m-%d-%Y")
           )
         )) %>%
  dplyr::select(PersonalID, ProjectName, TimeInProject, ProjectType)

combined <- enrollments_to_use %>%
  dplyr::filter(ProjectType %in% lh_project_types) %>%
  stats::setNames(c("PersonalID", paste0("LH_", names(.)[2:ncol(.)]))) %>%
  dplyr::full_join(enrollments_to_use %>%
              dplyr::filter(ProjectType %in% ph_project_types) %>%
              stats::setNames(c("PersonalID", paste0("PH_", names(.)[2:ncol(.)]))),
            by = "PersonalID") %>%
  dplyr::full_join(enrollments_to_use %>%
              dplyr::filter(!ProjectType %in% lh_project_types &
                       !ProjectType %in% ph_project_types) %>%
              stats::setNames(c("PersonalID", paste0("O_", names(.)[2:ncol(.)]))),
            by = "PersonalID") %>%
  dplyr::select(!dplyr::contains("ProjectType"))

veteran_active_list <- veteran_active_list_enrollments %>%
  dplyr::select(PersonalID, DateVeteranIdentified, VAEligible,
         SSVFIneligible, PHTrack, ExpectedPHDate,
         County, HOMESID, ListStatus, EntryDate,
         AgeAtEntry, DisablingCondition,
         DateToStreetESSH, TimesHomelessPastThreeYears,
         MonthsHomelessPastThreeYears, ExitAdjust, ProjectType) %>%
  dplyr::group_by(PersonalID, County) %>%
  dplyr::arrange(dplyr::desc(EntryDate)) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  chronic_determination() %>%
  dplyr::mutate(ActiveDate = dplyr::case_when(
           is.na(DateVeteranIdentified) ~ EntryDate,
           lubridate::ymd(DateVeteranIdentified) < lubridate::ymd(EntryDate) ~ DateVeteranIdentified,
           TRUE ~ EntryDate
    )) %>%
  dplyr::select(-c(DateToStreetESSH, TimesHomelessPastThreeYears,
            MonthsHomelessPastThreeYears, ExitAdjust, ProjectType)) %>%
  dplyr::left_join(combined, by = "PersonalID") %>%
  dplyr::left_join(most_recent_offer, by = "PersonalID") %>%
  dplyr::left_join(small_CLS, by = "PersonalID") %>%
  dplyr::left_join(hoh_chronicity, by = "PersonalID") %>%
  dplyr::mutate(
    ChronicStatus = dplyr::if_else(!is.na(HoHChronicStatus) &
                            HoHChronicStatus < ChronicStatus,
                          HoHChronicStatus, ChronicStatus),
    ActiveDateDisplay = paste0(ActiveDate,
                               "<br>(",
                               difftime(lubridate::today(), lubridate::ymd(ActiveDate)),
                               " days)"),
    DaysActive = difftime(lubridate::today(), lubridate::ymd(ActiveDate)),
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
    ActiveDate = format(ActiveDate, "%m-%d-%Y"),
    MostRecentOffer = dplyr::if_else(
      is.na(AcceptDeclineDate),
      "None",
      paste(
        "Offer of",
        PHTypeOffered,
        "on",
        format(OfferDate, "%m-%d-%Y"),
        "was",
        dplyr::if_else(OfferAccepted == "Yes", "accepted", "declined"),
        "on",
        format(AcceptDeclineDate, "%m-%d-%Y")
      )
    ),
    HousingPlan =
      dplyr::if_else(
        is.na(PHTrack) & is.na(ExpectedPHDate),
        paste("No Housing Track<br><br>Notes:",
              Notes),
        paste(PHTrack,
        "by",
        dplyr::if_else(
          is.na(ExpectedPHDate),
          "unknown date",
          format(ExpectedPHDate, "%m-%d-%Y")
        ),
      "<br><br>Notes:<br>",
      Notes)
      ),
    ListStatus = dplyr::case_when(
      stringr::str_detect(LH_TimeInProject, "Since") ~ "Active - ES/TH",
      ListStatus == "Inactive (Uknown/Missing)" ~ "Inactive (Unknown/Missing)",
      TRUE ~ ListStatus
    )
  ) %>%
  dplyr::left_join(responsible_providers, by = "County") %>%
  unique()

# Currently Homeless Vets -------------------------------------------------

# same as Active List except it only includes stayers and leaves out households
# that have exited to a temporary destination. Not sure we'll need this actually
# because we can just make it a widget on the report, to exclude those.

# Entered in Past 90 Days -------------------------------------------------

entered_past_90 <- vet_ees %>%
  dplyr::filter(HMIS::entered_between(., format(lubridate::today() - lubridate::days(90), "%m%d%Y"),
                         format(lubridate::today(), "%m%d%Y")))


# Veterans Missing Veteran Assessment -------------------------------------



# Long Term -------------------------------------------------------

# thinking of moving the code I already wrote for this in the Active List
# up to cohorts.R so I can get this easily from there instead of having to
# copy that code to here

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

# New GPD -----------------------------------------------------------------

new_gpd <- entered_past_90 %>%
  dplyr::filter(ProjectID %in% c(GPD_project_ids))

# Offers ------------------------------------------------------------------

# checking to be sure I'm not using "Most Recent Offer ..." data anywhere
# since I should be able to just use the subs in Rm/Rme and eliminate those
# redundant data elements once this is all done.

# Exited to PH ------------------------------------------------------------


# New and Exited to PH ----------------------------------------------------


# Save it out -------------------------------------------------------------

# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/Vet_Active_List.RData", compress = FALSE)
app_env$gather_deps()
app_env
}

