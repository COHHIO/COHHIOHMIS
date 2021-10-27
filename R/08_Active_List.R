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
Active_List <- function(
  co_clients_served,
  co_currently_homeless,
  covid19,
  Disabilities,
  Enrollment_extra_Exit_HH_CL_AaE,
  HealthAndDV,
  IncomeBenefits,
  Project,
  Scores,
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
) {
force(clarity_api)
if (is_app_env(app_env))
  app_env$set_parent(missing_fmls())
# clients currently entered into a homeless project in our system

co_currently_homeless <- co_clients_served %>%
  dplyr::filter((is.na(ExitDate) |
            ExitDate > lubridate::today()) &
           (ProjectType %in% c(4, lh_project_types) |
              (
                ProjectType %in% c(ph_project_types) &
                  is.na(MoveInDateAdjust)
              ))) %>%
  dplyr::select(
    PersonalID,
    ProjectName,
    ProjectType,
    HouseholdID,
    EnrollmentID,
    RelationshipToHoH,
    VeteranStatus,
    EntryDate,
    AgeAtEntry
  )

# Check Whether Each Client Has Income ---------------------------------

# getting income-related data and data collection stages. this will balloon
# out the number of rows per client, listing each yes/no update, then, using
# DateCreated, it picks out the most recent answer, keeping only that one

income_data <- co_currently_homeless %>%
  dplyr::left_join(
    IncomeBenefits %>%
      dplyr::select(
        PersonalID,
        EnrollmentID,
        IncomeFromAnySource,
        DateCreated,
        DataCollectionStage
      ),
    by = c("PersonalID", "EnrollmentID")
  ) %>%
  dplyr::mutate(DateCreated = lubridate::ymd_hms(DateCreated),
         IncomeFromAnySource = dplyr::if_else(is.na(IncomeFromAnySource),
                                       dplyr::if_else(AgeAtEntry >= 18 |
                                                 is.na(AgeAtEntry), 99, 0),
                                       IncomeFromAnySource)) %>%
  dplyr::group_by(PersonalID, EnrollmentID) %>%
  dplyr::arrange(dplyr::desc(DateCreated)) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::select(PersonalID,
         EnrollmentID,
         IncomeFromAnySource)

# Check Whether Each Client Has Any Indication of Disability ------------

# this checks the enrollment's 1.3 and 4.02 records to catch potential
# disabling conditions that may be used to determine PSH eligibility but
# were not reported in 3.08. If any of these three data elements (1.3,
# 4.02, 3.08) suggest the presence of a disabling condition, this section
# flags that enrollment as belonging to a disabled client. Otherwise,
# the enrollment is marked not disabled.

extended_disability <- co_currently_homeless %>%
  dplyr::left_join(Disabilities, by = c("EnrollmentID"))  %>%
  dplyr::group_by(EnrollmentID) %>%
  dplyr::mutate(D_Disability = dplyr::if_else(DisabilityResponse == 1 &
                                  IndefiniteAndImpairs != 0, 1, 0),
         D_Disability = max(D_Disability)) %>%
  dplyr::select(EnrollmentID, D_Disability) %>%
  dplyr::left_join(IncomeBenefits, by = c("EnrollmentID")) %>%
  dplyr::mutate(I_Disability = dplyr::if_else(SSDI == 1 |
                                  VADisabilityService == 1 |
                                  VADisabilityNonService == 1 |
                                  PrivateDisability == 1,
                                1, 0),
         I_Disability = max(I_Disability)) %>%
  dplyr::select(EnrollmentID, D_Disability, I_Disability) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::left_join(Enrollment, by = c("EnrollmentID")) %>%
  dplyr::mutate(any_disability = dplyr::case_when(D_Disability == 1 |
                                    I_Disability == 1 |
                                    DisablingCondition == 1 ~ 1,
                                    TRUE ~ 0)) %>%
  dplyr::select(EnrollmentID, any_disability)

# adding household aggregations into the full client list
co_currently_homeless <- co_currently_homeless %>%
  dplyr::left_join(
    income_data,
    by = c("PersonalID", "EnrollmentID")) %>%
  dplyr::left_join(extended_disability, by = "EnrollmentID") %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(EnrollmentID, PersonalID, HouseholdID, LivingSituation,
             DateToStreetESSH, TimesHomelessPastThreeYears, ExitAdjust,
             MonthsHomelessPastThreeYears, DisablingCondition),
    by = c("PersonalID",
           "EnrollmentID",
           "HouseholdID")
  ) %>%
  dplyr::mutate(SinglyChronic =
           dplyr::if_else(((lubridate::ymd(DateToStreetESSH) + lubridate::days(365) <= lubridate::ymd(EntryDate) &
                       !is.na(DateToStreetESSH)) |
                      (
                        MonthsHomelessPastThreeYears %in% c(112, 113) &
                          TimesHomelessPastThreeYears == 4 &
                          !is.na(MonthsHomelessPastThreeYears) &
                          !is.na(TimesHomelessPastThreeYears)
                      )
           ) &
             DisablingCondition == 1 &
             !is.na(DisablingCondition), 1, 0)) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::mutate(SinglyChronic = max(SinglyChronic)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::mutate(HouseholdSize = length(PersonalID),
         IncomeInHH = max(dplyr::if_else(IncomeFromAnySource == 1, 100, IncomeFromAnySource)),
         IncomeInHH = dplyr::if_else(IncomeInHH == 100, 1, IncomeInHH),
         DisabilityInHH = max(dplyr::if_else(any_disability == 1, 1, 0)),
         ChronicStatus = dplyr::if_else(max(SinglyChronic) == 1, "Chronic", "Not Chronic")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select("PersonalID", "ProjectName", "ProjectType", "HouseholdID", "EnrollmentID",
         "RelationshipToHoH", "VeteranStatus", "EntryDate", "AgeAtEntry",
         "DisablingCondition", "HouseholdSize", "IncomeInHH", "DisabilityInHH",
         "ChronicStatus")

# Account for Multiple EEs -------------------------------------------------

active_list <- co_currently_homeless %>%
  dplyr::group_by(PersonalID) %>%

  # label all program as either literally homeless or a housing program
  dplyr::mutate(PTCStatus = dplyr::case_when(
    ProjectType %in% c(lh_project_types, 4) ~ "LH",
    ProjectType %in% c(ph_project_types) ~ "PH"
  ),
  PTCStatus = factor(
    PTCStatus,
    levels = c(
      "LH", "PH"
    )),

  # label all clients as literally homeless or in a housing program
  client_status = dplyr::if_else(PTCStatus == "LH", 0, 1),
  client_status = max(client_status)
  ) %>%

  # if the client has at least one literally homeless entry, keep the most recent
  # otherwise, keep the most recent housing program entry
  dplyr::arrange(PTCStatus, dplyr::desc(EntryDate)) %>%
  dplyr::slice(1L) %>%

  # apply human-readable status labels
  dplyr::mutate(PTCStatus = dplyr::if_else(
    client_status == 1,
    "Has Entry into RRH or PSH",
    "Currently Has No Entry into RRH or PSH"
  )) %>%
  dplyr::ungroup() %>%
  dplyr::select(-client_status)

# correcting for bad hh data (while also flagging it) ---------------------

# what household ids exist in the data?
ALL_HHIDs <- active_list %>% dplyr::select(HouseholdID) %>% unique()

# marking who is a hoh (accounts for singles not marked as hohs in the data)
active_list <- active_list %>%
  dplyr::mutate(
    RelationshipToHoH = dplyr::if_else(is.na(RelationshipToHoH), 99, RelationshipToHoH),
    hoh = dplyr::if_else(stringr::str_detect(HouseholdID, stringr::fixed("s_")) |
                    RelationshipToHoH == 1, 1, 0))

# what household ids exist if we only count those with a hoh?
HHIDs_in_current_logic <- active_list %>%
  dplyr::filter(hoh == 1) %>%
  dplyr::select(HouseholdID) %>%
  unique()

# which hh ids did not have a hoh?
HHIDs_with_bad_dq <-
  dplyr::anti_join(ALL_HHIDs, HHIDs_in_current_logic,
            by = "HouseholdID")

# what household ids have multiple hohs?
mult_hohs <- active_list %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(hohs = sum(hoh)) %>%
  dplyr::filter(hohs > 1) %>%
  dplyr::select(HouseholdID)

# give me ALL household ids with some sort of problem
HHIDs_with_bad_dq <- rbind(HHIDs_with_bad_dq, mult_hohs)

# let's see those same household ids but with all the needed columns
HHIDs_with_bad_dq <-
  dplyr::left_join(HHIDs_with_bad_dq, active_list, by = "HouseholdID")

rm(ALL_HHIDs, HHIDs_in_current_logic, mult_hohs)

# assigning hoh status to the oldest person in the hh
Adjusted_HoHs <- HHIDs_with_bad_dq %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::arrange(dplyr::desc(AgeAtEntry)) %>% # picking oldest hh member
  dplyr::slice(1L) %>%
  dplyr::mutate(correctedhoh = 1) %>%
  dplyr::select(HouseholdID, PersonalID, EnrollmentID, correctedhoh) %>%
  dplyr::ungroup()

# merging the "corrected" hohs back into the main dataset with a flag, then
# correcting the RelationshipToHoH
hohs <- active_list %>%
  dplyr::left_join(Adjusted_HoHs,
            by = c("HouseholdID", "PersonalID", "EnrollmentID")) %>%
  dplyr::mutate(RelationshipToHoH = dplyr::if_else(correctedhoh == 1, 1, RelationshipToHoH)) %>%
  dplyr::select(PersonalID, HouseholdID, correctedhoh)


active_list <- active_list %>%
  dplyr::left_join(hohs, by = c("HouseholdID", "PersonalID")) %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::mutate(correctedhoh = dplyr::if_else(is.na(correctedhoh), 0, 1),
         HH_DQ_Issue = max(correctedhoh)) %>%
  dplyr::ungroup()

# COVID-19 ----------------------------------------------------------------

get_res_prior <- Enrollment %>%
  dplyr::select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::arrange(dplyr::desc(EntryDate)) %>%
  dplyr::slice(1L)

covid_clients <- covid19 %>%
  dplyr::mutate(
    C19AssessmentDate = lubridate::ymd(C19AssessmentDate),
    C19ContactWithConfirmedDate = lubridate::ymd(C19ContactWithConfirmedDate),
    C19ContactWithIllDate = lubridate::ymd(C19ContactWithIllDate),
    C19TestDate = lubridate::ymd(C19TestDate),
    C19InvestigationDate = lubridate::ymd(C19InvestigationDate)
  ) %>%
  dplyr::filter(lubridate::ymd(C19AssessmentDate) >= lubridate::ymd("20200401") &
           lubridate::ymd(C19AssessmentDate) <= lubridate::today()) %>%
  dplyr::left_join(get_res_prior, by = "PersonalID") %>%
  dplyr::mutate(LivingSituationDescr = living_situation(LivingSituation)) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    COVID19Priority = dplyr::case_when(
      (
        Tested == 1 &
          TestResults == "Positive" &
          lubridate::ymd(C19TestDate) > lubridate::today() - lubridate::days(14) &
          !is.na(C19TestDate)
      ) |
        # if tested positive in the past 14 days ^^
        (
          C19UnderInvestigation == 1 &
            lubridate::ymd(C19InvestigationDate) > lubridate::today() - lubridate::days(14)
        ) |
        (
          C19ContactWithConfirmed == 1 &
            (
              lubridate::ymd(C19ContactWithConfirmedDate) >
                lubridate::today() - lubridate::days(14) |
                is.na(C19ContactWithConfirmedDate)
              # contact with definite COVID-19 in the past 14 days ^^
            )
        ) |
        (
          C19ContactWithIll == 1 &
            (
              lubridate::ymd(C19ContactWithIllDate) >
                lubridate::today() - lubridate::days(14) |
                is.na(C19ContactWithIllDate)
            )
          # contact date with maybe COVID-19 was within the past 14 days ^^
        ) |
        (
          LivingSituation %in% c(7, 25) &
            EntryDate > lubridate::today() - lubridate::days(14) &
            EntryDate <= lubridate::today()
        ) |
        # if the client came from jail or nursing home ^^
        (
          Symptom1BreathingDifficult +
            Symptom1Cough +
            Symptom2Chills +
            Symptom2SoreThroat +
            Symptom2Fever +
            Symptom2Headache +
            Symptom2LostTasteSmell +
            Symptom2MusclePain +
            Symptom2Congestion +
            Symptom2Nausea +
            Symptom2Diarrhea +
            Symptom2Weak) > 0 ~ 1, # "Needs Isolation/Quarantine"
      # if the client has any symptoms at all ^^
      (
        HRHistoryOfRespiratoryIllness +
          HRChronicIllness +
          HROver65 +
          HRKidneyDisease +
          HRImmunocompromised +
          HRSmoke > 0
      )  ~ 2, # "Has Health Risk(s)",
      # if the client has any risks at all ^^
      TRUE ~ 4 # "No Known Risks or Exposure"
      # everyone else lands here ^
      # in the report, there will be another level: "Not Assessed Recently"
    )
  ) %>%
  dplyr::select(PersonalID, COVID19Priority)

covid_hhs <- active_list %>%
  dplyr::left_join(covid_clients, by = "PersonalID") %>%
  dplyr::mutate(
    COVID19Priority = dplyr::if_else(
      is.na(COVID19Priority),
      3, # "Not Assessed Recently"
      COVID19Priority
    )
  ) %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::mutate(COVID19Priority_hh = max(COVID19Priority)) %>%
  dplyr::ungroup() %>%
  dplyr::select(PersonalID, HouseholdID, COVID19Priority_hh) %>%
  dplyr::mutate(
    COVID19Priority = dplyr::case_when(
      COVID19Priority_hh == 1 ~ "Needs Isolation/Quarantine",
      COVID19Priority_hh == 2 ~ "Has Health Risk(s)",
      COVID19Priority_hh == 3 ~ "Not Assessed Recently",
      COVID19Priority_hh == 4 ~ "No Known Risks or Exposure"
    ),
    COVID19Priority = factor(
      COVID19Priority,
      levels = c(
        "Needs Isolation/Quarantine",
        "Has Health Risk(s)",
        "Not Assessed Recently",
        "No Known Risks or Exposure"
      )
    )
  ) %>%
  dplyr::select(-COVID19Priority_hh)

# adding COVID19Priority to active list
active_list <- active_list %>%
  dplyr::left_join(covid_hhs, by = c("PersonalID", "HouseholdID"))

# Adding in TAY, County, PHTrack ----------------------

# getting whatever data's needed from the Enrollment data frame, creating
# columns that tell us something about each household and some that are about
# each client
additional_data <- active_list %>%
  dplyr::left_join(
    Enrollment %>%
      dplyr::select(
        PersonalID,
        HouseholdID,
        CountyServed,
        PHTrack,
        ExpectedPHDate
      ),
    by = c("PersonalID", "HouseholdID")
  ) %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::mutate(
    CountyServed = dplyr::if_else(is.na(CountyServed), "MISSING County", CountyServed),
    TAY = dplyr::if_else(max(AgeAtEntry) < 25 & max(AgeAtEntry) >= 16, 1, 0),
    PHTrack = dplyr::if_else(
      !is.na(PHTrack) &
        !is.na(ExpectedPHDate) &
        lubridate::ymd(ExpectedPHDate) >= lubridate::today(), PHTrack, NULL)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-AgeAtEntry)

# saving these new columns back to the active list
active_list <- additional_data



# County Guessing ---------------------------------------------------------

# replacing non-Unsheltered-Provider missings with County of the provider
county <- active_list %>%
  dplyr::left_join(Project %>%
              dplyr::select(ProjectName, ProjectCounty), by = "ProjectName") %>%
  dplyr::mutate(
    CountyGuessed = dplyr::if_else(CountyServed == "MISSING County", 1, 0),
    CountyServed = dplyr::if_else(
      CountyServed == "MISSING County" &
        ProjectName != "Unsheltered Clients - OUTREACH",
      ProjectCounty,
      CountyServed
    ),
    ProjectCounty = NULL
  )

# replacing missings for the Unsheltered Provider with the County of the
# Default Provider of the person who entered the Enrollment (grrr!)
active_list <- county %>%
  dplyr::left_join(Enrollment %>%
              dplyr::select(EnrollmentID, UserCreating), by = "EnrollmentID") %>%
  dplyr::mutate(
    UserID = as.numeric(gsub(pattern = '[^0-9\\.]', '', UserCreating, perl = TRUE))
    ) %>%
  dplyr::left_join(Users %>%
              dplyr::select(UserID, UserCounty), by = "UserID") %>%
  dplyr::mutate(CountyServed = dplyr::if_else(CountyServed == "MISSING County" &
                                  ProjectName == "Unsheltered Clients - OUTREACH",
                                UserCounty,
                                CountyServed)) %>%
  dplyr::select(-dplyr::starts_with("User"))

# Add in Score ------------------------------------------------------------

# taking the most recent score on the client, but this score cannot be over a
# year old.
scores_staging <- Scores %>%
  dplyr::filter(ScoreDate > lubridate::today() - lubridate::years(1)) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::arrange(dplyr::desc(lubridate::ymd(ScoreDate))) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ScoreDate)

active_list <- active_list %>%
  dplyr::left_join(scores_staging, by = "PersonalID")

# Add Additional Chronic Statuses ---------------------------------------------

# adds current days in ES or SH projects to days homeless prior to entry and if
# it adds up to 365 or more, it marks the client as AgedIn
agedIntoChronicity <- active_list %>%
  dplyr::left_join(Enrollment %>%
              dplyr::select(EnrollmentID, PersonalID, HouseholdID, LivingSituation,
                     DateToStreetESSH, TimesHomelessPastThreeYears, ExitAdjust,
                     MonthsHomelessPastThreeYears),
            by = c("PersonalID",
                   "EnrollmentID",
                   "HouseholdID")) %>%
  dplyr::mutate(
    DaysHomelessInProject = difftime(lubridate::ymd(ExitAdjust),
                                     lubridate::ymd(EntryDate),
                                     units = "days"),
    DaysHomelessBeforeEntry = difftime(lubridate::ymd(EntryDate),
                                       dplyr::if_else(
                                         is.na(lubridate::ymd(DateToStreetESSH)),
                                         lubridate::ymd(EntryDate),
                                         lubridate::ymd(DateToStreetESSH)
                                       ),
                                       units = "days"),
    ChronicStatus = dplyr::if_else(
      ProjectType %in% c(1, 8) &
        ChronicStatus == "Not Chronic" &
        lubridate::ymd(DateToStreetESSH) + lubridate::days(365) > lubridate::ymd(EntryDate) &
        !is.na(DateToStreetESSH) &
        DaysHomelessBeforeEntry + DaysHomelessInProject >= 365,
      "Aged In",
      ChronicStatus
    )
  ) %>%
  dplyr::select(-DaysHomelessInProject,-DaysHomelessBeforeEntry)

# adds another ChronicStatus of "Nearly Chronic" which catches those hhs with
# almost enough times and months to qualify as Chronic
nearly_chronic <- agedIntoChronicity %>%
  dplyr::mutate(
    ChronicStatus = dplyr::if_else(
      ChronicStatus == "Not Chronic" &
        ((
          lubridate::ymd(DateToStreetESSH) + lubridate::days(365) <= lubridate::ymd(EntryDate) &
            !is.na(DateToStreetESSH)
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(110:113) &
              TimesHomelessPastThreeYears%in% c(3, 4) &
              !is.na(MonthsHomelessPastThreeYears) &
              !is.na(TimesHomelessPastThreeYears)
          )
        ) &
        DisablingCondition == 1 &
        !is.na(DisablingCondition),
      "Nearly Chronic",
      ChronicStatus
    )
  )

active_list <- active_list %>%
  dplyr::select(-ChronicStatus) %>%
  dplyr::left_join(
    nearly_chronic %>%
      dplyr::select("PersonalID",
             "HouseholdID",
             "EnrollmentID",
             "ChronicStatus"),
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  ) %>%
  dplyr::mutate(
    ChronicStatus = factor(
      ChronicStatus,
      levels = c(
        "Chronic",
        "Aged In",
        "Nearly Chronic",
        "Not Chronic"
      )
    )
  )

# THIS IS WHERE WE'RE SUMMARISING BY HOUSEHOLD (after all the group_bys)

active_list <- active_list %>%
  dplyr::mutate(
    HoH_Adjust = dplyr::case_when(HH_DQ_Issue == 1 ~ correctedhoh,
                           HH_DQ_Issue == 0 ~ hoh)
  ) %>%
  dplyr::filter(HoH_Adjust == 1) %>%
  dplyr::select(-correctedhoh, -RelationshipToHoH, -hoh, -HoH_Adjust)

# Add Referral Status -----------------------------------------------------

# thinking maybe it makes the most sense to only look at referrals that have
# been accepted for the purposes of the Active List. Because who cares if
# there's an open referral on a client who needs housing? That doesn't mean
# anything because we haven't really assigned a meaning to that. But an
# accepted referral does supposedly mean something, and it would add context
# to know that a household on this list has been accepted into (if not entered
# into) another project.

# also thinking the Refer-to provider should be an RRH or PSH? Maybe? Because
# referrals to a homeless project wouldn't mean anything on an Active List,
# right?

small_referrals <- Referrals %>%
  dplyr::left_join(Project %>%
              dplyr::select(ProjectName, "ReferToPTC" = ProjectType),
            by = c("ReferredToProjectID" = "ProjectID"))

# isolates hhs with an Accepted Referral into a PSH or RRH project
who_has_referrals <- active_list %>%
  dplyr::left_join(small_referrals %>%
              dplyr::filter(ReferralDate >= lubridate::today() - lubridate::days(14) &
                       ReferralOutcome == "Accepted" &
                       ReferToPTC %in% c(3, 9, 13)) %>%
              dplyr::group_by(PersonalID) %>%
              dplyr::arrange(dplyr::desc(lubridate::ymd(ReferralDate))) %>%
              dplyr::slice(1L) %>%
              dplyr::ungroup(),
            by = c("PersonalID")) %>%
  dplyr::select(PersonalID,
         HouseholdID,
         EnrollmentID,
         "ReferredToProvider" = "Referred-ToProvider",
         ReferralDate)

active_list <- active_list %>%
  dplyr::left_join(
    who_has_referrals,
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  )

# Add Program if Not Shown

# this looks up the program for clients that are currently enrolled in a housing program
# AND are also in a literally homeless program, IF the LH program is the one shown on the
# list. I'm thinking this is less repetitive--why show the program in the status column if
# we already have it somewhere else in the row? But it could go either way

who_has_entries <- active_list %>%
  dplyr::filter(PTCStatus == "Has Entry into RRH or PSH" &
           ProjectType %in% c(lh_project_types, 4)) %>%
  dplyr::select("PersonalID") %>%
  dplyr::left_join(co_currently_homeless %>%
              dplyr::filter(ProjectType %in% c(ph_project_types)),
            by = "PersonalID") %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::arrange(dplyr::desc(EntryDate)) %>%
  dplyr::slice(1L) %>%
  dplyr::select(PersonalID, "EntryProvider" = ProjectName)

active_list <- active_list %>%
  dplyr::left_join(
    who_has_entries,
    by = c("PersonalID")
  )

# Fleeing DV --------------------------------------------------------------

active_list <- active_list %>%
  dplyr::left_join(
    HealthAndDV %>%
      # get DV information only for those on the active list
      dplyr::inner_join(active_list %>%
                   dplyr::select(PersonalID),
                 by = "PersonalID")  %>%
      # get most recent DV information for those on the list
      dplyr::group_by(PersonalID) %>%
      dplyr::arrange(dplyr::desc(InformationDate)) %>%
      dplyr::slice(1L) %>%
      # pull variables we want
      dplyr::select(EnrollmentID,
             PersonalID,
             CurrentlyFleeing,
             WhenOccurred),
    by = c("EnrollmentID", "PersonalID")
  ) %>%
  dplyr::mutate(
    CurrentlyFleeing = dplyr::if_else(is.na(CurrentlyFleeing), 99, CurrentlyFleeing),
    WhenOccurred = dplyr::if_else(is.na(WhenOccurred), 99, WhenOccurred),
    CurrentlyFleeing = dplyr::case_when(
      CurrentlyFleeing %in% c(0, 99) &
        WhenOccurred %in% c(4, 8, 9, 99) ~ "No",
      CurrentlyFleeing == 1 |
        WhenOccurred %in% c(1:3) ~ "Yes",
      CurrentlyFleeing %in% c(8, 9) ~ "Unknown"
    )
  ) %>%
  dplyr::select(-WhenOccurred)

# Clean the House ---------------------------------------------------------

active_list <- active_list %>%
  dplyr::mutate(
    VeteranStatus = translate_HUD_yes_no(VeteranStatus),
    DisabilityInHH = translate_HUD_yes_no(DisabilityInHH),
    IncomeFromAnySource = translate_HUD_yes_no(IncomeInHH),
    TAY = dplyr::case_when(TAY == 1 ~ "Yes",
                    TAY == 0 ~ "No",
                    is.na(TAY) ~ "Unknown"),
    ProjectName = dplyr::if_else(
      ProjectName == "Unsheltered Clients - OUTREACH",
      paste("Unsheltered in",
            CountyServed,
            "County"),
      ProjectName
    ),
    PersonalID = as.character(PersonalID),
    Situation = dplyr::case_when(
      PTCStatus == "Has Entry into RRH or PSH" ~ dplyr::if_else(
        ProjectType %in% c(lh_project_types, 4),
        paste(
          "Has Entry into",
          EntryProvider
        ),
        PTCStatus
      ),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        !is.na(ReferredToProvider) ~
        paste(
          "No current Entry into RRH or PSH but",
          ReferredToProvider,
          "accepted this household's referral on",
          ReferralDate
        ),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        is.na(ReferredToProvider) &
        !is.na(PHTrack) ~ paste("Permanent Housing Track:",
                                PHTrack,
                                "by",
                                ExpectedPHDate),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        is.na(ReferredToProvider) &
        is.na(PHTrack) ~
        "No Entry or accepted Referral into PSH/RRH, and no current Permanent Housing Track"
    ),
    ShortSituation = factor(
      dplyr::case_when(
        stringr::str_starts(PTCStatus, "Has Entry") ~ "Enrolled in RRH/PSH",
        stringr::str_starts(Situation, "No current") |
        stringr::str_starts(Situation, "Permanent") ~ "Has Referral or Plan",
        stringr::str_starts(Situation, "No Entry") ~ "No Housing Plan"
      ),
      levels = c("No Housing Plan", "Has Referral or Plan", "Enrolled in RRH/PSH")
    )
  ) %>%
  dplyr::select(-IncomeInHH)

# landing_data <- active_list %>%
#   select(PersonalID, CountyServed, COVID19Priority, ShortSituation) %>%
#   # filter(CountyServed == "Lorain") %>%
#   # mutate(COVID19Priority = as.character(COVID19Priority),
#   #        ShortSituation = as.character(ShortSituation)) %>%
#   group_by(COVID19Priority, ShortSituation) %>%
#   summarise(HHs = n()) %>%
#   ungroup() %>%
#   as.data.frame()
#
# landing <- treemap(
#   landing_data,
#   title = "Currently Literally Homeless Households",
#   index = c("ShortSituation", "COVID19Priority"),
#   border.lwds = c(4, .5),
#   border.col = c("#FFFFFF", "#D2B48C"),
#   palette = "RdBu",
#   vSize = "HHs",
#   vColor = "COVID19Priority",
#   type = "categorical",
#   position.legend = "bottom",
#   fontsize.labels = c(17, 12),
#   fontcolor.labels = c("white", "black"),
#   fontface.labels = c(2, 1),
#   bg.labels = "transparent",
#   # position.legend = "none",
#   align.labels = list(c("center", "center"),
#                       c("left", "top"))
# )

# rowsum(plotly_attempt$HHs, group = plotly_attempt$COVID19Priority)

# plot_ly(
#   b,
#   parents = ~ COVID19Priority,
#   labels = ~ ShortSituation,
#   values = ~ HHs,
#   type = 'treemap'
# )
app_env$gather_deps()
app_env
}
