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

QPR_EEs <- function(
             clarity_api,
             app_env,
             e = rlang::caller_env()
            ) {
if (missing(clarity_api))
  clarity_api <- get_clarity_api(e = e)
if (missing(app_env))
  app_env <- get_app_env(e = e)

# decided to continue to use a separate file for Goals (instead of building it
# in a tribble) because this way the CoC team can review it more easily.
goals <- readr::read_csv("public_data/BoSGoals.csv", col_types = "cccdddddddd")

goals <- goals %>%
  tidyr::gather(key = "ProjectType",
         value = "Goal",
         -SummaryMeasure, -Measure, -Operator) %>%
  dplyr::mutate(ProjectType = as.numeric(ProjectType)) %>%
  dplyr::filter(!is.na(Goal))

# Building qpr_leavers ----------------------------------------------------

smallProject <- Project %>%
  dplyr::select(ProjectID,
         OrganizationName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectName,
         ProjectType,
         HMISParticipatingProject,
         GrantType,
         ProjectCounty,
         ProjectRegion) %>%
  dplyr::filter(HMISParticipatingProject == 1 &
           HMIS::operating_between(., calc$data_goes_back_to, meta_HUDCSV$Export_End) &
           !is.na(ProjectRegion) &
           ProjectType %in% c(1:4, 8:9, 12:14)) %>%
  dplyr::mutate(
    FriendlyProjectName = ProjectName)

smallEnrollment <- Enrollment %>%
  dplyr::select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    CountyServed,
    EntryDate,
    MoveInDate,
    ExitDate,
    EntryAdjust,
    MoveInDateAdjust,
    ExitAdjust,
    LivingSituation,
    Destination,
    DateCreated
  )

validation <- smallProject %>%
  dplyr::left_join(smallEnrollment, by = "ProjectID") %>%
  dplyr::select(
    ProjectID,
    ProjectName,
    ProjectType,
    CountyServed,
    EnrollmentID,
    PersonalID,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    EntryAdjust,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    LivingSituation,
    Destination,
    DateCreated
  ) %>%
  dplyr::filter(!is.na(EntryDate))

smallEnrollment <- smallEnrollment %>%
  dplyr::filter(stringr::str_detect(HouseholdID, stringr::fixed("s_")) |
           (stringr::str_detect(HouseholdID, stringr::fixed("h_")) &
              RelationshipToHoH == 1)) #<- only pulls in hohs and singles

# captures all leavers PLUS stayers in either HP or PSH because we include those
# stayers in Permanent Destinations. This is used for LoS and Exits to PH.

qpr_leavers <- smallProject %>%
  dplyr::left_join(smallEnrollment, by = "ProjectID") %>%
  dplyr::filter((!is.na(ExitDate) | ProjectType %in% c(3, 9, 12)) &
           HMIS::served_between(., calc$data_goes_back_to, meta_HUDCSV$Export_End) &
           RelationshipToHoH == 1) %>%
  dplyr::mutate(
    DestinationGroup = dplyr::case_when(
      Destination %in% c(temp_destinations) ~ "Temporary",
      Destination %in% c(perm_destinations) ~ "Permanent",
      Destination %in% c(institutional_destinations) ~ "Institutional",
      Destination %in% c(other_destinations) ~ "Other",
      is.na(Destination) ~ "Still in Program"
    ),
    DaysinProject = difftime(ExitAdjust, EntryDate, units = "days")
  ) %>%
  dplyr::filter(HMIS::stayed_between(., calc$data_goes_back_to, meta_HUDCSV$Export_End)) %>%
  dplyr::arrange(ProjectName)

qpr_rrh_enterers <- smallProject %>%
  dplyr::left_join(smallEnrollment, by = "ProjectID") %>%
  dplyr::filter(ProjectType == 13 &
           HMIS::entered_between(., calc$data_goes_back_to, meta_HUDCSV$Export_End) &
           RelationshipToHoH == 1) %>%
  dplyr::mutate(
    DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days"),
    DaysinProject = difftime(ExitAdjust, EntryAdjust, units = "days")
  )

smallMainstreamBenefits <- IncomeBenefits %>%
  dplyr::select(InsuranceFromAnySource, BenefitsFromAnySource,
         DataCollectionStage, EnrollmentID, InformationDate) %>%
  dplyr::group_by(EnrollmentID) %>%
  dplyr::slice(which.max(InformationDate)) %>% # most recent answer per Enrollment
  dplyr::ungroup()


qpr_benefits <- smallProject %>%
  dplyr::left_join(smallEnrollment, by = "ProjectID") %>%
  dplyr::filter(HMIS::exited_between(., calc$data_goes_back_to, meta_HUDCSV$Export_End) &
           RelationshipToHoH == 1) %>%
  dplyr::left_join(smallMainstreamBenefits, by = "EnrollmentID") %>%
  dplyr::select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         InsuranceFromAnySource, BenefitsFromAnySource, DataCollectionStage,
         InformationDate, ProjectRegion, ProjectCounty, ProjectType) %>%
  dplyr::mutate(ProjectType = dplyr::case_when(
    ProjectType == 1 ~ "Emergency Shelters",
    ProjectType == 2 ~ "Transitional Housing",
    ProjectType == 3 ~ "Permanent Supportive Housing",
    ProjectType == 4 ~ "Street Outreach",
    ProjectType == 8 ~ "Safe Haven",
    ProjectType == 9 ~ "Permanent Supportive Housing",
    ProjectType == 12 ~ "Prevention",
    ProjectType == 13 ~ "Rapid Rehousing"
  )) %>%
  dplyr::arrange(ProjectName, HouseholdID)

incomeMostRecent <- IncomeBenefits %>%
  dplyr::select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage,
         EnrollmentID, InformationDate) %>%
  dplyr::group_by(EnrollmentID) %>%
  dplyr::slice(which.max(InformationDate)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(RecentIncome = TotalMonthlyIncome) %>%
  dplyr::select(EnrollmentID, RecentIncome)

incomeAtEntry <- IncomeBenefits %>%
  dplyr::select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage,
         EnrollmentID, InformationDate) %>%
  dplyr::group_by(EnrollmentID) %>%
  dplyr::slice(which.min(InformationDate)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(EntryIncome = TotalMonthlyIncome) %>%
  dplyr::select(EnrollmentID, EntryIncome)

smallIncomeDiff <-
  dplyr::full_join(incomeAtEntry, incomeMostRecent, by = "EnrollmentID")

qpr_income <- smallProject %>%
  dplyr::left_join(smallEnrollment, by = "ProjectID") %>%
  dplyr::filter(HMIS::served_between(., calc$data_goes_back_to, meta_HUDCSV$Export_End) &
           RelationshipToHoH == 1) %>%
  dplyr::left_join(smallIncomeDiff, by = "EnrollmentID") %>%
  dplyr::select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         EntryIncome, RecentIncome, ProjectRegion, ProjectCounty, ProjectType) %>%
  dplyr::mutate(
    Difference = RecentIncome - EntryIncome,
    ProjectType = dplyr::case_when(
      ProjectType == 1 ~ "Emergency Shelters",
      ProjectType == 2 ~ "Transitional Housing",
      ProjectType == 3 ~ "Permanent Supportive Housing",
      ProjectType == 4 ~ "Street Outreach",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 9 ~ "Permanent Supportive Housing",
      ProjectType == 12 ~ "Prevention",
      ProjectType == 13 ~ "Rapid Rehousing"
    )
  ) %>%
  dplyr::arrange(ProjectName, HouseholdID)

qpr_spending <- Services %>%
  dplyr::left_join(Enrollment,
            by = c("EnrollmentID", "PersonalID")) %>%
  dplyr::left_join(smallProject, by = c("ProjectID", "ProjectType", "ProjectName")) %>%
  dplyr::select(
    PersonalID,
    OrganizationName,
    ProjectName,
    ProjectRegion,
    ProjectType,
    Amount,
    Description,
    RelationshipToHoH,
    ServiceStartDate,
    EntryDate,
    MoveInDateAdjust,
    ExitDate
  ) %>%
  dplyr::filter(ProjectType %in% c(13, 12) &
           RelationshipToHoH == 1 &
           !is.na(Amount)) %>%
  dplyr::select(-RelationshipToHoH)

rm(smallEnrollment,
   smallProject,
   smallMainstreamBenefits,
   incomeMostRecent,
   incomeAtEntry,
   smallIncomeDiff)



# COVID-19 plots for Rm ---------------------------------------------------

get_res_prior <- validation %>%
  dplyr::select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::arrange(dplyr::desc(EntryDate)) %>%
  dplyr::slice(1L)

covid19_plot <- covid19 %>%
  dplyr::left_join(get_res_prior, by = "PersonalID") %>%
  dplyr::filter(C19AssessmentDate >= lubridate::mdy("04012020") &
           C19AssessmentDate <= lubridate::today())

priority <- covid19_plot %>%
  dplyr::mutate(
    Priority = dplyr::case_when(
      # if tested positive
      (
        C19Tested == 1 &
          C19TestResults == "Positive" &
          C19TestDate > C19AssessmentDate - lubridate::days(14) &
          !is.na(C19TestDate)
      ) |
        # if under investigation
        (
          C19UnderInvestigation == 1 &
            C19InvestigationDate > C19AssessmentDate - lubridate::days(14)
        ) |
        # contact with COVID-19
        (
          C19ContactWithConfirmed == 1 &
            (
              C19ContactWithConfirmedDate >
                C19AssessmentDate - lubridate::days(14) |
                is.na(C19ContactWithConfirmedDate)
            )
          # compares contact date to the assessment date too since we want to
          # see severity at the time of assessment
        ) |
        (
          C19ContactWithIll == 1 &
            (
              C19ContactWithIllDate >
                C19AssessmentDate - lubridate::days(14) |
                is.na(C19ContactWithIllDate)
            )
        ) |
        # if the client came from jail or nursing home
        (
          LivingSituation %in% c(7, 25) &
            EntryDate > C19AssessmentDate - lubridate::days(14) &
            EntryDate <= C19AssessmentDate
        ) |
        # if the client has any symptoms at all
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
            Symptom2Weak
        ) > 0 ~ "Needs Isolation/Quarantine",
      # if the client has any risks at all
      (
        HRHistoryOfRespiratoryIllness +
          HRChronicIllness +
          HROver65 +
          HRKidneyDisease +
          HRImmunocompromised +
          HRSmoke > 0
      )  ~ "Has Health Risk(s)",
      TRUE ~ "No Known Risks or Exposure"
      # everyone else lands here ^
      # in the report, there will be a third level: "Not Assessed Recently"
    ),
    Priority = factor(Priority, levels = c("Needs Isolation/Quarantine",
                                           "Has Health Risk(s)",
                                           "No Known Risks or Exposure")),
    Month = paste0(lubridate::year(C19AssessmentDate),
                   stringr::str_pad(
                     lubridate::month(C19AssessmentDate),
                     width = 2,
                     pad = "0"
                   )),
    Month = as.numeric(factor(Month)),
    MonthOf = format.Date(C19AssessmentDate, "%b %Y")
  ) %>%
  dplyr::filter(lubridate::month(C19AssessmentDate) != lubridate::month(lubridate::today()))

priority_plot <- priority %>%
  dplyr::select(PersonalID, MonthOf, Month, Priority) %>%
  dplyr::group_by(MonthOf, Month, Priority) %>%
  dplyr::summarise(Clients = dplyr::n()) %>%
  dplyr::arrange(Month)

covid19_priority_plot <- priority_plot %>%
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(MonthOf, Month), y = Clients,
             fill = Priority, label = Clients)) +
  ggplot2::scale_fill_brewer(palette = "GnBu", direction = -1) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Month of", y = "Clients Assessed") +
  ggplot2::theme(legend.title=ggplot2::element_blank(),
        legend.position = "top",
        legend.text = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 11))

rm(priority, priority_plot)

# COVID Status plot -------------------------------------------------------

covid19_status <- covid19_plot %>%
  dplyr::mutate(
    COVID19Status = dplyr::case_when(
      Tested == 1 &
        TestResults == "Positive" &
        C19TestDate > C19AssessmentDate - lubridate::days(14) &
        !is.na(C19TestDate) ~ "Positive",
      # testing positive in the 14 days prior to assessment is the only way to
      # land in this bucket
      (
        C19ContactWithConfirmed == 1 &
          (
            C19ContactWithConfirmedDate >
              C19AssessmentDate - lubridate::days(14) |
              is.na(C19ContactWithConfirmedDate)
          )
        # compares contact date to date of the assessment
      ) |
        (
          C19ContactWithIll == 1 &
            (
              C19ContactWithIllDate >
                C19AssessmentDate - lubridate::days(14) |
                is.na(C19ContactWithIllDate)
            )
        ) |
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
            Symptom2Weak
        ) > 0
      |
        (
          C19UnderInvestigation == 1 &
            C19InvestigationDate > C19AssessmentDate - lubridate::days(14)
        ) ~
        "May Have COVID-19",
      # being Under Investigation (past 14 days), any Symptom, or any Contact
      # in the 14 days prior to the assessment date will land you here ^
      TRUE ~ "No Current Indications"
      # everyone else lands here ^
    ),
    COVID19Status = factor(
      COVID19Status,
      levels = c("No Current Indications",
                 "May Have COVID-19",
                 "Positive")
    ),
    Month = paste0(lubridate::year(C19AssessmentDate),
                   stringr::str_pad(
                     lubridate::month(C19AssessmentDate),
                     width = 2,
                     pad = "0"
                   )),
    Month = as.numeric(factor(Month)),
    MonthOf = format.Date(C19AssessmentDate, "%b %Y")
  ) %>%
  dplyr::filter(lubridate::month(C19AssessmentDate) != lubridate::month(lubridate::today()))

covid19_status_plot <- covid19_status %>%
  dplyr::select(PersonalID, MonthOf, Month, COVID19Status) %>%
  dplyr::group_by(MonthOf, Month, COVID19Status) %>%
  dplyr::summarise(Clients = dplyr::n()) %>%
  dplyr::arrange(Month) %>%
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(MonthOf, Month), y = Clients,
             fill = COVID19Status)) +
  ggplot2::geom_bar(stat = "identity",
           position = ggplot2::position_stack(reverse = TRUE)) +
  ggplot2::scale_fill_manual(values = c("#e0ecf4", "#9ebcda", "#8856a7")) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Month of", y = "Clients Assessed") +
  ggplot2::theme(legend.title=ggplot2::element_blank(),
        legend.position = "top",
        legend.text = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 11))

rm(covid19_status, covid19_plot)

# Save it out -------------------------------------------------------------
# WARNING save.image does not save the environment properly, save must be used.
app_env$gather_deps()
app_env

}
