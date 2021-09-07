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

dependencies$DataQuality <-
  c(
    "calc$data_goes_back_to",
    "Client",
    "Contacts",
    "covid19",
    "Disabilities",
    "dose_counts",
    "doses",
    "Enrollment",
    "Funder",
    "guidance",
    "hc",
    "HealthAndDV",
    "IncomeBenefits",
    "Inventory",
    "living_situation",
    "mahoning_projects",
    "meta_HUDCSV$Export_End",
    "Project",
    "Referrals",
    "Scores",
    "Services",
    "Users"
  )



DataQuality <- function(
    clarity_api,
    app_env,
    e = rlang::caller_env()
  ) {
  if (missing(clarity_api))
    clarity_api <- get_clarity_api(e = e)
  if (missing(app_env))
    app_env <- get_app_env(e = e)
  app_env$merge_deps_to_env()



  va_funded <- Funder |>
    Funder_VA_ProjectID()

  # Providers to Check ------------------------------------------------------
  projects_current_hmis <- projects_current_hmis(Project, Inventory)


  # Clients to Check --------------------------------------------------------

  served_in_date_range <- served_in_date_range(projects_current_hmis, app_env = Rm_env)

  # The Variables That We Want ----------------------------------------------

  vars <- list()
  vars$prep <- c(
    "HouseholdID",
    "PersonalID",
    "ProjectName",
    "ProjectType",
    "EntryDate",
    "MoveInDateAdjust",
    "ExitDate",
    "UserCreating",
    "ProjectRegion"
  )

  vars$we_want <- c(vars$prep,
                            "Issue",
                            "Type",
                            "Guidance")

  # Missing UDEs ------------------------------------------------------------

  dq_name <- served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        FirstName == "Missing" ~
          "Missing Name Data Quality",
        FirstName %in% c("DKR", "Partial") ~
          "Incomplete or Don't Know/Refused Name"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Name Data Quality" ~ "Error",
        Issue == "Incomplete or Don't Know/Refused Name" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_pii)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # TODO Check to ensure missing DOB are not present in imported.
  dq_dob <- served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(DOB) & DOBDataQuality %in% c(1, 2) ~ "Missing DOB",
        DOBDataQuality == 99 ~ "Missing Date of Birth Data Quality",
        DOBDataQuality %in% c(2, 8, 9) ~ "Don't Know/Refused or Approx. Date of Birth",
        AgeAtEntry < 0 |
          AgeAtEntry > 95 ~ "Incorrect Date of Birth or Entry Date"
      ),
      Type = dplyr::case_when(
        Issue %in% c(
          "Missing DOB",
          "Incorrect Date of Birth or Entry Date",
          "Missing Date of Birth Data Quality"
        ) ~ "Error",
        Issue ==  "Don't Know/Refused or Approx. Date of Birth" ~ "Warning"
      ),
      Guidance = dplyr::case_when(
        Issue == "Incorrect Date of Birth or Entry Date" ~
          "The HMIS data is indicating the client entered the project PRIOR to
      being born. Correct either the Date of Birth or the Entry Date, whichever
      is incorrect.",
      Issue %in% c("Missing DOB", "Missing Date of Birth Data Quality") ~
        guidance$missing_at_entry,
      Issue == "Don't Know/Refused or Approx. Date of Birth" ~
        guidance$dkr_data
      )
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dq_ssn <- served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        SSN == "Missing" ~ "Missing SSN",
        SSN == "Invalid" ~ "Invalid SSN",
        SSN == "DKR" ~ "Don't Know/Refused SSN",
        SSN == "Incomplete" ~ "Invalid SSN"
      ),
      Type = dplyr::case_when(
        Issue %in% c("Missing SSN", "Invalid SSN") ~ "Error",
        Issue == "Don't Know/Refused SSN" ~ "Warning"
      ),
      Guidance = dplyr::case_when(
        Issue == "Don't Know/Refused SSN" ~ guidance$dkr_data,
        Issue == "Missing SSN" ~ guidance$missing_pii,
        Issue == "Invalid SSN" ~ "The Social Security Number does not conform with
      standards set by the Social Security Administration. This includes rules
      like every SSN is exactly 9 digits and cannot have certain number patterns.
      Correct by navigating to the client's record, then clicking the Client
      Profile tab, then click into the Client Record pencil to correct the data."
      )
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dq_race <- served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        RaceNone == 99 ~ "Missing Race",
        RaceNone %in% c(8, 9) ~ "Don't Know/Refused Race"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Race" ~ "Error",
        Issue == "Don't Know/Refused Race" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_at_entry)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dq_ethnicity <- served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        Ethnicity == 99 ~ "Missing Ethnicity",
        Ethnicity %in% c(8, 9) ~ "Don't Know/Refused Ethnicity"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Ethnicity" ~ "Error",
        Issue == "Don't Know/Refused Ethnicity" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_at_entry)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  #TODO Change for FY 2022
  dq_gender <- served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        Gender == 99 ~ "Missing Gender",
        Gender %in% c(8, 9) ~ "Don't Know/Refused Gender"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Gender" ~ "Error",
        Issue == "Don't Know/Refused Gender" ~ "Warning"
      ),
      Guidance = dplyr::if_else(Type == "Warning",
                                guidance$dkr_data,
                                guidance$missing_at_entry)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dq_veteran <- served_in_date_range %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          VeteranStatus == 99 ~ "Missing Veteran Status",
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          VeteranStatus %in% c(8, 9) ~ "Don't Know/Refused Veteran Status",
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          RelationshipToHoH == 1 &
          VeteranStatus == 0 &
          Destination %in% c(19, 28) ~ "Check Veteran Status for Accuracy"
      ),
      Type = dplyr::case_when(
        Issue == "Missing Veteran Status" ~ "Error",
        Issue %in% c(
          "Don't Know/Refused Veteran Status",
          "Check Veteran Status for Accuracy"
        ) ~ "Warning"
      ),
      Guidance = dplyr::case_when(
        Issue == "Check Veteran Status for Accuracy" ~ "You have indicated the
      household exited to a destination that only veterans are eligible for, but
      the head of household appears to be not a veteran. Either the Veteran
      Status is incorrect or the Destination is incorrect.",
      Issue == "Missing Veteran Status" ~ guidance$missing_pii,
      Issue == "Don't Know/Refused Veteran Status" ~ guidance$dkr_data)
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Missing Vaccine data ----------------------------------------------------
  #TODO C19 Column names need to be updated
  dose_counts <- doses %>%
    dplyr::count(PersonalID) %>%
    dplyr::select(PersonalID, "Doses" = n)

  missing_vaccine_exited <- served_in_date_range %>%
    dplyr::filter(HMIS::served_between(., hc$bos_start_vaccine_data, lubridate::today())) %>%
    dplyr::left_join(covid19[c("PersonalID", "C19ConsentToVaccine", "C19VaccineConcerns")],
                     by = "PersonalID") %>%
    dplyr::left_join(dose_counts, by = "PersonalID") %>%
    dplyr::filter(
      !ProjectID %in% c(mahoning_projects) &
        !is.na(ExitDate) &
        ProjectID != 1695 &
        (
          is.na(ExitDate) |
            ExitDate >= hc$bos_start_vaccine_data
        ) &
        (
          C19ConsentToVaccine == "Data not collected (HUD)" |
            is.na(C19ConsentToVaccine)
        ) &
        is.na(Doses) &
        (ProjectType %in% c(1, 2, 4, 8) |
           (
             ProjectType %in% c(3, 9, 13) &
               is.na(MoveInDateAdjust)
           ))
    ) |>
    dplyr::mutate(Type = "Warning",
                  Issue = "Vaccine data not collected and client has exited",
                  Guidance = "Client was literally homeless on Feb 5th, 2021 or later and
         is missing their vaccine data, and the client has exited the project.
         If you are unable to follow up with the client, leave the client as is.
         Please see the guidance
         <a href = \"https://cohhio.org/boscoc/covid19/\" target = \"blank\">
         for more information</a>.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_vaccine_current <- served_in_date_range %>%
    dplyr::left_join(covid19[c("PersonalID", "C19ConsentToVaccine", "C19VaccineConcerns")],
                     by = "PersonalID") %>%
    dplyr::left_join(dose_counts, by = "PersonalID") %>%
    dplyr::filter(
      !ProjectID %in% c(mahoning_projects) &
        is.na(ExitDate) &
        ProjectID != 1695 &
        (
          is.na(ExitDate) |
            ExitDate >= hc$bos_start_vaccine_data
        ) &
        (
          C19ConsentToVaccine == "Data not collected (HUD)" |
            is.na(C19ConsentToVaccine)
        ) &
        is.na(Doses) &
        (ProjectType %in% c(1, 2, 4, 8) |
           (
             ProjectType %in% c(3, 9, 13) &
               is.na(MoveInDateAdjust)
           ))
    ) %>%
    dplyr::mutate(
      Type = "Error",
      Issue = "Vaccine data not collected on current client",
      Guidance = "Client was literally homeless on Feb 5th, 2021 or later and is
    missing their vaccine data. Because the client has not exited the project,
    this data can still be collected. Please see
    <a href = \"https://cohhio.org/boscoc/covid19/\" target = \"blank\">
    for more information</a>."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Dose Warnings -----------------------------------------------------------

  dose_date_error <- doses %>%
    dplyr::filter(C19DoseDate < hc$first_vaccine_administered_in_us) %>%
    dplyr::left_join(served_in_date_range %>%
                       dplyr::filter(HMIS::served_between(., hc$bos_start_vaccine_data, lubridate::today())),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Error",
                  Issue = "Vaccine Date Incorrect",
                  Guidance = "Vaccination date precedes the vaccine being available in the US.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # TODO Revise with new dose data coming in from Clarity
  dose_date_warning <- doses %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::summarise(Doses = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Doses > 1) %>%
    dplyr::left_join(doses, by = "PersonalID") %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::mutate(LastDose = dplyr::lag(C19DoseDate, order_by = C19DoseDate)) %>%
    dplyr::filter(!is.na(LastDose)) %>%
    dplyr::mutate(DaysBetweenDoses = difftime(C19DoseDate, LastDose, units = "days")) %>%
    dplyr::filter(C19DoseDate < hc$first_vaccine_administered_in_us |
                    DaysBetweenDoses < 20 |
                    (C19VaccineManufacturer == "Moderna") &
                    DaysBetweenDoses < 27) %>%
    dplyr::left_join(served_in_date_range %>%
                       dplyr::filter(HMIS::served_between(., hc$bos_start_vaccine_data, lubridate::today())),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Warning",
                  Issue = "Vaccine Dates or Vaccine Manufacturer Questionable",
                  Guidance = "The number of days between vaccines doses does not match
         the vaccine manufacturer’s recommended timeline. One of the vaccine
         records' Vaccine Date or the Vaccine Manufacturer may be entered
         incorrectly.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  differing_manufacturers <- doses %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::summarise(Doses = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Doses > 1) %>%
    dplyr::left_join(doses, by = "PersonalID") %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::mutate(
      minManufacturer = min(C19VaccineManufacturer),
      maxManufacturer = max(C19VaccineManufacturer),
      differs = minManufacturer != maxManufacturer,
      Type = "Error",
      Issue = "Client received different vaccines",
      Guidance = "The data shows that the client received vaccines from
    different manufacturers, but this is highly unlikely. Please correct the
    data in HMIS or let us know if the client actually received vaccines from
    different manufacturers."
    ) %>%
    dplyr::filter(differs == TRUE) %>%
    dplyr::left_join(served_in_date_range %>%
                       dplyr::filter(HMIS::served_between(., hc$bos_start_vaccine_data, lubridate::today())),
                     by = "PersonalID") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  unknown_manufacturer_error <- doses %>%
    dplyr::filter(stringr::str_starts(C19VaccineManufacturer, "Client doesn't know") &
                    C19VaccineDocumentation != "Self-report") %>%
    dplyr::left_join(served_in_date_range %>%
                       dplyr::filter(HMIS::served_between(., hc$bos_start_vaccine_data, lubridate::today())),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Error",
                  Issue = "Incorrect Vaccine Manufacturer or Incorrect Documentation Type",
                  Guidance = "If vaccine information was collected via Healthcare Provider
         or Vaccine card, then the vaccine manufacturer should be known and
         updated in HMIS.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  unknown_manufacturer_warning <- doses %>%
    dplyr::filter(stringr::str_starts(C19VaccineManufacturer, "Client doesn't know") &
                    C19VaccineDocumentation == "Self-report") %>%
    dplyr::left_join(served_in_date_range %>%
                       dplyr::filter(HMIS::served_between(., hc$bos_start_vaccine_data, lubridate::today())),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Warning",
                  Issue = "Unknown Vaccine Manufacturer",
                  Guidance = "If the client does not know the manufacturer of the vaccine,
         please try to find another source for the information. Reporting relies
         heavily on knowing the manufacturer of the vaccine your client received.
         If you absolutely cannot find it, it is ok to leave as is.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Missing Client Location -------------------------------------------------

  missing_client_location <- served_in_date_range %>%
    dplyr::filter(is.na(ClientLocation),
                  RelationshipToHoH == 1) %>%
    dplyr::mutate(Type = "High Priority",
                  Issue = "Missing Client Location",
                  Guidance = "If Client Location is missing, this household will be
         excluded from all HUD reporting.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Household Issues --------------------------------------------------------

  hh_children_only <- served_in_date_range %>%
    dplyr::filter(GrantType != "RHY" |
                    is.na(GrantType)) %>% # not checking for children-only hhs for RHY
    dplyr::group_by(HouseholdID) %>%
    dplyr::summarise(
      hhMembers = dplyr::n(),
      maxAge = max(AgeAtEntry),
      PersonalID = min(PersonalID)
    ) %>%
    dplyr::filter(maxAge < 18) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
    dplyr::mutate(Issue = "Children Only Household",
                  Type = "High Priority",
                  Guidance = "Unless your project serves youth younger than 18
         exclusively, every household should have at least one adult in it. If
         you are not sure how to correct this, please contact the HMIS team for
         help.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  hh_no_hoh <- served_in_date_range %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::summarise(hasHoH = dplyr::if_else(min(RelationshipToHoH) != 1,
                                             FALSE,
                                             TRUE),
                     PersonalID = min(PersonalID)) %>%
    dplyr::filter(hasHoH == FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
    dplyr::mutate(
      Issue = "No Head of Household",
      Type = "High Priority",
      Guidance = "Please be sure all members of the household are included in the program
      stay, and that each household member's birthdate is correct. If those
      things are both true, or the client is a single, check inside the Entry
      pencil to be sure each household member has \"Relationship to Head of
      Household\" answered and that one of them says Self (head of household).
      Singles are always Self (head of household)."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  hh_too_many_hohs <- served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1) %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::summarise(HoHsinHousehold = dplyr::n(),
                     PersonalID = min(PersonalID)) %>%
    dplyr::filter(HoHsinHousehold > 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
    dplyr::mutate(Issue = "Too Many Heads of Household",
                  Type = "High Priority",
                  Guidance = "Check inside the Entry pencil to be sure each household member has
      \"Relationship to Head of Household\" answered and that only one of
      them says \"Self (head of household)\".") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  hh_missing_rel_to_hoh <- served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 99) %>%
    dplyr::anti_join(hh_no_hoh["HouseholdID"], by = "HouseholdID") %>%
    dplyr::mutate(Issue = "Missing Relationship to Head of Household",
                  Type = "High Priority",
                  Guidance = "Check inside the Entry pencil to be sure each household member has
      \"Relationship to Head of Household\" answered and that only one of
      them says \"Self (head of household)\".") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  hh_issues <- rbind(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

  rm(hh_too_many_hohs, hh_no_hoh, hh_children_only, hh_missing_rel_to_hoh)

  # Missing Data at Entry ---------------------------------------------------
  # Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
  # DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

  missing_approx_date_homeless <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      EnrollmentID,
      ProjectID,
      AgeAtEntry,
      RelationshipToHoH,
      LOSUnderThreshold,
      DateToStreetESSH,
      PreviousStreetESSH
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    is.na(DateToStreetESSH) &
                    LOSUnderThreshold == 1 &
                    PreviousStreetESSH == 1
    ) %>%
    dplyr::mutate(Issue = "Missing Approximate Date Homeless",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_previous_street_ESSH <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      DateToStreetESSH,
      PreviousStreetESSH,
      LOSUnderThreshold
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    is.na(PreviousStreetESSH) &
                    LOSUnderThreshold == 1
    ) %>%
    dplyr::mutate(Issue = "Missing Previously From Street, ES, or SH (Length of Time Homeless questions)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_residence_prior <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LivingSituation) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    (is.na(LivingSituation) | LivingSituation == 99)) %>%
    dplyr::mutate(Issue = "Missing Residence Prior",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dkr_residence_prior <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LivingSituation) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    LivingSituation %in% c(8, 9)) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Residence Prior",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_LoS <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LengthOfStay) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
    dplyr::mutate(Issue = "Missing Length of Stay",
                  Type = "Error",
                  Guidance = "This data element may be answered with an old value or it
         may simply be missing. If the value selected is \"One week or less (HUD)\",
         you will need to change that value to either \"One night or less (HUD)\"
         or \"Two to six nights (HUD)\".") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dkr_LoS <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LengthOfStay) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    LengthOfStay %in% c(8, 9)) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Residence Prior",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_months_times_homeless <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    ProjectType %in% c(1, 4, 8) &
                    (
                      is.na(MonthsHomelessPastThreeYears) |
                        is.na(TimesHomelessPastThreeYears) |
                        MonthsHomelessPastThreeYears == 99 |
                        TimesHomelessPastThreeYears == 99
                    )
    ) %>%
    dplyr::mutate(Issue = "Missing Months or Times Homeless",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dkr_months_times_homeless <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    (
                      MonthsHomelessPastThreeYears %in% c(8, 9) |
                        TimesHomelessPastThreeYears %in% c(8, 9)
                    )
    ) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Months or Times Homeless",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  invalid_months_times_homeless <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears,
      DateToStreetESSH
    ) %>%
    dplyr::filter(
      ProjectType != 12 &
        (RelationshipToHoH == 1 | AgeAtEntry > 17) &
        EntryDate >= hc$prior_living_situation_required &
        TimesHomelessPastThreeYears == 1 &
        !is.na(DateToStreetESSH)
    ) %>%
    dplyr::mutate(
      MonthHomelessnessBegan = lubridate::floor_date(DateToStreetESSH, "month"),
      MonthEnteredProgram = lubridate::floor_date(EntryDate, "month"),
      MonthDiff = lubridate::interval(MonthHomelessnessBegan, MonthEnteredProgram) %/% months(1) + 1,
      MonthDiff = dplyr::if_else(MonthDiff >= 13, 13, MonthDiff),
      DateMonthsMismatch = dplyr::if_else(MonthsHomelessPastThreeYears - MonthDiff != 100, 1, 0),
      Issue = dplyr::case_when(
        MonthDiff <= 0 ~
          "Homelessness Start Date Later Than Entry",
        MonthsHomelessPastThreeYears < 100 ~
          "Number of Months Homeless Can Be Determined",
        DateMonthsMismatch == 1 ~
          "Invalid Homelessness Start Date/Number of Months Homeless"),
      Type = "Warning",
      Guidance = dplyr::case_when(
        MonthDiff <= 0 ~
          "This client has an Approximate Date Homeless in their Entry that is after
        their Entry Date. The information in the Entry should reflect the
        client's situation at the point of Entry, so this date may have been
        incorrectly entered.",
        MonthsHomelessPastThreeYears < 100 ~
          "According to this client's entry, they experienced a single episode of
        homelessness in the three years prior to their entry and the approximate
        start date of their homelessness is known, but there was no response
        entered for the number of months they experienced homelessness prior to
        this entry. It should be possible to determine and enter the number of
        months homeless based on the Approximate Date Homeless and the Entry Date.",
        DateMonthsMismatch == 1 ~
          "According to this client's entry, they experienced a single episode of
        homelessness in the three years prior to their entry and the approximate
        start date of their homelessness is known, but the recorded number of
        months they experienced homelessness prior to this entry is inconsistent
        with the given dates. Please double-check this information for
        consistency and accuracy.")) %>%
    dplyr::filter(!is.na(Guidance)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_living_situation <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      RelationshipToHoH,
      LivingSituation,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH,
      DateToStreetESSH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate >= hc$prior_living_situation_required &
                    # not req'd prior to this
                    ProjectType %in% c(2, 3, 6, 9, 10, 12, 13) &
                    (
                      (
                        LivingSituation %in% c(15, 6, 7, 24, 4, 5) &
                          LengthOfStay %in% c(2, 3, 10, 11) &
                          (is.na(LOSUnderThreshold) |
                             is.na(PreviousStreetESSH))
                      ) |
                        (
                          LivingSituation %in% c(2, 3, 12, 13, 14, 15, 19,
                                                         20, 21, 22, 23, 25, 26) &
                            LengthOfStay %in% c(10, 11) &
                            (is.na(LOSUnderThreshold) |
                               is.na(PreviousStreetESSH))
                        )
                    )
    ) %>%
    dplyr::mutate(Issue = "Incomplete Living Situation Data",
                  Type = "Error",
                  Guidance = "When responding to the Living Situation questions in your
         Entry Assessment, users must answer questions about some clients'
         situation prior to the \"Residence Prior\" that are important to help
         determine that client's Chronicity. Please answer these questions to
         the best of your knowledge.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dkr_living_situation <- served_in_date_range %>%
    dplyr::select(
      PersonalID,
      HouseholdID,
      EnrollmentID,
      ProjectID,
      ProjectType,
      ProjectName,
      ProjectRegion,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      AgeAtEntry,
      CountyServed,
      RelationshipToHoH,
      LivingSituation,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH,
      DateToStreetESSH,
      MonthsHomelessPastThreeYears,
      TimesHomelessPastThreeYears,
      UserCreating
    ) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    EntryDate > hc$prior_living_situation_required &
                    (
                      MonthsHomelessPastThreeYears %in% c(8, 9) |
                        TimesHomelessPastThreeYears %in% c(8, 9) |
                        LivingSituation %in% c(8, 9)
                    )
    ) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Living Situation",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # DisablingCondition at Entry

  detail_missing_disabilities <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  DisablingCondition) %>%
    dplyr::filter(DisablingCondition == 99 |
                    is.na(DisablingCondition)) %>%
    dplyr::mutate(Issue = "Missing Disabling Condition",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry)

  missing_disabilities <- detail_missing_disabilities %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  smallDisabilities <- Disabilities %>%
    dplyr::filter(DataCollectionStage == 1 &
                    ((DisabilityType == 10 &
                        DisabilityResponse %in% c(1:3)) |
                       (DisabilityType != 10 & DisabilityResponse == 1)
                    )) %>%
    dplyr::mutate(
      IndefiniteAndImpairs =
        dplyr::case_when(
          DisabilityType %in% c(6, 8) ~ 1,
          TRUE ~ IndefiniteAndImpairs)
    ) %>%
    dplyr::select(
      PersonalID,
      DisabilitiesID,
      EnrollmentID,
      InformationDate,
      DisabilityType,
      IndefiniteAndImpairs
    )

  # Developmental & HIV/AIDS get automatically IndefiniteAndImpairs = 1 per FY2020

  conflicting_disabilities <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  AgeAtEntry,
                  RelationshipToHoH,
                  DisablingCondition) %>%
    dplyr::left_join(
      smallDisabilities %>%
        dplyr::filter(IndefiniteAndImpairs == 1),
      by = c("PersonalID", "EnrollmentID")
    ) %>%
    dplyr::filter((DisablingCondition == 0 & !is.na(DisabilitiesID)) |
                    (DisablingCondition == 1 & is.na(DisabilitiesID))) %>%
    dplyr::mutate(
      Issue = "Conflicting Disability of Long Duration yes/no",
      Type = "Error",
      Guidance = "If the user answered \"Yes\" to the \"Does the client have a
    disabling condition?\", then there should be a disability subassessment that
    indicates the disability determination is Yes *and* the \"If yes,... long
    duration\" question is Yes. Similarly if the user answered \"No\", the
    client should not have any disability subassessments that indicate that they
    do have a Disabling Condition."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(smallDisabilities)

  # Mahoning 60 days CE -----------------------------------------------------

  mahoning_ce_60_days <- served_in_date_range %>%
    dplyr::filter(ProjectID == 2372 &
                    EntryDate <= lubridate::today() - lubridate::days(60) &
                    is.na(ExitDate)) %>%
    dplyr::mutate(
      Issue = "60 Days in Mahoning Coordinated Entry",
      Type = "Warning",
      Guidance = "If this household is \"unreachable\" as defined in the Mahoning County
    Coordinated Entry Policies and Procedures, they should be exited."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Extremely Long Stayers --------------------------------------------------

  th_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate))) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 2 &
                    !ProjectID %in% c(mahoning_projects))

  th_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate))) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 2 &
                    ProjectID %in% c(mahoning_projects))

  Top2_TH_bos <- subset(th_stayers_bos, Days > stats::quantile(Days, prob = 1 - 2 / 100))
  Top2_TH_mah <- subset(th_stayers_mah, Days > stats::quantile(Days, prob = 1 - 2 / 100))

  rrh_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 13 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  rrh_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 13 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top2_RRH_bos <- subset(rrh_stayers_bos, Days > stats::quantile(Days, prob = 1 - 2 / 100))
  Top2_RRH_mah <- subset(rrh_stayers_mah, Days > stats::quantile(Days, prob = 1 - 2 / 100))

  es_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 1 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  es_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 1 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top2_ES_bos <- subset(es_stayers_bos, Days > stats::quantile(Days, prob = 1 - 2 / 100))
  Top2_ES_mah <- subset(es_stayers_mah, Days > stats::quantile(Days, prob = 1 - 2 / 100))

  psh_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 3 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  psh_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 3 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top1_PSH_bos <- subset(psh_stayers_bos, Days > stats::quantile(Days, prob = 1 - 1 / 100))
  Top1_PSH_mah <- subset(psh_stayers_mah, Days > stats::quantile(Days, prob = 1 - 1 / 100))

  hp_stayers_bos <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 12 &
                    !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  hp_stayers_mah <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ProjectID) %>%
    dplyr::filter(is.na(ExitDate) &
                    ProjectType == 12 &
                    ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(Days = as.numeric(difftime(lubridate::today(), EntryDate)))

  Top5_HP_bos <- subset(hp_stayers_bos, Days > stats::quantile(Days, prob = 1 - 5 / 100))
  Top10_HP_mah <- subset(hp_stayers_mah, Days > stats::quantile(Days, prob = 90 / 100))

  extremely_long_stayers <- rbind(Top1_PSH_bos,
                                  Top2_ES_bos,
                                  Top2_RRH_bos,
                                  Top2_TH_bos,
                                  Top5_HP_bos,
                                  Top1_PSH_mah,
                                  Top2_ES_mah,
                                  Top2_RRH_mah,
                                  Top2_TH_mah,
                                  Top10_HP_mah) %>%
    dplyr::mutate(
      Issue = "Extremely Long Stayer",
      Type = "Warning",
      Guidance = paste(
        "This client is showing as an outlier for Length of Stay for this project
      type in the",
      dplyr::if_else(
        ProjectID %in% c(mahoning_projects),
        "Mahoning County",
        "Balance of State"
      ),
      "CoC. Please verify that
         this client is still in your project. If they are, be sure there are no
         alternative permanent housing solutions for this client. If the client
         is no longer in your project, please enter their Exit Date as the
         closest estimation of the day they left your project."
      )
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(list = ls(pattern = "Top*"),
     es_stayers_mah,
     th_stayers_mah,
     psh_stayers_mah,
     rrh_stayers_mah,
     hp_stayers_mah,
     es_stayers_bos,
     th_stayers_bos,
     psh_stayers_bos,
     rrh_stayers_bos,
     hp_stayers_bos)


  # Incorrect Destination ---------------------------------------------------

  # RRH mover inners only

  moved_in_rrh <- served_in_date_range %>%
    dplyr::filter(ProjectType == 13 & !is.na(MoveInDateAdjust)) %>%
    dplyr::mutate(RRH_range = lubridate::interval(EntryDate, ExitAdjust - lubridate::days(1))) %>%
    dplyr::select(PersonalID,
                  "RRHMoveIn" = MoveInDateAdjust,
                  RRH_range,
                  "RRHProjectName" = ProjectName)

  enrolled_in_rrh <- served_in_date_range %>%
    dplyr::filter(ProjectType == 13) %>%
    dplyr::mutate(RRH_range = lubridate::interval(EntryDate, ExitAdjust - lubridate::days(1))) %>%
    dplyr::select(PersonalID,
                  "RRHMoveIn" = MoveInDateAdjust,
                  RRH_range,
                  "RRHProjectName" = ProjectName)

  # maybe_rrh_destination <- served_in_date_range %>%
  #   left_join(enrolled_in_rrh, by = "PersonalID") %>%
  #   filter(ProjectType != 13 &
  #            ExitAdjust %within% RRH_range &
  #            Destination != 31) %>%
  #   mutate(
  #     Issue = "Check Exit Destination (may be \"Rental by client, with RRH...\")",
  #     Type = "Warning",
  #     Guidance = "This household appears to have an Entry into an RRH project that
  #     overlaps their Exit from your project. Typically this means the client moved
  #     into a Rapid Rehousing unit after their stay with you. If that is true, the
  #     Destination should be \"Rental by client, with RRH...\". If you are sure the
  #     current Destination is accurate, then please leave it the way it is."
  #   ) %>%
  #   select(all_of(vars$we_want))

  should_be_rrh_destination <- served_in_date_range %>%
    dplyr::left_join(moved_in_rrh, by = "PersonalID") %>%
    dplyr::filter(ProjectType != 13 &
                    ExitDate == RRHMoveIn &
                    Destination != 31) %>%
    dplyr::mutate(
      Issue = "Maybe Incorrect Exit Destination (did you mean \"Rental by client, with RRH...\"?)",
      Type = "Warning",
      Guidance = "This household has a Move-In Date into an RRH project that
    matches their Exit from your project, but the Exit Destination from your
    project does not indicate that the household exited to Rapid Rehousing. If
    the household exited to a Destination that was not \"Rental by client\", but
    it is a permanent destination attained through a Rapid Rehousing project,
    then there is no change needed. If this is not the case, then the Destination
    should be \"Rental by client, with RRH or equivalent subsidy\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # PSH mover inners only

  enrolled_in_psh <- served_in_date_range %>%
    dplyr::filter(ProjectType %in% c(3, 9) & !is.na(MoveInDateAdjust)) %>%
    dplyr::mutate(PSH_range = lubridate::interval(EntryDate, ExitAdjust - lubridate::days(1))) %>%
    dplyr::select(PersonalID,
                  PSH_range,
                  "PSHMoveIn" = MoveInDateAdjust,
                  "PSHProjectName" = ProjectName)

  maybe_psh_destination <- served_in_date_range %>%
    dplyr::left_join(enrolled_in_psh, by = "PersonalID") %>%
    dplyr::filter(!ProjectType %in% c(3, 9) &
                    ExitAdjust %within% PSH_range &
                    !Destination %in% c(3, 19, 26)) %>%
    dplyr::mutate(
      Issue = "Check Exit Destination (may be \"Permanent housing (other
    than RRH)...\")",
    Type = "Warning",
    Guidance = "This household appears to have an Entry into a PSH project that
    overlaps their Exit from your project. Typically this means the client moved
    into a Permanent Supportive Housing unit after their stay with you. If that
    is true, the Destination should be
    \"Permanent housing (other than RRH) for formerly homeless persons\". If you
    are sure the current Destination is accurate, then please leave it the way
    it is."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  should_be_psh_destination <- served_in_date_range %>%
    dplyr::left_join(enrolled_in_psh, by = "PersonalID") %>%
    dplyr::filter(!ProjectType %in% c(3, 9) &
                    ExitDate == PSHMoveIn &
                    !Destination %in% c(3, 19, 26)) %>%
    dplyr::mutate(
      Issue = "Incorrect Exit Destination (should be \"Permanent housing (other
    than RRH)...\")",
    Type = "Error",
    Guidance = "This household appears to have a Move-In Date into a PSH project
    that matches their Exit from your project, but the Exit Destination from your
    project does not indicate that the household exited to PSH. The correct
    Destination for households entering PSH from your project is
    \"Permanent housing (other than RRH) for formerly homeless persons\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # TH

  enrolled_in_th <- served_in_date_range %>%
    dplyr::filter(ProjectType == 2) %>%
    dplyr::mutate(TH_range = lubridate::interval(EntryDate, ExitAdjust - lubridate::days(1))) %>%
    dplyr::select(PersonalID, TH_range, "THProjectName" = ProjectName)

  should_be_th_destination <- served_in_date_range %>%
    dplyr::left_join(enrolled_in_th, by = "PersonalID") %>%
    dplyr::filter(ProjectType != 2 &
                    ExitAdjust %within% TH_range &
                    Destination != 2) %>%
    dplyr::mutate(
      Issue = "Incorrect Exit Destination (should be \"Transitional housing...\")",
      Type = "Error",
      Guidance = "This household appears to have an Entry into a Transitional
    Housing project that overlaps their Exit from your project, but the Exit
    Destination from your project does not indicate that the household exited to
    Transitional Housing. The correct Destination for households entering TH from
    your project is \"Transitional housing for homeless persons (including
    homeless youth)\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # SH

  enrolled_in_sh <- served_in_date_range %>%
    dplyr::filter(ProjectType == 8) %>%
    dplyr::mutate(SH_range = lubridate::interval(EntryDate, ExitAdjust - lubridate::days(1))) %>%
    dplyr::select(PersonalID, SH_range, "SHProjectName" = ProjectName)

  should_be_sh_destination <- served_in_date_range %>%
    dplyr::left_join(enrolled_in_sh, by = "PersonalID") %>%
    dplyr::filter(ProjectType != 8 &
                    ExitAdjust %within% SH_range &
                    Destination != 18) %>%
    dplyr::mutate(
      Issue = "Incorrect Exit Destination (should be \"Safe Haven\")",
      Type = "Error",
      Guidance = "This household appears to have an Entry into a Safe Haven that
    overlaps their Exit from your project, but the Exit Destination from your
    project does not indicate that the household exited to a Safe Haven. The
    correct Destination for households entering SH from your project is
    \"Safe Haven\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  # Missing Project Stay or Incorrect Destination ---------------------------

  # RRH

  destination_rrh <- served_in_date_range %>%
    dplyr::filter(Destination == 31)

  no_bos_rrh <- destination_rrh %>%
    dplyr::anti_join(enrolled_in_rrh, by = "PersonalID") %>%
    dplyr::mutate(
      Issue = "Missing RRH Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
    to Rapid Rehousing, but there is no RRH project stay on the client. If the
    RRH project the household exited to is outside of the Balance of State or
    Mahoning County CoCs, then no correction is necessary. If they received RRH services
    in the Balance of State CoC or Mahoning County CoC, then this household is missing
    their RRH project stay. If they did not actually receive RRH services at all,
    the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # PSH

  destination_psh <- served_in_date_range %>%
    dplyr::filter(Destination == 3)

  no_bos_psh <- destination_psh %>%
    dplyr::anti_join(enrolled_in_psh, by = "PersonalID") %>%
    dplyr::mutate(
      Issue = "Missing PSH Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
    to Permanent Supportive Housing, but there is no PSH project stay on the
    client. If the PSH project the household exited to is outside of the Balance
    of State CoC or Mahoning County CoC, then no correction is necessary. If they
    entered PSH in the Balance of State CoC or Mahoning County CoC, then this household
    is missing their PSH project stay. If they did not actually enter PSH at all,
    the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # TH

  destination_th <- served_in_date_range %>%
    dplyr::filter(Destination == 2)

  no_bos_th <- destination_th %>%
    dplyr::anti_join(enrolled_in_th, by = "PersonalID") %>%
    dplyr::mutate(
      Issue = "Missing TH Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
    to Transitional Housing, but there is no TH project stay on the client. If the
    TH project that the household exited to is outside of the Balance of State
    CoC or Mahoning County CoC, then no correction is necessary. If they went into a TH
    project in the Balance of State CoC or Mahoning County CoC, then this household is
    missing their TH project stay. If they did not actually enter Transitional
    Housing at all, the Destination should be corrected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # SH

  destination_sh <- served_in_date_range %>%
    dplyr::filter(Destination == 18)

  no_bos_sh <- destination_sh %>%
    dplyr::anti_join(enrolled_in_sh, by = "PersonalID") %>%
    dplyr::mutate(
      Issue = "Missing Safe Haven Project Stay or Incorrect Destination",
      Type = "Warning",
      Guidance = "The Exit Destination for this household indicates that they exited
    to a Safe Haven, but there is no Entry in HMIS into a Safe Haven. Keep in
    mind that there is only one Safe Haven in the Balance of State and they are
    no longer operating as of 1/1/2021. If you meant to indicate that the household
    exited to a Domestic Violence shelter, please select \"Emergency shelter, ...\"."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # CountyServed (BoS ONLY for now)

  missing_county_served <- served_in_date_range %>%
    dplyr::filter(is.na(CountyServed) & !ProjectID %in% c(mahoning_projects)) %>%
    dplyr::mutate(
      Issue = "Missing County Served",
      Type = "Error",
      Guidance = "County Served must be collected at Entry for all clients. County is
      very important so that the client is prioritized into the correct service
      areas for various housing solutions. This can be corrected through the
      Entry pencil."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # CountyPrior (BoS ONLY for now)

  missing_county_prior <- served_in_date_range %>%
    dplyr::filter(is.na(CountyPrior) & !ProjectID %in% c(mahoning_projects) &
                    (AgeAtEntry > 17 |
                       is.na(AgeAtEntry))) %>%
    dplyr::mutate(Issue = "Missing County of Prior Residence",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Check Eligibility, Project Type, Residence Prior ------------------------

  check_eligibility <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      ProjectID,
      AgeAtEntry,
      RelationshipToHoH,
      LivingSituation,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH,
      GrantType
    ) %>%
    dplyr::filter(
      RelationshipToHoH == 1 &
        AgeAtEntry > 17 &
        EntryDate > hc$check_eligibility_back_to &
        (ProjectType %in% c(3, 4, 8, 9, 10, 12, 13) |
           (ProjectType == 2 & (is.na(GrantType) | GrantType != "RHY"))) &
        (
          (ProjectType %in% c(2, 3, 9, 10, 13) &
             # PTCs that require LH status
             (
               is.na(LivingSituation) |
                 (
                   LivingSituation %in% c(4:7, 15, 25:27, 29) & # institution
                     (
                       !LengthOfStay %in% c(2, 3, 10, 11) | # <90 days
                         is.na(LengthOfStay) |
                         PreviousStreetESSH == 0 | # LH prior
                         is.na(PreviousStreetESSH)
                     )
                 ) |
                 (
                   LivingSituation %in% c(3, 10, 11, 14, 19:23, 28, 31, 35, 36) &
                     # not homeless
                     (
                       !LengthOfStay %in% c(10, 11) |  # <1 week
                         is.na(LengthOfStay) |
                         PreviousStreetESSH == 0 | # LH prior
                         is.na(PreviousStreetESSH)
                     )
                 )
             )) |
            (
              ProjectType == 12 &
                (!LivingSituation %in% c(3, 10, 11, 14, 19:23, 28, 31, 35, 36) |
                   PreviousStreetESSH != 0 )
            ) |
            (ProjectType %in% c(8, 4) & # Safe Haven and Outreach
               LivingSituation != 16) # unsheltered only
        )
    )

  detail_eligibility <- check_eligibility %>%
    dplyr::select(
      PersonalID,
      ProjectName,
      ProjectType,
      LivingSituation,
      EntryDate,
      ExitDate,
      LengthOfStay,
      LOSUnderThreshold,
      PreviousStreetESSH
    ) %>%
    dplyr::mutate(
      ResidencePrior =
        living_situation(LivingSituation),
      LengthOfStay = dplyr::case_when(
        LengthOfStay == 2 ~ "One week or more but less than one month",
        LengthOfStay == 3 ~ "One month or more but less than 90 days",
        LengthOfStay == 4 ~ "90 days or more but less than one year",
        LengthOfStay == 5 ~ "One year or longer",
        LengthOfStay == 8 ~ "Client doesn't know",
        LengthOfStay == 9 ~ "Client refused",
        LengthOfStay == 10 ~ "One night or less",
        LengthOfStay == 11 ~ "Two to six nights",
        LengthOfStay == 99 ~ "Data not collected"
      )
    )

  check_eligibility <- check_eligibility %>%
    dplyr::mutate(
      Issue = "Check Eligibility",
      Type = "Warning",
      Guidance = paste(
        "Your Residence Prior data suggests that this project is either
        serving ineligible households, the household was entered into the wrong
        project, or the Residence Prior data at Entry is incorrect. Please check
        the terms of your grant or speak with",
        dplyr::if_else(
          ProjectID %in% c(mahoning_projects),
          "the Mahoning County CoC Coordinator",
          "the CoC team at COHHIO"
        ),
        "if you are unsure of eligibility criteria for your project type."
      )
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Rent Payment Made, No Move-In Date
  rent_paid_no_move_in <- served_in_date_range %>%
    dplyr::filter(is.na(MoveInDateAdjust) &
                    RelationshipToHoH == 1 &
                    ProjectType %in% c(3, 9, 13)) %>%
    dplyr::inner_join(Services %>%
                        dplyr::filter(
                          Description %in% c(
                            "Rent Payment Assistance",
                            "Utility Deposit Assistance",
                            "Rental Deposit Assistance"
                          )
                        ) %>%
                        dplyr::select(-PersonalID),
                      by = "EnrollmentID") %>%
    dplyr::mutate(
      Issue = "Rent Payment Made, No Move-In Date",
      Type = "Error",
      Guidance =
        "This client does not have a valid Move-In Date, but there is at
    least one rent/deposit payment Service Transaction recorded for this program.
    Until a Move-In Date is entered, this client will continue to be counted as
    literally homeless while in your program. Move-in dates must be on or after
    the Entry Date. If a client is housed then returns to homelessness while
    in your program, they need to be exited from their original Entry and
    re-entered in a new one that has no Move-In Date until they are re-housed."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Missing Destination
  missing_destination <- served_in_date_range %>%
    dplyr::filter(!is.na(ExitDate) &
                    (is.na(Destination) | Destination %in% c(99, 30))) %>%
    dplyr::mutate(
      Issue = "Missing Destination",
      Type = "Warning",
      Guidance = paste(
        "It is widely understood that not every client will
             complete an exit interview, especially for high-volume emergency
             shelters. A few warnings for Missing Destination is no cause for
             concern, but if there is a large number, please contact",
        dplyr::if_else(
          ProjectID %in% c(mahoning_projects),
          "the Mahoning County CoC Coordinator",
          "the Balance of State CoC team at COHHIO"
        ),
        "to work out a way to improve client engagement."
      )
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dkr_destination <- served_in_date_range %>%
    dplyr::filter(Destination %in% c(8, 9)) %>%
    dplyr::mutate(Issue = "Don't Know/Refused Destination",
                  Type = "Warning",
                  Guidance = guidance$dkr_data) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Missing PATH Data -------------------------------------------------------

  #* Length of Stay in Res Prior
  ### adult, PATH-enrolled, and:
  ### Length of Stay is null or DNC -> error -OR-
  ### Length of Stay is DKR -> warning

  smallProject <- Project %>% dplyr::select(ProjectID,
                                            ProjectName,
                                            ProjectCounty)

  path_missing_los_res_prior <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      ProjectID,
      AgeAtEntry,
      ClientEnrolledInPATH,
      LengthOfStay
    ) %>%
    dplyr::left_join(smallProject, by = c("ProjectID", "ProjectName")) %>%
    dplyr::filter(AgeAtEntry > 17 &
                    ClientEnrolledInPATH == 1 &
                    (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
    dplyr::mutate(Issue = "Missing Residence Prior Length of Stay (PATH)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  #* Engagement at Exit
  ### adult, PATH-enrolled, Date of Engagement is null -> error

  path_no_status_at_exit <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      ClientEnrolledInPATH,
      DateOfPATHStatus,
      ReasonNotEnrolled
    ) %>%
    dplyr::left_join(smallProject, by = "ProjectName") %>%
    dplyr::filter(!is.na(ExitDate) &
                    AgeAtEntry > 17 &
                    (
                      is.na(ClientEnrolledInPATH) |
                        is.na(DateOfPATHStatus) |
                        (ClientEnrolledInPATH == 0 &
                           is.na(ReasonNotEnrolled))
                    )) %>%
    dplyr::mutate(Issue = "PATH Status at Exit Missing or Incomplete",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  #* Status Determination at Exit
  ### adult, PATH-Enrolled is not null
  ### Date of Status Determ is null -> error
  path_status_determination <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  ClientEnrolledInPATH,
                  DateOfPATHStatus) %>%
    dplyr::left_join(smallProject, by = "ProjectName") %>%
    dplyr::filter(AgeAtEntry > 17 &
        !is.na(ClientEnrolledInPATH) &
        is.na(DateOfPATHStatus)
    ) %>%
    dplyr::mutate(Issue = "Missing Date of PATH Status",
                  Type = "Error",
                  Guidance = "Users must indicate the PATH Status Date for any adult
             enrolled in PATH.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  #* PATH Enrolled at Exit
  ### adult and:
  ### PATH Enrolled null or DNC -> error -OR-

  path_enrolled_missing <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), AgeAtEntry, ClientEnrolledInPATH) %>%
    dplyr::left_join(smallProject, by = "ProjectName") %>%
    dplyr::filter(!is.na(ExitDate) &
        AgeAtEntry > 17 &
        (ClientEnrolledInPATH == 99 |
           is.na(ClientEnrolledInPATH))
    ) %>%
    dplyr::mutate(
      Issue = "Missing PATH Enrollment at Exit",
      Type = "Error",
      Guidance = "Please enter the data for this item by clicking into the
        Entry or Exit pencil and creating an Interim. In the assessment, enter
        the correct PATH Enrollment Date and Save."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  #* Not Enrolled Reason
  ### adult
  ### PATH Enrolled = No
  ### Reason is null -> error

  path_reason_missing <- served_in_date_range %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      ClientEnrolledInPATH,
      ReasonNotEnrolled,
      ProjectType
    ) %>%
    dplyr::left_join(smallProject, by = "ProjectName") %>%
    dplyr::filter(AgeAtEntry > 17 &
                    ClientEnrolledInPATH == 0 &
                    is.na(ReasonNotEnrolled)) %>%
    dplyr::mutate(
      Issue = "Missing Reason Not PATH Enrolled",
      Type = "Error",
      Guidance = "The user has indicated the household was not enrolled into
        PATH, but no reason was selected."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  #* Connection with SOAR at Exit
  ### adult
  ### Connection w/ SOAR is null or DNC -> error -OR-
  ### Connection w/ SOAR DKR -> warning

  smallIncomeSOAR <- IncomeBenefits %>%
    dplyr::select(PersonalID,
                  EnrollmentID,
                  ConnectionWithSOAR,
                  DataCollectionStage) %>%
    dplyr::filter(DataCollectionStage == 3)

  path_SOAR_missing_at_exit <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  AgeAtEntry,
                  ClientEnrolledInPATH) %>%
    dplyr::left_join(smallProject, by = "ProjectName") %>%
    dplyr::left_join(smallIncomeSOAR, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::filter(AgeAtEntry > 17 &
                    DataCollectionStage == 3 &
                    is.na(ConnectionWithSOAR)) %>%
    dplyr::mutate(Issue = "Missing Connection with SOAR at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(smallIncomeSOAR)

  # Missing PATH Contacts
  ## client is adult/hoh and has no contact record in the EE -> error
  ## this is a high priority data quality issue
  ## if the contact was an "Outreach" record after 10/1/2019, it is being
  ## filtered out because they should be using CLS subs past that date.

  small_contacts <- Contacts %>%
    dplyr::left_join(served_in_date_range, by = "PersonalID") %>%
    dplyr::filter(
      ContactDate >= EntryDate &
      ContactDate <= ExitAdjust &
      ContactDate < hc$outreach_to_cls
    ) %>%
    dplyr::group_by(PersonalID, ProjectName, EntryDate, ExitDate) %>%
    dplyr::summarise(ContactCount = dplyr::n()) %>%
    dplyr::ungroup()

  missing_path_contact <- served_in_date_range %>%
    dplyr::filter(GrantType == "PATH" &
                    (AgeAtEntry > 17 |
                       RelationshipToHoH == 1)) %>%
    dplyr::select(dplyr::all_of(vars$prep)) %>%
    dplyr::left_join(small_contacts,
                     by = c("PersonalID",
                                    "ProjectName",
                                    "EntryDate",
                                    "ExitDate")) %>%
    dplyr::mutate_at(dplyr::vars(ContactCount), ~replace(., is.na(.), 0)) %>%
    dplyr::filter(ContactCount == 0) %>%
    dplyr::mutate(Issue = "Missing PATH Contact",
                  Type = "High Priority",
                  Guidance = "Every adult or Head of Household must have a Living
             Situation contact record. If you see a record there but there is
             no Date of Contact, saving the Date of Contact will correct this
             issue.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Incorrect PATH Contact Date
  ## client is adult/hoh, has a contact record, and the first record in the EE
  ## does not equal the Entry Date ->  error
  ## if the contact was an "Outreach" record after 10/1/2019, it is being
  ## filtered out because they should be using CLS subs past that date.

  first_contact <- Contacts %>%
    dplyr::filter(ContactDate < hc$outreach_to_cls) %>%
    dplyr::left_join(served_in_date_range, by = "PersonalID") %>%
    dplyr::select(PersonalID, EntryDate, ExitAdjust, ExitDate, ContactDate, ProjectName,
                  EntryDate, ExitAdjust) %>%
    dplyr::filter(ContactDate >= EntryDate &
                    ContactDate <= ExitAdjust) %>%
    dplyr::group_by(PersonalID, ProjectName, EntryDate, ExitDate) %>%
    dplyr::arrange(ContactDate) %>%
    dplyr::slice(1L)

  incorrect_path_contact_date <- served_in_date_range %>%
    dplyr::filter(GrantType == "PATH" &
                    (AgeAtEntry > 17 |
                       RelationshipToHoH == 1)) %>%
    dplyr::select(dplyr::all_of(vars$prep)) %>%
    dplyr::inner_join(first_contact, by = c("PersonalID",
                                                    "ProjectName",
                                                    "EntryDate",
                                                    "ExitDate")) %>%
    dplyr::filter(ContactDate != EntryDate) %>%
    dplyr::mutate(
      Issue = "No PATH Contact Entered at Entry",
      Type = "Error",
      Guidance = "Every adult or head of household should have a Living
             Situation contact record where the Contact Date matches the Entry
             Date. This would represent the initial contact made with the
             client."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(first_contact, small_contacts)

  # Duplicate EEs -----------------------------------------------------------
  # this could be more nuanced but it's ok to leave it since we are also
  # looking at overlaps
  duplicate_ees <-
    janitor::get_dupes(served_in_date_range, PersonalID, ProjectID, EntryDate) %>%
    dplyr::mutate(
      Issue = "Duplicate Entry Exits",
      Type = "High Priority",
      Guidance = "Users sometimes create this error when they forget to click
        into a program stay by using the Entry pencil, and instead they click
        \"Add Entry/Exit\" each time. To correct, EDA to the project the Entry/Exit
      belongs to, navigate to the Entry/Exit tab and delete the program stay
      that was accidentally added for each household member."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  # Future Entry Exits ------------------------------------------------------
  # PSHs in the old days before Move In Dates would definitely have been entering
  # their clients prior to their Entry Date since back then the Entry Date was the
  # day they moved in. So they're excused from this prior to Move In Date's existence.

  future_ees <- served_in_date_range %>%
    dplyr::filter(EntryDate > lubridate::ymd_hms(DateCreated) &
                    (ProjectType %in% c(1, 2, 4, 8, 13) |
                       (
                         ProjectType %in% c(3, 9) &
                           EntryDate >= hc$psh_started_collecting_move_in_date
                       )))  %>%
    dplyr::mutate(
      Issue = "Future Entry Date",
      Type = "Warning",
      Guidance = "Users should not be entering a client into a project on a
        date in the future. If the Entry Date is correct, there is no action
        needed, but going forward, please be sure that your data entry workflow
        is correct according to your project type."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  future_exits <- served_in_date_range %>%
    dplyr::filter(ExitDate > lubridate::today()) %>%
    dplyr::mutate(
      Issue = "Future Exit Date",
      Type = "Error",
      Guidance = "This client's Exit Date is a date in the future. Please
        enter the exact date the client left your program. If this client has not
        yet exited, delete the Exit and then enter the Exit Date once the client
        is no longer in your program."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))





  # HoHs Entering PH without SPDATs -----------------------------------------

  ees_with_spdats <- served_in_date_range %>%
    dplyr::anti_join(va_funded, by = "ProjectID") %>%
    dplyr::left_join(Scores, by = "PersonalID") %>%
    dplyr::ungroup() %>%
    dplyr::select(PersonalID,
                  EnrollmentID,
                  RelationshipToHoH,
                  EntryDate,
                  ExitAdjust,
                  ScoreDate,
                  Score) %>%
    dplyr::filter(ScoreDate + lubridate::days(365) > EntryDate &
                    # score is < 1 yr old
                    ScoreDate < ExitAdjust) %>%  # score is prior to Exit
    dplyr::group_by(EnrollmentID) %>%
    dplyr::slice_max(ScoreDate) %>%
    dplyr::slice_max(Score) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ScoreAdjusted = dplyr::if_else(is.na(Score), 0, Score))

  entered_ph_without_spdat <-
    dplyr::anti_join(served_in_date_range, ees_with_spdats, by = "EnrollmentID") %>%
    dplyr::filter(
      ProjectType %in% c(2, 3, 9, 13) &
        EntryDate > hc$began_requiring_spdats &
        # only looking at 1/1/2019 forward
        RelationshipToHoH == 1 &
        (CurrentlyFleeing != 1 |
           is.na(CurrentlyFleeing) |
           !WhenOccurred %in% c(1:3))
    ) %>%
    dplyr::mutate(
      Issue = "Non-DV HoHs Entering PH or TH without SPDAT",
      Type = "Warning",
      Guidance = "Every household (besides those fleeing domestic violence)
        must have a VI-SPDAT score to aid with prioritization into a
        Transitional Housing or Permanent Housing (RRH or PSH) project."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # HoHs in Shelter without a SPDAT -----------------------------------------

  lh_without_spdat <- served_in_date_range %>%
    dplyr::filter(is.na(PHTrack) | PHTrack != "Self Resolve" |
                    ExpectedPHDate < lubridate::today()) %>%
    dplyr::anti_join(ees_with_spdats, by = "EnrollmentID") %>%
    dplyr::filter(
      ProjectType %in% c(1, 4, 8) &
        VeteranStatus != 1 &
        RelationshipToHoH == 1 &
        EntryDate < lubridate::today() - lubridate::days(8) &
        is.na(ExitDate) &
        EntryDate > hc$began_requiring_spdats
    ) %>%
    dplyr::mutate(
      Issue = "HoHs in shelter for 8+ days without SPDAT",
      Type = "Warning",
      Guidance = "Any household who has been in shelter or a Safe Haven for
        over 8 days should be assessed with the VI-SPDAT so that they can be
        prioritized for Permanent Housing (RRH or PSH)."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  spdat_on_non_hoh <- ees_with_spdats %>%
    dplyr::left_join(
      served_in_date_range,
      by = c(
        "PersonalID",
        "EnrollmentID",
        "RelationshipToHoH",
        "EntryDate",
        "ExitAdjust"
      )
    ) %>%
    dplyr::filter(RelationshipToHoH != 1) %>%
    dplyr::mutate(
      Issue = "SPDAT Created on a Non-Head-of-Household",
      Type = "Warning",
      Guidance = "It is very important to be sure that the VI-SPDAT score goes on the
        Head of Household of a given program stay because otherwise that score
      may not pull into any reporting. It is possible a Non Head of Household
      was a Head of Household in a past program stay, and in that situation,
      this should not be corrected unless the Head of Household of your program
      stay is missing their score. To correct this, you would need to completely
      re-enter the score on the correct client's record."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(ees_with_spdats)

  # Missing Income at Entry -------------------------------------------------
  # IncomeBenefits <- IncomeBenefits %>% select(-DateCreated)

  missing_income_entry <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      DataCollectionStage,
      TotalMonthlyIncome,
      IncomeFromAnySource
    ) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    (AgeAtEntry > 17 |
                       is.na(AgeAtEntry)) &
                    (IncomeFromAnySource == 99 |
                       is.na(IncomeFromAnySource))) %>%
    dplyr::mutate(Issue = "Income Missing at Entry",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  smallIncome <- IncomeBenefits %>%
    dplyr::select(
      PersonalID,
      EnrollmentID,
      Earned,
      Unemployment,
      SSI,
      SSDI,
      VADisabilityService,
      VADisabilityNonService,
      PrivateDisability,
      WorkersComp,
      TANF,
      GA,
      SocSecRetirement,
      Pension,
      ChildSupport,
      Alimony,
      OtherIncomeSource,
      DataCollectionStage
    )

  smallIncome[is.na(smallIncome)] <- 0

  smallIncome <-
    smallIncome %>% dplyr::full_join(IncomeBenefits[c(
      "PersonalID",
      "EnrollmentID",
      "DataCollectionStage",
      "TotalMonthlyIncome",
      "IncomeFromAnySource"
    )],
    by = c("PersonalID",
                   "EnrollmentID",
                   "DataCollectionStage"))

  income_subs <- served_in_date_range[c("EnrollmentID",
                                                "AgeAtEntry",
                                                vars$prep)] %>%
    dplyr::left_join(smallIncome, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::mutate(
      IncomeCount =
        Earned +
        Unemployment +
        SSI +
        SSDI +
        VADisabilityService +
        VADisabilityNonService +
        PrivateDisability +
        WorkersComp +
        TANF +
        GA +
        SocSecRetirement +
        Pension +
        ChildSupport +
        Alimony +
        OtherIncomeSource
    )


  conflicting_income_entry <- income_subs %>%
    dplyr::filter(DataCollectionStage == 1 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((IncomeFromAnySource == 1 &
                        IncomeCount == 0) |
                       (IncomeFromAnySource == 0 &
                          IncomeCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Income yes/no at Entry",
                  Type = "Error",
                  Guidance = guidance$conflicting_income) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Not calculating Conflicting Income Amounts bc they're calculating the TMI from the
  # subs instead of using the field itself. Understandable but that means I would
  # have to pull the TMI data in through RMisc OR we kill TMI altogether. (We
  # decided to kill TMI altogether.)

  # Missing Income at Exit --------------------------------------------------

  missing_income_exit <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      AgeAtEntry,
      DataCollectionStage,
      TotalMonthlyIncome,
      IncomeFromAnySource,
      UserCreating
    ) %>%
    dplyr::filter(DataCollectionStage == 3 &
                    (AgeAtEntry > 17 |
                       is.na(AgeAtEntry)) &
                    (IncomeFromAnySource == 99 |
                       is.na(IncomeFromAnySource))) %>%
    dplyr::mutate(Issue = "Income Missing at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  conflicting_income_exit <- income_subs %>%
    dplyr::filter(DataCollectionStage == 3 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((IncomeFromAnySource == 1 &
                        IncomeCount == 0) |
                       (IncomeFromAnySource == 0 &
                          IncomeCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Income yes/no at Exit",
                  Type = "Error",
                  Guidance = guidance$conflicting_income) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(income_subs)

  # Overlapping Enrollment/Move In Dates ------------------------------------

  # this only pulls the most recent EE in the overlap and I think that's fine but
  # some users won't like being flagged for it if it's someone else's fault
  # but you can't tell whose fault it is from the data so...

  staging_overlaps <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      EntryAdjust = dplyr::case_when(
        #for PSH and RRH, EntryAdjust = MoveInDate
        ProjectType %in% c(1, 2, 8, 12) |
          ProjectName == "Unsheltered Clients - OUTREACH" ~ EntryDate,
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
        ProjectType %in% c(3, 9, 13) &
          is.na(MoveInDateAdjust) ~ EntryDate
      ),
      ExitAdjust = ExitAdjust - lubridate::days(1),
      # bc a client can exit&enter same day
      LiterallyInProject = dplyr::if_else(
        ProjectType %in% c(3, 9, 13),
        lubridate::interval(MoveInDateAdjust, ExitAdjust),
        lubridate::interval(EntryAdjust, ExitAdjust)
      ),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the
        same time. Nor can they have a Move-In Date into a PSH or RRH project
        while they are still in an ES, TH, or Safe Haven. Further, they cannot
        be in any two RRH's or any two PSH's simultaneously, housed or not.
        Please look the client(s) up in HMIS and determine which project stay's
        Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the
        \"Previous Provider's\" mistake, but if you are seeing clients here, it
        means your project stay was entered last.
        If the overlap is not your project's mistake, please work with the
        project that has the incorrect Entry/Move-In/or Exit Date to get this
        corrected or send an email to hmis@cohhio.org if you cannot get it
        resolved. These clients will NOT show on their Data Quality app.
        If YOUR dates are definitely correct, it is fine to continue with other
        data corrections as needed."
    ) %>%
    dplyr::filter(!is.na(LiterallyInProject) &
                    lubridate::int_length(LiterallyInProject) > 0) %>%
    janitor::get_dupes(., PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryAdjust) %>%
    dplyr::mutate(
      PreviousEntryAdjust = dplyr::lag(EntryAdjust),
      PreviousExitAdjust = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(!is.na(PreviousEntryAdjust)) %>%
    dplyr::ungroup()

  same_day_overlaps <- served_in_date_range %>%
    dplyr::filter((ProjectType == 13 & MoveInDateAdjust == ExitDate) |
                    ProjectType != 13) %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      EntryAdjust = dplyr::case_when(
        #for PSH and RRH, EntryAdjust = MoveInDate
        ProjectType %in% c(1, 2, 8, 12) |
          ProjectName == "Unsheltered Clients - OUTREACH" ~ EntryDate,
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
        ProjectType %in% c(3, 9, 13) &
          is.na(MoveInDateAdjust) ~ EntryDate
      ),
      LiterallyInProject = dplyr::case_when(
        ProjectType %in% c(3, 9) ~ lubridate::interval(MoveInDateAdjust, ExitAdjust),
        ProjectType %in% c(1, 2, 4, 8, 12) ~ lubridate::interval(EntryAdjust, ExitAdjust)
      ),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the
        same time. Nor can they have a Move-In Date into a PSH or RRH project
        while they are still in an ES, TH, or Safe Haven. Further, they cannot
        be in any two RRH's or any two PSH's simultaneously, housed or not.
        Please look the client(s) up in HMIS and determine which project stay's
        Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the
        \"Previous Provider's\" mistake, but if you are seeing clients here, it
        means your project stay was entered last.
        If the overlap is not your project's mistake, please work with the
        project that has the incorrect Entry/Move-In/or Exit Date to get this
        corrected or send an email to hmis@cohhio.org if you cannot get it
        resolved. These clients will NOT show on their Data Quality app.
        If YOUR dates are definitely correct, it is fine to continue with other
        data corrections as needed."
    ) %>%
    dplyr::filter((!is.na(LiterallyInProject) & ProjectType != 13) |
                    ProjectType == 13) %>%
    janitor::get_dupes(., PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryAdjust) %>%
    dplyr::mutate(
      PreviousEntryAdjust = dplyr::lag(EntryAdjust),
      PreviousExitAdjust = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(ExitDate > PreviousEntryAdjust &
                    ExitDate < PreviousExitAdjust) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(vars$we_want), PreviousProject)

  rrh_overlaps <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      ExitAdjust = ExitAdjust - lubridate::days(1),
      # bc a client can exit&enter same day
      InProject = lubridate::interval(EntryDate, ExitAdjust),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the
        same time. Nor can they have a Move-In Date into a PSH or RRH project
        while they are still in an ES, TH, or Safe Haven. Further, they cannot
        be in any two RRH's or any two PSH's simultaneously, housed or not.
        Please look the client(s) up in HMIS and determine which project stay's
        Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the
        \"Previous Provider's\" mistake, but if you are seeing clients here, it
        means your project stay was entered last.
        If the overlap is not your project's mistake, please work with the
        project that has the incorrect Entry/Move-In/or Exit Date to get this
        corrected or send an email to hmis@cohhio.org if you cannot get it
        resolved. These clients will NOT show on their Data Quality app.
        If YOUR dates are definitely correct, it is fine to continue with other
        data corrections as needed."
    ) %>%
    dplyr::filter(ProjectType == 13) %>%
    janitor::get_dupes(., PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryDate) %>%
    dplyr::mutate(
      PreviousEntry = dplyr::lag(EntryDate),
      PreviousExit = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(!is.na(PreviousEntry)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      PreviousStay = lubridate::interval(PreviousEntry, PreviousExit),
      Overlap = lubridate::int_overlaps(InProject, PreviousStay)
    ) %>%
    dplyr::filter(Overlap == TRUE) %>%
    dplyr::select(dplyr::all_of(vars$we_want), PreviousProject)

  psh_overlaps <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep), ExitAdjust) %>%
    dplyr::mutate(
      ExitAdjust = ExitAdjust - lubridate::days(1),
      # bc a client can exit&enter same day
      InProject = lubridate::interval(EntryDate, ExitAdjust),
      Issue = "Overlapping Project Stays",
      Type = "High Priority",
      Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the
        same time. Nor can they have a Move-In Date into a PSH or RRH project
        while they are still in an ES, TH, or Safe Haven. Further, they cannot
        be in any two RRH's or any two PSH's simultaneously, housed or not.
        Please look the client(s) up in HMIS and determine which project stay's
        Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the
        \"Previous Provider's\" mistake, but if you are seeing clients here, it
        means your project stay was entered last.
        If the overlap is not your project's mistake, please work with the
        project that has the incorrect Entry/Move-In/or Exit Date to get this
        corrected or send an email to hmis@cohhio.org if you cannot get it
        resolved. These clients will NOT show on their Data Quality app.
        If YOUR dates are definitely correct, it is fine to continue with other
        data corrections as needed."
    ) %>%
    dplyr::filter(ProjectType == 3) %>%
    janitor::get_dupes(., PersonalID) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(PersonalID, EntryDate) %>%
    dplyr::mutate(
      PreviousEntry = dplyr::lag(EntryDate),
      PreviousExit = dplyr::lag(ExitAdjust),
      PreviousProject = dplyr::lag(ProjectName)
    ) %>%
    dplyr::filter(!is.na(PreviousEntry)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      PreviousStay = lubridate::interval(PreviousEntry, PreviousExit),
      Overlap = lubridate::int_overlaps(InProject, PreviousStay)
    ) %>%
    dplyr::filter(Overlap == TRUE) %>%
    dplyr::select(dplyr::all_of(vars$we_want), PreviousProject)

  dq_overlaps <- staging_overlaps %>%
    dplyr::mutate(
      PreviousStay = lubridate::interval(PreviousEntryAdjust, PreviousExitAdjust),
      Overlap = lubridate::int_overlaps(LiterallyInProject, PreviousStay)
    ) %>%
    dplyr::filter(Overlap == TRUE) %>%
    dplyr::select(dplyr::all_of(vars$we_want), PreviousProject)

  dq_overlaps <-
    rbind(dq_overlaps, rrh_overlaps, psh_overlaps, same_day_overlaps) %>%
    unique()

  rm(staging_overlaps,
     same_day_overlaps,
     rrh_overlaps,
     psh_overlaps)

  unsh_overlaps <- dq_overlaps %>%
    dplyr::filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
    dplyr::left_join(Users, by = "UserCreating") %>%
    dplyr::select(PersonalID,
                  DefaultProvider,
                  EntryDate,
                  ExitDate,
                  PreviousProject)

  # Missing Health Ins ------------------------------------------------------

  missing_health_insurance_entry <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  DataCollectionStage,
                  InsuranceFromAnySource) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    (InsuranceFromAnySource == 99 |
                       is.na(InsuranceFromAnySource))) %>%
    dplyr::mutate(Issue = "Health Insurance Missing at Entry",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  missing_health_insurance_exit <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  InsuranceFromAnySource) %>%
    dplyr::filter(DataCollectionStage == 3 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    (InsuranceFromAnySource == 99 |
                       is.na(InsuranceFromAnySource))) %>%
    dplyr::mutate(Issue = "Health Insurance Missing at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  health_insurance_subs <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      DataCollectionStage,
      InsuranceFromAnySource,
      Medicaid,
      Medicare,
      SCHIP,
      VAMedicalServices,
      EmployerProvided,
      COBRA,
      PrivatePay,
      StateHealthIns,
      IndianHealthServices,
      OtherInsurance,
      HIVAIDSAssistance,
      ADAP,
      UserCreating
    ) %>%
    dplyr::mutate(
      SourceCount = Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
        COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
        OtherInsurance + Medicare
    )

  conflicting_health_insurance_entry <- health_insurance_subs %>%
    dplyr::filter(DataCollectionStage == 1 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    ((InsuranceFromAnySource == 1 &
                        SourceCount == 0) |
                       (InsuranceFromAnySource == 0 &
                          SourceCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Health Insurance yes/no at Entry",
                  Type = "Error",
                  Guidance = guidance$conflicting_hi) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  conflicting_health_insurance_exit <- health_insurance_subs %>%
    dplyr::filter(DataCollectionStage == 3 &
                    ProjectName != "Unsheltered Clients - OUTREACH" &
                    ((InsuranceFromAnySource == 1 &
                        SourceCount == 0) |
                       (InsuranceFromAnySource == 0 &
                          SourceCount > 0)
                    )) %>%
    dplyr::mutate(
      Issue = "Conflicting Health Insurance yes/no at Exit",
      Type = "Error",
      Guidance = guidance$conflicting_hi
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(health_insurance_subs)

  # Missing NCBs at Entry ---------------------------------------------------

  ncb_subs <- IncomeBenefits %>%
    dplyr::select(
      PersonalID,
      EnrollmentID,
      DataCollectionStage,
      SNAP,
      WIC,
      TANFChildCare,
      TANFTransportation,
      OtherTANF,
      OtherBenefitsSource
    )

  ncb_subs[is.na(ncb_subs)] <- 0

  ncb_subs <- ncb_subs %>%
    dplyr::full_join(IncomeBenefits[c("PersonalID",
                                              "EnrollmentID",
                                              "DataCollectionStage",
                                              "BenefitsFromAnySource")],
                     by = c("PersonalID",
                                    "EnrollmentID",
                                    "DataCollectionStage"))

  ncb_subs <- served_in_date_range %>%
    dplyr::filter(ProjectName != "Unsheltered Clients - OUTREACH") %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      PersonalID,
      EnrollmentID,
      HouseholdID,
      AgeAtEntry,
      ProjectName,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      ProjectType,
      DataCollectionStage,
      BenefitsFromAnySource,
      SNAP,
      WIC,
      TANFChildCare,
      TANFTransportation,
      OtherTANF,
      OtherBenefitsSource,
      UserCreating
    ) %>%
    dplyr::mutate(
      BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation +
        OtherTANF + OtherBenefitsSource
    ) %>%
    dplyr::select(PersonalID,
                  EnrollmentID,
                  DataCollectionStage,
                  BenefitsFromAnySource,
                  BenefitCount) %>%
    unique()

  missing_ncbs_entry <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(AgeAtEntry,
                  dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  BenefitsFromAnySource) %>%
    dplyr::filter(
      DataCollectionStage == 1 &
        (AgeAtEntry > 17 |
           is.na(AgeAtEntry)) &
        (BenefitsFromAnySource == 99 |
           is.na(BenefitsFromAnySource))
    ) %>%
    dplyr::mutate(Issue = "Non-cash Benefits Missing at Entry",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  conflicting_ncbs_entry <- served_in_date_range %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(AgeAtEntry,
                  dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  BenefitsFromAnySource,
                  BenefitCount) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((BenefitsFromAnySource == 1 &
                        BenefitCount == 0) |
                       (BenefitsFromAnySource == 0 &
                          BenefitCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Non-cash Benefits yes/no at Entry",
                  Type = "Error",
                  Guidance = guidance$conflicting_ncbs) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  # Unlikely NCBs -----------------------------------------------------------

  unlikely_ncbs_entry <- served_in_date_range %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      AgeAtEntry,
      dplyr::all_of(vars$prep),
      DataCollectionStage,
      BenefitsFromAnySource,
      BenefitCount
    ) %>%
    dplyr::filter(DataCollectionStage == 1 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    (BenefitCount == 6)) %>%
    dplyr::mutate(Issue = "Client has ALL SIX Non-cash Benefits at Entry",
                  Type = "Warning",
                  Guidance = "This client has every single Non-Cash Benefit,
             according to HMIS, which is highly unlikely. Please correct (unless
             it's actually true).") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Missing NCBs at Exit ----------------------------------------------------
  missing_ncbs_exit <- served_in_date_range %>%
    dplyr::left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(AgeAtEntry,
                  dplyr::all_of(vars$prep),
                  DataCollectionStage,
                  BenefitsFromAnySource) %>%
    dplyr::filter(
      DataCollectionStage == 3 &
        (AgeAtEntry > 17 |
           is.na(AgeAtEntry)) &
        (BenefitsFromAnySource == 99 |
           is.na(BenefitsFromAnySource))
    ) %>%
    dplyr::mutate(Issue = "Non-cash Benefits Missing at Exit",
                  Type = "Error",
                  Guidance = guidance$missing_at_exit) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  conflicting_ncbs_exit <- served_in_date_range %>%
    dplyr::left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::select(
      AgeAtEntry,
      dplyr::all_of(vars$prep),
      DataCollectionStage,
      BenefitsFromAnySource,
      BenefitCount
    ) %>%
    dplyr::filter(DataCollectionStage == 3 &
                    (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
                    ((BenefitsFromAnySource == 1 &
                        BenefitCount == 0) |
                       (BenefitsFromAnySource == 0 &
                          BenefitCount > 0)
                    )) %>%
    dplyr::mutate(Issue = "Conflicting Non-cash Benefits yes/no at Exit",
                  Type = "Error",
                  Guidance = guidance$conflicting_ncbs) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(ncb_subs)

  # SSI/SSDI but no Disability (Q) ------------------------------------------
  smallIncome <- IncomeBenefits %>%
    dplyr::select(EnrollmentID, PersonalID, SSI, SSDI)

  check_disability_ssi <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  AgeAtEntry,
                  DisablingCondition) %>%
    dplyr::left_join(smallIncome, by = c("EnrollmentID", "PersonalID")) %>%
    dplyr::mutate(SSI = dplyr::if_else(is.na(SSI), 0, SSI),
                  SSDI = dplyr::if_else(is.na(SSDI), 0, SSDI)) %>%
    dplyr::filter(SSI + SSDI > 0 &
                    DisablingCondition == 0 & AgeAtEntry > 17) %>%
    dplyr::select(-DisablingCondition, -SSI, -SSDI, -AgeAtEntry) %>%
    unique() %>%
    dplyr::mutate(
      Issue = "Client with No Disability Receiving SSI/SSDI (could be ok)",
      Type = "Warning",
      Guidance = "If a client is receiving SSI or SSDI for THEIR OWN disability,
        that disability should be indicated in the Disabilities data elements. If
        an adult is receiving SSI or SSDI benefits on behalf a minor child,
        then there is no action needed."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  rm(smallIncome)

  # Non HoHs w Svcs or Referrals --------------------------------------------
  # SSVF projects should be showing this as an Error,7 whereas non-SSVF projects
  # should be showing it as a warning, and only back to Feb of 2019
  services_on_hh_members <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  RelationshipToHoH,
                  GrantType) %>%
    dplyr::filter(
      RelationshipToHoH != 1 &
        EntryDate >= hc$no_more_svcs_on_hh_members &
        (GrantType != "SSVF" | is.na(GrantType))
    ) %>%
    dplyr::semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::mutate(Issue = "Service Transaction on a Non Head of Household",
                  Type = "Warning",
                  Guidance = guidance$service_on_non_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  services_on_hh_members_ssvf <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  EnrollmentID,
                  RelationshipToHoH,
                  GrantType) %>%
    dplyr::filter(RelationshipToHoH != 1 &
                    GrantType == "SSVF") %>%
    dplyr::semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
    dplyr::mutate(Issue = "Service Transaction on a Non Head of Household (SSVF)",
                  Type = "Error",
                  Guidance = guidance$service_on_non_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))




  referrals_on_hh_members_ssvf <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  RelationshipToHoH,
                  EnrollmentID,
                  GrantType) %>%
    dplyr::filter(RelationshipToHoH != 1 &
                    GrantType == "SSVF") %>%
    dplyr::semi_join(Referrals, by = c("PersonalID")) %>%
    dplyr::mutate(Issue = "Referral on a Non Head of Household (SSVF)",
                  Type = "Error",
                  Guidance = guidance$referral_on_non_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Stray Services (fall outside EE) ----------------------------------------
  # Because a lot of these records are stray Services due to there being no
  # Entry Exit at all, this can't be shown in the same data set as all the other
  # errors. I'm going to have to make this its own thing. :(
  # stray_services_warning <- dq_stray_services(stray_services)

  # AP No Recent Referrals --------------------------------------------------
  co_APs <- Project %>%
    dplyr::filter(ProjectType == 14 & ProjectID != 2372) %>% # not incl Mah CE
    dplyr::select(
      ProjectID,
      OperatingStartDate,
      OperatingEndDate,
      ProjectName,
      HMISParticipatingProject,
      ProjectCounty
    )

  aps_no_referrals <- Referrals %>%
    dplyr::right_join(co_APs, by = c("ReferringProjectID" = "ProjectID")) %>%
    dplyr::filter(is.na(PersonalID)) %>%
    dplyr::select(ReferringProjectID) %>%
    unique()

  aps_with_referrals <- Referrals %>%
    dplyr::right_join(co_APs, by = c("ReferringProjectID" = "ProjectID")) %>%
    dplyr::filter(!is.na(PersonalID)) %>%
    dplyr::select(ReferringProjectID) %>%
    unique()

  data_APs <- dplyr::data.frame(
    category = c("No Referrals", "Has Created Referrals"),
    count = c(nrow(aps_no_referrals), nrow(aps_with_referrals)),
    providertype = rep("Access Points"),
    total = rep(c(
      nrow(aps_no_referrals) + nrow(aps_with_referrals)
    )),
    stringsAsFactors = FALSE
  )

  data_APs <- data_APs %>%
    dplyr::mutate(percent = count / total,
                  prettypercent = scales::percent(count / total))

  dq_plot_aps_referrals <-
    ggplot2::ggplot(data_APs, ggplot2::aes(fill = category, x = providertype, y = percent)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity",
                      width = .1) +
    ggplot2::geom_label(
      ggplot2::aes(label = paste(
        data_APs$category,
        "\n",
        data_APs$prettypercent
      )),
      position = ggplot2::position_stack(),
      vjust = 2,
      fill = "white",
      colour = "black",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = c("#00952e", "#a11207"), guide = FALSE) +
    ggplot2::theme_void()



  rm(aps_with_referrals, co_APs)

  # AP entering project stays -----------------------------------------------

  aps_with_ees <- served_in_date_range %>%
    dplyr::filter(ProjectType == 14 & !ProjectID %in% c(2372, 1858)) %>% # not incl Mah CE
    dplyr::mutate(
      Issue = "Access Point with Entry Exits",
      Type = "High Priority",
      Guidance = "Access Points should only be entering Referrals and Diversion Services
      into the AP provider- not Entry Exits. If a user has done this, the Entry
      Exit should be deleted. Please see the
      <a href=\"http://hmis.cohhio.org/index.php?pg=kb.page&id=151\"
          target=\"_blank\">Coordinated Entry workflow</a>."
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # Side Door ---------------------------------------------------------------
  # use Referrals, get logic from ART report- it's pretty lax I think


  # Old Outstanding Referrals -----------------------------------------------
  # CW says ReferringProjectID should work instead of Referred-From Provider
  # Using ReferringProjectID instead. Either way, I feel this should go in the
  # Provider Dashboard, not the Data Quality report.
# TODO Refresh this once ReferralOutcome is figured out in Clarity  2021-09-03
#internal_old_outstanding_referrals <- dq_internal_old_outstanding_referrals(served_in_date_range, Referrals, vars)

  # ^^this is pulling in neither the Unsheltered NOR referrals from APs

  staging_outstanding_referrals <-
    internal_old_outstanding_referrals %>%
    dplyr::left_join(Project[c("ProjectName", "ProjectID")], by = "ProjectName") %>%
    dplyr::select(ProjectName, ProjectID, PersonalID) %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(Open_Referrals = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(Open_Referrals)) %>%
    dplyr::mutate(Project = paste0(ProjectName, ":", ProjectID))

  dq_plot_outstanding_referrals <-
    ggplot2::ggplot(
      head(staging_outstanding_referrals, 20L),
      ggplot2::aes(
        x = stats::reorder(Project, Open_Referrals),
        y = Open_Referrals,
        fill = Open_Referrals
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Referrals") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)


  # Unsheltered Incorrect Residence Prior -----------------------------------
  unsheltered_enrollments <- served_in_date_range %>%
    dplyr::filter(ProjectID == 1695) %>%
    dplyr::select(
      dplyr::all_of(vars$prep),
      RelationshipToHoH,
      LivingSituation,
      AgeAtEntry,
      Destination,
      CountyServed,
      ProjectCounty,
      LivingSituation
    )

  # TODO Remove for Clarity users
  unsheltered_not_unsheltered <- unsheltered_enrollments %>%
    dplyr::filter(LivingSituation != 16) %>%
    dplyr::mutate(
      Type = "High Priority",
      Issue = "Wrong Provider (Not Unsheltered)",
      Guidance = "Clients who were incorrectly entered into the Unsheltered
          provider should be exited. Otherwise, correct the data. Please review
          the <a href=\"https://www.youtube.com/watch?v=qdmrqOHXoN0&t=174s\"
          target=\"_blank\">data entry portion of the Unsheltered video training</a>
          for more info.",
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


  # Unsheltered New Entries by County by Month ------------------------------

  unsheltered_by_month <- unsheltered_enrollments %>%
    dplyr::left_join(Users, by = "UserCreating") %>%
    dplyr::mutate(ExitAdjust = dplyr::if_else(is.na(ExitDate), lubridate::today(), ExitDate),
                  County = dplyr::if_else(is.na(CountyServed), UserCounty, CountyServed),
                  EntryDateDisplay = format.Date(EntryDate, "%b %Y")) %>%
    dplyr::select(EntryDate, EntryDateDisplay, HouseholdID, County)


  # SSVF --------------------------------------------------------------------

  ssvf_served_in_date_range <- Enrollment %>%
    dplyr::select(
      EnrollmentID,
      HouseholdID,
      PersonalID,
      ProjectName,
      ProjectType,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      UserCreating,
      RelationshipToHoH,
      PercentAMI,
      LastPermanentStreet,
      LastPermanentCity,
      LastPermanentState,
      LastPermanentZIP,
      AddressDataQuality,
      VAMCStation,
      HPScreeningScore,
      ThresholdScore,
      IraqAfghanistan,
      FemVet
    ) %>%
    dplyr::right_join(
      served_in_date_range %>%
        dplyr::filter(GrantType == "SSVF") %>%
        dplyr::select(PersonalID, EnrollmentID, HouseholdID, ProjectRegion),
      by = c("PersonalID", "EnrollmentID", "HouseholdID")
    ) %>%
    dplyr::left_join(
      Client %>%
        dplyr::select(
          PersonalID,
          VeteranStatus,
          YearEnteredService,
          YearSeparated,
          WorldWarII,
          KoreanWar,
          VietnamWar,
          DesertStorm,
          AfghanistanOEF,
          IraqOIF,
          IraqOND,
          OtherTheater,
          MilitaryBranch,
          DischargeStatus
        ),
      by = "PersonalID"
    )

  veteran_missing_year_entered <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(YearEnteredService) ~ "Missing Year Entered Service",
        YearEnteredService > lubridate::year(lubridate::today()) ~ "Incorrect Year Entered Service"),
      Type = "Error",
      Guidance = guidance$missing_at_entry
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_year_separated <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(YearSeparated) ~ "Missing Year Separated",
        YearSeparated > lubridate::year(lubridate::today()) ~ "Incorrect Year Separated"),
      Type = "Error",
      Guidance = guidance$missing_at_entry
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_wars <- ssvf_served_in_date_range %>%
    dplyr::filter(
      VeteranStatus == 1 &
        (
          is.na(WorldWarII) | WorldWarII == 99 |
            is.na(KoreanWar) | KoreanWar == 99 |
            is.na(VietnamWar) | VietnamWar == 99 |
            is.na(DesertStorm) | DesertStorm == 99 |
            is.na(AfghanistanOEF) | AfghanistanOEF == 99 |
            is.na(IraqOIF) | IraqOIF == 99 |
            is.na(IraqOND) | IraqOND == 99 |
            is.na(OtherTheater) |
            OtherTheater == 99
        )
    ) %>%
    dplyr::mutate(Issue = "Missing War(s)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_branch <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1 &
                    is.na(MilitaryBranch)) %>%
    dplyr::mutate(Issue = "Missing Military Branch",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_discharge_status <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1 & is.na(DischargeStatus)) %>%
    dplyr::mutate(Issue = "Missing Discharge Status",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dkr_client_veteran_info <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        WorldWarII %in% c(8, 9) |
          KoreanWar %in% c(8, 9) |
          VietnamWar %in% c(8, 9) |
          DesertStorm  %in% c(8, 9) |
          AfghanistanOEF %in% c(8, 9) |
          IraqOIF %in% c(8, 9) |
          IraqOND %in% c(8, 9) |
          OtherTheater  %in% c(8, 9)  ~ "Don't Know/Refused War(s)",
        MilitaryBranch %in% c(8, 9) ~ "Missing Military Branch",
        DischargeStatus %in% c(8, 9) ~ "Missing Discharge Status"
      ),
      Type = "Warning",
      Guidance = guidance$dkr_data
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  ssvf_missing_percent_ami <- ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    is.na(PercentAMI)) %>%
    dplyr::mutate(Issue = "Missing Percent AMI",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  ssvf_missing_vamc <- ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    is.na(VAMCStation)) %>%
    dplyr::mutate(Issue = "Missing VAMC Station Number",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  ssvf_missing_address <- ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    (
                      is.na(LastPermanentStreet) |
                        is.na(LastPermanentCity) |
                        # is.na(LastPermanentState) | # still not fixed in export
                        is.na(LastPermanentZIP)
                    )) %>%
    dplyr::mutate(Issue = "Missing Some or All of Last Permanent Address",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  # TEMPORARILY NOT REQUIRED FOR COVID-19 REASONS
  # ssvf_hp_screen <- ssvf_served_in_date_range %>%
  #   filter(ProjectType == 12 &
  #            RelationshipToHoH == 1 &
  #            (is.na(HPScreeningScore) |
  #               is.na(ThresholdScore))) %>%
  #   mutate(Issue = "Missing HP Screening or Threshold Score",
  #          Type = "Error",
  #          Guidance = guidance$missing_at_entry) %>%
  #   select(all_of(vars$we_want))


  # All together now --------------------------------------------------------


  dq_main <- rbind(
    aps_with_ees,
    check_disability_ssi,
    check_eligibility,
    conflicting_disabilities,
    conflicting_health_insurance_entry,
    conflicting_health_insurance_exit,
    conflicting_income_entry,
    conflicting_income_exit,
    conflicting_ncbs_entry,
    conflicting_ncbs_exit,
    differing_manufacturers,
    dkr_client_veteran_info,
    dkr_destination,
    dkr_living_situation,
    dkr_LoS,
    dkr_months_times_homeless,
    dkr_residence_prior,
    dose_date_error,
    dose_date_warning,
    dq_dob,
    dq_ethnicity,
    dq_gender,
    dq_name,
    dq_overlaps %>% dplyr::select(-PreviousProject),
    dq_race,
    dq_ssn,
    dq_veteran,
    duplicate_ees,
    entered_ph_without_spdat,
    extremely_long_stayers,
    future_ees,
    future_exits,
    hh_issues,
    incorrect_ee_type,
    incorrect_path_contact_date,
    internal_old_outstanding_referrals,
    invalid_months_times_homeless,
    lh_without_spdat,
    mahoning_ce_60_days,
    maybe_psh_destination,
    # maybe_rrh_destination,
    missing_approx_date_homeless,
    missing_client_location,
    missing_county_served,
    missing_county_prior,
    missing_destination,
    missing_disabilities,
    missing_health_insurance_entry,
    missing_health_insurance_exit,
    missing_income_entry,
    missing_income_exit,
    # missing_interims,
    missing_living_situation,
    missing_LoS,
    missing_months_times_homeless,
    missing_path_contact,
    missing_previous_street_ESSH,
    missing_ncbs_entry,
    missing_ncbs_exit,
    missing_residence_prior,
    missing_vaccine_current,
    missing_vaccine_exited,
    no_bos_rrh,
    no_bos_psh,
    no_bos_th,
    no_bos_sh,
    path_enrolled_missing,
    path_missing_los_res_prior,
    path_no_status_at_exit,
    path_reason_missing,
    path_SOAR_missing_at_exit,
    path_status_determination,
    referrals_on_hh_members,
    referrals_on_hh_members_ssvf,
    rent_paid_no_move_in,
    services_on_hh_members,
    services_on_hh_members_ssvf,
    should_be_psh_destination,
    should_be_rrh_destination,
    should_be_th_destination,
    should_be_sh_destination,
    spdat_on_non_hoh,
    ssvf_missing_address,
    ssvf_missing_vamc,
    ssvf_missing_percent_ami,
    # ssvf_hp_screen,
    unknown_manufacturer_error,
    unknown_manufacturer_warning,
    unlikely_ncbs_entry,
    veteran_missing_year_entered,
    veteran_missing_year_separated,
    veteran_missing_wars,
    veteran_missing_branch,
    veteran_missing_discharge_status
  ) %>%
    dplyr::filter(!ProjectName %in% c(
      "Diversion from Homeless System",
      "Unsheltered Clients - OUTREACH"
    ))

  dq_main <- dq_main %>%
    unique()   %>%
    dplyr::mutate(Type = factor(Type, levels = c("High Priority",
                                                         "Error",
                                                         "Warning")))

  # filtering out AP errors that are irrlevant to APs

  dq_main <- dq_main %>%
    dplyr::filter(ProjectType != 14 |
                    (
                      ProjectType == 14 &
                        Issue %in% c(
                          "60 Days in Mahoning Coordinated Entry",
                          "Access Point with Entry Exits",
                          "Missing Date of Birth Data Quality",
                          "Don't Know/Refused or Approx. Date of Birth",
                          "Missing DOB",
                          "Missing Name Data Quality",
                          "Incomplete or Don't Know/Refused Name",
                          "Rent Payment Made, No Move-In Date",
                          "Invalid SSN",
                          "Don't Know/Refused SSN",
                          "Missing SSN",
                          "Missing Veteran Status",
                          "Don't Know/Refused Veteran Status",
                          "Missing County Served"
                        )
                    ))

  # # Waiting on Something ----------------------------------------------------
  #
  #     dq_main <- dq_main %>%
  #       filter(
  #         !Issue %in% c(
  #           "Missing PATH Contact", # waiting on AW comments
  #           "No Contact End Date (PATH)", # waiting on AW comments
  #           "No PATH Contact Entered at Entry" # waiting on AW comments
  #         )
  #       )

  # Unsheltered DQ ----------------------------------------------------------

  dq_unsheltered <- rbind(
    check_disability_ssi,
    dkr_destination,
    dkr_months_times_homeless,
    dkr_residence_prior,
    dkr_LoS,
    dq_dob,
    dq_ethnicity,
    dq_race,
    dq_gender,
    dq_name,
    dq_overlaps %>% dplyr::select(-PreviousProject),
    duplicate_ees,
    future_ees,
    future_exits,
    hh_issues,
    incorrect_ee_type,
    internal_old_outstanding_referrals,
    lh_without_spdat,
    maybe_psh_destination,
    # maybe_rrh_destination,
    missing_approx_date_homeless,
    missing_destination,
    missing_county_served,
    missing_LoS,
    missing_months_times_homeless,
    missing_residence_prior,
    no_bos_rrh,
    no_bos_psh,
    no_bos_th,
    no_bos_sh,
    referrals_on_hh_members,
    should_be_psh_destination,
    should_be_rrh_destination,
    should_be_th_destination,
    should_be_sh_destination,
    spdat_on_non_hoh,
    unsheltered_not_unsheltered,
    unsheltered_long_not_referred
  ) %>%
    dplyr::filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
    dplyr::left_join(Users, by = "UserCreating") %>%
    dplyr::select(-UserID,-UserName,-ProjectRegion) %>%
    dplyr::filter(
      UserCounty != "Franklin" &
        !Issue %in% c(
          "Conflicting Health Insurance yes/no at Entry",
          "Conflicting Health Insurance yes/no at Exit",
          "Conflicting Income yes/no at Entry",
          "Conflicting Income yes/no at Exit",
          "Conflicting Non-cash Benefits yes/no at Entry",
          "Conflicting Non-cash Benefits yes/no at Exit",
          "Health Insurance Missing at Entry",
          "Health Insurance Missing at Exit",
          "Income Missing at Entry",
          "Income Missing at Exit",
          "Non-cash Benefits Missing at Entry",
          "Non-cash Benefits Missing at Exit"
        )
    )

  dq_unsheltered <- dq_unsheltered %>%
    dplyr::mutate(
      Type = dplyr::if_else(Issue == "Missing County Served", "High Priority", Type),
      Type = factor(Type, levels = c("High Priority",
                                             "Error",
                                             "Warning"))
    )

  # Controls what is shown in the CoC-wide DQ tab ---------------------------

  # for CoC-wide DQ tab

  dq_past_year <- dq_main %>%
    dplyr::filter(HMIS::served_between(., hc$check_dq_back_to,
                                       lubridate::today())) %>%
    dplyr::left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")

  # for project evaluation reporting

  dq_for_pe <- dq_main %>%
    dplyr::filter(HMIS::served_between(., hc$project_eval_start, hc$project_eval_end)) %>%
    dplyr::left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")


  dq_providers <- sort(projects_current_hmis$ProjectName)

  # Plots -------------------------------------------------------------------

  dq_data_errors_plot <- dq_past_year %>%
    dplyr::filter(
      Type %in% c("Error", "High Priority") &
        !Issue %in% c(
          "No Head of Household",
          "Missing Relationship to Head of Household",
          "Too Many Heads of Household",
          "Children Only Household"
        )
    ) %>%
    dplyr::select(PersonalID, ProjectID, ProjectName) %>%
    unique() %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(clientsWithErrors = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(clientsWithErrors))

  dq_data_errors_plot$hover <-
    with(dq_data_errors_plot,
         paste0(ProjectName, ":", ProjectID))

  dq_plot_projects_errors <-
    ggplot2::ggplot(
      head(dq_data_errors_plot, 20L),
      ggplot2::aes(
        x = stats::reorder(hover, clientsWithErrors),
        y = clientsWithErrors,
        fill = clientsWithErrors
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_warnings_plot <- dq_past_year %>%
    dplyr::filter(Type == "Warning") %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(Warnings = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(Warnings))

  dq_data_warnings_plot$hover <-
    with(dq_data_warnings_plot,
         paste0(ProjectName, ":", ProjectID))

  dq_plot_projects_warnings <-
    ggplot2::ggplot(head(dq_data_warnings_plot, 20L),
                    ggplot2::aes(
                      x = stats::reorder(hover, Warnings),
                      y = Warnings,
                      fill = Warnings
                    )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_error_types <- dq_past_year %>%
    dplyr::filter(Type %in% c("Error", "High Priority")) %>%
    dplyr::group_by(Issue) %>%
    dplyr::summarise(Errors = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(Errors))

  dq_plot_errors <-
    ggplot2::ggplot(head(dq_data_error_types, 10L),
                    ggplot2::aes(
                      x = stats::reorder(Issue, Errors),
                      y = Errors,
                      fill = Errors
                    )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_warning_types <- dq_past_year %>%
    dplyr::filter(Type == "Warning") %>%
    dplyr::group_by(Issue) %>%
    dplyr::summarise(Warnings = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(Warnings))

  dq_plot_warnings <-
    ggplot2::ggplot(head(dq_data_warning_types, 10L),
                    ggplot2::aes(
                      x = stats::reorder(Issue, Warnings),
                      y = Warnings,
                      fill = Warnings
                    )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_unsheltered_high <- dq_unsheltered %>%
    dplyr::filter(Type == "High Priority",
                  HMIS::served_between(., hc$unsheltered_data_start, meta_HUDCSV$Export_End)) %>%
    dplyr::select(PersonalID, HouseholdID, DefaultProvider) %>%
    unique() %>%
    dplyr::group_by(DefaultProvider) %>%
    dplyr::summarise(clientsWithErrors = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(clientsWithErrors))

  dq_plot_unsheltered_high <-
    ggplot2::ggplot(
      head(dq_data_unsheltered_high, 20L),
      ggplot2::aes(
        x = stats::reorder(DefaultProvider, clientsWithErrors),
        y = clientsWithErrors,
        fill = clientsWithErrors
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_hh_issues_plot <- dq_past_year %>%
    dplyr::filter(
      Type %in% c("Error", "High Priority") &
        Issue %in% c(
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Too Many Heads of Household",
          "Children Only Household"
        )
    ) %>%
    dplyr::select(PersonalID, ProjectID, ProjectName) %>%
    unique() %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(Households = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(Households))

  dq_data_hh_issues_plot$hover <-
    with(dq_data_hh_issues_plot,
         paste0(ProjectName, ":", ProjectID))

  dq_plot_hh_errors <-
    ggplot2::ggplot(head(dq_data_hh_issues_plot, 20L),
                    ggplot2::aes(
                      x = stats::reorder(hover, Households),
                      y = Households,
                      fill = Households
                    )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_outstanding_referrals_plot <- dq_past_year %>%
    dplyr::filter(Issue == "Old Outstanding Referral") %>%
    dplyr::select(PersonalID, ProjectID, ProjectName) %>%
    unique() %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(Households = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(Households)) %>%
    dplyr::mutate(hover = paste(ProjectName, ":", ProjectID))

  dq_plot_projects_outstanding_referrals <-
    ggplot2::ggplot(
      head(dq_data_outstanding_referrals_plot, 20L),
      ggplot2::aes(
        x = stats::reorder(hover, Households),
        y = Households,
        fill = Households
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_eligibility_plot <- dq_past_year %>%
    dplyr::filter(Type == "Warning" &
                    Issue %in% c("Check Eligibility")) %>%
    dplyr::select(PersonalID, ProjectID, ProjectName) %>%
    unique() %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(Households = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(Households))

  dq_data_eligibility_plot$hover <-
    with(dq_data_eligibility_plot,
         paste0(ProjectName, ":", ProjectID))

  dq_plot_eligibility <-
    ggplot2::ggplot(
      head(dq_data_eligibility_plot, 20L),
      ggplot2::aes(
        x = stats::reorder(hover, Households),
        y = Households,
        fill = Households
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  dq_data_without_spdat_plot <- dq_past_year %>%
    dplyr::filter(
      Type == "Warning" &
        Issue %in% c(
          "Non-DV HoHs Entering PH or TH without SPDAT",
          "HoHs in shelter for 8+ days without SPDAT"
        )
    ) %>%
    dplyr::select(PersonalID, ProjectID, ProjectName) %>%
    unique() %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(Households = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ProjectDisplay = paste0(ProjectName, ":", ProjectID)) %>%
    dplyr::arrange(dplyr::desc(Households))

  dq_plot_hh_no_spdat <-
    ggplot2::ggplot(
      head(dq_data_without_spdat_plot, 20L),
      ggplot2::aes(
        x = stats::reorder(ProjectDisplay, Households),
        y = Households,
        fill = Households
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

  # Clean up the house ------------------------------------------------------

  rm(
    aps_with_ees,
    check_disability_ssi,
    check_eligibility,
    conflicting_disabilities,
    conflicting_health_insurance_entry,
    conflicting_health_insurance_exit,
    conflicting_income_entry,
    conflicting_income_exit,
    conflicting_ncbs_entry,
    conflicting_ncbs_exit,
    # detail_eligibility, # the app needs this; keep this commented out
    detail_missing_disabilities,
    dkr_living_situation,
    dkr_months_times_homeless,
    dkr_residence_prior,
    dkr_destination,
    dkr_LoS,
    dkr_client_veteran_info,
    dq_data_unsheltered_high,
    dq_dob,
    dq_ethnicity,
    dq_gender,
    dq_name,
    dq_race,
    dq_ssn,
    dq_veteran,
    duplicate_ees,
    entered_ph_without_spdat,
    extremely_long_stayers,
    future_ees,
    future_exits,
    hh_issues,
    incorrect_ee_type,
    incorrect_path_contact_date,
    missing_path_contact,
    internal_old_outstanding_referrals,
    lh_without_spdat,
    missing_approx_date_homeless,
    missing_client_location,
    missing_county_prior,
    missing_county_served,
    missing_destination,
    missing_disabilities,
    missing_health_insurance_entry,
    missing_health_insurance_exit,
    missing_income_entry,
    missing_income_exit,
    invalid_months_times_homeless,
    missing_living_situation,
    missing_LoS,
    missing_months_times_homeless,
    missing_ncbs_entry,
    missing_ncbs_exit,
    missing_previous_street_ESSH,
    missing_residence_prior,
    path_enrolled_missing,
    path_missing_los_res_prior,
    path_no_status_at_exit,
    path_reason_missing,
    path_SOAR_missing_at_exit,
    path_status_determination,
    projects_current_hmis,
    referrals_on_hh_members,
    referrals_on_hh_members_ssvf,
    rent_paid_no_move_in,
    served_in_date_range,
    services_on_hh_members,
    services_on_hh_members_ssvf,
    smallProject,
    spdat_on_non_hoh,
    ssvf_missing_address,
    ssvf_missing_vamc,
    ssvf_missing_percent_ami,
    ssvf_served_in_date_range,
    staging_outstanding_referrals,
    stray_services_warning,
    unlikely_ncbs_entry,
    unsheltered_enrollments,
    unsheltered_not_unsheltered,
    unsheltered_long_not_referred,
    va_funded,
    vars$prep,
    vars$we_want,
    veteran_missing_year_entered,
    veteran_missing_year_separated,
    veteran_missing_wars,
    veteran_missing_branch,
    veteran_missing_discharge_status
  )
  rm(list = ls(pattern = "dq_data_"))
  rm(list = ls(pattern = "guidance$"))

  # WARNING save.image does not save the environment properly, save must be used.
app_env$gather_deps()
app_env
}
