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

# PLEASE NOTE THIS SCRIPT OVERWRITES THE CLIENT.CSV FILE ON YOUR HARD DRIVE!
# IT REPLACES THE NAMES AND SSNS WITH DATA QUALITY SIGNIFIERS!
# IT CAN BE RUN ON A CLEAN CLIENT.CSV FILE OR ONE THAT'S BEEN OVERWRITTEN.

#' @title Redact PII from Client HUD Export
#' @description This redacts all PII (except DOB) from Client HUD Export
#' @param Client \code{(tibble)} Client HUD Export
#' @return \code{(tibble)} Redacted Client HUD Export
#' @export

Client_redact <- function(Client) {
  Client %>%
    # our fake Client IDs are 5 and 4216
    dplyr::filter(!PersonalID %in% c(5, 4216)) %>%
    dplyr::mutate(
      FirstName = dplyr::case_when(
        NameDataQuality %in% c(8, 9) ~ "DKR",
        NameDataQuality == 2 ~ "Partial",
        NameDataQuality == 99 |
          is.na(NameDataQuality) |
          FirstName == "Anonymous" ~ "Missing",
        !(
          NameDataQuality %in% c(2, 8, 9, 99) |
            is.na(NameDataQuality) |
            FirstName == "Anonymous"
        ) ~ "ok"
      ),
      LastName = NULL,
      MiddleName = NULL,
      NameSuffix = NULL,
      SSN = dplyr::case_when(
        (is.na(SSN) & !SSNDataQuality %in% c(8, 9)) |
          is.na(SSNDataQuality) | SSNDataQuality == 99 ~ "Missing",
        SSNDataQuality %in% c(8, 9) ~ "DKR",
        (nchar(SSN) != 9 & SSNDataQuality != 2) |
          substr(SSN, 1, 3) %in% c("000", "666") |
          substr(SSN, 1, 1) == 9 |
          substr(SSN, 4, 5) == "00" |
          substr(SSN, 6, 9) == "0000" |
          SSNDataQuality == 2 |
          SSN %in% c(
            111111111,
            222222222,
            333333333,
            444444444,
            555555555,
            777777777,
            888888888,
            123456789
          ) ~ "Invalid",
        SSNDataQuality == 2 & nchar(SSN) != 9 ~ "Incomplete"
      )
    ) %>%
    dplyr::mutate(SSN = dplyr::case_when(is.na(SSN) ~ "ok",!is.na(SSN) ~ SSN))
}

load_export <- function(clarity_api = get0("clarity_api", envir = rlang::caller_env()),
                        app_env = get0("app_env", envir = rlang::caller_env()),
                        error = FALSE) {
  # Service Areas -----------------------------------------------------------
  ServiceAreas <- clarity.looker::hud_load("ServiceAreas.feather", dirs$public)

  # Affiliation -------------------------------------------------------------

  Affiliation <- clarity_api$Affiliation()

  # Client ------------------------------------------------------------------

  Client <- clarity_api$Client()
  # this saves Client as a feather file with redacted PII as a security measure.
  if(ncol(Client) == 36) {
    Client <- Client_redact(Client)
    clarity.looker::hud_feather(Client, dirs$export)
  }










  # CurrentLivingSituation <-
  #   read_csv(paste0(directory, "/CurrentLivingSituation.csv"),
  #             col_types = "nnnTncnnnnncTTcTc") DON'T NEED YET

  # Disabilities ------------------------------------------------------------

  Disabilities <- clarity_api$Disabilities()


  # EmploymentEducation -----------------------------------------------------

  EmploymentEducation <- clarity_api$EmploymentEducation()

  # Exit --------------------------------------------------------------------

  # Already loaded in 00_dates.R

  # Project -----------------------------------------------------------------

  Project <- clarity_api$Project()

  # geocodes is created by `hud.extract` using the hud_geocodes.R functions
  geocodes <- hud_load("geocodes", dirs$public)

  # provider_extras ----
  # Rminor: Coordinated Entry Access Points [CEAP]
  # Thu Aug 12 14:23:50 2021
  provider_extras <- clarity_api$Project_extras() |>
    # This assumes that the Description for the file maintains an up to date list of column names separated by ,\\s
    setNames(nm = stringr::str_split(clarity_api$Project_extras(details = TRUE)$description,  "\\,\\s")[[1]])
  # This should map a county to every geocode
  provider_extras <- provider_extras |>
    dplyr::left_join(geocodes |> dplyr::select(GeographicCode, County), by = c(Geocode = "GeographicCode"))

  # Some geocodes may be legacy and County will be NA - the following looks these geocodes up on the Google Geocode API via `ggmap`
fill_geocodes <- provider_extras |>
  dplyr::filter(is.na(County)) |>
  dplyr::distinct(Geocode, .keep_all = TRUE)
if (nrow(fill_geocodes) > 0) {
  # This environment variable must be set in the .Renviron file (at the project level preferably). Be sure to add .Renviron to .gitignore/.Rbuildignore if the file resides in the project directory
  ggmap::register_google(key = Sys.getenv("GGMAP_GOOGLE_API_KEY"))
  fill_geocodes <- slider::slide_dfr(fill_geocodes, ~{
    r <- purrr::keep(.x, ~!is.na(.x))
    .args <- purrr::list_modify(r[names(r) %in% c("Address", "City", "State", "ZIP")], State = "OH")
    out <- ggmap::geocode(glue::glue_data(.args, "{Address}, {City}, {State}, {ZIP}"), output = "all")
    .county <- purrr::keep(out$results[[1]]$address_components, ~any(stringr::str_detect(purrr::flatten(.x), "County")))[[1]]$short_name |>
      stringr::str_remove("\\sCounty")
    .x$County <- purrr::when(.county,
                UU::is_legit(.) ~ .county,
                ~ NA)
      .x
  })

  for (rn in 1:nrow(fill_geocodes)) {
    row <- fill_geocodes[rn,]
    geocodes <- geocodes |>
      tibble::add_row(GeographicCode = row$Geocode, State = "OH", County = row$County)
  }
  feather::write_feather(geocodes, hud_filename("geocodes", dirs$public))
  provider_extras <- provider_extras |>
    dplyr::left_join(geocodes, by = c(Geocode = "GeographicCode"))
}

provider_extras <- provider_extras |>
  dplyr::left_join(hud_load("Regions", dirs$public), by = "County")

# Missing Regions
missing_region <- provider_extras |>
  dplyr::filter(is.na(Region))
missing_region |>
  dplyr::pull(County) |>
  unique()
# TODO Handle missing regions by using Counties Served columns to determine which Regions it falls into.

CEAPs <- provider_extras |>
  dplyr::filter(Type == "Coordinated Entry") |>
  tidyr::pivot_longer(tidyselect::starts_with("AP"), names_to = "TargetPop", names_pattern = "(?<=^APCounties)(\\w+)", values_to = "CountiesServed")
  # Missing County Served data
# CEAPs |>
#   dplyr::group_by(ProjectID, Name) |>
#   dplyr::summarise(Missing_County = sum(is.na(`County/ies`))) |>
#   dplyr::filter(Missing_County > 2)


  coc_scoring <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"),
                                   sheet = 13,
                                   col_types = c("numeric",
                                                 "numeric",
                                                 "numeric",
                                                 "numeric",
                                                 "numeric"))
  #COMBAK
  coc_scoring <- coc_scoring %>%
    dplyr::mutate(DateReceivedPPDocs = as.Date(DateReceivedPPDocs, origin = "1899-12-30"))

  Project <- Project %>%
    dplyr::select(-ProjectName) %>%
    dplyr::left_join(provider_extras, by = "ProjectID") %>%
    dplyr::left_join(coc_scoring, by = "ProjectID")



  # Regions
  #TODO What is the origin of this CSV
  regions <- readr::read_csv("public_data/Regions.csv",
                             col_types = "cn") %>%
    dplyr::arrange(Region) %>%
    dplyr::mutate(RegionName = dplyr::if_else(
      Region == 0,
      "Mahoning County CoC",
      paste("Homeless Planning Region", Region)))
  #
  # Project <- left_join(project_county, regions, by = "County")

  # EnrollmentCoC -----------------------------------------------------------

  EnrollmentCoC <-
    clarity_api$EnrollmentCoC

  # VeteranCE --------------------------------------------------------------

  VeteranCE <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"),
                                 sheet = 14)

  VeteranCE <-
    dplyr::mutate(
      VeteranCE,
      DateVeteranIdentified = as.Date(DateVeteranIdentified, origin = "1899-12-30"),
      ExpectedPHDate = as.Date(ExpectedPHDate, origin = "1899-12-30")
    )

  # Enrollment --------------------------------------------------------------

  Enrollment <- clarity_api$Enrollment()

  # from sheets 1 and 2, getting EE-related data, joining both to En --------

  counties <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 1)#

  Enrollment <- Enrollment %>%
    dplyr::inner_join(counties, by = "EnrollmentID") %>%
    dplyr::left_join(VeteranCE %>% dplyr::select(EnrollmentID, PHTrack, ExpectedPHDate),
                     by = "EnrollmentID")

  rm(counties)

  # Adding Exit Data to Enrollment because I'm not tryin to have one-to-one
  # relationships in this!

  small_exit <- Exit %>% dplyr::select(EnrollmentID,
                                       ExitDate,
                                       Destination,
                                       OtherDestination)

  Enrollment <- dplyr::left_join(Enrollment, small_exit, by = "EnrollmentID") %>%
    dplyr::mutate(ExitAdjust = dplyr::if_else(is.na(ExitDate) |
                                                ExitDate > lubridate::today(),
                                              lubridate::today(), ExitDate))

  rm(small_exit)

  # Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust
  small_project <- Project %>%
    dplyr::select(ProjectID, ProjectType, ProjectName)

  # getting HH information
  # only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.
  HHMoveIn <- Enrollment %>%
    dplyr::left_join(small_project, by = "ProjectID") %>%
    dplyr::filter(ProjectType %in% c(3, 9, 13)) %>%
    dplyr::mutate(
      AssumedMoveIn = dplyr::if_else(
        lubridate::ymd(EntryDate) < hc_psh_started_collecting_move_in_date &
          ProjectType %in% c(3, 9),
        1,
        0
      ),
      ValidMoveIn = dplyr::case_when(
        AssumedMoveIn == 1 ~ EntryDate,
        AssumedMoveIn == 0 &
          ProjectType %in% c(3, 9) &
          lubridate::ymd(EntryDate) <= lubridate::ymd(MoveInDate) &
          lubridate::ymd(ExitAdjust) > lubridate::ymd(MoveInDate) ~ MoveInDate,
        # the Move-In Dates must fall between the Entry and ExitAdjust to be
        # considered valid and for PSH the hmid cannot = ExitDate
        lubridate::ymd(MoveInDate) <= lubridate::ymd(ExitAdjust) &
          lubridate::ymd(MoveInDate) >= lubridate::ymd(EntryDate) &
          ProjectType == 13 ~ MoveInDate
      )
    ) %>%
    dplyr::filter(!is.na(ValidMoveIn)) %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::mutate(HHMoveIn = min(ValidMoveIn)) %>%
    dplyr::ungroup() %>%
    dplyr::select(HouseholdID, HHMoveIn) %>%
    unique()

  HHEntry <- Enrollment %>%
    dplyr::left_join(small_project, by = "ProjectID") %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::mutate(FirstEntry = min(EntryDate)) %>%
    dplyr::ungroup() %>%
    dplyr::select(HouseholdID, "HHEntry" = FirstEntry) %>%
    unique() %>%
    dplyr::left_join(HHMoveIn, by = "HouseholdID")


  Enrollment <- Enrollment %>%
    dplyr::left_join(small_project, by = "ProjectID") %>%
    dplyr::left_join(HHEntry, by = "HouseholdID") %>%
    dplyr::mutate(
      MoveInDateAdjust = dplyr::if_else(!is.na(HHMoveIn) &
                                          lubridate::ymd(HHMoveIn) <= lubridate::ymd(ExitAdjust),
                                        dplyr::if_else(lubridate::ymd(EntryDate) <= lubridate::ymd(HHMoveIn),
                                                       HHMoveIn, EntryDate),
                                        NA_real_),
      EntryAdjust = dplyr::case_when(
        ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
      )
    )

  rm(small_project, HHEntry)

  # Client Location

  y <- EnrollmentCoC %>%
    dplyr::filter(DataCollectionStage == 1) %>%
    dplyr::select(EnrollmentID, "ClientLocation" = CoCCode)

  Enrollment <- Enrollment %>%
    dplyr::left_join(y, by = "EnrollmentID")

  rm(y)

  # Event <-
  #   read_csv(paste0(directory, "/Event.csv"),
  #            col_types = "nnnDnnncDTTcTc") <- no data

  # Export ------------------------------------------------------------------

  # Already loaded in 00_dates.R

  # Funder ------------------------------------------------------------------

  Funder <-
    clarity_api$Funder()

  # HealthAndDV -------------------------------------------------------------

  HealthAndDV <-
    clarity_api$HealthAndDV()

  # IncomeBenefits ----------------------------------------------------------

  IncomeBenefits <-
    clarity_api$IncomeBenefits()

  # Inventory ---------------------------------------------------------------

  Inventory <-
    clarity_api$Inventory()

  # Organization ------------------------------------------------------------

  Organization <-
    clarity_api$Organization()

  # ProjectCoC --------------------------------------------------------------

  ProjectCoC <-
    clarity_api$ProjectCoC()

  # Case Manager Records ----------------------------------------------------

  CaseManagers <-
    readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 5) %>%
    dplyr::mutate(
      CMStartDate = as.Date(CMStartDate, origin = "1899-12-30"),
      CMEndDate = as.Date(CMEndDate, origin = "1899-12-30")
    )

  # Interims ----------------------------------------------------------------

  Interims <-
    readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 20) %>%
    dplyr::mutate(InterimDate = as.Date(InterimDate, origin = "1899-12-30"))

  # Contacts ----------------------------------------------------------------
  # only pulling in contacts made between an Entry Date and an Exit Date

  Contacts <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 4) %>%
    dplyr::mutate(
      ContactDate = lubridate::ymd(as.Date(ContactDate, origin = "1899-12-30")),
      ContactProvider = stringr::str_remove(ContactProvider, "\\(.*\\)")
    )

  # Scores ------------------------------------------------------------------

  Scores <-  readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"),
                               sheet = 12) %>%
    dplyr::mutate(ScoreDate = as.Date(ScoreDate, origin = "1899-12-30"))

  # Offers -----------------------------------------------------------------

  Offers <-
    readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 7) %>%
    dplyr::mutate(AcceptDeclineDate = lubridate::ymd(as.Date(AcceptDeclineDate, origin = "1899-12-30")),
                  OfferDate = lubridate::ymd(as.Date(OfferDate, origin = "1899-12-30")))

  # Users ------------------------------------------------------------------
  Users <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"),
                             sheet = 2,#
                             range = readxl::cell_cols("A:H")) %>%
    dplyr::mutate(DefaultProvider = stringr::str_remove(DefaultProvider, "\\(.*\\)"),
                  UserCreatedDate = lubridate::ymd(as.Date(UserCreatedDate, origin = "1899-12-30"))) %>%
    dplyr::left_join(provider_extras, by = c("DefaultProvider" = "ProjectName")) %>%
    dplyr::select(
      UserCreating,
      UserID,
      UserName,
      UserTelephone,
      UserEmail,
      UserActive,
      UserCreatedDate,
      DefaultProvider,
      "UserCounty" = ProjectCounty,
      "UserRegion" = ProjectRegion
    )

  rm(provider_extras)

  # some users don't have a County bc their Default Provider doesn't have an
  # address.


  # COVID-19 ----------------------------------------------------------------

  covid19 <-
    readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 6) %>%
    dplyr::filter(!PersonalID %in% c(5, 4216)) %>%
    dplyr::mutate(
      COVID19AssessmentDate = lubridate::ymd(as.Date(COVID19AssessmentDate,
                                                     origin = "1899-12-30")),
      ContactWithConfirmedDate = lubridate::ymd(as.Date(ContactWithConfirmedDate,
                                                        origin = "1899-12-30")),
      ContactWithUnderInvestigationDate = lubridate::ymd(
        as.Date(ContactWithUnderInvestigationDate,
                origin = "1899-12-30")
      ),
      TestDate = lubridate::ymd(as.Date(TestDate,
                                        origin = "1899-12-30")),
      DateUnderInvestigation = lubridate::ymd(as.Date(DateUnderInvestigation,
                                                      origin = "1899-12-30")),
      Tested = replace_yes_no(Tested),
      UnderInvestigation = replace_yes_no(UnderInvestigation),
      ContactWithConfirmedCOVID19Patient = replace_yes_no(
        ContactWithConfirmedCOVID19Patient
      ),
      ContactWithUnderCOVID19Investigation = replace_yes_no(
        ContactWithUnderCOVID19Investigation
      )
    ) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("Symptom")), replace_yes_no) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("HealthRisk")), replace_yes_no)

  doses <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 21) %>%
    dplyr::mutate(
      COVID19DoseDate = lubridate::ymd(as.Date(COVID19DoseDate,
                                               origin = "1899-12-30"))) %>%
    dplyr::filter(!PersonalID %in% c(5, 4216))

  # Services ----------------------------------------------------------------

  raw_services <-
    readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 8) %>%
    dplyr::mutate(ServiceStartDate = lubridate::ymd(as.Date(ServiceStartDate,
                                                            origin = "1899-12-30")),
                  ServiceEndDate = lubridate::ymd(as.Date(ServiceEndDate,
                                                          origin = "1899-12-30")),
                  ServiceProvider = stringr::str_remove(ServiceProvider, "\\(.*\\)"),
                  ProviderCreating = stringr::str_remove(ProviderCreating, "\\(.*\\)"))

  services_funds <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 9)

  Services <- raw_services %>%
    dplyr::left_join(Enrollment[c("EnrollmentID",
                                  "PersonalID",
                                  "ProjectName",
                                  "EntryDate",
                                  "ExitAdjust")],
                     by = c("PersonalID")) %>%
    unique() %>%
    dplyr::left_join(services_funds, by = "ServiceID") %>%
    dplyr::mutate(
      ServiceEndAdjust = dplyr::if_else(is.na(ServiceEndDate) | ServiceEndDate > lubridate::today(), lubridate::today(), ServiceEndDate),
      service_interval = lubridate::interval(start = lubridate::ymd(ServiceStartDate), end = lubridate::ymd(ServiceEndAdjust)),
      ee_interval = lubridate::interval(start = lubridate::ymd(EntryDate), end = lubridate::ymd(ExitAdjust)),
      intersect_tf = lubridate::int_overlaps(service_interval, ee_interval),
      stray_service = is.na(intersect_tf) | intersect_tf == FALSE | ServiceProvider != ProjectName
    ) %>%
    dplyr::select(PersonalID, ServiceID, EnrollmentID, ServiceProvider, ServiceHHID,
                  ServiceStartDate, ServiceEndDate, Code, Description, ProviderCreating,
                  Fund, Amount, stray_service)

  stray_services <- Services %>%
    dplyr::filter(stray_service) %>%
    dplyr::select(-stray_service)

  Services <- Services %>%
    dplyr::filter(!stray_service) %>%
    dplyr::select(-stray_service)

  rm(raw_services, services_funds)

  # Referrals ---------------------------------------------------------------

  Referrals <-
    readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 10) %>%
    dplyr::mutate(ReferralDate = lubridate::ymd(as.Date(ReferralDate,
                                                        origin = "1899-12-30")),
                  ProviderCreating = stringr::str_remove(ProviderCreating, "\\(.*\\)"),
                  `Referred-ToProvider` = stringr::str_remove(`Referred-ToProvider`, "\\(.*\\)"))

  # HUD CSV Specs -----------------------------------------------------------
  #TODO Where does this csv come from
  HUD_specs <- readr::read_csv("public_data/HUDSpecs.csv",
                               col_types = "ccnc") %>%
    as.data.frame()

  # Adding Age at Entry to Enrollment ---------------------------------------
  small_client <- Client %>% dplyr::select(PersonalID, DOB)
  Enrollment <- Enrollment %>%
    dplyr::left_join(small_client, by = "PersonalID") %>%
    dplyr::mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
    dplyr::select(-DOB)

  rm(small_client)

}



# Save it out -------------------------------------------------------------
# WARNING save.image does not save the environment properly, save must be used.
# save(list = ls(), file = "images/COHHIOHMIS.RData", compress = FALSE)
