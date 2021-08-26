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
provider_extras_helpers <- list(
  add_regions = function(provider_extras, dirs) {
    # geocodes is created by `hud.extract` using the hud_geocodes.R functions
    geocodes <- hud_load("geocodes", dirs$public)
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

    Regions <- hud_load("Regions", dirs$public)

    provider_extras <- provider_extras |>
      dplyr::left_join(Regions |> dplyr::select(- RegionName), by = "County")

    # Missing Regions
    # missing_region <- provider_extras |>
    #   dplyr::filter(is.na(Region))
    # missing_region |>
    #   dplyr::pull(County) |>
    #   unique()
    provider_extras
  },
  create_APs = function(provider_extras, dirs) {
    Regions <- hud_load("Regions", dirs$public)
    APs <- provider_extras |>
      dplyr::select( !tidyselect::starts_with("CoCComp") & !Geocode:ZIP) |>
      dplyr::filter(Type == "Coordinated Entry") |>
      tidyr::pivot_longer(tidyselect::starts_with("AP"), names_to = "TargetPop", names_pattern = "(?<=^APCounties)(\\w+)", values_to = "CountiesServed") |>
      dplyr::filter(!is.na(CountiesServed)) |>
      dplyr::select(!tidyselect::starts_with("AP") & !Type)

    # Programs serve multiple Counties which may fall into multiple regions. This creates a row for each Region served by a Program such that Coordinated Entry Access Points will show all the appropriate programs when filtering by Region.
    # @Rm
    APs <- slider::slide_dfr(APs, ~{
      .counties <- trimws(stringr::str_split(.x$CountiesServed, ",\\s")[[1]])

      .x |>
        dplyr::select(- Region) |>
        cbind(Region = unique(Regions$Region[Regions$County %in% .counties]))
    }) |>
      dplyr::distinct_all()

    APs
  }
)



Enrollment_helpers <- list(
  add_Household = function(Enrollment, Project, app_env) {
    # getting HH information
    # only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.
    app_env$merge_deps_to_env("hc_psh_started_collecting_move_in_date")
    small_project <- Project %>%
      dplyr::select(ProjectID, ProjectType, ProjectName)
    HHMoveIn <- Enrollment %>%
      dplyr::left_join(
        # Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust
        small_project,
        by = "ProjectID") %>%
      dplyr::filter(ProjectType %in% c(3, 9, 13)) %>%
      dplyr::mutate(
        AssumedMoveIn = dplyr::if_else(
          EntryDate < hc_psh_started_collecting_move_in_date &
            ProjectType %in% c(3, 9),
          1,
          0
        ),
        ValidMoveIn = dplyr::case_when(
          AssumedMoveIn == 1 ~ EntryDate,
          AssumedMoveIn == 0 &
            ProjectType %in% c(3, 9) &
            EntryDate <= MoveInDate &
            ExitAdjust > MoveInDate ~ MoveInDate,
          # the Move-In Dates must fall between the Entry and ExitAdjust to be
          # considered valid and for PSH the hmid cannot = ExitDate
          MoveInDate <= ExitAdjust &
            MoveInDate >= EntryDate &
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
        MoveInDateAdjust = dplyr::if_else(
          !is.na(HHMoveIn) & HHMoveIn <= ExitAdjust,
          dplyr::if_else(EntryDate <= HHMoveIn,
                         HHMoveIn, EntryDate),
          NA_real_),
        EntryAdjust = dplyr::case_when(
          ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
          ProjectType %in% c(3, 9, 13) &
            !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
        )
      )
    Enrollment
  },
  add_VeteranCE = function(Enrollment, VeteranCE, app_env) {
    app_env$merge_deps_to_env("Exit")

    Enrollment |>
      # Join Veteran data
      dplyr::left_join(VeteranCE  |>  dplyr::select(EnrollmentID, PHTrack, ExpectedPHDate), by = "EnrollmentID") |>
      # Join Exit data (formerly small_exit)

      dplyr::left_join(
        Exit %>% dplyr::select(EnrollmentID,
                               ExitDate,
                               Destination,
                               OtherDestination) |>
          dplyr::mutate(EnrollmentID = as.numeric(EnrollmentID))
        ,
        by = "EnrollmentID"
      ) |>
      dplyr::mutate(ExitAdjust = dplyr::if_else(is.na(ExitDate) |
                                                  ExitDate > lubridate::today(),
                                                lubridate::today(), ExitDate))
  }
)

load_export <- function(
  clarity_api,
  app_env,
  error = FALSE,
  e = rlang::caller_env()
) {
  if (missing(clarity_api))
    clarity_api <- UU::find_by_class("clarity_api", e)
  if (missing(app_env))
    app_env <- UU::find_by_class("app_env", e)
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





  # Project -----------------------------------------------------------------
  # provider_extras
  # Thu Aug 12 14:23:50 2021
  provider_extras <- clarity_api$`HUD Extras`$Project_extras()
  provider_extras <- provider_extras_helpers$add_regions(provider_extras, dirs)
  # Rminor: Coordinated Entry Access Points [CEAP]
  APs <- provider_extras_helpers$create_APs(provider_extras, dirs)

Project <- clarity_api$Project() |>
  dplyr::select(-ProjectCommonName) |>
  dplyr::left_join(provider_extras, by = "ProjectID")

  # EnrollmentCoC -----------------------------------------------------------

  EnrollmentCoC <-
    clarity_api$EnrollmentCoC()



  # Enrollment --------------------------------------------------------------

  # from sheets 1 and 2, getting EE-related data, joining both to En --------

  Enrollment <- clarity_api$Enrollment()
  # Add Enrollment Extras
  Enrollment <- Enrollment |>
    dplyr::inner_join(clarity_api$`HUD Extras`$Enrollment_extras(), by = "EnrollmentID")

  Enrollment_helpers$add_Household(Enrollment, Project, app_env)

  # Veteran Client_extras ----
  VeteranCE <- clarity_api$`HUD Extras`$Client_extras() |>
    dplyr::mutate(
      DateVeteranIdentified = as.Date(DateVeteranIdentified, origin = "1899-12-30"),
      ExpectedPHDate = as.Date(ExpectedPHDate, origin = "1899-12-30")
    )

  Enrollment <- Enrollment_helpers$add_VeteranCE(Enrollment, VeteranCE, app_env)

  # Add Client Location
  Enrollment <- Enrollment %>%
    dplyr::left_join(
      EnrollmentCoC %>%
        dplyr::filter(DataCollectionStage == 1) %>%
        dplyr::select(EnrollmentID, "ClientLocation" = CoCCode),
      by = "EnrollmentID")

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


  # Contacts ----------------------------------------------------------------
  # only pulling in contacts made between an Entry Date and an Exit Date

  #TODO # Comes from CurrentLiving Situation
  Contacts <- readxl::read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 4) %>%
    dplyr::mutate(
      ContactDate = lubridate::ymd(as.Date(ContactDate, origin = "1899-12-30")),
      ContactProvider = stringr::str_remove(ContactProvider, "\\(.*\\)")
    )

  # Scores ------------------------------------------------------------------

  Scores <-  clarity_api$`HUD Extras`$Client_SPDAT_extras()

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

  # TODO Update col name:
  # https://github.com/COHHIO/COHHIO_HMIS/blob/d7f2249d5a8333ddddb2181c8bf30553aa7e7038/04_DataQuality.R#L339
  # https://github.com/COHHIO/COHHIO_HMIS/blob/cbb4d0734e4b60f0cca38dc35a0f5a3f07eafe93/09_covid.R#L98
  # https://github.com/COHHIO/COHHIO_HMIS/blob/cbb4d0734e4b60f0cca38dc35a0f5a3f07eafe93/09_covid.R#L297
  # https://github.com/COHHIO/COHHIO_HMIS/blob/cbb4d0734e4b60f0cca38dc35a0f5a3f07eafe93/02_QPR_EEs.R#L249
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
    dplyr::mutate_at(dplyr::vars(dplyr::matches("S\\d")), replace_yes_no) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("HR")), replace_yes_no)

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

