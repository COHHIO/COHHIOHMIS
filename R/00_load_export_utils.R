
#' @title Redact PII from Client HUD Export
#' @description This redacts all PII (except DOB) from Client HUD Export
#' @family Client functions
#' @inheritParams data_quality_tables
#' @return \code{(tibble)} Redacted Client HUD Export
#' @export

Client_redact <- function(Client) {
  Client |>
    clarity.looker::Client_filter() |>
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
    ) |>
    dplyr::mutate(SSN = dplyr::case_when(is.na(SSN) ~ "ok",!is.na(SSN) ~ SSN))
}


#' @title Add the UniqueID to the Client export
#'
#' @inheritParams data_quality_tables
#' @param Client_extras \code{(data.frame)} A custom look linking PersonalID & UniqueID
#' @param app_env
#'
#' @return \code{(data.frame)} with UniqueID column
#' @export

Client_add_UniqueID <- function(Client, Client_UniqueIDs, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  out <- dplyr::left_join(Client, dplyr::distinct(Client_UniqueIDs, PersonalID, UniqueID), by = "PersonalID")
  UU::join_check(Client, out)
  out
}

#' @title Add Exit data to Enrollments
#'
#' @param Enrollment \code{(data.frame)} HUD CSV Export Item
#' @param Exit \code{(data.frame)} HUD CSV Export Item
#'
#' @return \code{(data.frame)} Enrollments with `ExitDate`, `Destination`, `OtherDestination` and derived column `ExitAdjust`
#' @export

Enrollment_add_Exit <- function(Enrollment, Exit) {
  out <- dplyr::left_join(Enrollment, Exit |> dplyr::select(EnrollmentID,
                                                     ExitDate,
                                                     Destination,
                                                     OtherDestination), by = "EnrollmentID")  |>
    dplyr::mutate(ExitAdjust = dplyr::if_else(is.na(ExitDate) |
                                  ExitDate > Sys.Date(),
                                Sys.Date(), ExitDate))

  UU::join_check(Enrollment, out)
  out
}



#' Add Household Information to Enrollment from Project
#'
#' @param Enrollment with Exit data. See `Enrollment_add_Exit`
#' @inheritParams data_quality_tables
#' @inheritParams R6Classes
#' @return \code{(data.frame)} of Enrollment with Household Columns `MoveInDateAdjust` appended
#' @export

Enrollment_add_Household = function(Enrollment, Project, rm_dates, app_env = get_app_env(e = rlang::caller_env())) {

  # getting HH information
  # only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.


  small_project <- Project |>
    dplyr::select(ProjectID, ProjectType, ProjectName) |>
    dplyr::distinct()
  # TODO Check to see if Enrollment data has the MoveInDate
  # TODO Does Move-in Date in Clarity auto-populate from previous enrollments?
  HHMoveIn <- Enrollment |>
    dplyr::left_join(
      # Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust
      small_project,
      by = "ProjectID") |>
    dplyr::filter(ProjectType %in% c(3, 9, 13)) |>
    dplyr::mutate(
      AssumedMoveIn = dplyr::if_else(
        EntryDate < rm_dates$hc$psh_started_collecting_move_in_date &
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
    ) |>
    dplyr::filter(!is.na(ValidMoveIn)) |>
    dplyr::group_by(HouseholdID) |>
    dplyr::mutate(HHMoveIn = min(ValidMoveIn, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(HouseholdID, HHMoveIn) |>
    unique()

  HHEntry <- Enrollment |>
    dplyr::left_join(small_project, by = "ProjectID") |>
    dplyr::group_by(HouseholdID) |>
    dplyr::mutate(FirstEntry = min(EntryDate, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(HouseholdID, "HHEntry" = FirstEntry) |>
    unique() |>
    dplyr::left_join(HHMoveIn, by = "HouseholdID")


  out <- Enrollment |>
    dplyr::left_join(small_project, by = "ProjectID") |>
    dplyr::left_join(HHEntry, by = "HouseholdID") |>
    dplyr::mutate(
      # Puts EntryDate as MoveInDate for projects that don't use a MoveInDate
      MoveInDateAdjust = dplyr::case_when(
        !is.na(HHMoveIn) & HHMoveIn <= ExitAdjust & EntryDate <= HHMoveIn ~ HHMoveIn,
        TRUE ~ NA_real_),
      EntryAdjust = dplyr::case_when(
        ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
      )
    )
  UU::join_check(Enrollment, out)
  out
}

#' Add Veteran Coordinated Entry Date to Enrollments
#'
#' @param Enrollment that includes Exit Data. See `Enrollment_add_Exit`
#' @param VeteranCE HUD Extra that includes Veteran Coordinated Entry data
#'
#' @return \code{(data.frame)} Enrollment with the following columns added `PHTrack`, `ExpectedPHDate`, `ExitAdjust`
#' @export
Enrollment_add_VeteranCE = function(Enrollment, VeteranCE) {

  Enrollment |>
    # Join Veteran data
    dplyr::left_join(VeteranCE  |>  dplyr::select(EnrollmentID, PHTrack, ExpectedPHDate) |> dplyr::distinct(EnrollmentID, .keep_all = TRUE), by = "EnrollmentID") |>
    dplyr::mutate(
      ExitAdjust = dplyr::if_else(
        is.na(ExitDate) |
          ExitDate > lubridate::today(),
        lubridate::today(),
        ExitDate
      )
    )
}

#' @title Add Client Location to Enrollment
#'
#' @inheritParams data_quality_tables
#'
#' @return \code{(data.frame)} Enrollment with `ClientLocation` column
#' @export

Enrollment_add_ClientLocation = function(Enrollment, EnrollmentCoC) {
  dplyr::left_join(Enrollment,
      dplyr::filter(EnrollmentCoC, DataCollectionStage == 1) |>
      dplyr::select(EnrollmentID,
                    ClientLocation = CoCCode),
    by = "EnrollmentID")
}

#' @title Add AgeAtEntry to Enrollment
#' @description AgeAtEntry is the time elapsed from the Date of Birth `DOB` to the `EntryDate`
#' @inheritParams data_quality_tables
#'
#' @return \code{(data.frame)} Enrollment with `AgeAtEntry` column
#' @export

Enrollment_add_AgeAtEntry_UniqueID <- function(Enrollment, Client) {
  dplyr::left_join(Enrollment, dplyr::select(Client, UniqueID, PersonalID, DOB) |> dplyr::distinct(PersonalID, .keep_all = TRUE), by = c("PersonalID")) |>
    dplyr::mutate(AgeAtEntry = age_years(DOB, EntryDate)) |>
    dplyr::select(-DOB)
}


#' @title Remove specific CoCCode's from EnrollmentCoC
#'
#' @inheritParams data_quality_tables
#' @param codes_to_remove \code{(character)} codes to remove by filtering the \code{CoCCode} column
#'
#' @return \code{(data.frame)} without the entries for the specified \code{CoCCode}s
#' @export

EnrollmentCoC_RemoveCoCCodes <- function(EnrollmentCoC, codes_to_remove = c("Default")) {
  if ("CoCCode" %in% names(EnrollmentCoC))
    out <- dplyr::filter(EnrollmentCoC, !CoCCode %in% codes_to_remove)
  else
    out <- EnrollmentCoC
}

#' @title Filter Projects that have been retired (`zz`'d)
#'
#' @inheritParams data_quality_tables
#'
#' @return
#' @export
Project_rm_zz <- function(Project) {
  dplyr::filter(Project, stringr::str_detect(ProjectName, "^zz", negate = TRUE))
}

#' @title Add the Corresponding Region for each Project by way of Geocode matching
#'
#' @param provider_extras \code{(data.frame)} An extra from the from the Program Descriptor model. See `?clarity_api` for retrieving a Look with this info.
#' @inheritParams data_quality_tables
#'
#' @return \code{(data.frame)} provider_extras with Regions column
#' @export

pe_add_regions <- function(provider_extras, Regions = clarity.looker::hud_load("Regions", dirs$public), dirs) {
  # geocodes is created by `hud.extract` using the hud_geocodes.R functions
  geocodes <- clarity.looker::hud_load("geocodes", dirs$public)
  # This should map a county to every geocode
  out <- provider_extras |>
    dplyr::left_join(geocodes |> dplyr::select(GeographicCode, County), by = c(Geocode = "GeographicCode")) |>
    dplyr::filter(!Geocode %in% c("000000", "429003", "399018"))

  # Some geocodes may be legacy and County will be NA - the following looks these geocodes up on the Google Geocode API via `ggmap`
  fill_geocodes <- out |>
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
    out <- out |>
      dplyr::left_join(geocodes, by = c(Geocode = "GeographicCode"))
  }

  out <- out |>
    dplyr::left_join(Regions |> dplyr::select(- RegionName), by = "County") |>
    dplyr::rename(ProjectRegion = "Region",
                  ProjectCounty = "County")


  # Special cases
  #  St. Vincent de Paul of Dayton serves region 13
  out[out$Geocode %in% c("391361", "391362"), "ProjectRegion"] <- 13

  # Missing Regions
  # missing_region <- out |>
  #   dplyr::filter(is.na(ProjectRegion) & ProgramCoC == "OH-507")
  # missing_region |>
  #   dplyr::pull(ProjectCounty) |>
  #   unique()

  out |> dplyr::filter(!is.na(ProjectRegion))
}

#' @title Add Access Points to Provider_extras
#' @description Create data.frame of Coordinated Entry Access Points with info about the Counties & Populations Served
#' @param provider_extras \code{(data.frame)} provider_extras with Regions, see `pe_add_regions`
#' @param dirs
#'
#' @return \code{(data.frame)}
#' @export

pe_create_APs = function(provider_extras, ProjectCoC, dirs, app_env = get_app_env(e = rlang::caller_env())) {

  Regions <- clarity.looker::hud_load("Regions", dirs$public)
  APs <- provider_extras |>
    dplyr::select( !tidyselect::starts_with("CoCComp") & !Geocode:ZIP) |>
    dplyr::filter(ProjectTypeCode == "Coordinated Entry") |>
    tidyr::pivot_longer(tidyselect::starts_with("AP"), names_to = "TargetPop", names_pattern = "(?<=^APCounties)(\\w+)", values_to = "CountiesServed") |>
    dplyr::filter(!is.na(CountiesServed)) |>
    dplyr::select(!tidyselect::starts_with("AP") & !ProjectTypeCode)

  project_addresses <- ProjectCoC |>
    dplyr::select(ProjectID, CoCCode, Address1, Address2, City, State, ZIP) |>
    dplyr::distinct() |>
    dplyr::mutate(
      City = paste0(City, ", ", State, " ", ZIP),
      Addresses = dplyr::coalesce(Address1, Address2)
    )

  # Programs serve multiple Counties which may fall into multiple regions. This creates a row for each Region served by a Program such that Coordinated Entry Access Points will show all the appropriate programs when filtering by Region.
  # @Rm
  APs <- slider::slide_dfr(APs, ~{
    .counties <- trimws(stringr::str_split(.x$CountiesServed, ",\\s")[[1]])

    .x |>
      dplyr::select(- ProjectRegion) |>
      dplyr::bind_cols(Region = unique(Regions$Region[Regions$County %in% .counties]))
  }) |>
    dplyr::distinct_all() |>
    dplyr::mutate(OrgLink = dplyr::if_else(!is.na(Website), paste0(
      "<a href='",
      Website,
      "' target='_blank'>",
      ProjectName,
      "</a><small> (#",
      ProjectID,
      ")</small>"
    ), paste0(ProjectName,
              "<small> (#",
              ProjectID,
              ")</small>"))) |>
    dplyr::left_join(project_addresses, by = "ProjectID")  |>
    dplyr::select(ProjectID, OrganizationName, ProjectName, TargetPop,
                  "ProjectCountyServed" = CountiesServed
                  #, ProjectAreaServed
                  , Hours, Phone, OrgLink, CoCCode, Addresses, City)

  APs
}

#' @title Add ProjectType (dbl) to provider_extras
#'
#' @inheritParams pe_create_APs
#' @return \code{(data.frame)}

pe_add_ProjectType <- function(provider_extras) {
  PT <- HMIS::hud_translations$`2.02.6 ProjectType`(table = TRUE) |>
    tibble::add_row(Value = 12, Text = "Homeless Prevention")
  purrr::map_dbl(provider_extras$ProjectTypeCode, ~PT$Value[agrepl(stringr::str_remove(.x, "\\s\\([\\w\\s]+\\)$"), PT$Text)])
  provider_extras |>
    dplyr::rowwise() |>
    dplyr::mutate(ProjectType = PT$Value[agrepl(stringr::str_remove(ProjectTypeCode, "\\s\\([\\w\\s]+\\)$"), PT$Text)], .after = "ProjectTypeCode")
}

#' @title Add GrantType column to provider_extras
#' @description GrantType indicates if the program is funded by one of HOPWA, PATH, SSVF, or RHY
#'
#' @inheritParams pe_create_APs
#'
#' @return
#' @export
pe_add_GrantType <- function(provider_extras) {
  hash <- HMIS::hud_translations$`2.06.1 FundingSource`(table = TRUE)

  gt <- list(HOPWA = c(13:19), PATH = 21, SSVF = 33, RHY = 22:26)
  provider_extras |>
    dplyr::mutate(GrantType = dplyr::case_when(
      FundingSourceCode %in% gt$HOPWA ~ "HOPWA",
      FundingSourceCode %in% gt$PATH ~ "PATH",
      FundingSourceCode %in% gt$SSVF ~ "SSVF",
      FundingSourceCode %in% gt$RHY ~ "RHY"
      ))
}

provider_extras_helpers <- list(
  add_regions = pe_add_regions,
  add_ProjectType = pe_add_ProjectType,
  create_APs = pe_create_APs,
  add_GrantType = pe_add_GrantType
)


#' @title Load public data pre-requisites for `load_export`
#'
#' @inheritParams data_quality_tables
#' @inherit load_export return
#' @export

load_public <- function(app_env = get_app_env(e = rlang::caller_env())) {
  # Public -----------------------------------------------------------
  ServiceAreas <- clarity.looker::hud_load("ServiceAreas.feather", dirs$public)
  Regions <- clarity.looker::hud_load("Regions", dirs$public)
  app_env$gather_deps("everything")
}

#' @title Load Client data & extras pre-requisites for `load_export`
#'
#' @inheritParams data_quality_tables
#' @inherit load_export return
#' @export

load_client <- function(clarity_api = get_clarity_api(e = rlang::caller_env()),
                        app_env = get_app_env(e = rlang::caller_env())) {
  Client <- clarity_api$Client()
  # this saves Client as a feather file with redacted PII as a security measure.
  if (!all(Client$SSN %in% c("ok" ,"Invalid", "DKR", "Missing"))) {
    Client <- Client_redact(Client)
    clarity.looker::hud_feather(Client, dirs$export)
  }
  # Veteran Client_extras ----
  VeteranCE <- clarity_api$`HUD Extras`$Client_extras()

  Client <- Client_add_UniqueID(Client, clarity_api$`HUD Extras`$Client_UniqueID_extras())
  app_env$gather_deps("everything")
}

#' @title Load Project & extras pre-requisites for `load_export`
#'
#' @inheritParams data_quality_tables
#' @param Regions From public data. See `load_public`
#' @inherit load_export return
#' @export

load_project <- function(Regions, ProjectCoC, clarity_api = get_clarity_api(e = rlang::caller_env()),
                         app_env = get_app_env(e = rlang::caller_env())) {

  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  force(ProjectCoC)
  # Project_extras -----------------------------------------------------------------
  # provider_extras
  # Thu Aug 12 14:23:50 2021


  provider_extras <- clarity_api$`HUD Extras`$Project_extras()
  provider_extras <- pe_add_ProjectType(provider_extras) |>
    pe_add_regions(Regions, dirs = dirs) |>
    pe_add_GrantType()

  # Rminor: Coordinated Entry Access Points [CEAP]
  APs <- pe_create_APs(provider_extras, ProjectCoC, dirs = dirs)



  .Project <- clarity_api$Project()
  Project <- .Project |>
    dplyr::select(-ProjectCommonName) |>
    {\(x) {dplyr::left_join(x, provider_extras |> dplyr::select(- dplyr::matches("FundingSourceID")) |> dplyr::distinct(ProjectID, .keep_all = TRUE), by = UU::common_names(x, provider_extras))}}()
  UU::join_check(.Project, Project)

  mahoning_projects <- dplyr::filter(ProjectCoC, CoCCode %in% "OH-504") |>
    dplyr::select(ProjectID) |>
    {\(x) {
      dplyr::left_join(x, dplyr::select(Project, ProjectID, ProjectTypeCode, ProjectName), by = "ProjectID") |>
        Project_rm_zz() |>
        dplyr::distinct(ProjectID, .keep_all = TRUE) |>
        {\(y) {rlang::set_names(y$ProjectID, dplyr::pull(y, ProjectTypeCode))}}()
    }}()
  app_env$gather_deps(Project, APs, mahoning_projects)
}

#' @title Load Enrollment as Enrollment_extra_Client_Exit_HH_CL_AaE
#'
#' @inheritParams data_quality_tables params return
#' @param Enrollment_extras From Clarity Looker API Extras
#'
#' @export
#'

load_enrollment <- function(Enrollment,
                            EnrollmentCoC,
                            Enrollment_extras,
                            Exit,
                            Client,
                            Project,
                            rm_dates,
                            app_env = get_app_env(e = rlang::caller_env())) {

  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  # getting EE-related data, joining both to En


  Enrollment_extra_Client_Exit_HH_CL_AaE <- dplyr::left_join(Enrollment, Enrollment_extras, by = UU::common_names(Enrollment, Enrollment_extras)) |>
    # Add Exit
    Enrollment_add_Exit(Exit) |>
    # Add Households
    Enrollment_add_Household(Project, rm_dates = rm_dates) |>
    # Add Veteran Coordinated Entry
    Enrollment_add_VeteranCE(VeteranCE = VeteranCE) |>
    # Add Client Location from EnrollmentCoC
    Enrollment_add_ClientLocation(EnrollmentCoC) |>
    # Add Client AgeAtEntry
    Enrollment_add_AgeAtEntry_UniqueID(Client) |>
    dplyr::left_join(dplyr::select(Client,-dplyr::all_of(
      c(
        "DateCreated",
        "DateUpdated",
        "UserID",
        "DateDeleted",
        "ExportID"
      )
    )),
    by = c("PersonalID", "UniqueID"))

  UU::join_check(Enrollment, Enrollment_extra_Client_Exit_HH_CL_AaE)

  app_env$gather_deps(Enrollment_extra_Client_Exit_HH_CL_AaE)

}

#' @title Load Services as Services_enroll_extras
#'
#' @inherit data_quality_tables params return
#'
#' @export
load_services <- function(Services,
                          Services_extras,
                          Enrollment_extra_Client_Exit_HH_CL_AaE,
                          app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  Services_enroll_extras  <- dplyr::left_join(Services,
                                              Services_extras,
                                              by = UU::common_names(Services, Services_extras)) |>
    dplyr::left_join(dplyr::select(Enrollment_extra_Client_Exit_HH_CL_AaE, dplyr::all_of(
      c(
        "EnrollmentID",
        "PersonalID",
        "ProjectName",
        "EntryDate",
        "ExitAdjust",
        "ProjectID",
        "ProjectName"
      )
    )),
    by = c("PersonalID", "EnrollmentID")) |>
    unique() |>
    dplyr::mutate(
      ServiceEndAdjust = dplyr::if_else(
        is.na(ServiceEndDate) |
          ServiceEndDate > Sys.Date(),
        Sys.Date(),
        ServiceEndDate
      )
    ) |>
    # stray_service logic (not currently in use)
    # dplyr::mutate(
    #   service_interval = lubridate::interval(start = ServiceStartDate, end = ServiceEndAdjust),
    #   ee_interval = lubridate::interval(start = EntryDate, end = ExitAdjust),
    #   intersect_tf = lubridate::int_overlaps(service_interval, ee_interval),
    #   stray_service = is.na(intersect_tf) |
    #     intersect_tf == FALSE
    # ) |>
    dplyr::select(
      UniqueID,
      PersonalID,
      ServiceID,
      EnrollmentID,
      ProjectName,
      HouseholdID,
      ServiceStartDate,
      ServiceEndDate,
      RecordType,
      ServiceItemName,
      FundName,
      FundingSourceID,
      ServiceAmount
      #, stray_service
    )
  app_env$gather_deps(Services_enroll_extras)
}


#' @title Load Referrals
#' @description Loads Referrals, Referrals_full (unfiltered), and referral_result_summarize with filtering expressions used later
#' @inherit data_quality_tables params return
#' @export

load_referrals <- function(Referrals,
                           app_env = get_app_env(e = rlang::caller_env())) {
  Referrals <- Referrals |>
    dplyr::rename_with(.cols = - dplyr::matches("(?:^PersonalID)|^(?:^UniqueID)"), rlang::as_function(~paste0("R_",.x))) |>
    dplyr::mutate(R_ReferralConnectedPTC = stringr::str_remove(R_ReferralConnectedPTC, "\\s\\(disability required\\)$"),
                  R_ReferralConnectedPTC = dplyr::if_else(R_ReferralConnectedPTC == "Homeless Prevention", "Homelessness Prevention", R_ReferralConnectedPTC),
                  R_ReferralConnectedPTC = HMIS::hud_translations$`2.02.6 ProjectType`(R_ReferralConnectedPTC))

  # Full needed for dqu_aps
  Referrals_full <- Referrals

  referrals_expr <- rlang::exprs(
    housed1 = R_RemovedFromQueueSubreason %in% c(
      "Housed with Community Inventory",
      "Housed with Community Inventory - Not with CE",
      "Permanently Living with Family/Friends",
      "Return To Prior Residence",
      "Rental By Client"
    ),
    housed2 = !is.na(R_ReferralConnectedMoveInDate),
    housed3 = R_ExitHoused == "Housed",
    is_last = R_IsLastReferral == "Yes",
    is_last_enroll = R_IsLastEnrollment == "Yes",
    is_active = R_ActiveInProject == "Yes",
    accepted = stringr::str_detect(R_ReferralResult, "accepted$"),
    coq = R_ReferralCurrentlyOnQueue == "Yes"
  )
  referral_result_summarize <- purrr::map(referrals_expr, ~rlang::expr(isTRUE(any(!!.x, na.rm = TRUE))))


  Referrals <- Referrals |>
    filter_dupe_soft(!!referrals_expr$is_last_enroll,
                     !!referrals_expr$is_last,
                     !!referrals_expr$is_active,
                     !is.na(R_ReferralResult),
                     !!referrals_expr$housed3 & !!referrals_expr$accepted,
                     key = PersonalID) |>
    filter_dupe_last_EnrollmentID(key = PersonalID, R_ReferredEnrollmentID) |>
    dplyr::arrange(dplyr::desc(R_ReferredEnrollmentID)) |>
    dplyr::distinct(dplyr::across(-R_ReferredEnrollmentID), .keep_all = TRUE)

  app_env$gather_deps(Referrals, Referrals_full, referral_result_summarize)
}
#' @title Filter duplicates without losing any values from `key`
#'
#' @param .data \code{(data.frame)} Data with duplicates
#' @param ... \code{(expressions)} filter expressions with which to filter
#' @param key \code{(name)} of the column key that will be grouped by and for which at least one observation will be preserved.
#'
#' @return \code{(data.frame)} without duplicates
#' @export

filter_dupe_soft <- function(.data, ..., key) {
  .key <- rlang::enexpr(key)
  out <- .data
  x <- janitor::get_dupes(.data, !!.key) |>
    dplyr::arrange(PersonalID)

  clients <- dplyr::pull(x, !!.key) |> unique()
  .exprs <- rlang::enquos(...)
  to_add <- list()
  for (ex in .exprs) {
    new <- dplyr::filter(x, !!ex)

    new_n <- dplyr::summarise(dplyr::group_by(new, !!.key), n = dplyr::n())
    .to_merge <- dplyr::filter(new_n, n == 1) |> dplyr::pull(!!.key)

    # if some were reduced but not to one
    .reduced <- dplyr::left_join(new_n,
                                 dplyr::summarise(dplyr::group_by(x, !!.key), n = dplyr::n()), by = rlang::expr_deparse(.key), suffix = c("_new", "_old")) |>
      dplyr::filter(n_new < n_old & n_new > 1) |>
      dplyr::pull(!!.key)

    if (UU::is_legit(.to_merge)) {
      # remove rows where key is reduced to one, bind the deduplicated rows
      to_add <- append(to_add, list(dplyr::select(new, -dupe_count) |>
                                      dplyr::filter(!!.key %in% .to_merge)))
      # filter to_merge from dupes
      x <- dplyr::filter(x, !((!!.key) %in% .to_merge))
    }

    if (UU::is_legit(.reduced)) {
      # filter reduced from dupes
      x <- dplyr::filter(x, !((!!.key) %in% .reduced ) # is not one that was reduced
                         | (!!.key %in% .reduced & !!ex) # or matched the filter
      )

    }
  }
  to_add <- dplyr::bind_rows(to_add)
  out <- dplyr::filter(out, !(!!.key %in% c(to_add[[.key]], x[[.key]]))) |>
    dplyr::bind_rows(to_add, x)

  if (anyDuplicated(out[[.key]])) {
    rlang::warn("Duplicates still exist.")
  }
  out
}



#' @title Filter for the Last Enrollment ID
#' @description The maximum EnrollmentID will always be the latest in Clarity. This function will find the last EnrollmentID per `key`
#' @inheritParams filter_dupe_soft
#' @param EnrollmentID \code{(name)} of EnrollmentID column that will be filtered for the maximum (latest)
#'
#' @return \code{(data.frame)} With only the latest Enrollment
#' @export

filter_dupe_last_EnrollmentID <- function(.data, key, EnrollmentID) {
  .key <- rlang::enexpr(key)
  .eid <- rlang::enexpr(EnrollmentID)
  x <- janitor::get_dupes(.data, !!.key)
  x <- dplyr::group_by(x, !!.key)
  x <- dplyr::filter(x, as.numeric(!!.eid) == max(as.numeric(!!.eid)) | is.na(!!.eid))
  dplyr::bind_rows(
    dplyr::filter(.data, !(!!.key) %in% x[[.key]]),
    x
  )
}
