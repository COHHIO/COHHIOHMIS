
#' @title Redact PII from Client HUD Export
#' @description This redacts all PII (except DOB) from Client HUD Export
#' @param Client \code{(tibble)} Client HUD Export
#' @return \code{(tibble)} Redacted Client HUD Export
#' @export

Client_redact <- function(Client) {
  Client %>%
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


#' @title Add the UniqueID to the Client export
#'
#' @param Client \code{(data.frame)} Client HUD CSV Export item
#' @param Client_extras \code{(data.frame)} A custom look linking PersonalID & UniqueID
#' @param app_env
#'
#' @return
#' @export
#'
#' @examples
Client_add_UniqueID <- function(Client, Client_extras, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  dplyr::left_join(Client, dplyr::select(Client_extras, PersonalID, UniqueID), by = "PersonalID")
}

#' Add Exit data to Enrollments
#'
#' @param Enrollment
#' @param Exit
#'
#' @return \code{(data.frame)} Enrollments with `ExitDate`, `Destination`, `OtherDestination` and derived column `ExitAdjust`
#' @export

Enrollment_add_Exit <- function(Enrollment, Exit) {
  Enrollment <- dplyr::left_join(Enrollment, Exit |> dplyr::select(EnrollmentID,
                                                     ExitDate,
                                                     Destination,
                                                     OtherDestination), by = "EnrollmentID")  |>
    dplyr::mutate(ExitAdjust = dplyr::if_else(is.na(ExitDate) |
                                  ExitDate > Sys.Date(),
                                Sys.Date(), ExitDate))

}



#' Add Household Information to Enrollment from Project
#'
#' @param Enrollment with Exit data. See `Enrollment_add_Exit`
#' @param Project
#' @param hc From `dates`
#'
#' @return \code{(data.frame)} of Enrollment with Household Columns `MoveInDateAdjust` appended
#' @export

Enrollment_add_Household = function(Enrollment, Project, rm_dates) {
  # getting HH information
  # only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.


  small_project <- Project %>%
    dplyr::select(ProjectID, ProjectType, ProjectName)
  # TODO Check to see if Enrollment data has the MoveInDate
  # TODO Does Move-in Date in Clarity auto-populate from previous enrollments?
  HHMoveIn <- Enrollment %>%
    dplyr::left_join(
      # Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust
      small_project,
      by = "ProjectID") %>%
    dplyr::filter(ProjectType %in% c(3, 9, 13)) %>%
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
    ) %>%
    dplyr::filter(!is.na(ValidMoveIn)) %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::mutate(HHMoveIn = min(ValidMoveIn, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(HouseholdID, HHMoveIn) %>%
    unique()

  HHEntry <- Enrollment %>%
    dplyr::left_join(small_project, by = "ProjectID") %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::mutate(FirstEntry = min(EntryDate, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(HouseholdID, "HHEntry" = FirstEntry) %>%
    unique() %>%
    dplyr::left_join(HHMoveIn, by = "HouseholdID")


  Enrollment <- Enrollment %>%
    dplyr::left_join(small_project, by = "ProjectID") %>%
    dplyr::left_join(HHEntry, by = "HouseholdID") %>%
    dplyr::mutate(
      # Puts EntryDate as MoveInDate for projects that don't use a MoveInDate
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
}

#' Add Veteran Coordinated Entry Date to Enrollments
#'
#' @param Enrollment that includes Exit Data. See `Enrollment_add_Exit`
#' @param VeteranCE
#'
#' @return \code{(data.frame)} Enrollment with the following columns added `PHTrack`, `ExpectedPHDate`, `ExitAdjust`
#' @export
Enrollment_add_VeteranCE = function(Enrollment, VeteranCE) {

  Enrollment |>
    # Join Veteran data
    dplyr::left_join(VeteranCE  |>  dplyr::select(EnrollmentID, PHTrack, ExpectedPHDate), by = "EnrollmentID") |>
    dplyr::mutate(
      ExitAdjust = dplyr::if_else(
        is.na(ExitDate) |
          ExitDate > lubridate::today(),
        lubridate::today(),
        ExitDate
      )
    )
}

#' Add Client Location to Enrollment
#'
#' @param Enrollment
#' @param EnrollmentCoc
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

#' Add AgeAtEntry to Enrollment
#' @description AgeAtEntry is the time elapsed from the Date of Birth `DOB` to the `EntryDate`
#' @param Enrollment
#' @param Client
#'
#' @return \code{(data.frame)} Enrollment with `AgeAtEntry` column
#' @export

Enrollment_add_AgeAtEntry_UniqueID <- function(Enrollment, Client) {
  dplyr::left_join(Enrollment, dplyr::select(Client, UniqueID, PersonalID, DOB), by = "PersonalID") |>
    dplyr::mutate(AgeAtEntry = age_years(DOB, EntryDate)) |>
    dplyr::select(-DOB)
}



#' Add the Corresponding Region for each Project by way of Geocode matching
#'
#' @param provider_extras
#' @param dirs
#'
#' @return \code{(data.frame)} provider_extras with Regions column
#' @export

Pe_add_regions <- function(provider_extras, dirs) {
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

  Regions <- clarity.looker::hud_load("Regions", dirs$public)

  out <- out |>
    dplyr::left_join(Regions |> dplyr::select(- RegionName), by = "County") |>
    dplyr::rename(ProjectRegion = "Region",
                  ProjectCounty = "County")


  # Special cases
  #  St. Vincent de Paul of Dayton serves region 13
  out[out$Geocode == 391361, "ProjectRegion"] <- 13

  # Missing Regions
  # missing_region <- out |>
  #   dplyr::filter(is.na(ProjectRegion) & ProgramCoC == "OH-507")
  # missing_region |>
  #   dplyr::pull(ProjectCounty) |>
  #   unique()

  out |> dplyr::filter(!is.na(ProjectRegion))
}

#' Add Access Points to Provider_extras
#' @description Create data.frame of Coordinated Entry Access Points with info about the Counties & Populations Served
#' @param provider_extras \code{(data.frame)} provider_extras with Regions, see `Pe_add_regions`
#' @param dirs
#'
#' @return \code{(data.frame)}
#' @export

Pe_create_APs = function(provider_extras, dirs) {
  Regions <- clarity.looker::hud_load("Regions", dirs$public)
  APs <- provider_extras |>
    dplyr::select( !tidyselect::starts_with("CoCComp") & !Geocode:ZIP) |>
    dplyr::filter(ProjectTypeCode == "Coordinated Entry") |>
    tidyr::pivot_longer(tidyselect::starts_with("AP"), names_to = "TargetPop", names_pattern = "(?<=^APCounties)(\\w+)", values_to = "CountiesServed") |>
    dplyr::filter(!is.na(CountiesServed)) |>
    dplyr::select(!tidyselect::starts_with("AP") & !ProjectTypeCode)

  # Programs serve multiple Counties which may fall into multiple regions. This creates a row for each Region served by a Program such that Coordinated Entry Access Points will show all the appropriate programs when filtering by Region.
  # @Rm
  APs <- slider::slide_dfr(APs, ~{
    .counties <- trimws(stringr::str_split(.x$CountiesServed, ",\\s")[[1]])

    .x |>
      dplyr::select(- ProjectRegion) |>
      dplyr::bind_cols(Region = unique(Regions$Region[Regions$County %in% .counties]))
  }) |>
    dplyr::distinct_all()

  APs
}

#' @title Add ProjectType (dbl) to provider_extras
#'
#' @param provider_extras
#' @return \code{(data.frame)}

Pe_add_ProjectType <- function(provider_extras) {
  PT <- hud.extract::hud_translations$`2.02.6 ProjectType`(table = TRUE) |>
    tibble::add_row(Value = 12, Text = "Homeless Prevention")
  purrr::map_dbl(provider_extras$ProjectTypeCode, ~PT$Value[agrepl(stringr::str_remove(.x, "\\s\\([\\w\\s]+\\)$"), PT$Text)])
  provider_extras |>
    dplyr::rowwise() |>
    dplyr::mutate(ProjectType = PT$Value[agrepl(stringr::str_remove(ProjectTypeCode, "\\s\\([\\w\\s]+\\)$"), PT$Text)], .after = "ProjectTypeCode")
}

#' Add GrantType column to provider_extras
#' @description GrantType indicates if the program is funded by one of HOPWA, PATH, SSVF, or RHY
#'
#' @param provider_extras
#'
#' @return
#' @export
Pe_add_GrantType = function(provider_extras) {
  hash <- hud.extract::hud_translations$`2.06.1 FundingSource`(table = TRUE)

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
  add_regions = Pe_add_regions,
  add_ProjectType = Pe_add_ProjectType,
  create_APs = Pe_create_APs,
  add_GrantType = Pe_add_GrantType
)
