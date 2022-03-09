ohfa_recipients <- function(Services_extras, date_begin = lubridate::make_date(year = lubridate::year(Sys.Date()) - 1, month = 4, day = 1), date_end = lubridate::floor_date(Sys.Date(), "year"), IncomeBenefits, Enrollment_extra_Client_Exit_HH_CL_AaE, dirs, col_cats, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  report <- file.path("data", "random", "EHAP -GrantReport.xlsx") |>
    {\(x) {UU::file_fn(x)(x, sheet = "Funding Recipients", skip = 2)[1:32]}}()
  needed_cols <- names(report) |>
    stringr::str_remove("\\.{3}\\d{1,2}$") |>
    unique()
  Services_Client <- Services_extras |>
    dplyr::filter(stringr::str_detect(FundName, "\\s?OHFA\\s?") &
                    (dplyr::between(ServiceEndDate, date_begin, date_end) | is.na(ServiceEndDate)))
  .enrollment <- Enrollment_extra_Client_Exit_HH_CL_AaE |>
    dplyr::filter(EnrollmentID %in% Services_Client$EnrollmentID) |>
    dplyr::select(EnrollmentID,
                  "Unique Identifier" = UniqueID,
                  ProjectID,
                  ProjectName,
                  LivingSituation,
                  PercentAMI,
                  Address = LastPermanentAddress,
                  City = LastPermanentCity,
                  `Zip Code` = LastPermanentZIP)
  .client <- clarity.looker::hud_load("Client.csv", path = dirs$export) |>
    dplyr::mutate(`Last 4 SSN` = stringr::str_sub(SSN, start = -4),
                  Age = age_years(DOB, Sys.Date()),
                  Gender = gender_col(!!!rlang::syms(col_cats$Client$gender)),
                  PersonalID = as.character(PersonalID)) |>
    dplyr::select(PersonalID,
                  `Last 4 SSN`,
                  Age,
                  Gender,
                  "First Name" = FirstName,
                  "Last Name" = LastName
    )
  .income <- IncomeBenefits |>
    dplyr::filter(EnrollmentID %in% Services_Client$EnrollmentID) |>
    dplyr::select(EnrollmentID, `Monthly Gross Income` = "TotalMonthlyIncome")




  dplyr::left_join(Services_Client, .income, by = "EnrollmentID") |>
    dplyr::left_join(.client, by = "PersonalID") |>
    dplyr::left_join(.enrollment, by = "EnrollmentID") |>
    dplyr::group_by(HouseholdID) |>
    dplyr::mutate(`Total # People` = dplyr::n(),
                  `# <18` = sum(Age < 18, na.rm = TRUE),
                  `# >60` = sum(Age > 60, na.rm = TRUE),
                  `AMGI Status` = HMIS::hud_translations$`V4.1 PercentAMI`(PercentAMI)) |>
    dplyr::select(dplyr::any_of(needed_cols), dplyr::starts_with("Service"), LivingSituation, ProjectName)


}
ohfa_grant_excels <- function(ohfa_recipients, path = file.path("data", "ohfa")) {
  UU::mkpath(path, mkdir = TRUE)
  ohfa_recipients |>
    dplyr::group_by(ProjectName) |>
    dplyr::group_split() |>
    purrr::walk(~{
      pn <- unique(.x$ProjectName)
      fn <- fs::path(path, pn, ext = "xlsx")
      writexl::write_xlsx(dplyr::select(.x, - ProjectName), path = fn)
      if (fs::file_exists(fn))
        cli::cli_alert_success("{pn} data written to {.path {fn}}")
    })
}
