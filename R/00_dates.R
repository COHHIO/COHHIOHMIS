dates <- function(clarity_api = get_clarity_api(e = rlang::caller_env()),
                  app_env = get_app_env(e = rlang::caller_env()),
                  error = FALSE
) {

  force(app_env)
  force(clarity_api)
  rm_dates <- list()
  first <- lubridate::floor_date(Sys.Date(), "year")
  hc <- list(
    data_goes_back_to =  first - lubridate::years(2)
    )

  hc$unsheltered_data_start <- hc$data_goes_back_to
  hc$outreach_to_cls <- hc$check_dq_back_to <-  lubridate::make_date(lubridate::year(hc$data_goes_back_to), month = 10, day = 1) # the default ReportStart for DQ reporting
  hc$check_eligibility_back_to <- hc$check_dq_back_to - lubridate::years(3)
  hc$spm_range <- lubridate::interval(hc$check_dq_back_to, lubridate::`year<-`(hc$check_dq_back_to, lubridate::year(hc$check_dq_back_to) + 1) - lubridate::days(1))
  hc$project_eval_start = hc$data_goes_back_to + lubridate::years(1)
  hc$project_eval_end = lubridate::ceiling_date(hc$project_eval_start, "year")
  hc$project_eval_docs_due = lubridate::make_date(lubridate::year(hc$project_eval_end), 4, 23)
  hc$lsa_range <- hc$check_dq_back_to |>
    {\(x) {lubridate::interval(x, x + lubridate::years(1) - lubridate::days(1))}}()

  rm_dates$hc <- append(hc, purrr::map(
    c(
      bos_start_vaccine_data = "02052021",

      psh_started_collecting_move_in_date = "10012017",

      began_collecting_covid_data = "04012020",

      began_requiring_spdats = "01012019",

      prior_living_situation_required = "10012016",

      no_more_svcs_on_hh_members = "02012019",

      first_vaccine_administered_in_us = "12142020"
    ),
    lubridate::mdy
  ))



  # Dates from Metadata -----------------------------------------------------


  Export <- clarity_api$Export()

  rm_dates$meta_HUDCSV <- list(
    Export_Date = Export[["ExportDate"]][1],
    Export_Start = Export[["ExportStartDate"]][1],
    Export_End = Export[["ExportEndDate"]][1]
  )

  # Calculated Dates --------------------------------------------------------
  Exit <- clarity_api$Exit()
  rm_dates$calc <- list(data_goes_back_to =
                          Exit %>%
                          dplyr::arrange(ExitDate) %>%
                          utils::head(1) %>%
                          dplyr::pull(ExitDate))




  rm_dates$calc$full_date_range <-
    lubridate::interval(rm_dates$meta_HUDCSV$Export_End,
                        rm_dates$calc$data_goes_back_to)

  rm_dates$calc$two_yrs_prior_end <-
    lubridate::floor_date(Sys.Date(), "month") - lubridate::days(1)
  rm_dates$calc$two_yrs_prior_start <-
    lubridate::as_date(lubridate::floor_date(rm_dates$calc$two_yrs_prior_end, "month") - lubridate::years(2) + lubridate::dmonths(1))

  rm_dates$calc$two_yrs_prior_range <- lubridate::interval(rm_dates$calc$two_yrs_prior_start,
                                                           rm_dates$calc$two_yrs_prior_end)




  if(rm_dates$meta_HUDCSV$Export_Start > rm_dates$hc$data_goes_back_to |
     rm_dates$meta_HUDCSV$Export_End != Sys.Date())
    stop_with_instructions("The HUD CSV Export is not up to date", error = error)


  #  Check recency of Extras ----
  # Mon Aug 09 17:09:43 2021
  extras_last_update <- UU::last_updated(dirs$extras, path = TRUE)

  extra_info <- list(missing = setdiff(names(clarity.looker::folder_looks(clarity_api$folders$`HUD Extras`)), UU::ext(basename(names(extras_last_update)), strip = TRUE)),
                     not_updated = purrr::keep(extras_last_update, ~!lubridate::`%within%`(.x, lubridate::interval(lubridate::floor_date(Sys.Date(), "day") - 1, Sys.time()))))

  rm_dates$meta_Rmisc_last_run_date <- mean(extras_last_update)

  purrr::iwalk(purrr::when("missing" %in% names(extra_info), . ~ list(extra_info), ~extra_info), ~{
    if (UU::is_legit(.x)) {
      .fp <- paste0(basename(names(.x)), collapse = ", ")
    stop_with_instructions(glue::glue("The following extras ({{.path {dirs$extras}}}) are {switch(.y, missing = 'missing', not_updated = 'not up to date')}: {.fp}"), error = error)
    }
  })
  # Gather Dependencies ----
  # Mon Aug 09 17:09:52 2021

  app_env$gather_deps(rm_dates)
}
