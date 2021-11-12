dates <- function(clarity_api = get_clarity_api(e = rlang::caller_env()),
                  app_env = get_app_env(e = rlang::caller_env()),
                  error = FALSE
) {

  force(app_env)
  force(clarity_api)
  rm_dates <- list()
  rm_dates$hc <- purrr::map(list(
    data_goes_back_to = "01012019",

    check_dq_back_to = "10012019", # the default ReportStart for DQ reporting

    project_eval_start = "01012020",

    project_eval_end = "12312020",

    project_eval_docs_due = "04232021",

    bos_start_vaccine_data = "02052021",

    psh_started_collecting_move_in_date = "10012017",

    began_collecting_covid_data = "04012020",

    outreach_to_cls = "10012019",

    began_requiring_spdats = "01012019",

    unsheltered_data_start = "01012019",

    prior_living_situation_required = "10012016",

    check_eligibility_back_to = "10012016",

    no_more_svcs_on_hh_members = "02012019",

    first_vaccine_administered_in_us = "12142020"
  ), lubridate::mdy)

  # Dates from Metadata -----------------------------------------------------


  Export <- cl_api$Export()

  rm_dates$meta_HUDCSV <- list(
    Export_Date = Export[["ExportDate"]][1],
    Export_Start = Export[["ExportStartDate"]][1],
    Export_End = Export[["ExportEndDate"]][1]
  )












  # Calculated Dates --------------------------------------------------------
  Exit <- cl_api$Exit()
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




  if(rm_dates$meta_HUDCSV$Export_Start != rm_dates$hc$data_goes_back_to |
     rm_dates$meta_HUDCSV$Export_End != Sys.Date())
    stop_with_instructions("The HUD CSV Export update process errored. Please rerun.\n", error = error)


  #  Check recency of Extras ----
  # Mon Aug 09 17:09:43 2021
  extras_last_update <- clarity.looker::hud_last_updated(path = dirs$extras)

  extra_info <- list(missing = setdiff(names(clarity.looker::folder_looks(cl_api$folders$`HUD Extras`)), stringr::str_remove(names(extras_last_update), "\\.feather$")),
                     not_updated = purrr::keep(extras_last_update, ~!lubridate::`%within%`(.x, lubridate::interval(lubridate::floor_date(Sys.Date(), "day") - 1, Sys.time()))))

  rm_dates$meta_Rmisc_last_run_date <- mean(do.call(c, extras_last_update))
  purrr::iwalk(extra_info, ~{

    if (UU::is_legit(extra_info$missing))
      stop_with_instructions(paste0("The following *_extra files are missing ", paste0(extra_info$missing, collapse = ", ")), error = error)
    if (UU::is_legit(extra_info$not_updated))
      stop_with_instructions(paste0("The following files are not up to date: ", paste0(purrr::imap_chr(extra_info$not_update, ~paste0(.y,": ", .x)), collapse = "\n")), error = error)
  })
  # Gather Dependencies ----
  # Mon Aug 09 17:09:52 2021

  app_env$gather_deps("everything")
}
