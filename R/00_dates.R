dates <- function(
  clarity_api = get0("clarity_api", envir = rlang::caller_env()),
  app_env = get0("app_env", envir = rlang::caller_env()),
  .write = FALSE) {

  hc_data_goes_back_to <- lubridate::mdy("01012019")

  hc_check_dq_back_to <- lubridate::mdy("10012019") # the default ReportStart for DQ reporting

  hc_project_eval_start <- lubridate::mdy("01012020")

  hc_project_eval_end <- lubridate::mdy("12312020")

  hc_project_eval_docs_due <- lubridate::mdy("04232021")

  hc_bos_start_vaccine_data <- lubridate::mdy("02052021")

  hc_psh_started_collecting_move_in_date <- lubridate::mdy("10012017")

  hc_began_collecting_covid_data <- lubridate::mdy("04012020")

  hc_outreach_to_cls <- lubridate::mdy("10012019")

  hc_began_requiring_spdats <- lubridate::mdy("01012019")

  hc_unsheltered_data_start <- lubridate::mdy("01012019")

  hc_prior_living_situation_required <- lubridate::mdy("10012016")

  hc_check_eligibility_back_to <- lubridate::mdy("10012016")

  hc_no_more_svcs_on_hh_members <- lubridate::mdy("02012019")

  hc_first_vaccine_administered_in_us <- lubridate::mdy("12142020")

  # Dates from Metadata -----------------------------------------------------

  Export <- clarity_api$Export(.write = .write)

  meta_HUDCSV_Export_Date <- Export[["ExportDate"]][1]
  meta_HUDCSV_Export_Start <- Export[["ExportStartDate"]][1]
  meta_HUDCSV_Export_End <- Export[["ExportEndDate"]][1]



  # Calculated Dates --------------------------------------------------------
  Exit <- clarity_api$Exit(.write = .write)
   calc_data_goes_back_to <-
    Exit %>%
    dplyr::arrange(ExitDate) %>%
    utils::head(1) %>%
    dplyr::pull(ExitDate)

  calc_full_date_range <- lubridate::interval(meta_HUDCSV_Export_End,
                                              calc_data_goes_back_to)

  calc_2_yrs_prior_end <- lubridate::floor_date(Sys.Date(), "month") - lubridate::days(1)
  calc_2_yrs_prior_start <-
    lubridate::floor_date(calc_2_yrs_prior_end, "month") - lubridate::years(2) + lubridate::dmonths(1)

  calc_2_yrs_prior_range <- lubridate::interval(calc_2_yrs_prior_start,
                                                calc_2_yrs_prior_end)




  if(meta_HUDCSV_Export_Start != Sys.Date() |
     meta_HUDCSV_Export_End != Sys.Date())
    stop_with_instructions("The HUD CSV Export update process errored. Please rerun.\n")


  #  Check recency of Extras ----
  # Mon Aug 09 17:09:43 2021
  extras_last_update <- hud_last_updated(path = dirs$extras)

  extra_info <- list(missing = setdiff(names(.hud_extras), stringr::str_remove(names(extras_last_update), "\\.feather$")),
                     not_updated = purrr::keep(extras_last_update, ~!lubridate::`%within%`(.x, lubridate::interval(lubridate::floor_date(Sys.Date(), "day") - 1, Sys.time()))))

  purrr::iwalk(extra_info, ~{
    if (is_legit(extra_info$missing))
      stop_with_instructions(paste0("The following *_extra files are missing", paste0(extra_info$missing, collapse = ", ")))
    if (!is_legit(extra_info$not_updated))
      stop_with_instructions(paste0("The following files are not up to date: ", purrr::imap_chr(extra_info$not_update, ~paste0(paste0(.y,": ", .x), collapse = "\n"))))
  })
  # Gather Dependencies ----
  # Mon Aug 09 17:09:52 2021

  app_env$gather_deps("everything")
  app_env
}
