dates <- function(hud, write = FALSE) {

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

  Export <- hud$Export(path = "data/API", write = write)

  meta_HUDCSV_Export_Date <- Export[["ExportDate"]][1]
  meta_HUDCSV_Export_Start <- Export[["ExportStartDate"]][1]
  meta_HUDCSV_Export_End <- Export[["ExportEndDate"]][1]



  meta_Rmisc_last_run_date <- lubridate::floor_date(file.info("data/RMisc2.xlsx")$mtime,
                                                    unit = "day")

  # Calculated Dates --------------------------------------------------------
  Exit <- hud$Exit(write = write)
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
  #TODO
  # Ensure an update on each of these files each day:
  # Client
  # Enrollment
  # Export
  # Exit
  # Services
  if(meta_HUDCSV_Export_Start != Sys.Date() |
     meta_HUDCSV_Export_End != Sys.Date())
    stop_with_instructions("The HUD CSV Export update process errored. Please rerun.\n")


  if(meta_Rmisc_last_run_date != Sys.Date())
    stop_with_instructions("The RMisc Look update process errored.")
  environment()
}
