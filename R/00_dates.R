dates <- function(hud) {

  hc_data_goes_back_to <- mdy("01012019")

  hc_check_dq_back_to <- mdy("10012019") # the default ReportStart for DQ reporting

  hc_project_eval_start <- mdy("01012020")

  hc_project_eval_end <- mdy("12312020")

  hc_project_eval_docs_due <- mdy("04232021")

  hc_bos_start_vaccine_data <- mdy("02052021")

  hc_psh_started_collecting_move_in_date <- mdy("10012017")

  hc_began_collecting_covid_data <- mdy("04012020")

  hc_outreach_to_cls <- mdy("10012019")

  hc_began_requiring_spdats <- mdy("01012019")

  hc_unsheltered_data_start <- mdy("01012019")

  hc_prior_living_situation_required <- mdy("10012016")

  hc_check_eligibility_back_to <- mdy("10012016")

  hc_no_more_svcs_on_hh_members <- mdy("02012019")

  hc_first_vaccine_administered_in_us <- mdy("12142020")

  # Dates from Metadata -----------------------------------------------------

  Export <- hud$Export()

  meta_HUDCSV_Export_Date <- Export[["ExportDate"]][1]
  meta_HUDCSV_Export_Start <- Export[["ExportStartDate"]][1]
  meta_HUDCSV_Export_End <- Export[["ExportEndDate"]][1]



  meta_Rmisc_last_run_date <- floor_date(file.info(here("data/RMisc2.xlsx"))$mtime,
                                         unit = "day")

  # Calculated Dates --------------------------------------------------------
  Exit <- hud$Exit()
  calc_data_goes_back_to <-
    Exit %>%
    arrange(ExitDate) %>%
    head(1) %>%
    pull(ExitDate)

  calc_full_date_range <- interval(meta_HUDCSV_Export_End,
                                   calc_data_goes_back_to)

  calc_2_yrs_prior_end <- floor_date(Sys.Date(), "month") - days(1)
  calc_2_yrs_prior_start <-
    floor_date(calc_2_yrs_prior_end, "month") - years(2) + months(1)

  calc_2_yrs_prior_range <- interval(calc_2_yrs_prior_start,
                                     calc_2_yrs_prior_end)
  if(meta_HUDCSV_Export_Start != hc_data_goes_back_to |
     meta_HUDCSV_Export_End != Sys.Date())
    stop_with_instructions("The HUD CSV Export was not run on the correct date range.
                         Please rerun.\n")


  if(meta_Rmisc_last_run_date != Sys.Date())
    stop_with_instructions("The RMisc2.xlsx file is not up to date. Please run
                         this ART report and overwrite the current RMisc2.xlsx
                         with the new one.")
  rlang::env_get_list(nms = ls())
}
