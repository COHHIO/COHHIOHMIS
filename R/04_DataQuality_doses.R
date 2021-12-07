
# Missing Vaccine data ----------------------------------------------------
#' @title Find Missing or incorrect vaccine data for enrolled & exited clients
#' @param Doses \code{(data.frame)} See the `[["HUD Extras"]]$Client_Doses_extra` method in the instantiated clarity_api object
#' @family Clarity Checks
#' @family DQ: Vaccines
#' @inherit data_quality_tables params return
#' @describeIn data_quality_tables
#' @inheritParams served_in_date_range

dq_vax <- function(served_in_date_range, mahoning_projects = NULL, Doses = NULL, rm_dates = NULL, vars, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
 dose_exp = rlang::exprs(
   not_mp = !ProjectID %in% mahoning_projects,
   na_ed = is.na(ExitDate),
   ed_start = (
     is.na(ExitDate) |
       ExitDate >= rm_dates$hc$bos_start_vaccine_data
   ),
   vax_consent = (
     C19ConsentToVaccine == "Data not collected" |
       is.na(C19ConsentToVaccine)
   ),
   p_types = (ProjectType %in% c(1, 2, 4, 8) |
                (
                  ProjectType %in% c(3, 9, 13) &
                    is.na(MoveInDateAdjust)
                )),
   vax_manu = stringr::str_starts(C19VaccineManufacturer, "Client doesn't know"),
   vax_self_report = C19VaccineDocumentation == "Self-report"
 )

  served_in_date_range <- served_in_date_range |>
    HMIS::served_between(rm_dates$hc$bos_start_vaccine_data, lubridate::today()) %>%
    dplyr::filter(!!dose_exp$not_mp) |>
    dplyr::left_join(Doses,
                     by = c("PersonalID", "UniqueID"))

  out <- list()
  out$missing_vax_exited <- dplyr::filter(
    served_in_date_range,
      !(!!dose_exp$na_ed) &
      !!dose_exp$ed_start &
      !!dose_exp$vax_consent &
      !!dose_exp$p_types
  ) |>
    dplyr::mutate(
      Type = "Warning",
      Issue = "Vaccine data not collected and client has exited",
      Guidance = guidance$vax_missing_exit
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  out$missing_vax_current <- dplyr::filter(served_in_date_range,
      !!dose_exp$na_ed &
      !!dose_exp$ed_start &
      !!dose_exp$vax_consent &
      !!dose_exp$p_types
  ) %>%
    dplyr::mutate(
      Type = "Error",
      Issue = "Vaccine data not collected on current client",
      Guidance = guidance$vax_missing_current
    ) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  out$vax_incorrect_date <- dplyr::filter(served_in_date_range,
                C19AssessmentDate < rm_dates$hc$first_vaccine_administered_in_us) %>%
    dplyr::mutate(Type = "Error",
                  Issue = "Vaccine Date Incorrect",
                  Guidance = guidance$vax_incorrect_date) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  out$vax_incorrect_manufacturer <-
    dplyr::filter(served_in_date_range,
                  !!dose_exp$vax_manu &
                    !(!!dose_exp$vax_self_report)) %>%
    dplyr::mutate(Type = "Error",
                  Issue = "Incorrect Vaccine Manufacturer or Incorrect Documentation Type",
                  Guidance = guidance$vax_incorrect_manufacturer) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  out$vax_unknown_manufacturer <-
    dplyr::filter(served_in_date_range,
                  !!dose_exp$vax_manu &
                    !!dose_exp$vax_self_report) %>%
    dplyr::mutate(Type = "Warning",
                  Issue = "Unknown Vaccine Manufacturer",
                  Guidance = guidance$vax_unknown_manufacturer) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dplyr::bind_rows(out)
}



  # Missing Vaccine data (OLD LOGIC) ----------------------------------------------------

  # dose_counts <- Doses |>
  #   dplyr::mutate(Doses = sum(!is.na(C19Dose1Date), !is.na(C19Dose2Date), na.rm = TRUE)) |>
  #   dplyr::select(PersonalID, Doses) |>
  #   dplyr::distinct(PersonalID, .keep_all = TRUE)

  # missing_vaccines <- dq_missing_vaccines(served_in_date_range = served_in_date_range, mahoning_projects = mahoning_projects, Doses = Doses, rm_dates = rm_dates, vars = vars, app_env = app_env)




  # Dose Warnings -----------------------------------------------------------
  # dose_date_error <- dq_dose_date_error(served_in_date_range = served_in_date_range, Doses = Doses, rm_dates = rm_dates, vars = vars, app_env = app_env)


  # NOTE: Data not available in Clarity for this check
  # dose_date_warning <- Doses %>%
  #   dplyr::group_by(PersonalID) %>%
  #   dplyr::summarise(Doses = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(Doses > 1) %>%
  #   dplyr::left_join(Doses, by = "PersonalID") %>%
  #   dplyr::group_by(PersonalID) %>%
  #   # TODO LastDose should be taken care of by C19DoseDate on the Looker end
  #   dplyr::mutate(LastDose = dplyr::lag(C19DoseDate, order_by = C19DoseDate)) %>%
  #   dplyr::filter(!is.na(LastDose)) %>%
  #   dplyr::mutate(DaysBetweenDoses = difftime(C19Dose1Date, C19Dose2Date, units = "days")) %>%
  #   dplyr::filter(C19DoseDate < rm_dates$hc$first_vaccine_administered_in_us |
  #                   DaysBetweenDoses < 20 |
  #                   (C19VaccineManufacturer == "Moderna") &
  #                   DaysBetweenDoses < 27) %>%
  #   dplyr::left_join(HMIS::served_between(served_in_date_range, rm_dates$hc$bos_start_vaccine_data, lubridate::today()),
  #                    by = "PersonalID") %>%
  #   dplyr::mutate(Type = "Warning",
  #                 Issue = "Vaccine Dates or Vaccine Manufacturer Questionable",
  #                 Guidance = "The number of days between vaccines doses does not match
  #        the vaccine manufacturer’s recommended timeline. One of the vaccine
  #        records' Vaccine Date or the Vaccine Manufacturer may be entered
  #        incorrectly.") %>%
  #   dplyr::select(dplyr::all_of(vars$we_want))

  # NOTE: Data not available in Clarity for this check
  # differing_manufacturers <- Doses %>%
  #   dplyr::group_by(PersonalID) %>%
  #   dplyr::summarise(Doses = dplyr::n(), .group = TRUE) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(Doses > 1) %>%
  #   dplyr::left_join(doses, by = "PersonalID") %>%
  #   dplyr::group_by(PersonalID) %>%
  #   dplyr::mutate(
  #     minManufacturer = min(C19VaccineManufacturer),
  #     maxManufacturer = max(C19VaccineManufacturer),
  #     differs = minManufacturer != maxManufacturer,
  #     Type = "Error",
  #     Issue = "Client received different vaccines",
  #     Guidance = "The data shows that the client received vaccines from
  #   different manufacturers, but this is highly unlikely. Please correct the
  #   data in HMIS or let us know if the client actually received vaccines from
  #   different manufacturers."
  #   ) %>%
  #   dplyr::filter(differs == TRUE) %>%
  #   dplyr::left_join(served_in_date_range %>%
  #                      dplyr::filter(HMIS::served_between(., rm_dates$hc$bos_start_vaccine_data, lubridate::today())),
  #                    by = "PersonalID") %>%
  #   dplyr::select(dplyr::all_of(vars$we_want))

