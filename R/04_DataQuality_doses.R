data_quality_doses <- function(variables) {
  # Missing Vaccine data ----------------------------------------------------
  #TODO C19 Column names need to be updated
  dose_counts <- Doses |>
    dplyr::mutate(Doses = sum(!is.na(C19Dose1Date), !is.na(C19Dose2Date), na.rm = TRUE)) |>
    dplyr::select(PersonalID, Doses) |>
    dplyr::distinct(PersonalID, .keep_all = TRUE)

  missing_vaccine_exited <- dq_missing_vaccine_exited(served_in_date_range = served_in_date_range, dose_counts = dose_counts, vars = vars, app_env = Rm_env)


  missing_vaccine_current <- dq_missing_vaccine_current(served_in_date_range, dose_counts, app_env = Rm_env)

  # Dose Warnings -----------------------------------------------------------
  dose_date_error <- dose_date_error(Doses, served_in_date_range, hc)


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

  unknown_manufacturer_error <- Doses %>%
    dplyr::filter(stringr::str_starts(C19VaccineManufacturer, "Client doesn't know") &
                    C19VaccineDocumentation != "Self-report") %>%
    dplyr::left_join(served_in_date_range %>%
                       dplyr::filter(HMIS::served_between(., rm_dates$hc$bos_start_vaccine_data, lubridate::today())),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Error",
                  Issue = "Incorrect Vaccine Manufacturer or Incorrect Documentation Type",
                  Guidance = "If vaccine information was collected via Healthcare Provider
         or Vaccine card, then the vaccine manufacturer should be known and
         updated in HMIS.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  unknown_manufacturer_warning <- Doses %>%
    dplyr::filter(stringr::str_starts(C19VaccineManufacturer, "Client doesn't know") &
                    C19VaccineDocumentation == "Self-report") %>%
    dplyr::left_join(served_in_date_range %>%
                       dplyr::filter(HMIS::served_between(., rm_dates$hc$bos_start_vaccine_data, lubridate::today())),
                     by = "PersonalID") %>%
    dplyr::mutate(Type = "Warning",
                  Issue = "Unknown Vaccine Manufacturer",
                  Guidance = "If the client does not know the manufacturer of the vaccine,
         please try to find another source for the information. Reporting relies
         heavily on knowing the manufacturer of the vaccine your client received.
         If you absolutely cannot find it, it is ok to leave as is.") %>%
    dplyr::select(dplyr::all_of(vars$we_want))

}
