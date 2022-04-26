# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.


#' @title Does a vector/data.frame have no values?
#'
#' @param x \code{(data.frame/vector)}
#'
#' @return \code{(logical)}
#' @export

is_zero <- function(x) {
  UseMethod("is_zero")
}

#' @export
is_zero.data.frame <- function(x) {
  nrow(x) == 0
}

#' @export
is_zero.default <- function(x) {
  length(x) == 0
}

glue_skip_NA <- function(..., str_expr, na = "omit") {
  .data <- tibble::tibble(...)
  if (!nchar(na))
    is.na(.data) <- ""
  .d <- na.omit(.data)
  if (!is_zero(.d))
    glue::glue_data(.d, str_expr)
  else
    ""
}

gender_col <- function(Female, Male, NoSingleGender, Transgender, Questioning, GenderNone) {
  .data <-
    tibble::tibble(Female = Female,
                   Male = Male,
                   NoSingleGender = NoSingleGender,
                   Transgender = Transgender,
                   Questioning = Questioning,
                   GenderNone = GenderNone)
  nms <- names(.data)
  apply(.data, 1, function (x) {
    paste0(nms[which(as.numeric(x) > 0)], collapse = ", ")
    })
}

# stop_with_instructions ----
# Wed Mar 24 16:38:01 2021
#' @title Stop daily update with an informative error
#' @description Throws an error in 00_daily_update.R with additional details
#' @param ... Error Messages to print to the console
#' @export
stop_with_instructions <- function(..., error = FALSE) {
  .msgs <- paste0(..., collapse = "\n")
  .msg <- cli::cli_warn(c(x = .msgs, i = "Please contact {.emph hmisapps@cohhio.org} for help!"))
  if (error)
    stop(.msg, .call = FALSE)
  else {

    # authfile <- ifelse(clarity.looker::is_dev(), file.path("inst", "vault", "rminor@rminor-333915.iam.gserviceaccount.com.json"), file.path(system.file(package = "RmData"), "auth", "rminor@rminor-333915.iam.gserviceaccount.com.json"))
    # token <- gargle::token_fetch(scopes = "https://www.googleapis.com/auth/gmail.compose", gargle::credentials_service_account(scopes = "https://www.googleapis.com/auth/gmail.compose", path = authfile))
    # gmailr::gm_auth_configure()
    # gmailr::gm_auth(token = token)
  }

}

# increment ----
# Wed Mar 24 18:00:02 2021
#' @title increment steps of 00_copy_images.R
#' @description Provides informative messages and timing for 00_copy_images.R
#' @param ... Messages to console
#' @param cenv the calling environment
#' @export

increment <- function(..., cenv = rlang::caller_env()) {
  # pre allocate file path for previous timer
  .lt_path <- "data/last_timer.rds"

  # if the first step remove tracking objects from env (if there were previous
  # failures)
  msg <- paste0(...)
  if (stringr::str_detect(msg, "Importing raw"))
    suppressWarnings(rm(.update, .timer, .step, envir = cenv))

  # start the status progress process if it's not active
  if (is.null(cenv$.update)) cenv$.update <-
    cli::cli_process_start("Parsing COHHIO_HMIS data",
                           .auto_close = FALSE,
                           .envir = cenv)

  # if the last timer data exists load it and compute the total time from the
  # previous run
  if (file.exists(.lt_path) && is.null(cenv$.last_timer)) {
    cenv$.last_timer <- readRDS(.lt_path)
    cenv$.total_time <- difftime(utils::tail(cenv$.last_timer, 1)$ts,
                                 utils::head(cenv$.last_timer, 1)$ts, units = "mins")
    cenv$.total_steps <- utils::tail(cenv$.last_timer, 1)$step
    cli::cli_status_update(cenv$.update,
                           cli::col_blue("Expected time of completion: ",
                                         Sys.time() + cenv$.total_time))
  }
  # create the step object or increment it
  if (is.null(cenv$.step)) {
    cenv$.step <- 1
  } else {
    cenv$.step <- cenv$.step + 1
  }

  # send the status message to console
  cli::cli_status_update(cenv$.update,
                         msg = "Step {cenv$.step}/
                         {rlang::`%||%`(cenv$.total_steps, 12)}:
                         {msg}...")

  if (is.null(cenv$.timer)) cenv$.timer <-
    data.frame(ts = Sys.time(), step = cenv$.step, msg = msg)
  else {
    cenv$.timer <- rbind(cenv$.timer,
                         data.frame(ts = Sys.time(),
                                    step = cenv$.step,
                                    msg = msg))
  }
  if (stringr::str_detect(msg,"Done!")) {
    cli::cat_rule()
    cli::cat_boxx(cli::col_blue("Completed at ",
                                Sys.time(),
                                "\nSee ",
                                .lt_path),
                  border_style = "single",
                  padding  = 1,
                  margin = 0,
                  float = "center")
    cli::cli_process_done(cenv$.update)
    saveRDS(cenv$.timer, .lt_path)

    return(.lt_path)
  }
  # If no previous timer data, just give the elapsed time
  .elapsed <- round(difftime(utils::tail(cenv$.timer, 1)$ts,
                             utils::head(cenv$.timer, 1)$ts, units = "mins"),2)
  if (is.null(cenv$.last_timer)) {
    cli::cli_status_update(cenv$.update,
                           cli::col_grey("Time elapsed: ", .elapsed, " mins"))
  } else {
    cli::cli_status_update(cenv$.update,
                           cli::cli_verbatim(cli::col_magenta(
                             "Time elapsed: ",
                             .elapsed,
                             " mins - ",
                             paste0(
                               round(as.numeric(.elapsed) /
                                       as.numeric(cenv$.total_time), 2) * 100,
                               "% complete\nApprox. completion at: ",
                               cenv$.total_time - .elapsed + Sys.time()
                             )
                           )))
  }
}
# Age Function ------------------------------------------------------------

age_years <- function(earlier, later)
{
  lubridate::time_length(lubridate::interval(earlier, later), "years")
}

#' @title Copy files to directory
#' @description Copy a character vector of file paths to a directory with informative messages
#' @param files \code{(character)} of file paths
#' @param dir \code{(character)} path to directory where files are to be copied
#' @inheritParams base::file.copy
#' @export

copy_lgl <- function(files, dir, overwrite) {
  purrr::map_lgl(files, ~{
    .c <- file.copy(.x, to = dir, overwrite = overwrite)
    if (.c) message(.x, " copied to ", file.path(dir, basename(.x)))
    else
      message(.x, " did not copy. Perhaps it already exists? Set overwrite = TRUE to overwrite.")
    .c
  })
}

freeze_pe <- function(dir, overwrite = FALSE) {
  # if dir doesn't exist create it
  dirs <- c(dir, file.path(dir, "images"))
  if (any(!purrr::map_lgl(dirs, dir.exists))) purrr::walk(dirs, dir.create)

  # TODO needs update when project evaluation freeze comes around.
  files <- paste0(c("COHHIOHMIS", "Data_Quality", "cohorts"), ".Rdata")
  .a <- utils::askYesNo(paste0("Have ", paste0(files, collapse = ", ")," been created with today's data?"))
  if (.a) {
    .d_files <- list.files("data", full.names = TRUE, pattern = "csv$|xlsx$")
    .d_copied <- RmData::copy_lgl(.d_files, dirs[1], overwrite)
    .rd_files <- grep(paste0(paste0("(?:",files,"$)"), collapse = "|"), list.files("images", full.names = TRUE), value = TRUE, ignore.case = TRUE, perl = TRUE)
    .rd_copied <- RmData::copy_lgl(.rd_files, dirs[2], overwrite)
    out <- list(data = file.path(dirs[1], basename(.d_files[.d_copied])),
                rdata = file.path(dirs[2], basename(.rd_files[.rd_copied])))
  } else {
    out <- "No files copied. Ensure Rdata files have been created with today's data."
  }
  return(out)
}



# Deprecated, use HMIS::hud_translations$`3.12.1 Living Situation Option List` instead
# living_situation <- function(ReferenceNo) {
#   dplyr::case_when(
#     ReferenceNo == 1 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
#     ReferenceNo == 2 ~ "Transitional housing",
#     ReferenceNo == 3 ~ "Permanent housing (other than RRH) for formerly homeless persons",
#     ReferenceNo == 4 ~ "Psychiatric hospital/ other psychiatric facility",
#     ReferenceNo == 5 ~ "Substance abuse treatment facility or detox center",
#     ReferenceNo == 6 ~ "Hospital or other residential non-psychiatric medical facility",
#     ReferenceNo == 7 ~ "Jail/prison/juvenile detention",
#     ReferenceNo == 8 ~ "Client doesn't know",
#     ReferenceNo == 9 ~ "Client refused",
#     ReferenceNo == 32 ~ "Host Home (non-crisis)",
#     ReferenceNo == 13 ~ "Staying or living with friends, temporary tenure",
#     ReferenceNo == 36 ~ "Staying or living in a friend's room, apartment or house",
#     ReferenceNo == 18 ~ "Safe Haven",
#     ReferenceNo == 15 ~ "Foster care home of foster care group home",
#     ReferenceNo == 12 ~ "Staying or living with family, temporary tenure",
#     ReferenceNo == 25 ~ "Long-term care facility or nursing home",
#     ReferenceNo == 22 ~ "Staying or living with family, permanent tenure",
#     ReferenceNo == 35 ~ "Staying or living in a family member's room, apartment, or house",
#     ReferenceNo == 16 ~ "Place not meant for habitation",
#     ReferenceNo == 23 ~ "Staying or living with friends, permanent tenure",
#     ReferenceNo == 29 ~ "Residential project or halfway house with no homeless criteria",
#     ReferenceNo == 14 ~ "H/Motel paid for by household",
#     ReferenceNo == 26 ~ "Moved from one HOPWA funded project to HOPWA PH",
#     ReferenceNo == 27 ~ "Moved from HOPWA funded project to HOPWA TH",
#     ReferenceNo == 28 ~ "Rental by client, with GPD TIP housing subsidy",
#     ReferenceNo == 19 ~ "Rental by client, with VASH housing subsidy",
#     ReferenceNo == 31 ~ "Rental by client, with RRH or equivalent subsidy",
#     ReferenceNo == 33 ~ "Rental by client, with HCV voucher",
#     ReferenceNo == 34 ~ "Rental by client in a public housing unit",
#     ReferenceNo == 10 ~ "Rental by client, no ongoing housing subsidy",
#     ReferenceNo == 20 ~ "Rental by client, with other ongoing housing subsidy",
#     ReferenceNo == 21 ~ "Owned by client, with ongoing housing subsidy",
#     ReferenceNo == 11 ~ "Owned by client, no ongoing housing subsidy",
#     ReferenceNo == 30 ~ "No exit interview completed",
#     ReferenceNo == 17 ~ "Other",
#     ReferenceNo == 24 ~ "Deceased",
#     ReferenceNo == 37 ~ "Worker unable to determine",
#     ReferenceNo == 99 ~ "Data not collected"
#   )
# }
# Deprecated, use HMIS::hud_translations$`2.02.6 ProjectType` instead
# project_type <- function(ReferenceNo){
#   dplyr::case_when(
#     ReferenceNo == 1 ~ "Emergency Shelter",
#     ReferenceNo == 2 ~ "Transitional Housing",
#     ReferenceNo == 3 ~ "Permanent Supportive Housing",
#     ReferenceNo == 4 ~ "Street Outreach",
#     ReferenceNo == 6 ~ "Services Only",
#     ReferenceNo == 8 ~ "Safe Haven",
#     ReferenceNo == 12 ~ "Prevention",
#     ReferenceNo == 13 ~ "Rapid Rehousing",
#     ReferenceNo == 14 ~ "Coordinated Entry"
#   )
# }


#' @title replace "yes"/"no" character vector
#'
#' @param column_name \code{(character)}
#' @param numeric \code{(logical)} if `numeric = TRUE` numeric, otherwise logical. **Default** `TRUE`
#'
#' @return if `numeric = TRUE` numeric, otherwise logical.
#' @export

replace_yes_no <- function(column_name, numeric = TRUE) {
  if (!all(stringr::str_detect(names(table(column_name)), stringr::regex("Yes|No", ignore_case = TRUE))))
    stop("The vector has more options than yes/no.")
  if (numeric)
    out <- dplyr::case_when(column_name == "No" ~ 0,
                          column_name == "Yes" ~ 1,
                          is.na(column_name) ~ NA_real_)
  else
    out <- dplyr::case_when(column_name == "No" ~ FALSE,
                          column_name == "Yes" ~ TRUE,
                          is.na(column_name) ~ NA)
  out
}

sp2cl_tables <- list(
  tibble::tribble(
    ~Value,                 ~Text,
    1, "Healthcare Provider",
    2,         "Self-Report",
    3,        "Vaccine Card"
  ),
  tibble::tribble(
    ~Value,                                                                   ~Text,
    1,                                                     "Johnson & Johnson",
    2,                                                               "Moderna",
    3,                                                                "Pfizer",
    4,  "Client doesn't know and data could not be obtained from other source"
  ),
  tibble::tribble(
    ~Value,      ~Text,
    0, "Negative",
    1, "Positive",
    2,  "Unknown"
  ),
  tibble::tribble(
    ~Value,       ~Text,
    1,  "HUD VASH",
    2,  "Other PH",
    3, "Other RRH",
    4,  "SSVF RRH"
  ),
  tibble::tribble(
    ~Value,             ~Text,
    1,   "VI-SPDAT [V2]",
    2, "VI-F-SPDAT [V2]",
    3, "VI-Y-SPDAT [V1]"
  )
) |>
  rlang::set_names(c("c_covid19_vaccine_documentation",
                     "c_covid19_vaccine_manufacturer",
                     "c_covid19_test_results",
                     "c_offer_type",
                     "c_vispdat_type"))

tables_from_compare_list <- function(columns_to_parse = c("c_covid19_vaccine_documentation",
                                                          "c_covid19_vaccine_manufacturer",
                                                          "c_covid19_test_results",
                                                          "c_offer_type",
                                                          "c_vispdat_type"), data_match) {
  sp2cl_tables <-  columns_to_parse |>
    rlang::set_names() |>
    purrr::map(~{
      nm <- .x
      .matches <- purrr::map(purrr::keep(data_match, ~nm %in% names(.x)), nm) |> purrr::map(~unique(na.omit(.x)))
      unique(unlist(.matches)) |>
        {\(x) {tibble::tibble(Value = as.numeric(x[1:length(x) %% 2 != 0]), Text = x[1:length(x) %% 2 == 0])}}()
    })

}

case_when_text <- function(.x, .y, out = c("Text", "Value")[1]) {
  nms <- c("Text", "Value")
  rlang::parse_expr(paste0("dplyr::case_when(\n\t", paste0(append(
    purrr::pmap(x, ~{
      .x <- list(...)
      rlang::expr(x == !!.x[[setdiff(nms, out)]] ~ !!.x[[out]])
    }),
    rlang::expr(is.na(x) ~ !!ifelse(out == "Text", NA_character_, NA_real_))
  ), collapse = ",\n\t"), "\n)"))
}

#' @title Service Point / Clarity Field representation translations (numeric/character)
#' @inherit HMIS::hud_translate params return
#' @export
sp2cl_translations <-
  purrr::imap(sp2cl_tables, ~{
    rlang::new_function(args = rlang::pairlist2(.x = , table = FALSE), body = rlang::expr({
      hash <- sp2cl_tables[[!!.y]]
      if (table) {
        out <- hash
      } else {
        out <- HMIS::hud_translate(.x, hash)
      }
      out
    })
    )
  })|>
  {\(x) {rlang::list2(
    !!!x
  )}}()





enhanced_yes_no_translator <- function(ReferenceNo) {
  dplyr::case_when(
    ReferenceNo == 0 ~ "No",
    ReferenceNo == 1 ~ "Yes",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 99 ~ "Data not collected"
  )
}


#' @title Unzip the HUD Export zip file from the download folder
#'
#' @param download_folder \code{(character)} path to folder where HUD Exports are downloaded
#' @param dir \code{(character)} path of destination directory
#' @export

unzip_export <- function(download_folder = "~/../Downloads/", dir = "data") {
  .files <- list.files(download_folder, pattern = "^hudx", full.names = TRUE)
  .file_times <- do.call(c, purrr::map(.files, ~file.info(.x)$mtime))

  archive::archive_extract(.files[which.max(.file_times)], dir = dir)
  purrr::walk(.files, file.remove)
}
# this function translates the HUD .csv 1.7 and 1.8 lists
# and returns yes, no, or unknown as appropriate
translate_HUD_yes_no <- function(column_name){
  dplyr::case_when(
    column_name == 0 ~ "No",
    column_name == 1 ~ "Yes",
    column_name %in% c(8, 9, 99) ~ "Unknown",
    is.na(column_name) ~ NA_character_
  )
}

chronic_determination <- function(.data, aged_in = FALSE) {

  needed_cols <- c("PersonalID", "EntryDate",
                   "AgeAtEntry", "DisablingCondition",
                   "DateToStreetESSH", "TimesHomelessPastThreeYears",
                   "MonthsHomelessPastThreeYears", "ExitAdjust", "ProjectType")

  chronicity_levels <- if(aged_in) {
    c("Chronic", "Aged In", "Nearly Chronic", "Not Chronic")}
  else {c("Chronic", "Nearly Chronic", "Not Chronic")}

  if (all((needed_cols) %in% colnames(.data))) {
    return(
      .data %>%
        dplyr::mutate(DaysHomelessInProject = difftime(lubridate::ymd(ExitAdjust),
                                                       lubridate::ymd(EntryDate),
                                                       units = "days"),
                      DaysHomelessBeforeEntry = difftime(lubridate::ymd(EntryDate),
                                                         dplyr::if_else(
                                                           is.na(lubridate::ymd(DateToStreetESSH)),
                                                           lubridate::ymd(EntryDate),
                                                           lubridate::ymd(DateToStreetESSH)
                                                         ),
                                                         units = "days"),
                      ChronicStatus =
                        dplyr::case_when(
                          ((lubridate::ymd(DateToStreetESSH) + lubridate::days(365) <= lubridate::ymd(EntryDate) &
                              !is.na(DateToStreetESSH)) |
                             (
                               MonthsHomelessPastThreeYears %in% c(112, 113) &
                                 TimesHomelessPastThreeYears == 4 &
                                 !is.na(MonthsHomelessPastThreeYears) &
                                 !is.na(TimesHomelessPastThreeYears)
                             )
                          ) &
                            DisablingCondition == 1 &
                            !is.na(DisablingCondition) ~ "Chronic",
                          ProjectType %in% c(1, 8) &
                            lubridate::ymd(DateToStreetESSH) + lubridate::days(365) > lubridate::ymd(EntryDate) &
                            !is.na(DateToStreetESSH) &
                            DaysHomelessBeforeEntry + DaysHomelessInProject >= 365 ~ "Aged In",
                          ((
                            lubridate::ymd(DateToStreetESSH) + lubridate::days(365) <= lubridate::ymd(EntryDate) &
                              !is.na(DateToStreetESSH)
                          ) |
                            (
                              MonthsHomelessPastThreeYears %in% c(110:113) &
                                TimesHomelessPastThreeYears%in% c(3, 4) &
                                !is.na(MonthsHomelessPastThreeYears) &
                                !is.na(TimesHomelessPastThreeYears)
                            )
                          ) &
                            DisablingCondition == 1 &
                            !is.na(DisablingCondition) ~ "Nearly Chronic",
                          TRUE ~ "Not Chronic"),
                      ChronicStatus = dplyr::case_when(aged_in ~ ChronicStatus,
                                                       TRUE ~ dplyr::if_else(ChronicStatus == "Aged In",
                                                                             "Chronic",
                                                                             ChronicStatus)),
                      ChronicStatus = factor(
                        ChronicStatus,
                        ordered = TRUE,
                        levels = chronicity_levels)))
  }

  else {
    stop(paste0(
      "\nYou need to include the column \"",
      needed_cols[needed_cols %in% colnames(.data) == FALSE],
      "\" to use the chronic_determination() function"
    ))
  }
}


long_term_homeless_determination <- function(.data) {

  needed_cols <- c("PersonalID", "EntryDate",
                   "AgeAtEntry", "DateToStreetESSH", "TimesHomelessPastThreeYears",
                   "MonthsHomelessPastThreeYears", "ExitAdjust", "ProjectType")

  if (all((needed_cols) %in% colnames(.data))) {
    return(
      .data %>%
        dplyr::mutate(DaysHomelessInProject = difftime(lubridate::ymd(ExitAdjust),
                                                       lubridate::ymd(EntryDate),
                                                       units = "days"),
                      DaysHomelessBeforeEntry = difftime(lubridate::ymd(EntryDate),
                                                         dplyr::if_else(
                                                           is.na(lubridate::ymd(DateToStreetESSH)),
                                                           lubridate::ymd(EntryDate),
                                                           lubridate::ymd(DateToStreetESSH)
                                                         ),
                                                         units = "days"),
                      LongTermStatus =
                        dplyr::case_when(
                          ((lubridate::ymd(DateToStreetESSH) + lubridate::days(365) <= lubridate::ymd(EntryDate) &
                              !is.na(DateToStreetESSH)) |
                             (
                               MonthsHomelessPastThreeYears %in% c(112, 113) &
                                 TimesHomelessPastThreeYears == 4 &
                                 !is.na(MonthsHomelessPastThreeYears) &
                                 !is.na(TimesHomelessPastThreeYears)
                             )
                          ) |
                            ProjectType %in% c(1, 8) &
                            lubridate::ymd(DateToStreetESSH) + lubridate::days(365) > lubridate::ymd(EntryDate) &
                            !is.na(DateToStreetESSH) &
                            DaysHomelessBeforeEntry + DaysHomelessInProject >= 365 ~ "Long Term",
                          TRUE ~ "Not Long Term"),
                      LongTermStatus = factor(
                        LongTermStatus,
                        ordered = TRUE,
                        levels = c("Long Term", "Not Long Term"))))
  }

  else {
    stop(paste0(
      "\nYou need to include the column \"",
      needed_cols[needed_cols %in% colnames(.data) == FALSE],
      "\" to use the long_term_homeless_determination() function"
    ))
  }
}

# Experimental ------------------------------------------------------------

# HUD_value_to_description <-
#   function(table, element_name, element_column) {
#     element_name <- sym(element_name)
#     element_column <- enquo(element_column)
#
#     a <- HUD_specs %>%
#       filter(DataElement == element_name) %>%
#       select("ReferenceNo", "Description")
#
#     table$element_column <- with(a,
#                                  Description[match(table$element_column,
#                                                    HUD_specs$ReferenceNo)])
#   }
#
# a <- subset(HUD_specs,
#             DataElement == "HouseholdType",
#             select = c("ReferenceNo", "Description"))
# Inventory$HouseholdType <- with(a,
#                                 Description[match(Inventory$HouseholdType,
#                                                   ReferenceNo)])



# HMIS_participating_between <- function(table, start, end) {
#   HMISParticipating <-  if_else(
#     (table$HMISParticipatingBeds == 0 | is.na(table$HMISParticipatingBeds)) |
#     (is.na(table$InventoryStartDate) |
#       ymd(table$InventoryStartDate) > mdy(end)) |
#       (!is.na(table$InventoryEndDate) &
#          ymd(table$InventoryEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   HMISParticipating
# }
# not sure what the heck to do about this. :( will have to pull based
# on UsesSP which is super clunky and will leave out providers
