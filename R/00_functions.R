#' @title Does a vector/data.frame have no values?
#'
#' @param x \code{(data.frame/vector)}
#'
#' @return \code{(logical)}
#' @export

is_zero <- function(x) {
  UseMethod("is_zero")
}

#' Check if a value is valid for increment
#'
#' @param inc The value to check
#' @return TRUE if the value is non-null and numeric, FALSE otherwise
#' @export
is_inc <- function(inc) {
  !is.null(inc) && is.numeric(inc)
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


#' @title Stop Daily Update with an Informative Error
#'
#' @description Throws an error with additional details, or issues a warning if `error` is set to `FALSE`. The error message instructs the user to contact support for help.
#'
#' @param ... Additional messages to include in the error or warning. These messages will be concatenated and presented to the user.
#' @param error \code{(logical)} If \code{TRUE}, stops execution and throws an error. If \code{FALSE}, issues a warning instead. Default is \code{FALSE}.
#'
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
  floor(lubridate::time_length(lubridate::interval(earlier, later), "years"))
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





enhanced_yes_no_translator <- function(ReferenceNo) {
  dplyr::case_when(
    ReferenceNo == 0 ~ "No",
    ReferenceNo == 1 ~ "Yes",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client prefers not to answer",
    ReferenceNo == 99 ~ "Data not collected"
  )
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
                          ProjectType %in% c(0, 1, 8) &
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
                            ProjectType %in% c(0, 1, 8) &
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
