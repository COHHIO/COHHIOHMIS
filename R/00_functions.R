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





# stop_with_instructions ----
# Wed Mar 24 16:38:01 2021
#' @title Stop daily update with an informative error
#' @description Throws an error in 00_daily_update.R with additional details
#' @param ... Error Messages to print to the console
#' @importFrom cli cli_alert_danger cli_alert_info col_red
#' @export
stop_with_instructions <- function(...) {
  cli::cli_alert_danger(cli::col_red(paste0(..., collapse = "\n")))
  cli::cli_alert_info(
    "See instructions for details:\n
    https://docs.google.com/document/d/1iT_dgf0HtBzGOO8PqFNvyS_djA78JcYZsWaeZQYJC9E/edit#heading=h.xvdv7715aoi1"
  )
  stop("See above.", call. = FALSE)
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
    cenv$.total_time <- difftime(tail(cenv$.last_timer, 1)$ts,
                                 head(cenv$.last_timer, 1)$ts, units = "mins")
    cenv$.total_steps <- tail(cenv$.last_timer, 1)$step
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
  .elapsed <- round(difftime(tail(cenv$.timer, 1)$ts,
                             head(cenv$.timer, 1)$ts, units = "mins"),2)
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
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))

  dayOnLaterYear <- ifelse(
    format(lt[, 1], format = "%m-%d") != "02-29",
    as.Date(paste(
      format(lt[, 2], format = "%Y"), "-",
      format(lt[, 1], format = "%m-%d"),
      sep = ""
    )),
    ifelse(
      as.numeric(format(later, format = "%Y")) %%
        400 == 0 |
        as.numeric(format(later,
                          format =
                            "%Y")) %%
        100 != 0 &
        as.numeric(format(later, format = "%Y")) %%
        4 == 0,
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        format(lt[, 1], format =
                 "%m-%d"),
        sep = ""
      )),
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        "02-28",
        sep = ""
      ))
    )
  )

  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1

  age
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

  files <- paste0(c("COHHIOHMIS", "Data_Quality", "cohorts"), ".Rdata")
  .a <- utils::askYesNo(paste0("Have ", paste0(files, collapse = ", ")," been created with today's data?"))
  if (.a) {
    .d_files <- list.files("data", full.names = TRUE, pattern = "csv$|xlsx$")
    .d_copied <- copy_lgl(.d_files, dirs[1], overwrite)
    .rd_files <- grep(paste0(paste0("(?:",files,"$)"), collapse = "|"), list.files("images", full.names = TRUE), value = TRUE, ignore.case = TRUE, perl = TRUE)
    .rd_copied <- copy_lgl(.rd_files, dirs[2], overwrite)
    out <- list(data = file.path(dirs[1], basename(.d_files[.d_copied])),
                rdata = file.path(dirs[2], basename(.rd_files[.rd_copied])))
  } else {
    out <- "No files copied. Ensure Rdata files have been created with today's data."
  }
  return(out)
}
