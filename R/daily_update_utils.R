

#' @title Start a Progress Bar
#' @description Start a \link[cli]{cli_progress_bar} or \link[shiny]{Progress}-based progress bar.
#' @param total \code{(numeric)} `max` for \link[shiny]{Progress} & `total` for \link[cli]{cli_progress_bar}.
#' @param is_shiny \code{(logical)} Whether running in a shiny environment.
#' @param name \code{(character)} Name of the progress bar.
#' @param status \code{(character)} Status message for the progress bar.
#' @param type \code{(character)} Type of the progress bar. One of "iterator", "tasks", "download", or "custom".
#' @param e The environment in which to start the progress bar.
#' @param ... Additional arguments passed to either \link[shiny]{Progress$new} or \link[cli]{cli_progress_bar}.
#' @return An object of class `Progress` if `is_shiny` is `TRUE`, otherwise a `cli_progress_bar` object.
#' @export

pb_start <-
  function(name = "Daily Update",
           status = NULL,
           type = c("iterator", "tasks", "download", "custom")[1],
           total = NA,
           is_shiny = FALSE,
           e = rlang::caller_env(),
           ...) {
    .dots <- rlang::dots_list(..., .named = TRUE)

    if (is_shiny) {
      .dots <- .dots[names(.dots) %in% c("min", "session", "style")]
      out <- rlang::exec(shiny::Progress$new, max = total, !!!.dots)
    } else {
      .dots <- .dots[names(.dots) %in% rlang::fn_fmls_names(cli::cli_progress_bar)]
      out <- rlang::exec(
        cli::cli_progress_bar,
        name = name,
        status = status,
        type = type,
        total = total,
        .envir = e,
        !!!.dots
      )
    }
    return(out)
  }


#' @title Close a \link[cli]{cli_progress_bar} or \link[shiny]{Progress} based progress bar
#'
#' @inheritParams pb_update
#' @export

pb_close <- function(pbar, e = rlang::caller_env()) {
  force(e)
  UseMethod("pb_close")
}

#' @title Close a Character Progress Bar
#' @description Closes a progress bar for character input.
#' @param pbar The progress bar object to close.
#' @param e The environment in which to close the progress bar.
#' @export
pb_close.character <- function(pbar, e) {
  cli::cli_progress_done(pbar, .envir = e)
}

#' @title Close a Progress Object
#' @description Closes a progress bar for a `Progress` object.
#' @param pbar The progress bar object to close.
#' @param e The environment in which to close the progress bar.
#' @export
pb_close.Progress <- function(pbar, e) {
  pbar$close()
}

#' @title Update a \link[cli]{cli_progress_bar} or \link[shiny]{Progress} based progress bar
#'
#' @param pbar \code{(progress bar id (character) or R6 object)}
#' @param message \code{(character)} Status message
#' @param inc \code{(numeric)} amount to increment. **Must supply either `inc` or `set` and not both.**
#' @param set \code{(numeric)} amount to set at
#' @inheritDotParams cli::cli_progress_bar
#' @param e \code{(environment)} calling environment

#' @export
pb_update <- function(pbar, message = NULL, inc = NULL, set = NULL, ..., e = rlang::caller_env()) {
  UseMethod("pb_update")
}

#' @title S3 character Method pb_update
#' @description Update a \link[cli]{cli_progress_bar} object with a new message, increment, or set value when the progress bar is referenced by its ID.
#' @param pbar \code{(character)} ID of the progress bar to be updated.
#' @param message \code{(character)} New message to display with the progress bar.
#' @param inc \code{(numeric)} Amount to increment the progress bar by.
#' @param set \code{(numeric)} Value to set the progress bar to.
#' @param ... Additional arguments passed to the progress bar's update method.
#' @param e The environment from which to evaluate the function.
#' @return None. This function is called for its side effects.
#' @export
pb_update.character <- function(pbar, message = NULL, inc = NULL, set = NULL, ..., e = rlang::caller_env()) {
  inc_set <- purrr::map_lgl(list(inc = inc, set = set), ~!is.null(.x))

  if (any(inc_set)) {
    if (all(inc_set))
      stop("Either `set` or `inc` must be supplied, not both.")
    .dots <- rlang::dots_list(..., .named = TRUE)
    .dots <- .dots[names(.dots) %in% rlang::fn_fmls_names(cli::cli_progress_update)]
    .args <- rlang::list2(
      id = pbar,
      status = message,
      set = set,
      inc = inc,
      .envir = e,
      !!!.dots
    )
  } else if (all(!inc_set)) {
    .args <- list(
      msg = message,
      .envir = e,
      .auto_close = FALSE
    )
  }

  fn <- purrr::when(all(!inc_set),
                    . ~ cli::cli_progress_message,
                    ~ cli::cli_progress_update)
  do.call(fn, .args)
}

#' @title S3 Progress Method pb_update
#' @description Update a \link[shiny]{Progress} object with a new message, increment, or set value.
#' @param pbar \code{Progress} object to be updated.
#' @param message \code{(character)} New message to display with the progress bar.
#' @param inc \code{(numeric)} Amount to increment the progress bar by.
#' @param set \code{(numeric)} Value to set the progress bar to.
#' @param ... Additional arguments passed to the progress bar's update method. Typically used for passing "detail".
#' @param e The environment from which to evaluate the function.
#' @return None. This function is called for its side effects.
#' @export
pb_update.Progress <- function(pbar, message = NULL, inc = NULL, set = NULL, ..., e = rlang::caller_env()) {
  .is_set <- UU::is_legit(set)
  .is_inc <- is_inc(inc)
  .dots <- rlang::dots_list(..., .named = TRUE)
  .dots <- .dots[names(.dots) %in% "detail"]
  if (.is_set && .is_inc)
    stop("Either `set` or `inc` must be supplied, not both.")
  .action <- purrr::when(.is_set,
              isTRUE(.) ~ "set",
              ~ "inc")
  .args <- rlang::list2(message = message,
                !!!.dots)
  if (.is_set)
    .args$value = set
  if (.is_inc)
    .args$amount = inc
  do.call(pbar[[.action]], .args)
}





#' @title Is the data in `dir` updated?
#'
#' @param dir \code{(character)} path to directory
#'
#' @return \code{(logical)}
#' @export

data_ready <- function(dir = clarity.looker::dirs$export) {
  if (!dir.exists(dir))
    UU::mkpath(dir)
  if (basename(dir) == "export")
    .files <- UU::list.files2(dir, pattern = "csv$")
  else
    .files <- UU::list.files2(dir)
  UU::needs_update(.files)

}


#' @title Update HUD CSV Export & Extras data
#'
#' @inheritParams data_quality_tables
#'
#' @return \code{(none)} retrieves and saves Export CSVs via `clarity.looker` if not up to date. See \code{\link[clarity.looker]{clarity_api}$get_export} & \code{$get_folder_looks} for details.
#' @export

update_data <- function(clarity_api = RmData::get_clarity_api(e = rlang::caller_env())) {
  .export_ready <- data_ready(clarity_api$dirs$export)


  if (any(UU::`%|0|%`(.export_ready$needs_update, TRUE))) {
    cli::cli_inform(message = cli::col_grey("Updating export..."))
    clarity_api$get_export()
  }

  .extras_ready <- data_ready(clarity_api$dirs$extras)

  if (any(UU::`%|0|%`(.extras_ready$needs_update, TRUE))) {
    start_time <- Sys.time()
    cli::cli_inform(message = cli::col_grey("Updating extras..."))
    clarity_api$get_folder_looks(clarity_api$folders,
                                 .write = TRUE,
                                 path = clarity_api$dirs$extras)
    end_time <- Sys.time()
    logger::log_info("Load extras elapsed time:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds")
  }
}


#' @title Update the Rminor apps
#'
#' @param steps \code{(character)} vector of the steps to run. Any of:
#' \itemize{
#'   \item{\code{ update }}{  Update Data }
#'   \item{\code{ funs }}{  Run RmData functions }
#'   \item{\code{ send }}{  Send Dependencies to Dropbox }
#' }
#' @param funs \code{(character)} vector of functions to run. Any of:
#' \itemize{
#'   \item{\code{ dates }}
#'   \item{\code{ load_export }}
#'   \item{\code{ cohorts }}
#'   \item{\code{ client_counts }}
#'   \item{\code{ qpr_ees }}
#'   \item{\code{ qpr_spdats }}
#'   \item{\code{ vets }}
#'   \item{\code{ vet_active }}
#'   \item{\code{ prioritization }}
#'   \item{\code{ bed_unit_utilization }}
#'   \item{\code{ data_quality }}
#'   \item{\code{ data_quality_summary }}
#' }
#' @inheritParams update_data
#' @param remote \code{(logical)} Whether to send dependencies to the remote storage.
#' @param backup \code{(logical)} Whether to back up all raw dependencies in the dependencies environment.
#' @inheritParams data_quality_tables
#' @param session \code{(shiny session object)}
#' @param e \code{(calling environment)}
#'
#' @return Updates the Rminor apps and provides status messages along the way
#' @export

daily_update <- function(steps = c(
  "Update Data" = "update",
  "Run RmData functions" = "funs",
  "Send Dependencies to Dropbox" = "send"
),
funs = rlang::set_names(c(
  "dates",
  "load_export",
  "cohorts",
  "client_counts",
  "qpr_ees",
  "qpr_spdats",
  "vets",
  "vet_active",
  "bed_unit_utilization",
  "data_quality",
  "data_quality_summary",
  # "project_evaluation",
  "project_evaluation_mahoning",
  "prioritization",
  "spms"

)),
remote = FALSE,
backup = FALSE,
app_env,
clarity_api,
session,
e = rlang::caller_env()
) {
  now = Sys.time()
  if (missing(app_env))
    app_env <- get_app_env(e = e)
  if (missing(clarity_api))
    clarity_api <- get_clarity_api(e = e)
  if (missing(session)) {
    session <- e$session
    .shiny = UU::is_legit(session)
  } else
    .shiny = FALSE

  # Prepare Progress bar
  pbar <- pb_start(total = length(steps), type = "tasks", is_shiny = .shiny, auto_terminate = FALSE)
  if ("update" %in% steps) {
    update_data(clarity_api = clarity_api)
  }

  if ("funs" %in% steps) {
    cli::cli_inform(cli::col_grey("Running functions..."))
    pb_fn <- pb_start("Functions", total = length(funs), type = "tasks", auto_terminate = FALSE, format = "{cli::pb_name}: {.path {cli::pb_status}} {cli::pb_current}/{cli::pb_total} [{cli::col_br_blue(cli::pb_elapsed)}]")
    funs <- funs[order(match(funs, eval(rlang::fn_fmls()$funs)))]
    for (fn in funs) {
      pb_update(pb_fn, message = fn, set = which(funs %in% fn) - 1, e = environment())
      .fn <- getFromNamespace(fn, "RmData")
      .args <- list()
      if ("app_env" %in% rlang::fn_fmls_names(.fn))
        .args$app_env = app_env
      if ("clarity_api" %in% rlang::fn_fmls_names(.fn))
        .args$clarity_api = clarity_api

      app_env <- do.call(.fn, .args)
      pb_update(pb_fn, message = paste0(fn, " done"), set = which(funs %in% fn), e = environment())
    }
    pb_close(pb_fn, e = environment())
  }

  if (backup) {
    cli::cli_inform(cli::col_grey("Backing up dependencies..."))
    app_env$deps_to_destination(clean = FALSE, deps =  "all", dest_folder = file.path("data", "backup"))
  }
  if ("send" %in% steps) {
    cli::cli_inform(cli::col_grey("Sending Dependencies to apps..."))
    app_env$deps_to_destination(clean = FALSE, dest_folder = if (clarity.looker::is_dev()) file.path("..",c("Rminor", "RminorElevated"),"data") else file.path("data", "db"), remote = remote)
  }

  cli::cli_alert_success(paste0("Total runtime: ", round(difftime(Sys.time(), now, units = "mins"), 2), "m"))
}
