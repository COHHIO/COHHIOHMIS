

#' @title Start a \link[cli]{cli_progress_bar} or \link[shiny]{Progress} based progress bar
#'
#' @inheritParams cli::cli_progress_bar
#' @param total \code{(numeric)} `max` for \link[shiny]{Progress} & `total` for \link[cli]{cli_progress_bar}
#' @param is_shiny \code{(logical)} Whether running in a shiny environment
#'
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

#' @title S3 character Method pb_close
#' @export
pb_close.character <- function(pbar, e) {
  cli::cli_progress_done(pbar, .envir = e)
}
#' @title S3 Progress Method pb_close
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
#' @export
pb_update.Progress <- function(pbar, message = NULL, inc = NULL, set = NULL, ..., e = rlang::caller_env()) {
  .is_set <- UU::is_legit(set)
  .is_inc <- UU::is_inc(inc)
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
  max(clarity.looker::hud_last_updated(path = dir), na.rm = TRUE) > lubridate::floor_date(Sys.time(), "day")
}


#' @title Update HUD CSV Export & Extras data
#'
#' @param export_zip \code{(logical)} Did the HUD Export CSV's come from an uploaded zip file?
#' @inheritParams data_quality_tables
#'
#' @return \code{(none)} retrieves and saves Export CSVs via `clarity.looker` if `export_zip = FALSE` (the default). See \code{\link[clarity.looker]{clarity_api}$get_export} & \code{$get_folder_looks} for details.
#' @export

update_data <- function(export_zip = FALSE, clarity_api = RmData::get_clarity_api(e = rlang::caller_env())) {


  if (!data_ready() && !export_zip) {
    cli::cli_inform(message = cli::col_grey("Updating export..."))
    clarity_api$get_export()
  }



  if (!data_ready(clarity_api$dirs$extras)) {
    cli::cli_inform(message = cli::col_grey("Updating extras..."))
    clarity_api$get_folder_looks(clarity_api$folders$`HUD Extras`,
                                 .write = TRUE,
                                 path = clarity_api$dirs$extras)
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
#'   \item{\code{ covid19 }}
#'   \item{\code{ covid19_plots }}
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
  "covid19",
  "covid19_plots",
  "vets",
  "vet_active",
  "prioritization",
  "bed_unit_utilization",
  "data_quality",
  "data_quality_summary"

)),
export_zip = TRUE,
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
    update_data(export_zip, clarity_api = clarity_api)
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
    app_env$deps_to_destination("all", dest_folder = file.path("data", "backup"))
  }

  if ("send" %in% steps) {
    cli::cli_inform(cli::col_grey("Sending Dependencies to apps..."))
    app_env$deps_to_destination(dest_folder = ifelse(interactive(), file.path("..",c("Rminor", "RminorElevated"),"data"), file.path("data", "db")), remote = remote)
  }

  cli::cli_alert_success(paste0("Total runtime: ", round(difftime(Sys.time(), now, units = "mins"), 2), "m"))
}
