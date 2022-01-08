

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





data_ready <- function(dir = clarity.looker::dirs$export) {
  max(clarity.looker::hud_last_updated(path = dir), na.rm = TRUE) > lubridate::floor_date(Sys.time(), "day")
}


update_data <- function(export_zip = FALSE, clarity_api = RmData::get_clarity_api(e = rlang::caller_env()), is_shiny = FALSE) {
  .total <- total <-  1
  pbar <- pb_start(name = "Update Data", type = "iterator", total = .total, is_shiny = is_shiny, auto_terminate = FALSE)
  if (!data_ready() && !export_zip) {
    pb_update(pbar, message = "Updating export", set = 0)
    clarity_api$get_export()
    .total  = .total / 2
    pb_update(pbar, message = "Updated export", set = .total)
  }



  if (!data_ready(clarity_api$dirs$extras)) {
    pb_update(pbar, message = "Updating extras", set = .total)
    clarity_api$get_folder_looks(clarity_api$folders$`HUD Extras`,
                                 .write = TRUE,
                                 path = clarity_api$dirs$extras)
    pb_update(pbar, message = "Updated extras", set = total)
  }
  pb_close(pbar, e = environment())
}


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
now = Sys.time(),
app_env,
clarity_api,
session,
e = rlang::caller_env()
) {
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
    pb_update(pbar, message = "Updating Data")
    update_data(export_zip, clarity_api = clarity_api)
    pb_update(pbar, message = "Updated Data", inc = 1)
  }

  if ("funs" %in% steps) {
    pb_fn <- pb_start("Functions", total = length(funs), type = "iterator", auto_terminate = FALSE)
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
      pb_update(pb_fn, message = paste0(fn, " Done"), set = which(funs %in% fn), e = environment())
    }
    pb_close(pb_fn, e = environment())
  }

  if (backup) {
    cli::cli_progress_message("Backing up dependencies")
    app_env$deps_to_destination("all", dest_folder = file.path("data", "backup"))
  }

  if ("send" %in% steps) {
    pb_update(pbar, message = "Sending Dependencies to apps")
    app_env$deps_to_destination(dest_folder = file.path("data", "db"), remote = remote)
    pb_update(pbar, inc = 1, message = "App data updated")
  }

  pb_close(pbar)

  cli::cli_inform(paste0("Total runtime: ", round(difftime(Sys.time(), now, units = "mins"), 2), "m"))
}
