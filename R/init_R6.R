#' @include app_dependencies.R
if (file.exists("RmData.Rproj") && interactive()) {
  if (!exists("cl_api")) {
    pkgload::load_all("../clarity.looker")
    .GlobalEnv$cl_api <- clarity.looker::clarity_api$new(file.path("inst","vault","Looker.ini"))
  }

  if (!exists("rm_env"))
    .GlobalEnv$rm_env <- app_env$new()
}


#' @title Setup RmData Options
#'
#' @description Configure and/or create the `.Rprofile` file to set options for the RmData package.
#'
#' @param Clarity \code{(logical)} Indicates whether the CoC uses Clarity HMIS. **Default** is \code{TRUE}.
#' @param ServicePoint \code{(logical)} Indicates whether the CoC uses ServicePoint HMIS. **Default** is \code{FALSE}.
#' @param Clarity_URL \code{(character)} The URL for the Clarity HMIS. **Default** is \code{"https://cohhio.clarityhs.com"}.
#' @param clients_to_filter \code{(list or NULL)} A list of clients to filter. If \code{NULL}, no filtering is applied.
#'
#' @return This function creates or modifies the `.Rprofile` file in the project directory to include HMIS options.
#' @export
#'
#' @examples
#' \dontrun{
#' setup_RmData(Clarity = TRUE, ServicePoint = FALSE)
#' }
setup_RmData <- function(Clarity = TRUE, ServicePoint = FALSE, Clarity_URL = "https://cohhio.clarityhs.com", clients_to_filter = NULL) {
  if (!UU::is_legit(list.files(pattern = "^.Rprofile$")))
    file.create(".Rprofile")
  .msg <- glue::glue("options(HMIS = list(Clarity = {Clarity},
              ServicePoint = {ServicePoint},
             Clarity_URL = {Clarity_URL},
             clients_to_filter = {clients_to_filter}))")
  write(.msg, ".Rprofile", append = TRUE)
  cli::cli_alert_success("HMIS options written to .Rprofile in project directory.")
}

from_ns <- function(nm, ns) {
  purrr::possibly(~getFromNamespace(nm, ns), otherwise = NULL)(nm, ns)
}

by_class <- function(class, e) {
  purrr::possibly(~UU::find_by_class(class, e), otherwise = NULL)(class, e)
}


#' Find the `clarity_api` R6 object
#' @family Get R6 Classes
#' @param nm The name of the instantiated object
#' @param e The environment in which to search
#' @param ifnotfound Warning message if env not found
#' @details Searches the `RmData` package environment and then the environment specified
#' @return `clarity_api` R6 object
#' @export
get_clarity_api <- function(nm = "cl_api", e = rlang::caller_env(), ifnotfound = stop("Clarity API object not found")) {
  from_ns(nm, "RmData") %||% by_class("clarity_api", e) %||% by_class("clarity_api", .GlobalEnv) %||% ifnotfound
}

#' Find the `app_env` R6 object
#' @family Get R6 Classes
#' @param nm The name of the instantiated object
#' @param e The environment in which to search
#' @param ifnotfound Warning message if env not found
#' @details Searches the `RmData` package environment and then the environment specified
#' @return `app_env` R6 object
#' @export

get_app_env <- function(nm = "rm_env", e = rlang::caller_env(), ifnotfound = stop("app_env object not found")) {
  from_ns(nm, "RmData") %||% by_class("app_env", e) %||% by_class("app_env", .GlobalEnv) %||% ifnotfound
}


#' @title Is object of class `app_env`
#'
#' @description Check if the provided object is of class `app_env`.
#'
#' @param x \code{(any)} The object to check.
#'
#' @return \code{(logical)} Returns \code{TRUE} if \code{x} is of class \code{"app_env"}, otherwise \code{FALSE}.
#' @export

is_app_env <- function(x) {
  inherits(x, "app_env")
}

#' @title Is object of class `clarity_api`
#'
#' @description Check if the provided object is of class `clarity_api`.
#'
#' @param x \code{(any)} The object to check.
#'
#' @return \code{(logical)} Returns \code{TRUE} if \code{x} is of class \code{"clarity_api"}, otherwise \code{FALSE}.
#' @export

is_clarity_api <- function(x) {
  inherits(x, "clarity_api")
}

