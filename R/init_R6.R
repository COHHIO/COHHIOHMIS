#' @include app_dependencies.R
if (file.exists("RmData.Rproj") && interactive()) {
  if (!exists("cl_api")) {
    devtools::load_all("../clarity.looker")
    .GlobalEnv$cl_api <- clarity.looker::clarity_api$new(file.path("inst","auth","Looker.ini"))
  }

  if (!exists("Rm_env"))
    .GlobalEnv$Rm_env <- app_env$new()
}


#' @title Setup RmData options
#'
#' @param Clarity \code{(logical)} Does the CoC use Clarity HMIS? **Default** \code{TRUE}
#' @param ServicePoint \code{(logical)} Does the CoC use ServicePoint HMIS? **Default** \code{FALSE}
#'
#' @return
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
#' @details Searches the `RmData` package environment and then the environment specified
#' @return
#' @export
get_clarity_api <- function(nm = "cl_api", e = rlang::caller_env(), ifnotfound = stop("Clarity API object not found")) {
  from_ns(nm, "RmData") %||% by_class("clarity_api", e) %||% by_class("clarity_api", .GlobalEnv) %||% ifnotfound
}

#' Find the `app_env` R6 object
#' @family Get R6 Classes
#' @param nm The name of the instantiated object
#' @param e The environment in which to search
#' @details Searches the `RmData` package environment and then the environment specified
#' @return
#' @export

get_app_env <- function(nm = "Rm_env", e = rlang::caller_env(), ifnotfound = stop("app_env object not found")) {
  from_ns(nm, "RmData") %||% by_class("app_env", e) %||% by_class("app_env", .GlobalEnv) %||% ifnotfound
}


#' @title Is object of class `app_env`
#'
#' @param x
#'
#' @return \code{(logical)}
#' @export

is_app_env <- function(x) {
  inherits(x, "app_env")
}

#' @title Is object of class `clarity_api`
#'
#' @param x
#'
#' @return \code{(logical)}
#' @export

is_clarity_api <- function(x) {
  inherits(x, "clarity_api")
}

