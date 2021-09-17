#' @include app_dependencies.R
if (Sys.info()["nodename"] == "DESKTOP-2SK9RKR" && file.exists("Rm_data.Rproj")) {
  if (!exists("cl_api")) {

    devtools::load_all("../clarity.looker")
    .GlobalEnv$cl_api <- clarity.looker::clarity_api$new(file.path("inst","auth","Looker.ini"))
  }

  if (!exists("Rm_env"))
    .GlobalEnv$Rm_env <- app_env$new()
}


#' Find the `clarity_api` R6 object
#' @family Get R6 Classes
#' @param nm The name of the instantiated object
#' @param e The environment in which to search
#' @details Searches the `Rm_data` package environment and then the environment specified
#' @return
#' @export
#'
#' @examples
#' get_clarity_api()
get_clarity_api <- function(nm = "cl_api", e = rlang::caller_env()) {
  tryCatch(getFromNamespace(nm, "Rm_data"), error = rlang::as_function(~{NULL})) %||% UU::find_by_class("clarity_api", e)
}

#' Find the `app_env` R6 object
#' @family Get R6 Classes
#' @param nm The name of the instantiated object
#' @param e The environment in which to search
#' @details Searches the `Rm_data` package environment and then the environment specified
#' @return
#' @export
#'
#' @examples
#' get_clarity_api()
get_app_env <- function(nm = "Rm_env", e = rlang::caller_env()) {
  tryCatch(getFromNamespace(nm, "Rm_data"), error = rlang::as_function(~{NULL})) %||% UU::find_by_class("app_env", e)
}


