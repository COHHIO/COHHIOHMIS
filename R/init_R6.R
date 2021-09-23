#' @include app_dependencies.R
if (Sys.info()["nodename"] %in% c("LAPTOP-O2IG5O1H", "DESKTOP-2SK9RKR") && file.exists("Rm_data.Rproj")) {
  if (!exists("cl_api")) {

    devtools::load_all("../clarity.looker")
    .GlobalEnv$cl_api <- clarity.looker::clarity_api$new(file.path("inst","auth","Looker.ini"))
  }

  if (!exists("Rm_env"))
    .GlobalEnv$Rm_env <- app_env$new()
}


#' @title Setup Rm_data options
#'
#' @param Clarity \code{(logical)} Does the CoC use Clarity HMIS? **Default** \code{TRUE}
#' @param ServicePoint \code{(logical)} Does the CoC use ServicePoint HMIS? **Default** \code{FALSE}
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' setup_Rm_data(Clarity = TRUE, ServicePoint = FALSE)
#' }
setup_Rm_data <- function(Clarity = TRUE, ServicePoint = FALSE) {
  if (!UU::is_legit(list.files(pattern = "^.Rprofile$")))
    file.create(".Rprofile")
  write(paste0("options(HMIS = list(Clarity = ",Clarity,",
              ServicePoint = ",ServicePoint,"))", ".Rprofile"), append = TRUE)
  cli::cli_alert_success("HMIS options written to .Rprofile in project directory.")
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


