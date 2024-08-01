#' @title Reimports from other packages
#' @name Reimports
#' @description Functions imported from other packages
#' @importFrom rlang "%||%" "%|%"
#' @importFrom dplyr "%>%"
#' @importFrom UU "%|0|%"
#' @importFrom clarity.looker dirs
NULL

#' @title Roxygen Template for `app_env` & `clarity_api`
#' @name R6Classes
#' @param app_env \code{(app_env)} R6 Object containing dependencies. If all arguments to this function are saved in the `app_env`, then they will be called from there and arguments do not need to be specified.
#' @param clarity_api \code{(clarity_api)} R6 Object containing the connection to the Clarity Looker instance.
NULL

#' @title Data Quality Tables Roxygen Template
#' @name data_quality_tables
#' @param served_in_date_range \code{(data.frame)} See `served_in_date_range`
#' @param IncomeBenefits \code{(data.frame)} From the HUD CSV Export
#' @param Inventory \code{(data.frame)} From the HUD CSV Export
#' @param HealthAndDV \code{(data.frame)} From the HUD CSV Export
#' @param Disabilities \code{(data.frame)} From the HUD CSV Export
#' @param Project \code{(data.frame)} From the HUD CSV Export
#' @param Enrollment \code{(data.frame)} From the HUD CSV Export
#' @param EnrollmentCoC \code{(data.frame)} From the HUD CSV Export
#' @param Exit \code{(data.frame)} From the HUD CSV Export
#' @param Client \code{(data.frame)} From the HUD CSV Export
#' @param Funder \code{(data.frame)} From the HUD CSV Export
#' @param Services \code{(data.frame)} From the HUD CSV Export
#' @param Services_extras \code{(data.frame)} From the Clarity Looker API Extras
#' @param Referrals \code{(data.frame)} From the Clarity Looker API Extras
#' @param mahoning_projects \code{(named numeric)} Vector of ProjectIDs associated with projects operating in Mahoning. Created in `load_export`.
#' @param guidance \code{(list)} See `guidance`
#' @param vars \code{(named list)}
#' \itemize{
#'   \item{\code{prep}}{ Column names for Prep}
#'   \item{\code{we_want}}{ Column names for output}
#' }
#' @param rm_dates \code{(named list)} with all dates specified or calculated in `dates`
#' @param app_env \code{(app_env)} R6 Object containing dependencies. If all arguments to this function are saved in the `app_env`, then they will be called from there and arguments do not need to be specified.
#' @param clarity_api \code{(clarity_api)} R6 Object containing the connection to the Clarity Looker instance.
#' @return \code{(data.frame)} `vars$we_want` and `Issue` (Issue Name), `Type` (Error or Warning), and `Guidance` (How to correct the issue)
NULL

#' @title Retrieve an HMIS option set via `setup_RmData`
#' @param opt \code{(character)} Option name to retrieve
#' @inheritParams base::getOption
#'

hmis_option <- function(x, default = FALSE) {

  .w <- glue::glue("HMIS option `{x}`")
  .msg <- " not setup, please see ?setup_RmData to fix this."

  opts <- getOption("HMIS")
  out <- opts[[x]]
  if (!UU::is_legit(out)) {
    warning(glue::glue("{.w}{.msg}"))
    out <- default
  }

  out
}

#' @title Is this instance using Clarity
#' @description Set an option in `.Rprofile` using `usethis::edit_r_profile('project')` called HMIS which is a list containing two logical values:
#' \itemize{
#'   \item{\code{Clarity}}{ A logical to indicate whether Clarity is (or has been) used by this CoC}
#'   \item{\code{ServicePoint}}{ A logical to indicate whether Servicepoint is (or has been) used by this CoC}
#' }
#' @return \code{(logical)}

is_clarity <- function() {
  hmis_option("Clarity")
}

#' @title Is this instance using ServicePoint
#' @inherit is_clarity description return

is_sp <- function() {
  hmis_option("ServicePoint")
}

#' @title Retrieve the Clarity URL from options.
#' See `?setup_RmData` for details.
#'
#' @return \code{(character)}
#' @export

clarity_url <- function() {
  hmis_option("Clarity_URL", "https://cohhio.clarityhs.com")
}

#' @title This instance must be using ServicePoint, otherwise throw an error.
#' @inherit is_clarity description return

must_sp <- function(.call = match.call()[[1]]) {
  if (!is_sp())
    rlang::abort(.call, " is a ServicePoint specific data quality check.")
  TRUE
}
