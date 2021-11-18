#' @title Reimports from other packages
#' @name Reimports
#' @description Functions imported from other packages
#' @importFrom rlang "%||%" "%|%"
#' @importFrom dplyr "%>%"
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
#' @param HealthAndDV \code{(data.frame)} From the HUD CSV Export
#' @param Disabilities \code{(data.frame)} From the HUD CSV Export
#' @param Project \code{(data.frame)} From the HUD CSV Export
#' @param Enrollment \code{(data.frame)} From the HUD CSV Export
#' @param EnrollmentCoC \code{(data.frame)} From the HUD CSV Export
#' @param Exit \code{(data.frame)} From the HUD CSV Export
#' @param Client \code{(data.frame)} From the HUD CSV Export
#' @param mahoning_projects \code{(named numeric)} Vector of ProjectIDs associated with projects operating in Mahoning. Created in `load_export`.
#' @param guidance \code{(list)} See `guidance`
#' @param vars \code{(named list)}
#' \itemize{
#'   \item{\code{prep}}{ Column names for Prep}
#'   \item{\code{we_want}}{ Column names for output}
#' }
#' @param rm_dates \code{(named list)} with all dates specified or calculated in `dates`
#' @inherit R6Classes params
#' @return \code{(data.frame)} `vars$we_want` and `Issue` (Issue Name), `Type` (Error or Warning), and `Guidance` (How to correct the issue)
NULL

