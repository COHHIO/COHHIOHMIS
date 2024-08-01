
# Map data dependencies to functions in `DataQuality_utils`
# Ensure dependencies are created and named appropriately when gathered from DataQuality
# -OR-
# Add this logic to the respective `Data_Quality_utils` function with a `plot` argument that is `FALSE` by default.




#' @title Render Data Quality plots & tables for RminorElevated dq_system_wide
#'
#' @inheritParams data_quality_tables
#' @param dq_past_year \code{(data.frame)} See `data_quality`
#' @param dq_eligibility_detail \code{(data.frame)} See `data_quality`
#' @param data_APs \code{(data.frame)} See `data_APs`
#'
#' @return A data frame of data quality summary.
#' @export
#' @include 04_DataQuality.R 04_DataQuality_utils.R 04_DataQuality_summary_utils.R

data_quality_summary <- function(served_in_date_range, Referrals, Project, dq_past_year, dq_eligibility_detail, dq_overlaps, data_APs, rm_dates, vars, app_env = get_app_env(e = rlang::caller_env())) {

  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())





  # ^^this is pulling in neither the Unsheltered NOR referrals from APs

dq_summary <- list()

client_summary <- dqu_summary(co_clients_served, distinct = FALSE) |>
  dplyr::rename(`Total Clients` = n)

  dq_summary$projects_errors <- dqu_summary(dq_past_year, filter_exp = Type %in% c("Error", "High Priority") &
            !Issue %in% c(
              "No Head of Household",
              "Missing Relationship to Head of Household",
              "Too Many Heads of Household",
              "Children Only Household"
            ), join = client_summary)

  dq_summary$error_types <- dqu_summary(dq_past_year, filter_exp = Type %in% c("Error", "High Priority"), groups = "Issue", distinct = FALSE)

  dq_summary$projects_warnings <- dqu_summary(dq_past_year, filter_exp = Type == "Warning", distinct = FALSE, join = client_summary)

  dq_summary$warning_types <- dqu_summary(dq_past_year, filter_exp = Type %in% c("Warning"), groups = "Issue", distinct = FALSE)


  dq_summary$hh_issues <- dqu_summary(dq_past_year, filter_exp = Type %in% c("Error", "High Priority") &
                                        Issue %in% c(
                                          "No Head of Household",
                                          "Missing Relationship to Head of Household",
                                          "Too Many Heads of Household",
                                          "Children Only Household"
                                        ), join = client_summary)



  dq_summary$outstanding_referrals <- dqu_summary(dq_past_year, filter_exp = Issue == "Old Outstanding Referral", distinct = FALSE, join = client_summary)


  dq_summary$eligibility <- dqu_summary(HMIS::served_between(dq_eligibility_detail, rm_dates$hc$check_dq_back_to, lubridate::today()), filter_exp = Type == "Warning" & Issue %in% c("Check Eligibility"), join = client_summary)

  dq_summary$clients_without_spdat <- dqu_summary(dq_past_year, filter_exp = Type == "Warning" & Issue %in% c("Non-DV HoHs Entering PH or TH without SPDAT",
                                                                                             "HoHs in shelter for 8+ days without SPDAT"), join = client_summary)

  dq_summary$overlaps <- dqu_summary(HMIS::served_between(dq_overlaps, rm_dates$hc$check_dq_back_to, lubridate::today()), distinct = FALSE, join = client_summary)
  dq_summary$long_stayer <- dqu_summary(dq_past_year, filter_exp = Type == "Warning" & Issue == "Extremely Long Stayer", join = client_summary)

  dq_summary$incorrect_destination <- dqu_summary(dq_past_year, filter_exp = stringr::str_detect(Issue, "Incorrect.*Destination"), join = client_summary)
  dq_summary$psh_destination <- dqu_summary(dq_past_year, filter_exp = stringr::str_detect(Issue, "(?:Destination|Missing).*(?:PSH)"), join = client_summary)


  app_env$gather_deps(dq_summary)
}
