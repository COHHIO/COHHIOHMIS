# TODO Determine data dependencies
# Map data dependencies to functions in `DataQuality_utils`
# Ensure dependencies are created and named appropriately when gathered from DataQuality
# -OR-
# Add this logic to the respective `Data_Quality_utils` function with a `plot` argument that is `FALSE` by default.




#' @title Render Data Quality plots & tables for RminorElevated dq_system_wide
#'
#' @param Project
#' @param dq_past_year
#' @param eligibility_detail
#' @param rm_dates
#' @param data_APs
#' @param Referrals
#' @param app_env
#'
#' @return
#' @export
#' @include 04_DataQuality.R 04_DataQuality_utils.R 04_DataQuality_summary_utils.R
#' @examples
data_quality_summary <- function(served_in_date_range, Referrals, Project, dq_past_year, eligibility_detail, data_APs, rm_dates, vars, app_env = get_app_env(e = rlang::caller_env())) {

  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())

  # use Referrals, get logic from ART report- it's pretty lax I think


  # Old Outstanding Referrals -----------------------------------------------
  # CW says ProviderCreating should work instead of Referred-From Provider
  # Using ProviderCreating instead. Either way, I feel this should go in the
  # Provider Dashboard, not the Data Quality report.

  internal_old_outstanding_referrals <- served_in_date_range %>%
    dplyr::semi_join(Referrals,
                     by = c("PersonalID", "UniqueID")) %>%
    dplyr::left_join(Referrals, by = c("PersonalID", "UniqueID")) |>
    dplyr::select(dplyr::all_of(vars$prep),
                  R_ReferringProjectID,
                  R_ReferralDaysElapsed,
                  R_ReferringProjectName,
                  R_DaysInQueue,
                  EnrollmentID) %>%
    dplyr::filter(R_ReferralDaysElapsed %|% R_DaysInQueue > 14) %>%
    dplyr::mutate(
      ProjectName = R_ReferringProjectName,
      ProjectID = R_ReferringProjectID,
      Issue = "Old Outstanding Referral",
      Type = "Warning",
      Guidance = "Referrals should be closed in about 2 weeks. Please be sure you are following up with any referrals and helping the client to find permanent housing. Once a Referral is made, the receiving agency should be saving the 'Referral Outcome' once it is known. If you have Referrals that are legitimately still open after 2 weeks because there is a lot of follow up going on, no action is needed since the HMIS data is accurate."
    ) %>%
    dplyr::select(dplyr::all_of(c(vars$we_want, "ProjectID")))

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



  # Deprecated in Clarity
  # dq_data_unsheltered_high <- dq_unsheltered %>%
  #   dplyr::filter(Type == "High Priority",
  #                 HMIS::served_between(., rm_dates$hc$unsheltered_data_start, rm_dates$meta_HUDCSV$Export_End)) %>%
  #   dplyr::select(PersonalID, HouseholdID, DefaultProvider) %>%
  #   unique() %>%
  #   dplyr::group_by(DefaultProvider) %>%
  #   dplyr::summarise(clientsWithErrors = dplyr::n()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::arrange(dplyr::desc(clientsWithErrors))
  #
  # dq_plot_unsheltered_high <-
  #   ggplot2::ggplot(
  #     utils::head(dq_data_unsheltered_high, 20L),
  #     ggplot2::aes(
  #       x = stats::reorder(DefaultProvider, clientsWithErrors),
  #       y = clientsWithErrors,
  #       fill = clientsWithErrors
  #     )
  #   ) +
  #   dqu_summary_theme_labs(x = "",
  #                                y = "Clients")
  dq_summary$hh_issues <- dqu_summary(dq_past_year, filter_exp = Type %in% c("Error", "High Priority") &
                                        Issue %in% c(
                                          "No Head of Household",
                                          "Missing Relationship to Head of Household",
                                          "Too Many Heads of Household",
                                          "Children Only Household"
                                        ), join = client_summary)



  dq_plot_aps_referrals <-
    ggplot2::ggplot(data_APs, ggplot2::aes(fill = category, x = providertype, y = percent)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity",
                      width = .1) +
    ggplot2::geom_label(
      ggplot2::aes(label = paste(
        category,
        "\n",
        prettypercent
      )),
      vjust = -1,
      hjust = "outward",
      fill = "white",
      colour = "black",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = c("#a11207", "#00952e"), guide = FALSE) +
    ggplot2::coord_flip()+
    ggplot2::theme_void()


  dq_summary$outstanding_referrals <- dqu_summary(internal_old_outstanding_referrals, filter_exp = Issue == "Old Outstanding Referral", distinct = FALSE, join = client_summary)


  dq_summary$eligibility <- dqu_summary(eligibility_detail, filter_exp = Type == "Warning" & Issue %in% c("Check Eligibility"), join = client_summary)

  dq_summary$clients_without_spdat <- dqu_summary(dq_past_year, filter_exp = Type == "Warning" & Issue %in% c("Non-DV HoHs Entering PH or TH without SPDAT",
                                                                                             "HoHs in shelter for 8+ days without SPDAT"), join = client_summary)
  dq_main <- dplyr::bind_rows(dq_main, internal_old_outstanding_referrals)
  app_env$gather_deps(dq_plot_aps_referrals, dq_summary, dq_main)
}
