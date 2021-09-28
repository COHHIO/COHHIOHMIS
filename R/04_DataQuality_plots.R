# TODO Determine data dependencies
# Map data dependencies to functions in `DataQuality_utils`
# Ensure dependencies are created and named appropriately when gathered from DataQuality
# -OR-
# Add this logic to the respective `Data_Quality_utils` function with a `plot` argument that is `FALSE` by default.




#' @title Render Data Quality plots for RminorElevated
#'
#' @param staging_outstanding_referrals
#' @param Project
#' @param dq_past_year
#' @param dq_unsheltered
#' @param rm_dates
#' @param data_APs
#' @param ProjectDisplay
#' @param app_env
#'
#' @return
#' @export
#' @include 04_DataQuality.R 04_DataQuality_utils.R
#' @examples
  data_quality_plots <- function(served_in_date_range, Referrals, Project, dq_past_year, dq_unsheltered, rm_dates, data_APs, ProjectDisplay, app_env = get_app_env(e = rlang::caller_env())) {
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    # Plots -------------------------------------------------------------------
    # Side Door ---------------------------------------------------------------
    # use Referrals, get logic from ART report- it's pretty lax I think


    # Old Outstanding Referrals -----------------------------------------------
    # CW says ProviderCreating should work instead of Referred-From Provider
    # Using ProviderCreating instead. Either way, I feel this should go in the
    # Provider Dashboard, not the Data Quality report.

    internal_old_outstanding_referrals <- served_in_date_range %>%
      semi_join(Referrals,
                by = c("PersonalID")) %>%
      left_join(Referrals,
                by = c("PersonalID")) %>%
      filter(ProviderCreating == ProjectName &
               ProjectID != 1695) %>%
      select(all_of(vars_prep),
             ProviderCreating,
             ReferralDate,
             ReferralOutcome,
             EnrollmentID) %>%
      filter(is.na(ReferralOutcome) &
               ReferralDate < today() - days(14)) %>%
      mutate(
        ProjectName = ProviderCreating,
        Issue = "Old Outstanding Referral",
        Type = "Warning",
        Guidance = "Referrals should be closed in about 2 weeks. Please be sure you are
      following up with any referrals and helping the client to find permanent
      housing. Once a Referral is made, the receiving agency should be saving
      the \"Referral Outcome\" once it is known. If you have Referrals that are
      legitimately still open after 2 weeks because there is a lot of follow
      up going on, no action is needed since the HMIS data is accurate."
      ) %>%
      select(all_of(vars_we_want))

    # ^^this is pulling in neither the Unsheltered NOR referrals from APs

    staging_outstanding_referrals <-
      internal_old_outstanding_referrals %>%
      left_join(Project[c("ProjectName", "ProjectID")], by = "ProjectName") %>%
      select(ProjectName, ProjectID, PersonalID) %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(Open_Referrals = n()) %>%
      arrange(desc(Open_Referrals)) %>%
      mutate(Project = paste0(ProjectName, ":", ProjectID))


    dq_plot_outstanding_referrals <-
      ggplot2::ggplot(
        head(staging_outstanding_referrals, 20L),
        ggplot2::aes(
          x = stats::reorder(Project, Open_Referrals),
          y = Open_Referrals,
          fill = Open_Referrals
        )
      ) +
      dqu_plot_theme_labs(x = "",
                    y = "Referrals")


    dq_data_errors_plot <- dq_past_year %>%
      dplyr::filter(
        Type %in% c("Error", "High Priority") &
          !Issue %in% c(
            "No Head of Household",
            "Missing Relationship to Head of Household",
            "Too Many Heads of Household",
            "Children Only Household"
          )
      ) %>%
      dplyr::select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      dplyr::group_by(ProjectName, ProjectID) %>%
      dplyr::summarise(clientsWithErrors = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(clientsWithErrors))

    dq_data_errors_plot$hover <-
      with(dq_data_errors_plot,
           paste0(ProjectName, ":", ProjectID))

    dq_plot_projects_errors <-
      ggplot2::ggplot(
        head(dq_data_errors_plot, 20L),
        ggplot2::aes(
          x = stats::reorder(hover, clientsWithErrors),
          y = clientsWithErrors,
          fill = clientsWithErrors
        )
      ) +
      dqu_plot_theme_labs(x = "",
                    y = "Clients")

    dq_data_warnings_plot <- dq_past_year %>%
      dplyr::filter(Type == "Warning") %>%
      dplyr::group_by(ProjectName, ProjectID) %>%
      dplyr::summarise(Warnings = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(Warnings))

    dq_data_warnings_plot$hover <-
      with(dq_data_warnings_plot,
           paste0(ProjectName, ":", ProjectID))

    dq_plot_projects_warnings <-
      ggplot2::ggplot(head(dq_data_warnings_plot, 20L),
                      ggplot2::aes(
                        x = stats::reorder(hover, Warnings),
                        y = Warnings,
                        fill = Warnings
                      )) +
      dqu_plot_theme_labs(x = "",
                    y = "Clients")

    dq_data_error_types <- dq_past_year %>%
      dplyr::filter(Type %in% c("Error", "High Priority")) %>%
      dplyr::group_by(Issue) %>%
      dplyr::summarise(Errors = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(Errors))

    dq_plot_errors <-
      ggplot2::ggplot(head(dq_data_error_types, 10L),
                      ggplot2::aes(
                        x = stats::reorder(Issue, Errors),
                        y = Errors,
                        fill = Errors
                      )) +
      dqu_plot_theme_labs(x = "",
                    y = "Clients")

    dq_data_warning_types <- dq_past_year %>%
      dplyr::filter(Type == "Warning") %>%
      dplyr::group_by(Issue) %>%
      dplyr::summarise(Warnings = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(Warnings))

    dq_plot_warnings <-
      ggplot2::ggplot(head(dq_data_warning_types, 10L),
                      ggplot2::aes(
                        x = stats::reorder(Issue, Warnings),
                        y = Warnings,
                        fill = Warnings
                      )) +
      dqu_plot_theme_labs(x = "",
                    y = "Clients")

    dq_data_unsheltered_high <- dq_unsheltered %>%
      dplyr::filter(Type == "High Priority",
                    HMIS::served_between(., rm_dates$hc$unsheltered_data_start, rm_dates$meta_HUDCSV$Export_End)) %>%
      dplyr::select(PersonalID, HouseholdID, DefaultProvider) %>%
      unique() %>%
      dplyr::group_by(DefaultProvider) %>%
      dplyr::summarise(clientsWithErrors = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(clientsWithErrors))

    dq_plot_unsheltered_high <-
      ggplot2::ggplot(
        head(dq_data_unsheltered_high, 20L),
        ggplot2::aes(
          x = stats::reorder(DefaultProvider, clientsWithErrors),
          y = clientsWithErrors,
          fill = clientsWithErrors
        )
      ) +
      dqu_plot_theme_labs(x = "",
                    y = "Clients")

    dq_data_hh_issues_plot <- dq_past_year %>%
      dplyr::filter(
        Type %in% c("Error", "High Priority") &
          Issue %in% c(
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Too Many Heads of Household",
            "Children Only Household"
          )
      ) %>%
      dplyr::select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      dplyr::group_by(ProjectName, ProjectID) %>%
      dplyr::summarise(Households = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(Households))

    dq_data_hh_issues_plot$hover <-
      with(dq_data_hh_issues_plot,
           paste0(ProjectName, ":", ProjectID))

    dq_plot_hh_errors <-
      ggplot2::ggplot(head(dq_data_hh_issues_plot, 20L),
                      ggplot2::aes(
                        x = stats::reorder(hover, Households),
                        y = Households,
                        fill = Households
                      )) +
      dqu_plot_theme_labs(x = "")


    data_APs <- dqu_aps(data = TRUE)

    dq_plot_aps_referrals <-
      ggplot2::ggplot(data_APs, ggplot2::aes(fill = category, x = providertype, y = percent)) +
      ggplot2::geom_bar(position = "fill",
                        stat = "identity",
                        width = .1) +
      ggplot2::geom_label(
        ggplot2::aes(label = paste(
          data_APs$category,
          "\n",
          data_APs$prettypercent
        )),
        position = ggplot2::position_stack(),
        vjust = 2,
        fill = "white",
        colour = "black",
        fontface = "bold"
      ) +
      ggplot2::scale_fill_manual(values = c("#00952e", "#a11207"), guide = FALSE) +
      ggplot2::theme_void()

    dq_data_outstanding_referrals_plot <- dq_past_year %>%
      dplyr::filter(Issue == "Old Outstanding Referral") %>%
      dplyr::select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      dplyr::group_by(ProjectName, ProjectID) %>%
      dplyr::summarise(Households = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(Households)) %>%
      dplyr::mutate(hover = paste(ProjectName, ":", ProjectID))

    dq_plot_projects_outstanding_referrals <-
      ggplot2::ggplot(
        head(dq_data_outstanding_referrals_plot, 20L),
        ggplot2::aes(
          x = stats::reorder(hover, Households),
          y = Households,
          fill = Households
        )
      ) +
      dqu_plot_theme_labs(x = "")

    dq_data_eligibility_plot <- dq_past_year %>%
      dplyr::filter(Type == "Warning" &
                      Issue %in% c("Check Eligibility")) %>%
      dplyr::select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      dplyr::group_by(ProjectName, ProjectID) %>%
      dplyr::summarise(Households = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(Households))

    dq_data_eligibility_plot$hover <-
      with(dq_data_eligibility_plot,
           paste0(ProjectName, ":", ProjectID))

    dq_plot_eligibility <-
      ggplot2::ggplot(
        head(dq_data_eligibility_plot, 20L),
        ggplot2::aes(
          x = stats::reorder(hover, Households),
          y = Households,
          fill = Households
        )
      ) +
      dqu_plot_theme_labs(x = "")

    dq_data_without_spdat_plot <- dq_past_year %>%
      dplyr::filter(
        Type == "Warning" &
          Issue %in% c(
            "Non-DV HoHs Entering PH or TH without SPDAT",
            "HoHs in shelter for 8+ days without SPDAT"
          )
      ) %>%
      dplyr::select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      dplyr::group_by(ProjectName, ProjectID) %>%
      dplyr::summarise(Households = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ProjectDisplay = paste0(ProjectName, ":", ProjectID)) %>%
      dplyr::arrange(dplyr::desc(Households))

    dq_plot_hh_no_spdat <-
      ggplot2::ggplot(
        head(dq_data_without_spdat_plot, 20L),
        ggplot2::aes(
          x = stats::reorder(ProjectDisplay, Households),
          y = Households,
          fill = Households
        )
      ) +
      dqu_plot_theme_labs(x = "")
  }


