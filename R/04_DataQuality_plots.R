data_quality_plots <- function() {
  # Plots -------------------------------------------------------------------
  dq_plot_outstanding_referrals <-
    ggplot2::ggplot(
      head(staging_outstanding_referrals, 20L),
      ggplot2::aes(
        x = stats::reorder(Project, Open_Referrals),
        y = Open_Referrals,
        fill = Open_Referrals
      )
    ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Referrals") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)


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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
                  y = "Clients") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

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
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)

}
