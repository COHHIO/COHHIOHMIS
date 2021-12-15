# Plot utils ----
# Mon Sep 20 11:16:46 2021

dqu_plot_theme_labs <- function(g, x = NULL, y = NULL) {
  .labs <- list()
  if (!missing(x))
    .labs <- list(x = x)
  if (!missing(y))
    .labs <- list(x = y)
  g +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    do.call(ggplot2::labs, .labs) +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)
}

dq_plot <- function(.data, filter_exp, groups = c("ProjectName", "ProjectID"), y_label = "Clients", select  = TRUE, x_label = "") {
  p_data <- .data
  if (!missing(filter_exp))
    p_data <- p_data %>%
      dplyr::filter(
        !!rlang::enquo(filter_exp)
      )
  if (select)
    p_data <- dplyr::select(p_data, PersonalID, ProjectID, ProjectName) %>%
      unique()

  p_data <- dplyr::group_by(p_data, !!!purrr::map(groups, rlang::sym)) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(hover = paste0(ProjectName, ":", ProjectID))

  out <-
    ggplot2::ggplot(
      utils::head(p_data, 20L),
      ggplot2::aes(
        x = stats::reorder(hover, n),
        y = n,
        fill = n
      )
    ) |>
    dqu_plot_theme_labs(x = x_label, y = y_label)
  out
}
