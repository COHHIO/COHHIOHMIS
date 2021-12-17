# Plot utils ----
# Mon Sep 20 11:16:46 2021

dqu_plot_theme_labs <- function(g, x = NULL, y = NULL) {
  .labs <- list()
  if (UU::is_legit(x))
    .labs <- list(x = x)
  if (UU::is_legit(y))
    .labs <- list(x = y)
  g +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    do.call(ggplot2::labs, .labs) +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_minimal(base_size = 18)
}

dqu_summary <- function(.data, filter_exp, groups = c("ProjectName", "ProjectID"), distinct = TRUE, x_label = "", y_label = "", data = TRUE, join, suffix = c("_Issue", "_Clients")) {
  p_data <- .data
  if (!missing(filter_exp))
    p_data <- p_data %>%
      dplyr::filter(
        !!rlang::enquo(filter_exp)
      )
  if (distinct)
    p_data <- dplyr::distinct(p_data, PersonalID, ProjectID, ProjectName)
  if (!missing(join))
    ns <- purrr::map(suffix, ~rlang::sym(paste0("n", .x)))
  else
    ns <- list(rlang::sym("n"))
  p_data <- dplyr::group_by(p_data, !!!purrr::map(groups, rlang::sym)) %>%
    dplyr::summarise(!!ns[[1]] := dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(!!ns[[1]]))

  if (!missing(join)) {
    p_data <- dplyr::left_join(p_data, join, by = UU::common_names(p_data, join))
    ns <- purrr::map(stringr::str_subset(names(p_data), stringr::regex("(?:^n_)|(?:Clients$)", ignore_case = TRUE)), rlang::sym)
    ex <- rlang::expr(round(!!ns[[1]] / !!ns[[2]], 5))

    p_data <- dplyr::mutate(p_data, `Frequency (Errors / Total Clients)` = !!ex,
                    from_mean = dplyr::percent_rank(`Frequency (Errors / Total Clients)`) - .5)
  }

  if (!data) {
    if (all(c("ProjectName", "ProjectID") %in% groups))
      p_data <- dplyr::mutate(p_data, hover = paste0(ProjectName, ":", ProjectID))

    x_order <- purrr::when("hover" %in% names(p_data), . ~ rlang::expr(hover), ~ rlang::expr(Issue))
    out <-
      ggplot2::ggplot(
        utils::head(p_data, 20L),
        ggplot2::aes(
          x = stats::reorder(!!x_order, n),
          y = n,
          fill = n
        )
      ) |>
      dqu_plot_theme_labs(x = x_label, y = y_label)
  } else
    out <- p_data

  out
}



