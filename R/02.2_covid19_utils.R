#' @title Create covid19 Prioritization plots
#'
#' @param x \code{(data.frame)} priority or covid19_status
#' @param y_col \code{(symbol)} Column that will populate the Y axis values
#' @inheritParams ggplot2::scale_x_date
#' @inheritParams ggplot2::scale_fill_brewer
#'
#' @return \code{(Plot)}
#' @export

c19_plot <-
  function(x,
           y_col,
           date_breaks = "2 months",
           palette = "GnBu") {
    rlang::enexpr(y_col)
    dplyr::select(x, PersonalID, YM, {{y_col}}) %>%
      dplyr::group_by(YM, {{y_col}}) %>%
      dplyr::summarise(Clients = dplyr::n()) %>%
      dplyr::arrange(YM) %>%
      ggplot2::ggplot(ggplot2::aes(
        x = YM,
        y = Clients,
        fill = {{y_col}},
        label = Clients
      )) +
      ggplot2::geom_bar(stat = "identity",
                        position = "dodge") +
      # tsibble::scale_x_yearmonth(date_breaks = "2 months") +
      ggplot2::scale_y_continuous(breaks = rlang::as_function(~{seq(0, .x[2], by = 100)})) +
      ggplot2::scale_fill_brewer(palette = palette) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Month of", y = "Clients Assessed") +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "top",
        legend.text = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1,
          size = 11
        )
      )
  }
