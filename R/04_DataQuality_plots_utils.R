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
