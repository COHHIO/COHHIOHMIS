covid19_plots <- function(priority, covid19status, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())

  # COVID Priority Plots -------------------------------------------------------

  covid19_priority_plot <- c19_plot(priority, Priority)

  # COVID19 Status Plots ----
  # Tue Nov 02 16:10:02 2021

  covid19_status_plot <- c19_plot(covid19_status, COVID19Status, palette = "OrRd")

  app_env$gather_deps("everything")
}
