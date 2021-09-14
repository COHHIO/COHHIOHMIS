client_counts <- function(Project, Enrollment, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$merge_deps_to_env(missing_fmls())

  project_small <- qpr_project_small(Project)
  app
  enrollment_small <- qpr_enrollment_small(Enrollment)

  validation <- qpr_validation(project_small, enrollment_small)
  app_env$gather_deps("everything")
}



