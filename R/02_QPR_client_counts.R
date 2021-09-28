client_counts <- function(Project, Enrollment_extra_Exit_HH_CL_AaE, rm_dates, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())

  project_small <- qpr_project_small(Project, rm_dates)

  enrollment_small <- qpr_enrollment_small(Enrollment_extra_Exit_HH_CL_AaE)

  validation <- qpr_validation(project_small, enrollment_small)
  app_env$gather_deps("everything")
}



