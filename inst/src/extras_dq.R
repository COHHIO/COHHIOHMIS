devtools::load_all()
dirs <- clarity.looker::dirs
guidance <- guidance
# Use the HUD CSV from the UI until the Looker API is fixed
dirs$export <- "data"
Rm_env$.__enclos_env__$self$dirs <- dirs
Rm_env$get_folder_looks(cl_api, app_env = Rm_env$folders$`HUD Extras`, .write = TRUE, path = dirs$extras)
Rm_env$gather_deps(guidance)
Rm_env$gather_deps(dirs)
Rm_env <- dates(clarity_api = cl_api, app_env = Rm_env)
Rm_env <- load_export(clarity_api = cl_api, app_env = Rm_env)
Rm_env <- client_counts(app_env = Rm_env)
Rm_env <- cohorts(clarity_api = cl_api, app_env = Rm_env)
Rm_env <- bed_unit_utilization(clarity_api = cl_api, app_env = Rm_env)
Rm_env <- data_quality(clarity_api = cl_api, app_env = Rm_env)
Rm_env$write_app_deps(objs = Rm_env$app_objs$RminorElevated, path = file.path("data", "db", "RminorElevated"), dep_nms = Rm_env$app_deps$RminorElevated)
# Uses RminorElevated as the default
# Rm_env$dropbox_auth()
Rm_env$deps_to_apps(dropbox = FALSE)
