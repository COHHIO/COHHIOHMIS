devtools::load_all()
dirs <- clarity.looker::dirs
guidance <- guidance
# Use the HUD CSV from the UI until the Looker API is fixed
dirs$export <- "data"
cl_api$.__enclos_env__$self$dirs <- dirs
cl_api$get_folder_looks(cl_api$folders$`HUD Extras`, .write = TRUE, path = dirs$extras)
Rm_env$gather_deps(guidance)
Rm_env$gather_deps(dirs)
Rm_env <- dates(app_env = cl_api)
Rm_env <- load_export(app_env = cl_api)
Rm_env <- client_counts(app_env = cl_api)
Rm_env <- cohorts(app_env = cl_api)
Rm_env <- bed_unit_utilization(app_env = cl_api)
Rm_env <- data_quality(app_env = cl_api)
Rm_env$write_app_deps(objs = Rm_env$app_objs$RminorElevated, path = file.path("data", "db", "RminorElevated"), dep_nms = Rm_env$app_deps$RminorElevated)
# Uses RminorElevated as the default
# Rm_env$dropbox_auth()
Rm_env$deps_to_apps(dropbox = FALSE)
