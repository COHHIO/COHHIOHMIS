cl_api$get_folder_looks(cl_api$folders$`HUD Extras`, .write = TRUE,
                        path = dirs$extras)
pkgload::load_all()
rm_env <- dates(clarity_api = cl_api, app_env = rm_env)
rm_env <- load_export(clarity_api = cl_api, app_env = rm_env)
rm_env <- client_counts(app_env = rm_env)
rm_env <- cohorts(clarity_api = cl_api, app_env = rm_env)
rm_env <- bed_unit_utilization(clarity_api = cl_api, app_env = rm_env)
rm_env <- data_quality(clarity_api = cl_api, app_env = rm_env)
rm_env$write_app_deps(objs = rm_env$app_objs$RminorElevated,
                      path = file.path("data", "db", "RminorElevated"),
                      dep_nms = rm_env$app_deps$RminorElevated)

# Uses RminorElevated as the default
rm_env$deps_to_apps(dropbox = FALSE)
