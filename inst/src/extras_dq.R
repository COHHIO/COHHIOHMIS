if (!exists("dirs"))
  dirs <- clarity.looker::dirs
cl_api$get_folder_looks(cl_api$folders$`HUD Extras`, .write = TRUE, path = dirs$extras)
Rm_env$gather_deps(guidance)
Rm_env$gather_deps(dirs)
Rm_env <- dates()
Rm_env <- load_export()
Rm_env <- client_counts()
Rm_env <- cohorts()
Rm_env <- data_quality()
Rm_env$write_app_deps(objs = Rm_env$app_objs$RminorElevated, path = file.path("data", "db", "RminorElevated"), dep_nms = Rm_env$app_deps$RminorElevated)
# Uses RminorElevated as the default
Rm_env$dropbox_auth()
Rm_env$deps_to_apps()
