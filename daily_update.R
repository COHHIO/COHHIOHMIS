# Must set GGMAP_GOOGLE_API_KEY in .Renviron (should not be needed until 2022), see ?ggmap::register_google for details.
# Set this option to bypass errors in ggmap
# options(ggmap = list(google = list(second_limit = 50L,
#                                    day_limit = 2500)))
# These are necessary when rapidly modifying these dependencies

# devtools::load_all("../../lookr")
#devtools::load_all("../hud.extract")

# RPushBullet setup
#RPushbullet::pbSetup(conffile = file.path("inst","auth","rpushbullet.json"))

# This is the default directory tree used by Rm_data. It can be changed and amended and passed to hud_export in the `dirs` argument if necessary.
dirs <- clarity.looker::dirs

# must load Rm_data
devtools::load_all()
rstudioapi::jobRunScript(file.path("inst","src","update_data.R"), importEnv = TRUE, workingDir = getwd())

Rm_env$gather_deps(guidance)
Rm_env <- dates()
Rm_env <- load_export()
Rm_env <- client_counts()
Rm_env <- data_quality()
Rm_env$write_app_deps(Rm_env$app_objs$RminorElevated, Rm_env$app_deps$RminorElevated, file.path("data", "db", "RminorElevated"))
# Uses RminorElevated as the default
Rm_env$dropbox_upload()



increment("Importing raw HMIS data\n")

# list.files(full.names = TRUE, "~/R/Contributor_Repos/COHHIO/COHHIO_HMIS/public_data") %>%
#   purrr::walk(~{
#     file.copy(.x, file.path("~/R/Contributor_Repos/COHHIO/COHHIOHMIS/data/public",basename(.x)))
#   })
# file.copy("~/R/Contributor_Repos/COHHIO/COHHIO_HMIS/Looker.ini", "~/R/Contributor_Repos/COHHIO/COHHIOHMIS/Looker.ini")
# c("cli",
#   "feather",
#   "HMIS",
#   "lubridate",
#   "readxl",
#   "rlang",
#   "scales",
#   "janitor",
#   "devtools",
#   "fs",
#   "purrr",
#   # "urbnmapr",
#   "sf",
#   # "choroplethrMaps",
#   "plotly") %>% sort %>% cat(sep = ",\n")


# cohorts <- function(env) {
#   #load("")
#   # Code  ----
#   # Mon Sep 28 11:20:46 2020
#   # save.image
#   environment()
# }
#
#
# env$cohorts <- cohorts()
# Veterans <- function(env) {
#   parent.env(environment(env$cohorts))
#   #load(")
#   #Veterans
#   #save.image(")
#   environment()
# }
#
#
# Veterans(env$cohorts)
