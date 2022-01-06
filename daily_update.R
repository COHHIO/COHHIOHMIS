# Must set GGMAP_GOOGLE_API_KEY in .Renviron (should not be neeqded until 2022), see ?ggmap::register_google for details.
# Set this option to bypass errors in ggmap
# options(ggmap = list(google = list(second_limit = 50L,
#                                    day_limit = 2500)))
# These are necessary when rapidly modifying these dependencies

# devtools::load_all("../../lookr")
# devtools::load_all("../hud.extract")

# RPushBullet setup
#RPushbullet::pbSetup(conffile = file.path("inst","auth","rpushbullet.json"))



# must load RmData
devtools::load_all()
# must set directories if using a directory structure differing from the default in clarity.looker:
dirs <- clarity.looker::dirs
# Use the HUD CSV from the UI until the Looker API is fixed
Rm_env$gather_deps(dirs)
# Try services
# rstudioapi::jobRunScript(file.path("inst","src","Services_test.R"), importEnv = TRUE, workingDir = getwd())
daily_update(backup = TRUE)
beepr::beep(sound = 3)




# increment("Importing raw HMIS data\n")

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
