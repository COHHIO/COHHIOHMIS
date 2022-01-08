# Must set GGMAP_GOOGLE_API_KEY in .Renviron (should not be neeqded until 2022), see ?ggmap::register_google for details.
# Set this option to bypass errors in ggmap
# options(ggmap = list(google = list(second_limit = 50L,
#                                    day_limit = 2500)))
# These are necessary when rapidly modifying these dependencies

# devtools::load_all("../../lookr")
# devtools::load_all("../hud.extract")

# RPushBullet setup
#RPushbullet::pbSetup(conffile = file.path("inst","auth","rpushbullet.json"))



#1. must load RmData
devtools::load_all()

#2. must set directories if using a directory structure differing from the default in clarity.looker:
# dirs <- clarity.looker::dirs
# Rm_env$gather_deps(dirs)

# 3. Run update
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
