# Must set GGMAP_GOOGLE_API_KEY in .Renviron, see ?ggmap::register_google for details
# Set this option to bypass errors in ggmap
options(ggmap = list(google = list(second_limit = 50L,
                                   day_limit = 2500)))
devtools::load_all("../clarity.looker")
devtools::load_all("../../lookr")
#devtools::load_all("../hud.extract")
# This is the default directory tree used by hud_export. It can be changed and amended and passed to hud_export in the `dirs` argument if necessary.
clarity.looker::dirs
clarity_api <- clarity.looker::clarity_api$new("inst/auth/Looker.ini")
jobs::jobscript({
  devtools::load_all("../clarity.looker")
  devtools::load_all("../../lookr")
  #clarity_api$get_export(.write = TRUE) # only need to run once
  clarity_api$get_folder_looks(clarity_api$folders$`HUD Extras`, .write = TRUE, path = dirs$extras)
})
# must load COHHIOHMIS
devtools::load_all()
Rm_env <- app_env$new()


Rm_env <- dates()

Rm_env <- guidance()

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
