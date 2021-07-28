devtools::load_all("../hud.export")
# This is the default directory tree used by hud_export. It can be changed and amended and passed to hud_export in the `dirs` argument if necessary.
dirs <- list(export = "data/API",
             public = "data/public",
             spdat = "data/spdat",
             extra = "data/extra")
hud <- hud.export::hud_export$new("inst/auth/Looker.ini")

hud$get_all()
dates_env <- dates(hud, .write = TRUE)
increment("Importing raw HMIS data\n")
#TODO Where does public_data come from
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
