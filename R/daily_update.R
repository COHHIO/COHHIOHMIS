hud <- hud.export::hud_export$new("Looker.ini")

dates_obj <- dates(hud)
increment("Importing raw HMIS data\n")
#TODO Where does public_data come from
# list.files(full.names = TRUE, "~/R/Contributor_Repos/COHHIO/COHHIO_HMIS/public_data") %>%
#   purrr::walk(~{
#     file.copy(.x, file.path("~/R/Contributor_Repos/COHHIO/COHHIOHMIS/data/public",basename(.x)))
#   })

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



cohorts <- function(env) {
  #load("")
  # Code  ----
  # Mon Sep 28 11:20:46 2020
  # save.image
  environment()
}


env$cohorts <- cohorts()
Veterans <- function(env) {
  parent.env(environment(env$cohorts))
  #load(")
  #Veterans
  #save.image(")
  environment()
}


Veterans(env$cohorts)
