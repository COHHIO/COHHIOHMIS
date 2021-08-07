devtools::load_all("../clarity.looker")
devtools::load_all("../hud.extract")
devtools::load_all("../../lookr")
# This is the default directory tree used by hud_export. It can be changed and amended and passed to hud_export in the `dirs` argument if necessary.
clarity.looker::dirs
clarity_api <- clarity.looker::clarity_api$new("inst/auth/Looker.ini")
clarity_api$get_export() # only need to run once



.hud_extra_data$Project_extras
.hud_extras <- purrr::map2(.hud_extras, .hud_extra_data, ~{
  purrr::list_modify(.x, !!!.y)
})
looks <- purrr::imap(looks, ~{
  rlang::eval_bare(rlang::expr(clarity_api[[!!.y]](look_type = "parity")))
})
# update all
clarity_api$update_export()
dates_env <- dates(clarity_api, .write = TRUE)
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
