source("renv/activate.R")
if (file.exists("~/.Rprofile"))
  source("~/.Rprofile")
if (file.exists(".Renviron"))
  readRenviron(".Renviron")
options(renv.settings.ignored.packages = c("jobs"),
        renv.settings.snapshot.type = "explicit",
        renv.config.sandbox.enabled = FALSE,
        ggmap = list(google = list(second_limit = 50L,
                                   day_limit = 2500)),
        rpushbullet.dotfile = file.path("inst","auth","rpushbullet.json"),
        HMIS = list(Clarity = TRUE,
                    ServicePoint = TRUE,
                    Clarity_URL = "https://cohhio.clarityhs.com",
                    clients_to_filter = c(`1E2025A5D` = "335", `08EF0EF09` = "1", `211F2D38E` = "9")))

