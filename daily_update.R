# Optional: These are necessary when rapidly modifying these dependencies

# devtools::load_all("../../lookr")
# devtools::load_all("../hud.extract")


#1. must load RmData
devtools::load_all()

#2. must set directories if using a directory structure differing from the default in clarity.looker:
# dirs <- clarity.looker::dirs
# Rm_env$gather_deps(dirs)

# 3. Extract the HUD Export zip
clarity.looker::hud_export_extract()
# 4. Run update
daily_update(backup = TRUE, remote = FALSE)
beepr::beep(sound = 3)




# WHEN NEW GEOCODES ARE ISSUED BY HUD, some projects may fail to match with their county in `pe_add_regions`.  ggmap is used to determine the missing county for these projects. ggmap and its dependencies are currently ignored in the renv/settings.dcf See ?ggmap::register_google for details on setting up ggmap.
# 1. Set GGMAP_GOOGLE_API_KEY in .Renviron.
# 2. Set this option to bypass errors in ggmap
# options(ggmap = list(google = list(second_limit = 50L,
#                                    day_limit = 2500)))
