library(clarity.looker)
cl_api$get_export(.write = TRUE) # only need to run once
cl_api$get_folder_looks(cl_api$folders$`HUD Extras`, .write = TRUE, path = dirs$extras)
