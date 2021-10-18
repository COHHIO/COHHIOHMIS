if (!exists("dirs"))
  dirs <- clarity.looker::dirs
cl_api$get_export(.write = TRUE) # only need to run once
