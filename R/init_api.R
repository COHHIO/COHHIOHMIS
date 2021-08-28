if (Sys.info()["nodename"] == "DESKTOP-2SK9RKR") {
  devtools::load_all("../clarity.looker")
  clarity_api <- clarity.looker::clarity_api$new(file.path("inst","auth","Looker.ini"))
}
