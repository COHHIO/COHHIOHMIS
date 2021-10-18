go_to_daily_update <- function() {
  rstudioapi::navigateToFile("daily_update.R")
}
bg_scripts <- c("data", "export", "extras", "services") |>
  {\(x) {file.path("inst", "src", paste0("update_",x, ".R")) |> rlang::set_names(x)}}()

run_bg <- function(file = bg_scripts[1]) {
  rstudioapi::jobRunScript(file, importEnv = TRUE, workingDir = getwd())
}


