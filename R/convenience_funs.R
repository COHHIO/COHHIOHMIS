go_to <- function(x, path = "R") {
  f <- list.files(path, pattern = x, full.names = TRUE)
  if (UU::is_legit(f))
    rstudioapi::navigateToFile(f)
  else
    rlang::abort(paste0("Can't find file ",x))
}

bg_scripts <- list.files(file.path("inst", "src"), full.names = TRUE, pattern = "R$") |>
  {\(x) {rlang::set_names(x, stringr::str_remove(basename(x), "\\.R$"))}}()

run_bg <- function(file = bg_scripts[1]) {
  rstudioapi::jobRunScript(file, importEnv = TRUE, workingDir = getwd())
}


