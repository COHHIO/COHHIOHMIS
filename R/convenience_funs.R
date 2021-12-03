go_to <- function(x, path = "R") {
  if (file.exists(x))
    f <- x
  else
    f <- list.files(path, pattern = x, full.names = TRUE)

  if (UU::is_legit(f))
    rstudioapi::navigateToFile(f)
  else
    rlang::abort(paste0("Can't find file ",x))
}

bg_scripts <- list.files(file.path("inst", "src"), full.names = TRUE, pattern = "R$") |>
  {\(x) {rlang::set_names(x, stringr::str_remove(basename(x), "\\.R$"))}}()

run_bg <- function(path = bg_scripts[1],
                   name = rlang::expr(basename(path)),
                   workingDir = getwd(),
                   importEnv = TRUE,
                   exportEnv = "R_GlobalEnv") {
  name <- rlang::eval_bare(name)
  rstudioapi::jobRunScript(path.expand(path), importEnv = importEnv, workingDir = workingDir, exportEnv = exportEnv, name = name)
}


