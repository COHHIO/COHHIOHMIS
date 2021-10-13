jobs::jobscript({

  if (!exists("export")) {
    export <- list.files(clarity.looker::dirs$export) |>
      stringr::str_remove("\\.[A-Za-z]+$") |>
      rlang::set_names() |>
      purrr::map(~{
        list(api = clarity.looker::hud_load(.x, clarity.looker::dirs$export),
             ui = clarity.looker::hud_load(.x, "data") |> dplyr::select(-ExportID))
      })

    # arrange
    export <- purrr::imap(export, ~{
      nm <- .y
      purrr::map(.x, ~dplyr::arrange(.x, dplyr::all_of(ifelse(nm == "Export", "ExportDate", "DateCreated"))))
    })
  }

  compares <- purrr::imap(export, ~{
    waldo::compare(.x$api, .x$ui, x_arg = "api", y_arg = "ui")
  })

}, exportEnv = "R_GlobalEnv", workingDir = getwd())

