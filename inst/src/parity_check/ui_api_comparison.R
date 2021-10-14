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
    ui_dateupdated_max <- purrr::imap(export, ~{
      if (.y != "Export")
        c(max(.x$api$DateUpdated, na.rm = TRUE), max(.x$ui$DateUpdated, na.rm = TRUE))
    }) |> purrr::map(2) |> unlist() |> max() |> lubridate::as_datetime()

    export <- purrr::imap(export, ~{
      if (.y != "Export")
        .x$api <- dplyr::filter(.x$api, DateUpdated <= ui_dateupdated_max)
      .x
    })
  }

  compares <- purrr::imap(export, ~{
    .d <- .x
    out <- list(rows = c(api = nrow(.d$api), ui = nrow(.d$ui)))
    out$diffs <- purrr::imap(.x$api, ~{
      list(api.ui = setdiff(unique(.x), unique(.d$ui[[.y]])),
           ui.api = setdiff(unique(.d$ui[[.y]]), unique(.x)))
    })
    out
  })

}, exportEnv = "R_GlobalEnv", workingDir = getwd())


purrr::map(compares, ~{
  do.call(`-`, unname(.x$rows) |> as.list())
}) |> unlist() |> {\(x) {paste0(names(x)," = ", x)}}() |> cat(sep = "\n")
