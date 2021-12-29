seq_vec <- function(x) {
  purrr::map2(x[-length(x)], dplyr::lead(x)[-length(x)] - 1, `:`)
}

load_spm <- function(filename) {
  spm <- readxl::read_xlsx(filename)
  # Get the DQ Section index
  dq_idx <- stringr::str_which(spm[[1]], "^Data Quality")
  # Get the Measure indexes
  m_idx <- c(stringr::str_which(spm[[1]], "^Measure \\d{1,2}(?:\\w and \\d\\w)?\\:"),  dq_idx - 1)
  # Subset off the dq section
  spm_dq <- spm[dq_idx:nrow(spm),]
  # Make a list of the Measure sections
  spm <- purrr::map(seq_vec(m_idx), ~spm[.x, ])
  names(spm) <- purrr::map_chr(spm, ~.x[[1]][1])
  spm <- purrr::map(spm, ~{
    out <- .x[-1, ]
    # make two vectors denoting the begin & end index of the sections
    .m_idx <- stringr::str_which(out[[1]], UU::regex_or(c("^Metric", "^Measure")))
    .e_idx <- which(is.na(out[[1]])) - 1
    if (length(.m_idx) != length(.e_idx))
      .e_idx <- c(.e_idx, nrow(out))
    # if it has subsections
    if (length(.m_idx) != 0) {
      # Subset
      out <- purrr::map2(.m_idx, .e_idx, ~out[.x:.y, ]) |>
        # Set the names of each subsection
        {\(x) {rlang::set_names(x, purrr::map_chr(x, ~.x[[1]][1]))}}()
      out <- out |>
        purrr::map(~{
          # make column headers
          rlang::set_names(.x[-1,], make.names(.x[1,,drop = TRUE], unique = TRUE)) |>
            # remove all na cols
            dplyr::select(where(~!all(is.na(.x)))) |>
            # make numeric
            dplyr::mutate(dplyr::across(- 1, as.numeric))
        })

    } else {
      # Same as above for Measures not split into subsections
      out <- stats::setNames(out[-1,], make.names(out[1,,drop = TRUE], unique = TRUE)) |>
        dplyr::select(where(~!all(is.na(.x)))) |>
        dplyr::mutate(dplyr::across(- 1, as.numeric))
    }

    out
  })
  spm$data_quality <- spm_dq
  spm
}
