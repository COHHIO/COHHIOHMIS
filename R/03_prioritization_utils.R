filter_dupe_soft <- function(.data, ..., key) {
  .key <- rlang::enexpr(key)
  out <- .data
  x <- janitor::get_dupes(.data, !!.key) |>
    dplyr::arrange(PersonalID)

  clients <- dplyr::pull(x, !!.key) |> unique()
  .exprs <- rlang::enexprs(...)
  to_add <- list()
  browser()
  for (ex in .exprs) {
    new <- dplyr::filter(x, !!ex)

    new_n <- dplyr::summarise(dplyr::group_by(new, !!.key), n = dplyr::n())
    .to_merge <- dplyr::filter(new_n, n == 1) |> dplyr::pull(!!.key)

    # if some were reduced but not to one
    .reduced <- dplyr::left_join(new_n,
                                 dplyr::summarise(dplyr::group_by(x, !!.key), n = dplyr::n()), by = rlang::expr_deparse(.key), suffix = c("_new", "_old")) |>
      dplyr::filter(n_new < n_old & n_new > 1) |>
      dplyr::pull(!!.key)

    if (UU::is_legit(.to_merge)) {
      # remove rows where key is reduced to one, bind the deduplicated rows
      to_add <- append(to_add, list(dplyr::select(new, -dupe_count) |>
                                      dplyr::filter(!!.key %in% .to_merge)))
      # filter to_merge from dupes
      x <- dplyr::filter(x, !((!!.key) %in% .to_merge))
    }

    if (UU::is_legit(.reduced)) {
      # filter reduced from dupes
      x <- dplyr::filter(x, !((!!.key) %in% .reduced ) # is not one that was reduced
                         | (!!.key %in% .reduced & !!ex) # or matched the filter
      )

    }
  }
  to_add <- dplyr::bind_rows(to_add)
  out <- dplyr::filter(out, !PersonalID %in% to_add$PersonalID) |>
    dplyr::bind_rows(to_add)
  if (anyDuplicated(out[[.key]])) {

    rlang::warn("Duplicates still exist.")
  }
  out
}


min_na <- function(...) {
  x <- data.frame(...)
  if (any(!purrr::map_lgl(x$ExpectedPHDate, is.na))) {
    idx <- which.max(x$ExpectedPHDate)
  } else {
    idx <- which.min(apply(x, 1, rlang::as_function(~sum(is.na(.x)))))
  }
  x[idx,]
}
