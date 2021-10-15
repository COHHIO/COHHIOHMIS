hoh_count <- function(served) {
  count_colname <- stringr::str_remove(rlang::expr_deparse(rlang::enexpr(served)), "co\\_")
  served |>
    dplyr::distinct(PersonalID, ProjectName) |>
    dplyr::group_by(ProjectName) |>
    dplyr::summarise(!!count_colname := dplyr::n())
}
