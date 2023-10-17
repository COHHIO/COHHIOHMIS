assessment_custom$data_match <- purrr::imap(rlang::set_names(unique(assessment_custom$box$PersonalID)), ~{
  message(.y)
  api <- dplyr::filter(assessment_custom$api, Alias == .x)
  box <- dplyr::filter(assessment_custom$box, PersonalID == .x)
  differing_data <- purrr::map2_lgl(box[common_nms], api[common_nms], ~{
    !all(unique(na.omit(.x)) %in% unique(na.omit(.y)) &
           unique(na.omit(.y)) %in% unique(na.omit(.x)))
  })
  box_differing <- box[c("PersonalID", "AssessmentID", "AssessmentDate", common_nms)][c(TRUE, TRUE, TRUE, differing_data)] |> tibble::add_column(UpdatedDate = NA, UserUpdating = NA)
  api_differing <- api[c("PersonalID", "AssessmentID", "AssessmentDate", "UpdatedDate","UserUpdating", common_nms)][c(TRUE, TRUE, TRUE, TRUE, TRUE, differing_data)]


  if (!all(!differing_data))
    out <- dplyr::bind_rows(box = box_differing,
                     api = api_differing, .id = "source")
  else
    out <- NULL
  out
}) |> purrr::compact()
