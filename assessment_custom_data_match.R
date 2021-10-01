Assessment_Custom$data_match <- slider::slide(Assessment_Custom$box, ~{
  api <- dplyr::filter(Assessment_Custom$api, Alias == .x$PersonalID)
  differing_data <- purrr::map2_lgl(.x[common_nms], api[common_nms], ~{
    length(unique(na.omit(.x))) != length(unique(na.omit(.y)))
  })
  box_differing <- .x[c("PersonalID", "AssessmentID", "AssessmentDate", common_nms)][c(T,T,T, differing_data)] |> tibble::add_column(UpdatedDate = lubridate::NA_Date_)
  api_differing <- api[c("PersonalID", "AssessmentID", "AssessmentDate", "UpdatedDate", common_nms)][c(T,T,T,T, differing_data)]


  if (!all(!differing_data))
    dplyr::bind_rows(box = box_differing,
                     api = api_differing, .id = "source")
})
