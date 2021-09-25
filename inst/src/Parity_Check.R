client_mapping <- cl_api$api$runLook(
  69310,
  resultFormat = "csv",
  queryParams = list(limit = -1, apply_vis = TRUE)
)
Enrollment_Custom <-
  list(
    # Get look 69207
    api = cl_api$api$runLook(
      69207,
      resultFormat = "csv",
      queryParams = list(limit = -1, apply_vis = TRUE)
    ))
# Load the file from box
Enrollment_Custom$box = hud_load("Enrollment_Custom", "data") |>
  dplyr::mutate(PersonalID = as.character(PersonalID)) |>
  dplyr::filter(DataCollectionStage == 1)
# # Anti-join to find missing records
Enrollment_Custom$join <-
  dplyr::anti_join(
    Enrollment_Custom$box,
    Enrollment_Custom$api,
    by = c(PersonalID = "Alias")
  )

records_per_id <- function(x, ID) {
  x |>
    dplyr::group_by(!!rlang::enexpr(ID)) |>
    dplyr::summarise(Records = dplyr::n())
}
api <- records_per_id(Enrollment_Custom$api, Alias)
box <- records_per_id(Enrollment_Custom$box, PersonalID)

joined <- dplyr::left_join(
  box,
  api,
  by = c(PersonalID = "Alias"),
  suffix = c("_box", "_api")
)


missing_records <- joined |>
  dplyr::filter(Records_box != Records_api)



readr::write_csv(Enrollment_Custom$join, "Enrollment_Custom_missing_2021-09-23.csv")
readr::write_csv(missing_records, "Enrollment_Custom_record_discrepancies_2021-09-24.csv")
