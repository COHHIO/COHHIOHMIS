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
Enrollment_Custom$box <- hud_load("Enrollment_Custom", "data") |>
  dplyr::filter(
    DataCollectionStage == 1 &
      !PersonalID %in% c(5L,
                         654L,
                         2807L,
                         3421L,
                         1089,
                         1092,
                         8268) &
      !EnrollmentID %in% c(
        7759L,
        451865L,
        332383L,
        362590L,
        392831L,
        443788,
        447065,
        277948,
        427599,
        317126

      )
  )
# Further exclusions
Enrollment_Custom$exclusions <- purrr::map(c(EnrollmentID = "cohhio_enrollments_enrollmentid_excluded",
                                             PersonalID = "cohhio_enrollments_personalid_excluded"), clarity.looker::hud_load)

Enrollment_Custom$exclusions <- Enrollment_Custom$exclusions |> purrr::map(~rlang::set_names(.x, c("EnrollmentCustomID", "PersonalID", "EnrollmentID")))

Enrollment_Custom$exclusions <- dplyr::bind_rows(!!!Enrollment_Custom$exclusions) |>
  dplyr::distinct()
Enrollment_Custom$join <- dplyr::anti_join(
  Enrollment_Custom$box,
  Enrollment_Custom$exclusions
)
# # Anti-join to find missing records
Enrollment_Custom$join <-
  dplyr::anti_join(
    Enrollment_Custom$join,
    Enrollment_Custom$api |> dplyr::select(-PersonalID),
    by = c("PersonalID" = "Alias")
  )

# 0 Records

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
