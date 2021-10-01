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
PersonalID = "cohhio_enrollments_personalid_excluded"), hud_load)

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

# Services_Custom ----
# Thu Sep 30 15:03:07 2021
FundingSources <- readxl::read_xlsx("data/cohhio_connection_model agencies 2021-09-27T1409.xlsx")
# Deleted funding sources
FundingSources[1:97,]

#  Assessment_Custom ----
# Thu Sep 30 15:04:13 2021
Assessment_Custom <- list()
Assessment_Custom$box <- hud_load("Assessment_Custom_combined")
Assessment_Custom$api = cl_api$api$runLook(
      69576,
      resultFormat = "csv",
      queryParams = list(limit = -1, apply_vis = TRUE)
    )
# Returns 0, passes!
Assessment_Custom$join <-
  dplyr::anti_join(
    Assessment_Custom$box,
    Assessment_Custom$api |> dplyr::select(-PersonalID),
    by = c("PersonalID" = "Alias")
  )
# There are far more records in Clarity than box, but no records unaccounted for.
Assessment_Custom$record_match <-
  dplyr::left_join(
    Assessment_Custom$box |>
      dplyr::group_by(PersonalID) |>
      dplyr::summarise(N_box = dplyr::n()),
    Assessment_Custom$api |>
      dplyr::group_by(Alias) |>
      dplyr::summarise(N_api = dplyr::n()),
    by = c("PersonalID" = "Alias")
  ) |>
  dplyr::filter(N_box > N_api)


Assessment_Custom$box <- Assessment_Custom$box |> dplyr::rename(AssessmentDate = "assessment_date")
Assessment_Custom[1:2] <- purrr::map(Assessment_Custom[1:2], ~{
  dplyr::mutate(.x, dplyr::across(dplyr::ends_with("date"), lubridate::as_date))
})
common_nms <- do.call(UU::common_names, Assessment_Custom[1:2]) |> stringr::str_subset("(?:ID$)|(?:AssessmentName)", negate = TRUE)

type_differences <- purrr::map2_lgl(Assessment_Custom$box[common_nms], Assessment_Custom$api[common_nms], ~{
  !inherits(.x, class(.y))
})
num_lgl_nms <- common_nms[type_differences][purrr::map_lgl(Assessment_Custom$box[common_nms[type_differences]], ~{all(unique(.x) %in% c(0, 1, NA))})] |>
  stringr::str_subset("^assessment", negate = TRUE)
to_chr <- setdiff(common_nms[type_differences], num_lgl_nms) |>
  stringr::str_subset("^assessment", negate = TRUE)

# Match classes
Assessment_Custom$box <- dplyr::mutate(Assessment_Custom$box,
              assessment_level = hud.extract::hud_translations$`4.19.4 AssessmentLevel`(assessment_level),
              assessment_type = hud.extract::hud_translations$`4.19.3 AssessmentType`(assessment_type),
              assessment_location = as.character(assessment_location),
              dplyr::across(dplyr::all_of(num_lgl_nms), translate_HUD_yes_no),
              dplyr::across(dplyr::all_of(to_chr), as.character)

  )
# Data match
Assessment_Custom$data_match <- slider::slide(Assessment_Custom$box, ~{
    api <- dplyr::filter(Assessment_Custom$api, Alias == .x$PersonalID)
    differing_data <- purrr::map2_lgl(.x[common_nms], api[common_nms], ~{
      length(unique(na.omit(.x))) != length(unique(na.omit(.y)))
    })
    if (!all(differing_data))
      dplyr::bind_rows(box = .x,
                       api = api[common_nms][differing_data])
  })

