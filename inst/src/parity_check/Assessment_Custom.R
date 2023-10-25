
#  assessment_custom ----
# Thu Sep 30 15:04:13 2021
assessment_custom <- list()
assessment_custom$box <- clarity.looker::hud_load("Assessment_Custom_combined")
assessment_custom$api <- cl_api$api$runLook(
  69576,
  resultFormat = "csv",
  queryParams = list(limit = -1, apply_vis = TRUE)
)
# Returns 0, passes!
assessment_custom$join <-
  dplyr::anti_join(
    assessment_custom$box,
    assessment_custom$api |> dplyr::select(-PersonalID),
    by = c("PersonalID" = "Alias")
  )
# There are far more records in Clarity than box, but no records unaccounted for.
assessment_custom$record_match <-
  dplyr::left_join(
    assessment_custom$box |>
      dplyr::group_by(PersonalID) |>
      dplyr::summarise(N_box = dplyr::n()),
    assessment_custom$api |>
      dplyr::group_by(Alias) |>
      dplyr::summarise(N_api = dplyr::n()),
    by = c("PersonalID" = "Alias")
  ) |>
  dplyr::filter(N_box > N_api)


assessment_custom$box <- assessment_custom$box |> dplyr::rename(AssessmentDate = "assessment_date")
assessment_custom[1:2] <- purrr::map(assessment_custom[1:2], ~{
  dplyr::mutate(.x, dplyr::across(dplyr::ends_with("date"), lubridate::as_date))
})
common_nms <- do.call(UU::common_names, assessment_custom[1:2]) |> stringr::str_subset("^c\\_")

type_differences <- purrr::map2_lgl(assessment_custom$box[common_nms], assessment_custom$api[common_nms], ~{
  !inherits(.x, class(.y))
})
num_lgl_nms <- common_nms[type_differences][purrr::map_lgl(assessment_custom$box[common_nms[type_differences]], ~{
  all(unique(.x) %in% c(0, 1, NA))})] |>
  stringr::str_subset("^assessment", negate = TRUE)

to_chr <- setdiff(common_nms[type_differences], c(num_lgl_nms, c("c_covid19_vaccine_documentation", "c_covid19_vaccine_manufacturer",
                                                                 "c_covid19_test_results", "c_offer_type", "c_vispdat_type"))) |>
  stringr::str_subset("^assessment|date$", negate = TRUE)

# Match classes
assessment_custom$box <- dplyr::mutate(assessment_custom$box,
                                       assessment_level = hud.extract::hud_translations$`4.19.4 AssessmentLevel`(assessment_level),
                                       assessment_type = hud.extract::hud_translations$`4.19.3 AssessmentType`(assessment_type),
                                       assessment_location = as.character(assessment_location),
                                       c_vispdat_score = tidyr::replace_na(0),
                                       c_covid19_vaccine_documentation = sp2cl_translations$c_covid19_vaccine_documentation(c_covid19_vaccine_documentation),
                                       c_covid19_vaccine_manufacturer = sp2cl_translations$c_covid19_vaccine_manufacturer(c_covid19_vaccine_manufacturer),
                                       c_covid19_test_results = sp2cl_translations$c_covid19_test_results(c_covid19_test_results),
                                       c_offer_type = sp2cl_translations$c_offer_type(c_offer_type),
                                       c_vispdat_type = sp2cl_translations$c_vispdat_type(c_vispdat_type),
                                       dplyr::across(dplyr::all_of(num_lgl_nms), translate_HUD_yes_no),
                                       dplyr::across(dplyr::all_of(to_chr), as.character)

)

# Filter for API
assessment_custom$api <- assessment_custom$api |> dplyr::filter(UserUpdating == "internal_api")
rstudioapi::jobRunScript("assessment_custom_data_match.R", workingDir = getwd(), importEnv = TRUE, exportEnv = "data_match")

x <- purrr::keep(assessment_custom$data_match, ~!all(names(.x) %in% c("source", "PersonalID", "AssessmentID", "AssessmentDate", "c_vispdat_score", "UpdatedDate", "UserUpdating")))

# 0 All set!
