
#  Assessment_Custom ----
# Thu Sep 30 15:04:13 2021
Assessment_Custom <- list()
Assessment_Custom$box <- clarity.looker::hud_load("Assessment_Custom_combined")
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
common_nms <- do.call(UU::common_names, Assessment_Custom[1:2]) |> stringr::str_subset("^c\\_")

type_differences <- purrr::map2_lgl(Assessment_Custom$box[common_nms], Assessment_Custom$api[common_nms], ~{
  !inherits(.x, class(.y))
})
num_lgl_nms <- common_nms[type_differences][purrr::map_lgl(Assessment_Custom$box[common_nms[type_differences]], ~{all(unique(.x) %in% c(0, 1, NA))})] |>
  stringr::str_subset("^assessment", negate = TRUE)

to_chr <- setdiff(common_nms[type_differences], c(num_lgl_nms, c("c_covid19_vaccine_documentation", "c_covid19_vaccine_manufacturer",
                                                                 "c_covid19_test_results", "c_offer_type", "c_vispdat_type"))) |>
  stringr::str_subset("^assessment|date$", negate = TRUE)

# Match classes
Assessment_Custom$box <- dplyr::mutate(Assessment_Custom$box,
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
Assessment_Custom$api <- Assessment_Custom$api |> dplyr::filter(UserUpdating == "internal_api")
rstudioapi::jobRunScript("assessment_custom_data_match.R", workingDir = getwd(), importEnv = TRUE, exportEnv = "data_match")

x <- purrr::keep(Assessment_Custom$data_match, ~!all(names(.x) %in% c("source", "PersonalID", "AssessmentID", "AssessmentDate", "c_vispdat_score", "UpdatedDate", "UserUpdating")))

# 0 All set!
