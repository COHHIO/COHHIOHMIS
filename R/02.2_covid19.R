

#' @title COVID-19 Data Objects
#'
#' @inherit R6Classes params
#' @include 02.2_covid19_utils.R
#' @export

covid19 <- function(
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  # COVID-19 ----------------------------------------------------------------

  covid19 <- clarity_api$`HUD Extras`$Client_COVID_extras() |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c(dplyr::matches("^C19Tested$"),
                              dplyr::matches("^C19UnderInvestigation$"),
                              dplyr::matches("^C19ContactWithConfirmed$"),
                              dplyr::matches("^C19ContactWithIll$"),
                              dplyr::starts_with("HR"),
                            dplyr::starts_with("Symptom"))),
      .f = replace_yes_no
    ))


  get_res_prior <- validation |>
    dplyr::select(PersonalID, EntryDate, ExitDate, LivingSituation) |>
    dplyr::group_by(PersonalID) |>
    dplyr::arrange(dplyr::desc(EntryDate)) |>
    dplyr::slice(1L)

  c19priority <- covid19 |>
    dplyr::left_join(get_res_prior, by = "PersonalID") |>
    dplyr::filter(C19AssessmentDate >= lubridate::mdy("04012020") &
                    C19AssessmentDate <= lubridate::today()) |>
    dplyr::mutate(
      # if tested positive
      C19D_TestPos = (
        C19Tested == 1 &
          C19TestResults == "Positive" &
          C19TestDate > C19AssessmentDate - lubridate::days(14) &
          !is.na(C19TestDate)
      ),
      # if under investigation
      C19D_UnderInv = (
        C19UnderInvestigation == 1 &
          C19InvestigationDate > C19AssessmentDate - lubridate::days(14)
      ),
      # contact with COVID-19
      C19D_ContactC19 = C19ContactWithConfirmed == 1 &
        (
          # compares contact date to the assessment date too since we want to see severity at the time of assessment
          C19ContactWithConfirmedDate >
            C19AssessmentDate - lubridate::days(14)
          | is.na(C19ContactWithConfirmedDate)
        ),
      C19D_ContactIll =  C19ContactWithIll == 1 &
        (
          C19ContactWithIllDate >
            C19AssessmentDate - lubridate::days(14)
          | is.na(C19ContactWithIllDate)
        ),
      # if the client came from jail or nursing home
      C19D_HRLivingSituation = LivingSituation %in% c(7, 25) &
        EntryDate > C19AssessmentDate - lubridate::days(14) &
        EntryDate <= C19AssessmentDate,
      # if the client has any symptoms at all
      C19D_AnySymptom = Symptom1BreathingDifficult |
        Symptom1Cough |
        Symptom2Chills |
        Symptom2SoreThroat |
        Symptom2Fever |
        Symptom2Headache |
        Symptom2LostTasteSmell |
        Symptom2MusclePain |
        Symptom2Congestion |
        Symptom2Nausea |
        Symptom2Diarrhea |
        Symptom2Weak,
      # if the client has any risks at all
      C19D_AnyRisk = HRHistoryOfRespiratoryIllness |
        HRChronicIllness |
        HROver65 |
        HRKidneyDisease |
        HRImmunocompromised |
        HRSmoke,
Priority = dplyr::case_when(

        C19D_TestPos | C19D_UnderInv | C19D_ContactC19 | C19D_ContactIll | C19D_HRLivingSituation | C19D_AnySymptom ~ "Needs Isolation/Quarantine",
        C19D_AnyRisk  ~ "Has Health Risk(s)",
        TRUE ~ "No Known Risks or Exposure"
        # everyone else lands here ^
        # in the report, there will be a third level: "Not Assessed Recently"
      ),
      C19Priority = factor(Priority, levels = c("No Known Risks or Exposure", "Has Health Risk(s)", "Needs Isolation/Quarantine"
      ), ordered = TRUE),
      YM = tsibble::yearmonth(C19AssessmentDate)
    )
rm(get_res_prior)


  covid19_status <- c19priority |>
    dplyr::mutate(
      COVID19Status = factor(dplyr::case_when(
        C19D_TestPos ~ "Positive",
        # testing positive in the 14 days prior to assessment is the only way to
        # land in this bucket
        C19D_ContactC19 |
          # compares contact date to date of the assessment
          C19D_ContactIll |
          C19D_AnySymptom |
          C19D_UnderInv ~
          "May Have COVID-19",
        # being Under Investigation (past 14 days), any Symptom, or any Contact in the 14 days prior to the assessment date will land you here ^
        TRUE ~ "No Current Indications"
        # everyone else lands here ^
      ), c("No Current Indications",
           "May Have COVID-19",
           "Positive"))
    )
  app_env$gather_deps("everything")
}
