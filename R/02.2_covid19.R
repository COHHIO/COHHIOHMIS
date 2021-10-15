covid19 <- function(
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  # COVID-19 ----------------------------------------------------------------

  covid19 <- clarity_api$`HUD Extras`$Client_COVID_extras() |>
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(
      c(
        dplyr::matches("^C19Tested$"),
        dplyr::starts_with("HR"),
        dplyr::starts_with("Symptom2")
      )
    ), .f = replace_yes_no))

  # COVID-19 plots for Rm ---------------------------------------------------

  get_res_prior <- validation %>%
    dplyr::select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
    dplyr::group_by(PersonalID) %>%
    dplyr::arrange(dplyr::desc(EntryDate)) %>%
    dplyr::slice(1L)

  covid19_plot <- covid19 %>%
    dplyr::left_join(get_res_prior, by = "PersonalID") %>%
    dplyr::filter(C19AssessmentDate >= lubridate::mdy("04012020") &
                    C19AssessmentDate <= lubridate::today())

  priority <- covid19_plot %>%
    dplyr::mutate(
      Priority = dplyr::case_when(
        # if tested positive
        (
          C19Tested == 1 &
            C19TestResults == "Positive" &
            C19TestDate > C19AssessmentDate - lubridate::days(14) &
            !is.na(C19TestDate)
        ) |
          # if under investigation
          (
            C19UnderInvestigation == 1 &
              C19InvestigationDate > C19AssessmentDate - lubridate::days(14)
          ) |
          # contact with COVID-19
          (
            C19ContactWithConfirmed == 1 &
              (
                C19ContactWithConfirmedDate >
                  C19AssessmentDate - lubridate::days(14) |
                  is.na(C19ContactWithConfirmedDate)
              )
            # compares contact date to the assessment date too since we want to
            # see severity at the time of assessment
          ) |
          (
            C19ContactWithIll == 1 &
              (
                C19ContactWithIllDate >
                  C19AssessmentDate - lubridate::days(14) |
                  is.na(C19ContactWithIllDate)
              )
          ) |
          # if the client came from jail or nursing home
          (
            LivingSituation %in% c(7, 25) &
              EntryDate > C19AssessmentDate - lubridate::days(14) &
              EntryDate <= C19AssessmentDate
          ) |
          # if the client has any symptoms at all
          (
            Symptom1BreathingDifficult +
              Symptom1Cough +
              Symptom2Chills +
              Symptom2SoreThroat +
              Symptom2Fever +
              Symptom2Headache +
              Symptom2LostTasteSmell +
              Symptom2MusclePain +
              Symptom2Congestion +
              Symptom2Nausea +
              Symptom2Diarrhea +
              Symptom2Weak
          ) > 0 ~ "Needs Isolation/Quarantine",
        # if the client has any risks at all
        (
          HRHistoryOfRespiratoryIllness +
            HRChronicIllness +
            HROver65 +
            HRKidneyDisease +
            HRImmunocompromised +
            HRSmoke > 0
        )  ~ "Has Health Risk(s)",
        TRUE ~ "No Known Risks or Exposure"
        # everyone else lands here ^
        # in the report, there will be a third level: "Not Assessed Recently"
      ),
      Priority = factor(Priority, levels = c("Needs Isolation/Quarantine",
                                             "Has Health Risk(s)",
                                             "No Known Risks or Exposure")),
      Month = paste0(lubridate::year(C19AssessmentDate),
                     stringr::str_pad(
                       lubridate::month(C19AssessmentDate),
                       width = 2,
                       pad = "0"
                     )),
      Month = as.numeric(factor(Month)),
      MonthOf = format.Date(C19AssessmentDate, "%b %Y")
    ) %>%
    dplyr::filter(lubridate::month(C19AssessmentDate) != lubridate::month(lubridate::today()))

  priority_plot <- priority %>%
    dplyr::select(PersonalID, MonthOf, Month, Priority) %>%
    dplyr::group_by(MonthOf, Month, Priority) %>%
    dplyr::summarise(Clients = dplyr::n()) %>%
    dplyr::arrange(Month)

  covid19_priority_plot <- priority_plot %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(MonthOf, Month), y = Clients,
                                 fill = Priority, label = Clients)) +
    ggplot2::scale_fill_brewer(palette = "GnBu", direction = -1) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Month of", y = "Clients Assessed") +
    ggplot2::theme(legend.title=ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = ggplot2::element_text(size = 11),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 11))

  rm(priority, priority_plot)

  # COVID Status plot -------------------------------------------------------

  covid19_status <- covid19_plot %>%
    dplyr::mutate(
      COVID19Status = dplyr::case_when(
        Tested == 1 &
          TestResults == "Positive" &
          C19TestDate > C19AssessmentDate - lubridate::days(14) &
          !is.na(C19TestDate) ~ "Positive",
        # testing positive in the 14 days prior to assessment is the only way to
        # land in this bucket
        (
          C19ContactWithConfirmed == 1 &
            (
              C19ContactWithConfirmedDate >
                C19AssessmentDate - lubridate::days(14) |
                is.na(C19ContactWithConfirmedDate)
            )
          # compares contact date to date of the assessment
        ) |
          (
            C19ContactWithIll == 1 &
              (
                C19ContactWithIllDate >
                  C19AssessmentDate - lubridate::days(14) |
                  is.na(C19ContactWithIllDate)
              )
          ) |
          (
            Symptom1BreathingDifficult +
              Symptom1Cough +
              Symptom2Chills +
              Symptom2SoreThroat +
              Symptom2Fever +
              Symptom2Headache +
              Symptom2LostTasteSmell +
              Symptom2MusclePain +
              Symptom2Congestion +
              Symptom2Nausea +
              Symptom2Diarrhea +
              Symptom2Weak
          ) > 0
        |
          (
            C19UnderInvestigation == 1 &
              C19InvestigationDate > C19AssessmentDate - lubridate::days(14)
          ) ~
          "May Have COVID-19",
        # being Under Investigation (past 14 days), any Symptom, or any Contact
        # in the 14 days prior to the assessment date will land you here ^
        TRUE ~ "No Current Indications"
        # everyone else lands here ^
      ),
      COVID19Status = factor(
        COVID19Status,
        levels = c("No Current Indications",
                   "May Have COVID-19",
                   "Positive")
      ),
      Month = paste0(lubridate::year(C19AssessmentDate),
                     stringr::str_pad(
                       lubridate::month(C19AssessmentDate),
                       width = 2,
                       pad = "0"
                     )),
      Month = as.numeric(factor(Month)),
      MonthOf = format.Date(C19AssessmentDate, "%b %Y")
    ) %>%
    dplyr::filter(lubridate::month(C19AssessmentDate) != lubridate::month(lubridate::today()))

  covid19_status_plot <- covid19_status %>%
    dplyr::select(PersonalID, MonthOf, Month, COVID19Status) %>%
    dplyr::group_by(MonthOf, Month, COVID19Status) %>%
    dplyr::summarise(Clients = dplyr::n()) %>%
    dplyr::arrange(Month) %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(MonthOf, Month), y = Clients,
                                 fill = COVID19Status)) +
    ggplot2::geom_bar(stat = "identity",
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = c("#e0ecf4", "#9ebcda", "#8856a7")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Month of", y = "Clients Assessed") +
    ggplot2::theme(legend.title=ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = ggplot2::element_text(size = 11),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust=1, size = 11))

  rm(covid19_status, covid19_plot)

}
