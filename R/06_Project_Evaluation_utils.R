merge_projects <- function(x, to_merge) {
  var <- rlang::enexpr(x)
  for (i in seq_along(to_merge)) {
    idx <- which(x %in% to_merge[[i]][[var]])
    if (UU::is_legit(idx))
      x[idx] <- switch(rlang::expr_deparse(var),
                       ProjectName = names(to_merge)[i],
                       # the first projectID multipled by 1000 (current ProjectIDs are just over 1100, so this should be fine given the lowest projectID is 2)
                       ProjectID = as.character(as.numeric(to_merge[[i]][[var]][1]) * 1000))
  }
  x
}

peval_filter_select <- function(x,
                               vars,
                               ...,
                               stayed = FALSE,
                               served = FALSE,
                               exited = FALSE,
                               entered = FALSE,
                               arrange = TRUE,
                               distinct = TRUE,
                               rm_dates,
                               start = rm_dates$hc$project_eval_start,
                               end = rm_dates$hc$project_eval_end,
                               app_env = get_app_env(e = rlang::caller_env())
) {

  addtl_filters <- rlang::enexprs(...)
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  out <- x

  if (served)
    out <- out |> HMIS::served_between(start, end)
  if (stayed)
    out <- out |> HMIS::stayed_between(start, end)
  if (exited)
    out <- out |> HMIS::exited_between(start, end)
  if (entered)
    out <- out |> HMIS::entered_between(start, end)

  out <- out |>
    dplyr::select("PersonalID", "ProjectID", "EnrollmentID") %>%
    dplyr::inner_join(pe_coc_funded, by = "ProjectID") %>%
    dplyr::left_join(
      Enrollment_extra_Client_Exit_HH_CL_AaE %>%
        dplyr::select(- tidyselect::all_of(c("UserID",
                                             "DateCreated",
                                             "DateUpdated",
                                             "DateDeleted",
                                             "ExportID"))),
      by = c(
        "PersonalID",
        "EnrollmentID",
        "ProjectID",
        "ProjectType",
        "ProjectName"
      )
    )

  if (UU::is_legit(addtl_filters))
    out <- dplyr::filter(out, !!!addtl_filters)

  out <- dplyr::select(out, dplyr::all_of(vars))

  if (arrange)
    out <- dplyr::arrange(out, PersonalID, AltProjectID, dplyr::desc(EntryDate))
  if (distinct)
    out <- dplyr::distinct(out, PersonalID, AltProjectName, .keep_all = TRUE)
  # no dupes w/in a project
  out
}

peval_summary <- function(x, nm, app_env = get_app_env(e = rlang::caller_env())) {
  app_env$merge_deps_to_env("pe_coc_funded")
  if (missing(nm))
    nm <- rlang::expr_deparse(rlang::enexpr(x))
  nm <- nm |>
      stringr::str_extract("(?<=summary\\_)[\\w\\_]+")|>
      snakecase::to_upper_camel_case() |>
      stringr::str_replace("Hoh", "HoH") |>
      rlang::sym()
  out <- x

  dplyr::group_by(out, AltProjectID) %>%
    dplyr::summarise(!!nm := dplyr::n(), .groups = "drop") |>
    dplyr::right_join(unique(pe_coc_funded["AltProjectID"]), by = "AltProjectID") %>%
    tidyr::replace_na(rlang::list2(!!nm := 0L))
}
