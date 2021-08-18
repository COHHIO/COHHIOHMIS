app_deps <- list(
  ## to Rm:
  Rm = c(
    "APs",
    "bos_counties",
    "BoS_PIT",
    "calc_2_yrs_prior_end",
    "calc_2_yrs_prior_range",
    "calc_2_yrs_prior_start",
    "calc_data_goes_back_to",
    "calc_full_date_range",
    "Client",
    "covid19",
    "covid19_priority_plot",
    "covid19_status_plot",
    "current_tay_hohs",
    # "FileEnd",
    "goals",
    "hc_began_collecting_covid_data",
    "hc_check_dq_back_to",
    "hc_data_goes_back_to",
    "hc_project_eval_start",
    "hc_project_eval_end",
    "hc_project_eval_docs_due",
    "hc_psh_started_collecting_move_in_date",
    "Mah_PIT",
    "meta_HUDCSV_Export_Date",
    "meta_HUDCSV_Export_End",
    "meta_HUDCSV_Export_Start",
    "meta_Rmisc_last_run_date",
    "note_bed_utilization",
    "note_calculation_utilization",
    "note_qpr_dq_community_need",
    "note_qpr_housed_county",
    "note_qpr_served_county",
    "note_unit_utilization",
    "Organization",
    "pe_validation_summary",
    "project_type",
    "qpr_benefits",
    "qpr_income",
    "qpr_leavers",
    "qpr_rrh_enterers",
    "qpr_spdats_county",
    "qpr_spdats_project",
    "qpr_spending",
    "regions",
    "Scores",
    "Services",
    "spm_Metric_1b",
    "spm_Metric_2",
    "spm_Metric_7",
    "spm_current_end_date",
    "spm_current_start_date",
    "spm_prior_end_date",
    "spm_prior_start_date",
    "summary_pe_final_scoring",
    "Users",
    "utilization",
    "utilization_bed",
    "utilization_unit",
    "validation",
    "veteran_current_in_project"
  ),

  # to Rme

  Rme = c(
    "active_list",
    "aps_no_referrals",
    "Beds",
    "calc_2_yrs_prior_end",
    "calc_2_yrs_prior_range",
    "calc_2_yrs_prior_start",
    "calc_data_goes_back_to",
    "calc_full_date_range",
    "Client",
    "dq_main",
    "dq_past_year",
    "dq_unsheltered",
    "data_APs",
    "dq_overlaps",
    "detail_eligibility",
    "dq_plot_eligibility",
    "dq_plot_errors",
    "dq_plot_hh_errors",
    "dq_plot_hh_no_spdat",
    "dq_plot_outstanding_referrals",
    "dq_plot_projects_errors",
    "dq_plot_projects_warnings",
    "dq_plot_unsheltered_high",
    "dq_plot_warnings",
    "dq_providers",
    "enhanced_yes_no_translator",
    "hc_began_collecting_covid_data",
    "hc_bos_start_vaccine_data",
    "hc_check_dq_back_to",
    "hc_data_goes_back_to",
    "hc_project_eval_start",
    "hc_project_eval_end",
    "hc_project_eval_docs_due",
    "hc_psh_started_collecting_move_in_date",
    "HUD_specs",
    "living_situation",
    "meta_HUDCSV_Export_Date",
    "meta_HUDCSV_Export_End",
    "meta_HUDCSV_Export_Start",
    "meta_Rmisc_last_run_date",
    "Organization",
    # "pe_increase_income",
    "pe_exits_to_ph",
    "pe_homeless_history_index",
    "pe_length_of_stay",
    "pe_benefits_at_exit",
    "pe_entries_no_income",
    "pe_long_term_homeless",
    "pe_res_prior",
    # "pe_own_housing",
    "pe_validation_summary",
    "pe_scored_at_ph_entry",
    "project_type",
    "qpr_income",
    "qpr_benefits",
    "qpr_leavers",
    "qpr_rrh_enterers",
    "qpr_spending",
    "qpr_spdats_project",
    "qpr_spdats_county",
    "Referrals",
    "regions",
    "responsible_providers",
    "Scores",
    "summary_pe_final_scoring",
    "unsheltered_by_month",
    "unsh_overlaps",
    "Users",
    "utilizers_clients",
    "utilization",
    "utilization_bed",
    "vaccine_needs_second_dose",
    "vaccine_status",
    "validation",
    "veteran_active_list"
  )
)

write_deps_to_disk <- rlang::new_function(
  args =
    rlang::pairlist2(
      app_deps = rlang::expr(self$app_deps),
      paths = rlang::expr(purrr::imap_chr(self$app_deps, ~
                                file.path("data", "db", .y))),
      accessor = function(x = as.character(match.call()[[1]]),
                          path = "data/db",
                          ...) {
        .file <-
          list.files(path,
                     pattern = paste0("^", x, "\\."),
                     full.names = TRUE)
        ext <-
          stringr::str_extract(basename(.file), "(?<=\\.)\\w+$")

        load_fun <- file_io_fn(ext = ext)

        load_fun(file)
      }
    ),
  body =
    base::quote({
      .missing <- purrr::map_depth(app_deps, 2, ~ is.null(self$app_objs[[.x]]))
      # Stop if dependencies are missing
      if (any(purrr::flatten_lgl(.missing)))
        stop(paste0(purrr::imap_chr(.missing, ~{
          paste0("The following objects are missing from ", .y,": ", paste0(app_deps[.x], collapse = ", "))
        }), collapse = "\n"))

      purrr::walk(paths, ~{
        if (!dir.exists(.x))
          stop("`path` directory does not exist")
      })


      purrr::walk2(self$app_objs, paths, ~purrr::iwalk(.x, path = .y, ~{
        .fp <- file.path(path, paste0(.y, clarity.looker::file_io_ext(.x)))
        rlang::exec(clarity.looker::file_io_fn(.x), .x, .fp)
        cli::cli_alert_success(.y, " saved to ", .fp)
      }))
      # make the execution environment the baseenv for these functions to avoid inadvertently grabbing variables or functions from the environment inside the app where it will be executed
      rlang::fn_env(accessor) <- rlang::env(baseenv())
      # replace the objects with the accessor function
      accessors <- purrr::map2(self$app_objs, paths, ~purrr::imap(.x, path = .y, ~{
        .fmls <- rlang::fn_fmls(accessor)
        .fmls$path = path
        rlang::fn_fmls(accessor) <- .fmls
        accessor
      }))

      accessors <- purrr::map2(self$app_objs, paths, ~purrr::imap(.x, path = .y, ~{
        .fmls <- rlang::fn_fmls(accessor)
        .fmls$path = path
        rlang::fn_fmls(accessor) <- .fmls
        accessor
      }))

      purrr::pwalk(list(accessors, paths, names(accessors)), ~saveRDS(.x, file.path(.y, paste0(..3, ".rds"))))
    })

)

# app_env ----
# Thu Aug 05 10:07:19 2021

#' @title Easily extract dependencies and pass them between functions
#' @description Save dependencies for future functions in an environment and put app dependencies into lists
#' @param app_deps \code{(named list)} with items for each app that contain character vectors of the objects each app depends on to be saved.
#' @export

app_env <- R6::R6Class(
  "app_env",
  public = list(
    #' @description Gather the objects passed to \code{app_env}s internal environment to be passed to subsequent functions. Save app dependencies into a list.
    #' @param ... \code{(objects)} Dependencies for subsequent functions. Use \code{"everything"} to capture all objects from the parent environment.
    gather_deps = function(...,
                           app_deps = self$app_deps,
                           env = rlang::caller_env(),
                           .args = names(rlang::fn_fmls(rlang::call_fn(rlang::call_standardise(match.call(call = sys.call(1))))))) {
      # must be forced to get the calling environment where the user called it since env isn't used until inside the purrr::map call
      force(env)
      force(.args)
      .work_deps <- rlang::dots_list(..., .named = TRUE)
      if (length(.work_deps) == 1 && .work_deps[1] == "everything")
        .work_deps <- rlang::env_get_list(env, ls(env, all.names = TRUE) |> stringr::str_subset(negate = TRUE, pattern = paste0("(?:",.args,")", collapse = "|")))
      .new_wdeps <- names(.work_deps)
      private$work_deps <-
        append(private$work_deps, .new_wdeps) %>%
        {
          .[!duplicated(.)]
        }
      cli::cli({
        cli::cli_h2("Global")
        cli::cli_alert_success(paste0("dependencies saved: ", paste0(.new_wdeps, collapse = ", ")))
      })
      rlang::env_bind(self$.__enclos_env__,!!!.work_deps)
      self$app_objs <- purrr::imap(app_deps, ~ {
        .deps <-
          purrr::compact(rlang::env_get_list(env, .x, default = NULL))
        if (UU::is_legit(.deps)) {
          app_objs <- purrr::list_modify(self$app_objs, !!!.deps)
          cli::cli({
            cli::col_blue(cli::cli_h2(.y))
            cli::cli_alert_success(paste0(" dependencies saved: ", paste0(names(.deps), collapse = ", ")))
          })
          app_objs
        }
      })
    },
    #' @description Pass all dependencies saved from previous functions to an environment for use
    #' @param nms \code{(character)} of the names of the dependencies to load into the `env`. **Default** load all previously stored objects.
    #' @param env \code{(environment)} to pass dependencies to. **Default** the calling environment
    merge_deps_to_env = function(nms, env = rlang::caller_env()) {
      if (missing(nms))
        nms <- private$work_deps
      rlang::env_bind(env,!!!rlang::env_get_list(self$.__enclos_env__, nms))
    },
    #' @description Write app dependencies to disk
    #' @param app_deps \code{(named list)} with each name corresponding to an app with each item containing a character vector of the app dependencies. **Default** the `app_deps` stored in the public field \code{app_env$app_deps}.
    #' @param paths \code{(named list)} Paths to write app dependencies to, one path for each app in `app_deps` (in the same order).
    #' @param accessor \code{(function)} An accessor function that will be used to read the files from disk in the live app.
    write_app_deps = write_deps_to_disk,
    #' @field \code{(list)} with all app dependencies as objects
    app_objs = list(),
    #' @field \code{(list)} with all app dependencies as character vectors
    app_deps = c(),
    #' @description Instantiate with default app dependencies to be collected (if they exist) each time \code{$gather_deps} is called
    initialize = rlang::new_function(list(app_deps = rlang::expr(!!app_deps)), body = base::quote({
      self$app_deps <- app_deps
    }))
  ),
  private = list(
    #' @field Save a vector of the names of working dependencies that have been saved for future reference when \code{$merge_deps_to_env} is called.
    work_deps = c()
  ),
  lock_objects = FALSE
)

