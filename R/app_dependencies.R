#' @include aaa_imports.R
app_deps <- list(
  ## to Rm:
  Rminor = c(
    "APs",
    "bos_counties",
    "BoS_PIT",
    "rm_dates",
    "Client",
    "covid19",
    "covid19_priority_plot",
    "covid19_status_plot",
    "current_tay_hohs",
    # "FileEnd",
    "goals",
    "Mah_PIT",
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

  RminorElevated = c(
    "active_list",
    "aps_no_referrals",
    "Beds",
    "rm_dates",
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
    "HUD_specs",
    "living_situation",
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
    # QPR_client_counts
    "validation",
    # Veterans
    "veteran_active_list",
    "permanently_housed_vets",
    "entered_past_90_vets",
    "new_gpd_vets"

  )
)
missing_args <-
  function(calling_function = rlang::caller_fn(2),
           include_null = TRUE,
           exclude_defaults = TRUE)
  {
    all_args <- formals(calling_function)

    arg_names <- names(all_args)
    matched_call <- match.call(calling_function,
                               sys.call(1),
                               expand.dots = FALSE)

    passed_args <- names(as.list(matched_call)[-1])
    out <- setdiff(arg_names, passed_args)
    if (include_null)
      out <-
      c(out, setdiff(names(purrr::keep(
        all_args, ~ is.null(.x)
      )), passed_args))
    if (exclude_defaults)
      out <-
      setdiff(out, names(purrr::keep(
        all_args, ~ !is.null(.x) & !rlang::is_missing(.x)
      )))
    out
  }

missing_fmls <- function(ma = missing_args()) {
  ma[!ma %in% c("app_env", "clarity_api")]
}

fun_insert <-
  function(f,
           expr = app_env$merge_deps_to_env(get_fmls(rlang::current_fn())),
           after = 1) {
    body(f) <-
      as.call(append(as.list(body(f)), rlang::enexpr(expr), after = after))
    assignInNamespace(rlang::expr_text(rlang::enexpr(f)),
                      f,
                      utils::packageName(rlang::fn_env(f)))
  }


# app_env ----
# Thu Aug 05 10:07:19 2021

#' @title Easily extract dependencies and pass them between functions
#' @description Save dependencies for future functions in an environment and put app dependencies into lists
#' @param app_deps \code{(named list)} with items for each app that contain character vectors of the objects each app depends on to be saved.
#' @export

app_env <- R6::R6Class(
  "app_env",
  public = rlang::list2(
    #' @description Pass all dependencies saved from previous functions to an environment for use
    #' @param nms \code{(character)} of the names of the dependencies to load into the `env`. **Default** load all previously stored objects.
    #' @param env \code{(environment)} to pass dependencies to. **Default** the calling environment
    merge_deps_to_env = function(...,
                                 env = rlang::caller_env(),
                                 as_list = FALSE) {
      .dots <- rlang::dots_list(..., .named = TRUE)
      nms <- purrr::flatten_chr(unname(.dots))
      # If called with no arguments
      if (!UU::is_legit(nms) && !UU::is_legit(names(.dots)))
        nms <- private$work_deps
      .missing_nms <- !nms %in% ls(self$.__enclos_env__)
      if (any(.missing_nms))
        rlang::abort(paste0(
          paste0(nms[.missing_nms], collapse = ", "),
          " not found in working environment. Has it been saved?"
        ))
      if (!as_list) {
        rlang::env_bind(env, !!!rlang::env_get_list(self$.__enclos_env__, nms))
      } else {
        return(rlang::env_get_list(self$.__enclos_env__, nms))
      }

    },
    #' @description Gather the objects passed to \code{app_env}s internal environment to be passed to subsequent functions. Save app dependencies into a list.
    #' @param ... \code{(objects)} Dependencies for subsequent functions, passed as objects and not character vector of names. Use \code{"everything"} to capture all objects from the parent environment.
    gather_deps = function(...,
                           app_deps = self$app_deps,
                           env = rlang::caller_env(),
                           .args = names(rlang::fn_fmls(rlang::call_fn(rlang::call_standardise(
                             match.call(call = sys.call(1))
                           ))))) {
      # must be forced to get the calling environment where the user called it since env isn't used until inside the purrr::map call
      force(env)

      .work_deps <- rlang::dots_list(..., .named = TRUE) |>
        {
          \(x) {
            rlang::set_names(x, stringr::str_remove_all(names(x), "\""))
          }
        }()
      if (length(.work_deps) == 1 &&
          identical(.work_deps[[1]], "everything")) {
        .all_objs <- ls(env, all.names = TRUE)
        .args <- try(force(.args), silent = TRUE)
        if (UU::is_legit(.args))
          .all_objs <- stringr::str_subset(
            .all_objs,
            negate = TRUE,
            pattern = paste0("(?:^", .args, "$)", collapse = "|")
          )

        .work_deps <- rlang::env_get_list(env, .all_objs)

      } else if (length(.work_deps) == 1 && is.character(.work_deps[[1]]) && any(.work_deps[[1]] %in% ls(env))) {
        .work_deps <- rlang::env_get_list(env, .work_deps[[1]])

      }
      .new_wdeps <- names(.work_deps)
      private$work_deps <-
        append(private$work_deps, .new_wdeps)  |>
        {
          \(x) {
            x[!duplicated(x)]
          }
        }()

      cli::cli({
        cli::cli_h2("Global")
        cli::cli_alert_success(paste0("dependencies saved: ", paste0(.new_wdeps, collapse = ", ")))
      })
      #TODO need handling for unnamed
      rlang::env_bind(self$.__enclos_env__, !!!.work_deps)


      purrr::iwalk(app_deps, ~ {
        .deps <-
          purrr::compact(rlang::env_get_list(env, .x, default = NULL))
        if (UU::is_legit(.deps)) {
          self$app_objs[[.y]] <<- purrr::list_modify(self$app_objs[[.y]],!!!.deps)
          cli::cli({
            cli::col_blue(cli::cli_h2(.y))
            cli::cli_alert_success(paste0(" dependencies saved: ", paste0(names(.deps), collapse = ", ")))
          })
        }
      })
      invisible(self)
    },
    #' @description Write app dependencies to disk
    #' @param deps \code{(character)} with names of app dependencies.
    #' @param path \code{(character)} of directory to write app dependencies to
    #' @param overwrite \code{(logical)} Whether to overwrite existing files.
    write_app_deps = function (objs,
                               deps,
                               path = file.path("data", "db", "RminorElevated"),
                               overwrite = TRUE)
    {
      # Dir check
      if (!dir.exists(path))
        UU::mkpath(path)

      .nms <- names(objs)
      .missing <- setdiff(deps, .nms)
        if (UU::is_legit(.missing)) {
          rlang::warn(paste0(
            "The following objects are missing from the app dependencies and will not be written to disk: ",
            paste0(.missing, collapse = ", ")))

        }


      purrr::iwalk(objs, ~{
        if (overwrite) {
          fp <- file.path(path, paste0(.y, UU::object_ext(.x)))
          rlang::exec(UU::object_fn(.x), .x, fp)
          if (file.info(fp)$mtime > Sys.Date())
            cli::cli_alert_success(fp, " saved.")
        }
      })



    },
    #' @field \code{(list)} with all app dependencies as objects
    app_objs = list(),
    #' @field \code{(list)} with all app dependencies as character vectors
    app_deps = c(),
    dropbox_upload = function(folder = file.path("data","db","RminorElevated"), db_folder = "RminorElevated") {
      files <- list.files(folder, full.names = TRUE)
      purrr::walk(files, ~{
        message("Uploading ",.x)
        rdrop2::drop_upload(.x, file.path(db_folder, basename(.x)))
      })
    },
    dropbox_auth = function(db_auth_token = "~/R/auth_tokens/db_token.rds") {
      # Dropbox Auth
      if (!file.exists(db_auth_token)) {
        token <- rdrop2::drop_auth(key = Sys.getenv("db_key"),
                                   secret = Sys.getenv("db_secret"),
                                   cache = FALSE)
        saveRDS(token, db_auth_token)
      } else {
        rdrop2::drop_auth(rdstoken = db_auth_token)
      }
    }
    #' @description Instantiate with default app dependencies to be collected (if they exist) each time \code{\$gather_deps} is called
    initialize = function(app_deps) {
      if (missing(app_deps))
        app_deps <- Rm_data:::app_deps
      self$app_deps <- app_deps
      self$app_objs <- purrr::map(app_deps, ~list())
    }
  ),
  private = list(#' @field Save a vector of the names of working dependencies that have been saved for future reference when \code{\$merge_deps_to_env} is called.
    work_deps = c()),
  lock_objects = FALSE
)

is_app_env <- function(x)
  inherits(x, "app_env")

dependencies <- list()



