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
    "project_types",
    "qpr_benefits",
    "qpr_income",
    "qpr_leavers",
    "qpr_rrh_enterers",
    "qpr_spdats_county",
    "qpr_spdats_project",
    "qpr_spending",
    "Regions",
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
  ) |> rlang::set_names(),

  # to Rme

  RminorElevated = c(
    "Beds",
    "guidance",
    # dates
    "rm_dates",
    "Client",
    # cohorts
    "co_clients_served",
    # data_quality
    "dq_aps_no_referrals",
    "dq_past_year",
    "dq_unsheltered",
    "dq_APs",
    "dq_overlaps",
    "eligibility_detail",
    "dq_providers",
    # data_quality_plots
    "dq_summary",
    "dq_plot_aps_referrals",
    "dq_main",
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
    "prioritization",
    "prioritization_colors",
    "project_types",
    "qpr_income",
    "qpr_benefits",
    "qpr_leavers",
    "qpr_rrh_enterers",
    "qpr_spending",
    "qpr_spdats_project",
    "qpr_spdats_county",
    "Referrals",
    "Regions",
    "Scores",
    "summary_pe_final_scoring",
    "unsheltered_by_month",
    "Users",
    "utilization_clients",
    "utilization",
    # "vaccine_needs_second_dose",
    # "vaccine_status",
    # client_counts
    "validation",
    # vet_active
    "veteran_active_list",
    "permanently_housed_vets",
    "entered_past_90_vets",
    "new_gpd_vets"

  ) |> rlang::set_names()
)
missing_args <-
  function(calling_function = rlang::caller_fn(2),
           include_null = TRUE,
           exclude_defaults = TRUE)
  {
    all_args <- formals(calling_function)

    arg_names <- names(all_args)


    .call <- rlang::trace_back(bottom = 3) |>
      {\(x) {x[[1]][[rlang::trace_length(x)]]}}()
    matched_call <- match.call(calling_function,
                               .call,
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
    #' @field dependencies Dependency environment
    dependencies = new.env(parent = .GlobalEnv),
    #' @description Pass all dependencies saved from previous functions to an environment for use
    #' @param ... \code{(character)} names of objects to share with `env`. **Default** load all previously stored objects.
    #' @param env \code{(environment)} to pass dependencies to. **Default** the calling environment
    #' @param as_list \code{(logical)} `TRUE` return the named objects as a list. `FALSE` saves them directly to the environment
    merge_deps_to_env = function(...,
                                 env = rlang::caller_env(),
                                 as_list = FALSE) {
      .dots <- rlang::dots_list(..., .named = TRUE)
      nms <- purrr::flatten_chr(unname(.dots))
      # If called with no arguments
      if (!UU::is_legit(nms) && !UU::is_legit(names(.dots)))
        nms <- private$work_deps
      .missing_nms <- !nms %in% ls(self$dependencies)
      if (any(.missing_nms))
        rlang::abort(paste0(
          paste0(nms[.missing_nms], collapse = ", "),
          " not found in working environment. Has it been saved?"
        ))
      if (!as_list) {
        rlang::env_bind(env, !!!rlang::env_get_list(self$dependencies, nms))
      } else {
        return(rlang::env_get_list(self$dependencies, nms))
      }

    },
    #' @description Gather the objects passed to \code{app_env}s internal environment to be passed to subsequent functions. Save app dependencies into a list.
    #' @param ... \code{(objects)} Dependencies for subsequent functions, passed as named objects or a character of the object name. If no name is provided, the name of the object will be retained. Use \code{"everything"} to capture all objects from the parent environment.
    #' @param app_deps \code{(logical/list)} **Default: `TRUE`** to save all app dependencies specified at initialization of the `app_env` object. Otherwise, a named list of the dependencies to save as a character vector with the list names corresponding to the apps for which to save the dependencies..
    #' @param env \code{(env)} The environment from which objects should be saved. **Default: the calling environment**
    #' @param .args I don't remember why this is here but it has to be for the function to work properly.
    #' @return \code{(environment)} The `app_env` object with the saved objects in the internal environment.
    gather_deps = function(...,
                           app_deps = TRUE,
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
        # Case when "everything" is specified
        .all_objs <- ls(env, all.names = TRUE)
        .args <- try(force(.args), silent = TRUE)
        if (UU::is_legit(.args))
          .all_objs <- stringr::str_subset(
            .all_objs,
            negate = TRUE,
            pattern = paste0("(?:^", .args, "$)", collapse = "|")
          )

        .dep_nms <- stringr::str_subset(.all_objs, "(?:app_env)|(?:clarity_api)", negate = TRUE)
        .work_deps <- purrr::compact(rlang::env_get_list(env, .dep_nms, default = NULL))
      } else if (length(.work_deps) == 1 && is.character(.work_deps[[1]]) && any(.work_deps[[1]] %in% ls(env))) {
        # case when character vector of objects to gather is provided
        .dep_nms <- .work_deps[[1]]
        .work_deps <- purrr::compact(rlang::env_get_list(env, .dep_nms, default = NULL))
        stopifnot(names(.work_deps) == .dep_nms)
      } else {
        .dep_nms <- names(.work_deps)
      }

      private$work_deps <- unique(c(private$work_deps, .dep_nms))


      rlang::env_bind(self$dependencies, !!!.work_deps)




      cli::cli_h2("Saved Dependencies")
      cli::cli_alert_success(paste0("Global: ", paste0(.dep_nms, collapse = ", ")))


      if (isTRUE(app_deps))
        app_deps <- self$app_deps

      if (is.list(app_deps)) {
        purrr::imap(app_deps, ~ {
          .work_deps <- purrr::compact(rlang::env_get_list(env, .x, default = NULL))
          if (UU::is_legit(.work_deps)) {
            # Add Client_filter for all dependencies to ensure test clients are removed from reporting
            rlang::env_bind(self$dependencies, !!!.work_deps)

            cli::cli_alert_success(paste0(.y, ": ", paste0(names(.work_deps), collapse = ", ")))
          }
        })
      }

      invisible(self)
    },
    #' @description Set the given environment to inherit from the internal 'global' environment
    #' @param vars_to_remove missing variables to remove from env that will otherwise mask the objects in the parent environment
    #' @param env child environment
    set_parent = function(vars_to_remove = NULL, env = rlang:::caller_env()) {
      if (UU::is_legit(vars_to_remove))
        rm(list = vars_to_remove, envir = env)
      parent.env(self$dependencies) <- .GlobalEnv
      parent.env(env) <- self$dependencies
    },
    #' @description Write app dependencies to disk
    #' @param deps \code{(character)} with names of app dependencies to write.
    #' @param path \code{(character)} of directory to write app dependencies to
    #' @param overwrite \code{(logical)} Whether to overwrite existing files. **Default:`TRUE`**
    #' @param all \code{(logical)} Whether to backup all dependencies (overrides deps)
    #' @return \code{(character)} vector of the files written
    write_app_deps = function (deps,
                               path = file.path("data", "db", "RminorElevated"),
                               overwrite = TRUE,
                               all = FALSE)
    {
      # Dir check
      if (!dir.exists(path))
        UU::mkpath(path)

      saved_deps <- ls(self$dependencies, all.names = TRUE)
      to_write <- purrr::when(all,
                  . ~ saved_deps,
                  !. ~ intersect(deps, saved_deps)) |> rlang::set_names()
      if (!missing(deps)) {
        .missing <- setdiff(deps, saved_deps)

        if (UU::is_legit(.missing)) {
          rlang::warn(paste0(
            "The following objects are missing from the app dependencies environment and were not written to disk:\n",
            paste0(.missing, collapse = ", ")))

        }
      }




      .pid <- cli::cli_progress_bar(status = "Writing: ", type = "iterator",
                                    total = length(to_write))


      out <- purrr::map_chr(to_write, ~{
        o <- get0(.x, envir = self$dependencies, inherits = FALSE)
        .ext <- UU::object_ext(o)
          fp <- file.path(path, paste0(.x, .ext))
        cli::cli_progress_update(id = .pid, status = .x)
        if (overwrite || !file.exists(fp)) {
          if (UU::is_legit(names(o)) && isTRUE(all(c("PersonalID", "UniqueID") %in% names(o))) && is_clarity() && !all)
            o <- clarity.looker::make_linked_df(o, UniqueID)
          if (UU::is_legit(names(o)) && isTRUE(all(c("PersonalID", "EnrollmentID") %in% names(o))) && is_clarity() && !all)
            o <- clarity.looker::make_linked_df(o, EnrollmentID)

          UU::object_write(o, fp, verbose = FALSE)
          stopifnot(file.info(fp)$mtime > Sys.Date())
        }
        fp
      })
      cli::cli_process_done(.pid)
      .deps <- stringr::str_remove(basename(out), '\\.\\w{1,10}$')
      cli::cli_alert_success(paste0("Dependencies written to {.path {unique(dirname(out))}}: {.emph {paste0(.deps, collapse = ', ')}}"))

    },
    #' @field app_deps \code{(list)} with all app dependencies as character vectors
    app_deps = c(),
    #' @description Load backed up dependencies from path
    #' @param path \code{(character)} path to folder with backed up dependencies
    load_deps = function(path = file.path("data","backup")) {
      .files <- UU::list.files2(path)
      .pid <- cli::cli_progress_bar(status = "Reading: ", type = "iterator",
                                    total = length(.files))
      purrr::iwalk(.files, ~{
        cli::cli_progress_update(id = .pid, status = .y)
        if (!stringr::str_detect(.x, "(?:png$)|(?:jpg$)|(?:jpeg$)"))
          self$dependencies[[.y]] <- clarity.looker::hud_load(.x)
      })
      cli::cli_process_done(.pid)
      cli::cli_alert_success(paste0("Dependencies loaded from {.path {path}}: {.emph {paste0(names(.files), collapse = ', ')}}"))
      invisible(self$dependencies)
    },
#' @description Transfer all files in the data dependencies folder to the applications via dropbox or `file.copy`. If using dropbox, requires an authorized token to dropbox. See `dropbox_auth`.
#' @param deps \code{(character/logical)} character vector of files to write to disk. Or `TRUE` **Default** to use the list of app dependencies matching the `dest_folder` name.
#' @param folder \code{(character)} path to folder with files to transfer transfer
#' @param dest_folder \code{(character)} folder to transfer too. This will be created inside the `HMIS Apps` folder if using Dropbox.
#' @param dropbox \code{(logical)} **Default** Upload dependencies to Dropbox, `FALSE` to pass to the `data` folder in the sibling directory: `dest_folder`.
#' @return

    deps_to_destination = function(deps = TRUE, folder = file.path("data","db","RminorElevated"), dest_folder = file.path("..","RminorElevated","data"), dropbox = TRUE) {

      dest_app = stringr::str_subset(stringr::str_split(dest_folder, "\\/")[[1]], paste0("(?:", names(self$app_deps), ")") |> paste0(collapse = "|"))

      if (isTRUE(deps))
        deps <- self$app_deps[[dest_app]]
      if (UU::is_legit(deps)) {
        files <- purrr::flatten_chr(purrr::compact(purrr::map(deps, hud_filename, path = folder)))
      } else
        files <- list.files(folder, full.names = TRUE)
      .pid <- cli::cli_progress_bar(status = "Transferring: ", type = "iterator",
                            total = length(files))
      out <- purrr::map_chr(files, ~{
        cli::cli_progress_update(id = .pid, status = basename(.x))
        if (dropbox) {
          rdrop2::drop_upload(.x, file.path(dest_app))
        } else {
          file.copy(.x, file.path(dest_folder, basename(.x)), overwrite = TRUE)
        }
        .x
      })
      cli::cli_process_done(.pid)
      cli::cli_alert_success(paste0("Transferred: {.emph {paste0(basename(out), collapse = ', ')}} to {.path {dest_folder}}"))
      .missing <- setdiff(deps, stringr::str_remove(basename(out), "\\.\\w{1,10}$"))
      if (UU::is_legit(.missing))
        warning(.missing, " was not found in ", folder)

    },

#' @description Authorize Dropbox
#' @param db_auth_token \code{(character)} path to the Dropbox authorization token. See \link[rdrop2]{drop_auth}
    dropbox_auth = function(db_auth_token = file.path("~","R","auth_tokens", "db_token.rds")) {
      db_auth_token <- path.expand(db_auth_token)
      # Dropbox Auth
      if (!file.exists(db_auth_token)) {
        token <- rdrop2::drop_auth(key = Sys.getenv("db_key"),
                                   secret = Sys.getenv("db_secret"),
                                   cache = FALSE)
        saveRDS(token, db_auth_token)
      } else {
        rdrop2::drop_auth(rdstoken = db_auth_token)
      }
    },
    #' @description Instantiate with default app dependencies to be collected (if they exist) each time \code{\$gather_deps} is called
    #' @param app_deps \code{(list)} with each app and it's dependencies as a character vector. See `app_deps` for formatting.
    initialize = function(app_deps) {
      if (missing(app_deps))
        app_deps <- Rm_data:::app_deps
      self$app_deps <- app_deps
    }
  ),
  private = list(#' @field Save a vector of the names of working dependencies that have been saved for future reference when \code{\$merge_deps_to_env} is called.
    work_deps = c()),
  lock_objects = FALSE
)

is_app_env <- function(x)
  inherits(x, "app_env")

dependencies <- list()

reset_Rm_env <- function(app_env = get_app_env(e = rlang::caller_env())) {
  deps <- purrr::compact(rlang::env_get_list(app_env$dependencies, ls(app_env$dependencies, all.names = TRUE), default = NULL))
  devtools::load_all()
  rm("Rm_env", envir = .GlobalEnv)
  .GlobalEnv$Rm_env <- Rm_data::app_env$new()

  rlang::env_bind(.GlobalEnv$Rm_env$dependencies, !!!deps)
  deps <- ls(.GlobalEnv$Rm_env$dependencies, all.names = TRUE)
  cli::cli_alert_success(paste0("Rm_env reset and deps re-added: {.emph {paste0(deps, collapse = ', ')}}"))
  invisible(deps)
}


