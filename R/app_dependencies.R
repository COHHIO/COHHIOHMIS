#' @include aaa_imports.R guidance.R 00_lists.R
app_deps <- list(
  ## to Rm:
  Rminor = c(
    # "APs",
    "bos_counties",
    "BoS_PIT",
    "rm_dates",
    "covid19",
    "covid19_priority_plot",
    "covid19_status_plot",
    "current_tay_hohs",
    # "FileEnd",
    "goals",
    "Mah_PIT",
    "pe_summary_validation",
    "data_types",
    "qpr_benefits",
    "qpr_income",
    "qpr_leavers",
    "qpr_rrh_enterers",
    "qpr_spdats_county",
    "qpr_spdats_project",
    "qpr_spending",
    "Regions",
    "spm_Metric_1b",
    "spm_Metric_2",
    "spm_Metric_7",
    "spm_current_end_date",
    "spm_current_start_date",
    "spm_prior_end_date",
    "spm_prior_start_date",
    "pe_summary_final_scoring",
    "utilization",
    "utilization_bed",
    "utilization_unit",
    "validation",
    "vets_current"
  ) |> rlang::set_names(),

  # to Rme

  RminorElevated = c(
    "co_clients_served",
    "data_types",
    "dq_APs",
    "dq_aps_no_referrals",
    "dq_eligibility_detail",
    "dq_main",
    "dq_overlaps",
    "dq_past_year",
    "dq_providers",
    "dq_summary",
    "dq_unsheltered",
    "guidance",
    "path_referrals",
    "pe_benefits_at_exit",
    "pe_entries_no_income",
    "pe_exits_to_ph",
    "pe_homeless_history_index",
    "pe_increase_income",
    "pe_length_of_stay",
    "pe_long_term_homeless",
    "pe_own_housing",
    "pe_users_info",
    "pe_res_prior",
    "pe_scored_at_ph_entry",
    "pe_summary_final_scoring",
    "pe_summary_validation",
    "pe_benefits_at_exit_mahoning",
    "pe_entries_no_income_mahoning",
    "pe_exits_to_ph_mahoning",
    "pe_homeless_history_index_mahoning",
    "pe_length_of_stay_mahoning",
    "pe_long_term_homeless_mahoning",
    "pe_users_info_mahoning",
    "pe_res_prior_mahoning",
    "pe_scored_at_ph_entry_mahoning",
    "pe_summary_final_scoring_mahoning",
    "pe_summary_validation_mahoning",
    "program_lookup",
    "prioritization",
    "prioritization_colors",
    "qpr_benefits",
    "qpr_income",
    "qpr_leavers",
    "qpr_rrh_enterers",
    "qpr_spdats_county",
    "qpr_spdats_project",
    "qpr_spending",
    "Regions",
    "rm_dates",
    "utilization",
    "utilization_beds",
    "utilization_clients",
    "validation",
    "veteran_active_list",
    "vets_housed"
  ) |> rlang::set_names()
)
# browser()
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



folder_clean <- function(files, dest_files, remote = FALSE) {
  to_clean <- setdiff(basename(dest_files), basename(files))
  # destpath <- unique(dirname(dest_files))
  destpath <- "data"
  if (UU::is_legit(to_clean)) {
    if (remote) {
      .cleaned <- purrr::map(to_clean, rdrop2::drop_delete)
      .cleaned <- purrr::map_chr(.cleaned, ~basename(.x$metadata$path_display))
      .cleaned <- intersect(to_clean, .cleaned)
    } else {
      .cleaned <- purrr::map_lgl(to_clean, ~file.remove(file.path(destpath, .x)))
      .cleaned <- to_clean[.cleaned]
    }
    cli::cli_alert_info("Cleaned from {.path {ifelse(remote, 'Remote', destpath)}}: {cli::col_silver(basename(.cleaned))}")
  }
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
    #' @return \code{(environment)} The `app_env` object with the saved objects in the internal environment.
    gather_deps = function(...,
                           app_deps = TRUE,
                           env = rlang::caller_env()) {
      # must be forced to get the calling environment where the user called it since env isn't used until inside the purrr::map call
      force(env)

      .work_deps <- rlang::dots_list(..., .named = TRUE) |>
        {
          \(x) {
            rlang::set_names(x, stringr::str_remove_all(names(x), "\""))
          }
        }()

      .all_objs <- ls(env, all.names = TRUE)
      if (length(.work_deps) == 1 &&
          identical(.work_deps[[1]], "everything")) {
        # Case when "everything" is specified
        .dep_nms <- stringr::str_subset(.all_objs, "(?:app_env)|(?:clarity_api)", negate = TRUE)
        work_deps_has_obs <- FALSE
      } else if (length(.work_deps) == 1 && is.character(.work_deps[[1]]) && any(.work_deps[[1]] %in% .all_objs)) {
        # case when character vector of objects to gather is provided
        .dep_nms <- .work_deps[[1]]
        work_deps_has_obs <- FALSE
      } else {
        .dep_nms <- names(.work_deps)
        work_deps_has_obs <- TRUE
      }

      # Determine what needs to be saved for the apps
      if (isTRUE(app_deps))
        app_deps <- self$app_deps

      if (is.list(app_deps)) {
        app_vars <- purrr::imap(app_deps, ~ {
          intersect(.x, c(.all_objs, .dep_nms))
        })
        app_vars_chr <- purrr::flatten_chr(app_vars)
        .all_nms <- unique(c(app_vars_chr, .dep_nms))
      } else
        .all_nms <- .dep_nms


      .all_deps <- purrr::compact(rlang::env_get_list(env, purrr::when(work_deps_has_obs, . ~ setdiff(.all_nms, .dep_nms), ~ .all_nms), default = NULL))

      if (work_deps_has_obs)
        .all_deps <- purrr::list_modify(.all_deps, !!!.work_deps)

      # Remove test clients
      .all_deps <- purrr::map(.all_deps, clarity.looker::Client_filter)

      private$work_deps <- unique(c(private$work_deps, .all_nms))

      rlang::env_bind(self$dependencies, !!!.all_deps)


      cli::cli_h2("Saved Dependencies")
      cli::cli_alert_success("{cli::col_br_green('Global')}: {paste0(.dep_nms, collapse =', ')}")
      if (exists("app_vars", inherits = FALSE))
        purrr::iwalk(app_vars, ~ if (UU::is_legit(.x)) cli::cli_alert_success("{.path {.y}}: {paste0(.x, collapse = ', ')}"))

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
    #' @field app_deps \code{(list)} with all app dependencies as character vectors
    app_deps = c(),
    #' @description Load backed up dependencies from path
    #' @param deps \code{(chr)} of deps to search for and load
    #' @param path \code{(character)} path to folder with backed up dependencies
    load_deps = function(deps, path = file.path("data","backup")) {
      if (!missing(deps))
        .files <- do.call(c, purrr::map(deps, clarity.looker::hud_filename, path = path))
      else
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
#' @description Write `deps` to a folder via `file.copy` or to dropbox. If using dropbox, requires an authorized token to dropbox. See `dropbox_auth`.
#' @param deps \code{(character/logical)} character vector of files to write to disk. Or `TRUE` **Default** to use `app_deps`. Use `"all"` to write all objects in the `dependencies` environment to `dest_folder`.
#' @param dest_folder \code{(character)} folder(s) to transfer deps to - must be same length as `deps` or length 1 and will be recycled if `deps` is a list. When **dropbox = TRUE** th(is/ese) folder(s) will be used to stage files for upload.
#' @param remote \code{(logical/character)} Transfer `deps` to the root folder assigned by the API key (**HMIS Apps** at COHHIO) on Dropbox.
#' @param clean \code{(logical)}  clean unused dependencies from folder. **Default** `FALSE` to preserve unused dependencies in `dest_folder`
#' @return

    deps_to_destination = function(deps = TRUE, dest_folder = file.path("..",c("Rminor", "RminorElevated"),"data"), remote = FALSE, clean = FALSE) {

      all <- identical(deps, "all")
      self_deps <- isTRUE(deps)
      .remote <- isTRUE(remote) || is.character(remote)
      if (all) {
        .remote = FALSE
        deps_flat <- ls(self$dependencies, all.names = TRUE)
        deps <- list(all = deps_flat)
      } else {
        # Use self$app_deps if TRUE, if not a list, make one for iteration

        deps <- purrr::when(deps, isTRUE(.) ~ self$app_deps, !rlang::is_list(deps) ~ list(deps), ~ deps)
        if (!is.null(names(deps))) {
          # Add dependencies vectors to the objects that are saved
          deps <- purrr::imap(deps, ~{
            nm <- paste0("deps_", .y)
            assign(nm, .x, envir = self$dependencies)
            c(nm, .x)
          })
        }

        deps_flat <- purrr::flatten_chr(deps) |> unique()


        # Get all dependencies
        saved_deps <- ls(self$dependencies, all.names = TRUE)

        # Handle missing
        to_write <- intersect(deps_flat, saved_deps) |>
          unique() |>
          rlang::set_names()


        .missing <- setdiff(deps_flat, saved_deps)

        if (UU::is_legit(.missing)) {
          rlang::warn(paste0(
            "The following objects are missing from the app dependencies environment and will not be written to disk:\n",
            paste0(.missing, collapse = ", ")))

        }
        deps_flat <- setdiff(deps_flat, .missing)
      }
      # make folders if they don't exist
      purrr::map(dest_folder, UU::mkpath)


      # load app_deps
      if (.remote) {
        self$dropbox_auth()
        # rdrop2::drop_auth()
        db_info <- rdrop2::drop_dir()
        db_files <- basename(db_info$path_display)
        db_updated <- rlang::set_names(db_info$client_modified, db_files)
      }


      # don't mess with images
      # stringr::str_subset(UU::regex_or(paste0(c("jpg", "png", "jpeg"), "$")), negate = TRUE)
      # # make the destination folder
      # if (length(dest_folder) != length(deps))
      #   dest_folder <- rlang::set_names(rep(dest_folder, length(deps)))

      # Check recency
      .pid <- cli::cli_progress_bar(status = "Checking recency: ", type = "tasks",
                                    total = length(deps_flat),
                                    auto_terminate = TRUE,
                                    format = "{cli::pb_name}: {.path {cli::pb_status}} {cli::pb_current}/{cli::pb_total} [{cli::col_br_blue(cli::pb_elapsed)}]"
      )
      maybe_write <- purrr::map_dfr(deps_flat, ~{
        cli::cli_progress_update(id = .pid, status = .x)
        o_info <- list(nm = .x)
        o_info$ex <- list(rlang::expr(get0(!!.x, envir = self$dependencies, inherits = FALSE)))
        o <- rlang::eval_bare(o_info$ex[[1]])
        o_info$ext <- UU::object_ext(o)
        o_info$class <- list(class(o))
        if (!all) {
          # If not making a backup, prep the dfs for the apps
          if (UU::is_legit(names(o)) && is_clarity() && !all) {
            .o <- o
            if (isTRUE(all(c("PersonalID", "UniqueID") %in% names(.o))))
              .o <- clarity.looker::make_linked_df(.o, UniqueID)
            if (isTRUE(all(c("PersonalID", "EnrollmentID") %in% names(.o))))
              .o <- clarity.looker::make_linked_df(.o, EnrollmentID)
          }
          # Avoid making these links twice by saving the object as the expression
          if (exists(".o", inherits = FALSE) && !identical(o, .o)) {
            o_info$ex <- list(.o)
            o <- .o
          }
        }
        # Check if it's an image as these should be overwritten
        .is_image <- o_info$ext %in% paste0(".", c("png", "jpg", "jpeg"))

        out <- purrr::map2_dfr(deps, dest_folder, ~{
          if (o_info$nm %in% .x) {
            fp <- file.path(.y, paste0(o_info$nm, o_info$ext))
            if (file.exists(fp)) {
              fn <- UU::file_fn(fp)
              if (identical(fn, arrow::read_feather))
                .args <- list(mmap = FALSE)
              else
                .args <- list()
              .file <- try(rlang::exec(fn, fp, !!!.args), silent = TRUE)
              # Sometimes invalid files, delete them
              if (!UU::is_legit(.file))
                file.remove(fp)
              if (.is_image)
                .same_data <- FALSE
              else
                .same_data <- isTRUE(all.equal(.file, o, check.attributes = FALSE, check.tzone = FALSE))
            } else
              .same_data <- FALSE

            out <- tibble::tibble_row(
              !!!o_info,
              filepath = fp,
              to_update = !.same_data
            )
          } else
            out <- NULL
          out
        })
      })

      to_write <- dplyr::filter(maybe_write, to_update)
      if (nrow(to_write)) {
        # If sending all to one folder, don't duplicate writes
        if (length(unique(dest_folder)) == 1)
          to_write <- dplyr::distinct(to_write, nm, .keep_all = TRUE)
        # Stage files
        .pid <- cli::cli_progress_bar(status = "Writing: ", type = "tasks",
                                      total = nrow(to_write),
                                      auto_terminate = TRUE,
                                      format = "{cli::pb_name}: {.path {cli::pb_status}} {cli::pb_current}/{cli::pb_total} [{cli::col_br_blue(cli::pb_elapsed)}]"
        )

        purrr::pwalk(to_write, ~{
          .x <- list(...)
          cli::cli_progress_update(id = .pid, status = .x$filepath)
          o <- rlang::eval_bare(.x$ex)
          .args <- list(o, .x$filepath, verbose = FALSE)
          if (UU::ext(.x$filepath) == "feather")
            .args$compression = "uncompressed"
          fp <- do.call(UU::object_write, .args)
          if (UU::ext(fp) == "png")
            knitr::plot_crop(fp)
        })

        # Clean folder
        if (clean && !self_deps)
          purrr::walk(unique(dest_folder), ~ {
            dplyr::mutate(maybe_write,
                          dirpath = dirname(filepath)) |>
              dplyr::filter(dirpath == .x) |>
              dplyr::pull(filepath) |>
              folder_clean(UU::list.files2(.x))
          })

        if (.remote) {
          if (clean)
            folder_clean(unique(basename(maybe_write$filepath)), db_files, remote = .remote)

          to_upload <- dplyr::mutate(maybe_write,
                                     filename = basename(filepath),
                                     updated = file.info(filepath)$mtime,
                                     db_updated = db_updated[filename],
                                     needs_update = (updated > db_updated) %|% TRUE) |>
            dplyr::filter(needs_update) |>
            dplyr::distinct(nm, .keep_all = TRUE)

          purrr::walk(to_upload$filepath, rdrop2::drop_upload)
        }
      }





      .success <- c(
        if (nrow(to_write))
          to_write |>
            dplyr::mutate(path = dirname(filepath)) |>
            dplyr::group_by(path) |>
            dplyr::group_split() |>
            purrr::map(~glue::glue("{cli::col_br_blue('Wrote')} to {{.path {unique(.x$path)}}}: {{.emph {paste0(.x$nm, collapse = ', ')}}}")),
        if (exists("to_upload", inherits = FALSE))
          to_upload |>
          dplyr::mutate(path = dirname(filepath)) |>
          dplyr::group_by(path) |>
          dplyr::group_split() |>
          purrr::map(~glue::glue(
            "{cli::col_br_green('Transferred to Remote')} from {{.path {unique(.x$path)}}}: {{.emph {paste0(basename(.x$filepath), collapse = ', ')}}}"
          ))
      )
      purrr::walk(.success, cli::cli_alert_success)

      .skipped <- maybe_write[!maybe_write$to_update,]
      if (nrow(.skipped))
        .skipped |>
          dplyr::mutate(path = dirname(filepath)) |>
          dplyr::group_by(path) |>
          dplyr::group_split() |>
          purrr::map(~glue::glue("{cli::col_yellow('Unchanged & Skipped')} in {{.path {unique(.x$path)}}}: {cli::col_silver(paste0(.x$nm, collapse = ', '))}")) |>
          cli::cli_inform()
      },

#' @description Authorize Dropbox
#' @param db_auth_token \code{(character)} path to the Dropbox authorization token. See \link[rdrop2]{drop_auth}
    dropbox_auth = function(db_auth_token = file.path("inst", "vault", "db_token.rds")) {
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
    #' @param dirs \code{(list)} See \link[clarity.looker]{dirs}
    #' @param guidance \code{(list)} that is named of character vectors with guidance for each type of Data Quality Issue. See `?guidance`
    #' @param data_types \code{(list)} that is named with common groupings of HUD CSV data types. See `?data_types`
    initialize = function(app_deps, dirs = clarity.looker::dirs, guidance, data_types) {
      self$dependencies$dirs <- dirs
      self$app_deps <- if (missing(app_deps)) get0("app_deps", envir = rlang::ns_env("RmData")) else app_deps
      self$dependencies$guidance <- if (missing(guidance)) get0("guidance", envir = rlang::ns_env("RmData")) else guidance
      self$dependencies$data_types <- if (missing(data_types)) get0("data_types", envir = rlang::ns_env("RmData")) else data_types
    }
  ),
  private = list(#' @field Save a vector of the names of working dependencies that have been saved for future reference when \code{\$merge_deps_to_env} is called.
    work_deps = c()),
  lock_objects = FALSE
)

dependencies <- list()

reset_Rm_env <- function(app_env = get_app_env(e = rlang::caller_env())) {
  deps <- purrr::compact(rlang::env_get_list(app_env$dependencies, ls(app_env$dependencies, all.names = TRUE), default = NULL))
  pkgload::load_all()
  rm("Rm_env", envir = .GlobalEnv)
  .GlobalEnv$Rm_env <- RmData::app_env$new()

  rlang::env_bind(.GlobalEnv$Rm_env$dependencies, !!!deps)
  deps <- ls(.GlobalEnv$Rm_env$dependencies, all.names = TRUE)
  cli::cli_alert_success(paste0("Rm_env reset and deps re-added: {.emph {paste0(deps, collapse = ', ')}}"))
  invisible(deps)
}


