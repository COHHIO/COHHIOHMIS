# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.
#' @include app_dependencies.R 04_DataQuality_utils.R

dependencies$DataQuality <-
  c(
    "Client",
    "Contacts",
    "covid19",
    "Disabilities",
    "dose_counts",
    "doses",
    "Enrollment",
    "Funder",
    "guidance",
    "HealthAndDV",
    "IncomeBenefits",
    "Inventory",
    "living_situation",
    "mahoning_projects",
    "rm_dates",
    "Project",
    "Referrals",
    "Scores",
    "Services",
    "Users"
  )

check_fns <- stringr::str_subset(ls(envir = .getNamespace("Rm_data"), pattern = "^dq\\_"), "^((?!\\_sp\\_)(?!dose)(?!\\_vaccine)(?!\\_referrals)(?!\\_services)(?!\\_spdats)(?!\\_overlaps)(?!\\_check_eligibility).)*$")

data_quality <- function(check_fns = Rm_data::check_fns,
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
  ) {
  force(clarity_api)
  force(app_env)




  # Providers to Check ------------------------------------------------------
  projects_current_hmis <- projects_current_hmis()

  app_env$gather_deps(projects_current_hmis)
  # The Variables That We Want ----------------------------------------------

  vars <- list()
  vars$prep <- c(
    "EntryAdjust",
    "EntryDate",
    "ExitDate",
    "HouseholdID",
    "MoveInDateAdjust",
    "PersonalID",
    "ProjectID",
    "ProjectName",
    "ProjectRegion",
    "ProjectType",
    "UniqueID",
    "UserCreating"
  )

  vars$we_want <- c(vars$prep,
                            "Issue",
                            "Type",
                            "Guidance")
  app_env$gather_deps(vars)

  # Clients to Check --------------------------------------------------------

  served_in_date_range <- served_in_date_range()
  app_env$gather_deps(served_in_date_range)

  ssvf_served_in_date_range <- ssvf_served_in_date_range()
  app_env$gather_deps(ssvf_served_in_date_range)

  .total <- length(check_fns)
  .pid <- cli::cli_progress_bar(type = "iterator",
                        total = .total + 3)
  dqs <- purrr::map(rlang::set_names(check_fns), ~{
    i <- which(check_fns == .x)
    cli::cli_progress_update(id = .pid,,
                             status = paste0(i,"/",.total,": ",stringr::str_remove(.x, "^dq\\_")))
    fn <- getFromNamespace(.x, "Rm_data")
    arg_names <- rlang::set_names(rlang::fn_fmls_names(fn))
    arg_names <- arg_names[!purrr::map_lgl(rlang::fn_fmls(fn), is.logical)]
    arg_names <- arg_names[arg_names != c("app_env")]

    .call <- rlang::call2(fn, !!!purrr::map(arg_names, ~rlang::expr(app_env$.__enclos_env__[[!!.x]])), app_env = NULL)

    out <- rlang::eval_bare(.call)  |>
      dplyr::distinct(PersonalID, Issue, .keep_all = TRUE)
    UU::join_check(out)
    out
  })

  cli::cli_progress_update(id = .pid,,
                           status = "Creating data quality table")
dq_main <- do.call(rbind, dqs) |>
  unique() %>%
  dplyr::mutate(Type = factor(Type, levels = c("High Priority",
                                               "Error",
                                               "Warning"))) %>%
  dplyr::filter(ProjectType != 14 |
                  (
                    ProjectType == 14 &
                      Issue %in% c(
                        "60 Days in Mahoning Coordinated Entry",
                        "Access Point with Entry Exits",
                        "Missing Date of Birth Data Quality",
                        "Don't Know/Refused or Approx. Date of Birth",
                        "Missing DOB",
                        "Missing Name Data Quality",
                        "Incomplete or Don't Know/Refused Name",
                        "Rent Payment Made, No Move-In Date",
                        "Invalid SSN",
                        "Don't Know/Refused SSN",
                        "Missing SSN",
                        "Missing Veteran Status",
                        "Don't Know/Refused Veteran Status",
                        "Missing County Served"
                      )
                  ))
cli::cli_progress_update(id = .pid,,
                         status = "Finish dq_main")
if (is_clarity())
  dq_main <- make_profile_link_df(dq_main)

# Controls what is shown in the CoC-wide DQ tab ---------------------------
# for CoC-wide DQ tab
app_env$merge_deps_to_env(c("rm_dates", "Project"))

dq_main |>
  {\(x) {
    app_env$gather_deps(
      dq_past_year = HMIS::served_between(x, rm_dates$hc$check_dq_back_to, lubridate::today()),
      # for project evaluation reporting
      dq_for_pe = HMIS::served_between(x, rm_dates$hc$project_eval_start, rm_dates$hc$project_eval_end),
      dq_main = x)

  }}()


cli::cli_progress_update(id = .pid,,
                         status = "Addtl Data")

eligibility_detail <- dq_check_eligibility()
dq_overlaps <- dq_overlaps()

if (is_clarity()) {
  eligibility_detail <- make_profile_link_df(eligibility_detail)
  dq_overlaps <- make_profile_link_df(dq_overlaps)
}

# TODO See note in dq_overlaps
# if (is_sp()) {
#   unsh_overlaps <- dq_overlaps(unsh = TRUE)
# }

# Missing Client Location -------------------------------------------------
# missing_client_location <- dq_missing_client_location(served_in_date_range, vars)
# Household Issues --------------------------------------------------------
# hh_children_only <- dq_hh_children_only(served_in_date_range, vars)
# hh_no_hoh <- dq_hh_no_hoh()
# hh_too_many_hohs <- dq_hh_too_many_hohs()
#
#
# hh_missing_rel_to_hoh <- dq_hh_missing_rel_to_hoh()
# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears
# dq_missing_approx_date_homeless <- missing_approx_date_homeless(served_in_date_range, guidance, vars)
# dq_missing_previous_street_ESSH <- missing_previous_street_ESSH(served_in_date_range, guidance, vars)
# dq_missing_residence_prior <- missing_residence_prior(served_in_date_range, guidance, vars)
# dkr_residence_prior <- dkr_residence_prior(served_in_date_range, guidance, vars)
# dq_missing_LoS <- missing_LoS(served_in_date_range, vars = vars)
# dkr_LoS <- dkr_LoS(served_in_date_range, vars, guidance)
# dq_missing_months_times_homeless <- missing_months_times_homeless(served_in_date_range, vars, guidance, hc)
# dq_dkr_months_times_homeless
# invalid_months_times_homeless
# missing_living_situation
# dkr_living_situation
# DisablingCondition at Entry ----
# Thu Sep 09 13:53:45 2021
# Mahoning 60 days CE -----------------------------------------------------
# Extremely Long Stayers --------------------------------------------------
# Incorrect Destination ---------------------------------------------------
# RRH mover inners only
# SH
# PSH
# TH
# SH
# TH
# SH
# Missing Project Stay or Incorrect Destination ---------------------------
# RRH
# PSH
# TH
# SH
# CountyServed (BoS ONLY for now)
# CountyPrior (BoS ONLY for now)
# Check Eligibility, Project Type, Residence Prior ------------------------
# dq_check_eligibility
# Rent Payment Made, No Move-In Date
#dq_rent_paid_no_move_in
# Missing Destination
#dq_missing_destination
# dq_dkr_destination
# Missing PATH Data -------------------------------------------------------
#* Length of Stay in Res Prior
### adult, PATH-enrolled, and:
### Length of Stay is null or DNC -> error -OR-
### Length of Stay is DKR -> warning
#dq_path_missing_los_res_prior
#* Engagement at Exit
### adult, PATH-enrolled, Date of Engagement is null -> error
#dq_path_no_status_at_exit
#* Status Determination at Exit
### adult, PATH-Enrolled is not null
### Date of Status Determ is null -> error
# dq_path_status_determination
#* PATH Enrolled at Exit
### adult and:
### PATH Enrolled null or DNC -> error -OR-
#dq_path_enrolled_missing
#* Not Enrolled Reason
### adult
### PATH Enrolled = No
### Reason is null -> error
#dq_path_reason_missing
#* Connection with SOAR at Exit
### adult
### Connection w/ SOAR is null or DNC -> error -OR-
### Connection w/ SOAR DKR -> warning
# dq_path_SOAR_missing_at_exit
# Missing PATH Contacts
## client is adult/hoh and has no contact record in the EE -> error
## this is a high priority data quality issue
## if the contact was an "Outreach" record after 10/1/2019, it is being
## filtered out because they should be using CLS subs past that date.
# dq_missing_path_contact
# Incorrect PATH Contact Date
## client is adult/hoh, has a contact record, and the first record in the EE
## does not equal the Entry Date ->  error
## if the contact was an "Outreach" record after 10/1/2019, it is being
## filtered out because they should be using CLS subs past that date.
#dq_incorrect_path_contact_date
# Duplicate EEs -----------------------------------------------------------
# this could be more nuanced but it's ok to leave it since we are also
# looking at overlaps
#duplicate_ees <- dq_duplicate_ees()
# Future Entry Exits ------------------------------------------------------
# PSHs in the old days before Move In Dates would definitely have been entering
# their clients prior to their Entry Date since back then the Entry Date was the
# day they moved in. So they're excused from this prior to Move In Date's existence.
#future_ees <- dq_future_ees()
#future_exits <- dq_future_exits()
# HoHs Entering PH without SPDATs -----------------------------------------
#ph_without_spdats <- dq_ph_without_spdates()
# Missing Income at Entry -------------------------------------------------
# IncomeBenefits <- IncomeBenefits %>% select(-DateCreated)
# Not calculating Conflicting Income Amounts bc they're calculating the TMI from the
# subs instead of using the field itself. Understandable but that means I would
# have to pull the TMI data in through RMisc OR we kill TMI altogether. (We
# decided to kill TMI altogether.)
# Missing Income at Exit --------------------------------------------------
#missing_income <- dq_missing_income()
#conflicting_income <- dq_conflicting_income()
# Overlapping Enrollment/Move In Dates ------------------------------------
# this only pulls the most recent EE in the overlap and I think that's fine but
# some users won't like being flagged for it if it's someone else's fault
# but you can't tell whose fault it is from the data so...
#staging_overlaps <- dq_staging_overlaps()
#same_day_overlaps <- dq_overlaps()
# rrh_overlaps <- dq_overlaps_rrh()
# psh_overlaps <- dq_overlaps_psh()
#unsh_overlaps <- dq_overlaps(unsh = TRUE)
# Missing Health Ins ------------------------------------------------------
#missing_health_insurance_entry <- dq_missing_hi_entry()
#missing_health_insurance_exit <- dq_missing_hi_exit()
#conflicting_health_insurance <- dq_conflicting_hi_ee()
# Missing, Unlikely, or Conflicting NCBs ---------------------------------------------------
# Unlikely NCBs -----------------------------------------------------------
# Missing NCBs at Exit ----------------------------------------------------
# dq_conflicting_unlikely_ncbs
# dq_missing_ncbs
# SSI/SSDI but no Disability (Q) ------------------------------------------
# check_disability_ssi <- dq_check_disability_ssi()
# Non HoHs w Svcs or Referrals --------------------------------------------
# SSVF projects should be showing this as an Error,7 whereas non-SSVF projects
# should be showing it as a warning, and only back to Feb of 2019
# services_on_hh_members <- dq_services_on_hh_members()
# services_on_hh_members_ssvf <- dq_services_on_hh_members_ssvf()
# referrals_on_hh_members_ssvf <- dq_referrals_on_hh_members_ssvf()
# AP entering project stays -----------------------------------------------
#aps_with_ees <- dq_aps_with_ees
# Stray Services (fall outside EE) ----------------------------------------
# Because a lot of these records are stray Services due to there being no
# Entry Exit at all, this can't be shown in the same data set as all the other
# errors. I'm going to have to make this its own thing. :(
# stray_services_warning <- dq_stray_services(stray_services)
# Side Door ---------------------------------------------------------------
# moved to Data_Quality_plots
# SSVF --------------------------------------------------------------------
#
#   ssvf_served_in_date_range <-
#     ssvf_served_in_date_range()
#
#
#   veteran_missing_year_entered <- dq_veteran_missing_year_entered()
#
#   veteran_missing_year_separated <- dq_veteran_missing_year_separated()
#
#   veteran_missing_branch <-
#     dq_veteran_missing_branch()
#
#   veteran_missing_discharge_status <- dq_veteran_missing_discharge_status()
#
#   dkr_client_veteran_info <- dq_dkr_client_veteran_info()
#
#   ssvf_missing_percent_ami <- dq_ssvf_missing_percent_ami()
#ssvf_missing_address <- dq_ssvf_missing_address()
#
# TEMPORARILY NOT REQUIRED FOR COVID-19 REASONS
# ssvf_hp_screen <- ssvf_served_in_date_range %>%
#   filter(ProjectType == 12 &
#            RelationshipToHoH == 1 &
#            (is.na(HPScreeningScore) |
#               is.na(ThresholdScore))) %>%
#   mutate(Issue = "Missing HP Screening or Threshold Score",
#          Type = "Error",
#          Guidance = guidance$missing_at_entry) %>%
#   select(all_of(vars$we_want))

dq_providers <- sort(projects_current_hmis$ProjectName)

# APs without referrals ----
# Mon Sep 20 16:31:46 2021
aps_no_referrals <- dqu_aps(data_APs = FALSE)
data_APs <- dqu_aps()




app_env$gather_deps(served_in_date_range, dq_providers, aps_no_referrals, data_APs, eligibility_detail, dq_overlaps)
}
