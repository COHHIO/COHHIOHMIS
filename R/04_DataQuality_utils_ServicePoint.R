if (is_sp()) {
  # ServicePoint DQ Checks ----
  # Tue Sep 14 10:14:17 2021
  #' @title Incorrect Entry/Exit Types
  #' @inherit data_quality_tables params return
  #' @family ServicePoint Checks
  #' @export

  dq_sp_incorrect_ee_type <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
    must_sp()
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    served_in_date_range %>%
      dplyr::filter(
        (
          is.na(GrantType) &
            !grepl("GPD", ProjectName) &
            !grepl("HCHV", ProjectName) &
            !grepl("VET", ProjectName) &
            !grepl("Veterans", ProjectName) &
            ProjectID != 1695 &
            EEType != "HUD"
        ) |
          ((
            GrantType == "SSVF" |
              grepl("GPD", ProjectName) |
              grepl("HCHV", ProjectName) |
              grepl("Veterans", ProjectName) |
              grepl("VET", ProjectName) |
              grepl("VASH", ProjectName)
          ) &
            EEType != "VA"
          ) |
          (GrantType == "RHY" &
             !grepl("YHDP", ProjectName) &
             !grepl("ODH", ProjectName) &
             EEType != "RHY") |
          (GrantType == "RHY" &
             grepl("YHDP", ProjectName) &
             grepl("ODH", ProjectName) &
             EEType != "HUD") |
          (GrantType == "PATH" & EEType != "PATH") |
          (ProjectID == 1695 & EEType != "Standard")
      ) %>%
      dplyr::mutate(Issue = "Incorrect Entry Exit Type",
                    Type = "High Priority",
                    Guidance = guidance$incorrect_ee_type) %>%
      dplyr::select(dplyr::all_of(vars$we_want))
  }


  #' @title Find Access Points with Entrys/Exits
  #' @family ServicePoints Checks
  #' @family DQ: EE Checks
  #' @inherit data_quality_tables params return
  #' @export

  dq_sp_aps_with_ees <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
    must_sp()
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    served_in_date_range %>%
      dplyr::filter(ProjectType == 14) %>% # not incl Mah CE
      dplyr::mutate(
        Issue = "Access Point with Entry Exits",
        Type = "High Priority",
        Guidance = guidance$aps_with_ees
      ) %>%
      dplyr::select(dplyr::all_of(vars$we_want))
  }

  #' @title Find Stray Services
  #' @inherit data_quality_tables params return
  #' @family ServicePoint Checks
  #' @export


  dq_sp_stray_services <- function(stray_services, guidance, app_env = get_app_env(e = rlang::caller_env())) {
    must_sp()
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    stray_services %>%
      dplyr::mutate(Issue = "Service Not Attached to an Entry Exit",
                    Type = "Warning",
                    Guidance = guidance$stray_service) %>%
      dplyr::select(PersonalID, ServiceProvider, ServiceStartDate, Issue, Type)
  }


  #' @title Find Referrals on Household Members
  #' @inherit data_quality_tables params return
  #' @family ServicePoint Checks
  #' @export

  dq_sp_referrals_on_hh_members <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
    must_sp()
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    served_in_date_range %>%
      dplyr::select(dplyr::all_of(vars$prep),
                    RelationshipToHoH,
                    EnrollmentID,
                    GrantType) %>%
      dplyr::filter(RelationshipToHoH != 1 &
                      (GrantType != "SSVF"  | is.na(GrantType))) %>%
      dplyr::semi_join(Referrals,
                       by = c("PersonalID", "ProjectName" = "ProviderCreating")) %>%
      dplyr::mutate(
        Issue = "Referral on a Non Head of Household",
        Type = "Warning",
        Guidance = guidance$referral_on_non_hoh
      ) %>%
      dplyr::select(dplyr::all_of(vars$we_want))
  }


  #' @title Find Referrals on Household Members SSVF
  #' @inherit data_quality_tables params return
  #' @family ServicePoint Checks
  #' @export

  dq_sp_referrals_on_hh_members_ssvf <- function(served_in_date_range, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
    must_sp()
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    served_in_date_range %>%
      dplyr::select(dplyr::all_of(vars$prep),
                    RelationshipToHoH,
                    EnrollmentID,
                    GrantType) %>%
      dplyr::filter(RelationshipToHoH != 1 &
                      GrantType == "SSVF") %>%
      dplyr::semi_join(Referrals, by = c("PersonalID")) %>%
      dplyr::mutate(Issue = "Referral on a Non Head of Household (SSVF)",
                    Type = "Error",
                    Guidance = guidance$referral_on_non_hoh) %>%
      dplyr::select(dplyr::all_of(vars$we_want))
  }

  #' @title Find Internal Outstanding Referrals
  #' @inherit data_quality_tables params return
  #' @family ServicePoint Checks
  #' @export


  dq_sp_internal_old_outstanding_referrals <- function(served_in_date_range, Referrals, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
    must_sp()
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    served_in_date_range %>%
      dplyr::semi_join(Referrals,
                       by = c("PersonalID")) %>%
      dplyr::left_join(Referrals,
                       by = c("PersonalID")) %>%
      dplyr::filter(ReferringProjectID == ProjectName &
                      ProjectID != 1695) %>%
      dplyr::select(dplyr::all_of(vars$prep),
                    ReferringProjectID,
                    ReferredDate,
                    ReferralOutcome,
                    EnrollmentID) %>%
      dplyr::filter(is.na(ReferralOutcome) &
                      ReferredDate < lubridate::today() - lubridate::days(14)) %>%
      dplyr::mutate(
        ProjectName = ReferringProjectID,
        Issue = "Old Outstanding Referral",
        Type = "Warning",
        Guidance = "Referrals should be closed in about 2 weeks. Please be sure you are following up with any referrals and helping the client to find permanent housing. Once a Referral is made, the receiving agency should be saving the 'Referral Outcome' once it is known. If you have Referrals that are legitimately still open after 2 weeks because there is a lot of follow up going on, no action is needed since the HMIS data is accurate."
      ) %>%
      dplyr::select(dplyr::all_of(vars$we_want))
  }

  # Unsheltered Incorrect Residence Prior -----------------------------------
  dq_sp_unsheltered_enrollments <- function(served_in_date_range, Users, unsheltered_ProjectID = 1695, by_month = FALSE, vars, guidance, app_env = get_app_env(e = rlang::caller_env())) {
    if (is_app_env(app_env))
      app_env$set_parent(missing_fmls())
    # Unsheltered
    unsheltered_enrollments <- served_in_date_range %>%
      dplyr::filter(ProjectID == unsheltered_ProjectID) %>%
      dplyr::select(
        dplyr::all_of(vars$prep),
        RelationshipToHoH,
        LivingSituation,
        AgeAtEntry,
        Destination,
        CountyServed,
        ProjectCounty,
        LivingSituation
      )


    unsheltered_not_unsheltered <- unsheltered_enrollments %>%
      dplyr::filter(LivingSituation != 16) %>%
      dplyr::mutate(
        Type = "High Priority",
        Issue = "Wrong Provider (Not Unsheltered)",
        Guidance = "Clients who were incorrectly entered into the Unsheltered
            provider should be exited. Otherwise, correct the data. Please review
            the <a href=\"https://www.youtube.com/watch?v=qdmrqOHXoN0&t=174s\"
            target=\"_blank\">data entry portion of the Unsheltered video training</a>
            for more info.",
      ) %>%
      dplyr::select(dplyr::all_of(vars$we_want))


    # Unsheltered New Entries by County by Month ------------------------------
    # TODO SP: Add to SP Checks
    unsheltered_by_month <- unsheltered_enrollments %>%
      dplyr::left_join(Users, by = "UserCreating") %>%
      dplyr::mutate(ExitAdjust = dplyr::if_else(is.na(ExitDate), lubridate::today(), ExitDate),
                    County = dplyr::if_else(is.na(CountyServed), UserCounty, CountyServed),
                    EntryDateDisplay = format.Date(EntryDate, "%b %Y")) %>%
      dplyr::select(EntryDate, EntryDateDisplay, HouseholdID, County)
    if (by_month)
      return(unsheltered_by_month)
    else
      return(unsheltered_not_unsheltered)
  }

  # Unsheltered DQ ----------------------------------------------------------

  #' @title Generate the unsheltered Data Quality report
  #'
  #' @inherit data_quality_tables params return


  dq_sp_unsheltered <- function(app_env = get_app_env(e = rlang::caller_env())) {

    unsh_dq <- c("dq_check_disability_ssi",
                 "dq_dkr_destination",
                 "dq_dkr_months_times_homeless",
                 "dq_dkr_residence_prior",
                 "dq_dkr_LoS",
                 "dq_dob",
                 "dq_ethnicity",
                 "dq_race",
                 "dq_gender",
                 "dq_name",
                 dq_overlaps(unsh = TRUE),
                 "dq_duplicate_ees",
                 "dq_future_ees",
                 "dq_future_exits",
                 "dq_hh_too_many_hohs",
                 "dq_hh_no_hoh",
                 "dq_hh_children_only",
                 "dq_hh_missing_rel_to_hoh", #collectively hh_issues
                 "dq_sp_incorrect_ee_type",
                 "dq_internal_old_outstanding_referrals",
                 dq_ph_without_spdats(unsh = TRUE), # lh_without_spdat, spdat_on_non_hoh"
                 dq_psh_check_exit_destination, # "maybe_psh_destination"
                 "missing_approx_date_homeless",
                 "missing_destination",
                 "missing_county_served",
                 "missing_LoS",
                 "missing_months_times_homeless",
                 "missing_residence_prior",
                 dq_rrh_missing_project_stay, #"no_bos_rrh"
                 dq_psh_missing_project_stay, #"no_bos_psh"
                 dq_th_missing_project_stay, # "no_bos_th"
                 dq_sh_missing_project_stay, # "no_bos_sh"
                 dq_sp_referrals_on_hh_members, #"referrals_on_hh_members",
                 dq_psh_incorrect_destination, # "should_be_psh_destination"
                 dq_rrh_check_exit_destination, #"should_be_rrh_destination",
                 dq_th_check_exit_destination, #"should_be_th_destination",
                 dq_sh_check_exit_destination #"should_be_sh_destination",
    ) %>%
      dplyr::filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
      dplyr::left_join(Users, by = "UserCreating") %>%
      dplyr::select(-UserID,-UserName,-ProjectRegion) %>%
      dplyr::filter(
        UserCounty != "Franklin" &
          !Issue %in% c(
            "Conflicting Health Insurance yes/no at Entry",
            "Conflicting Health Insurance yes/no at Exit",
            "Conflicting Income yes/no at Entry",
            "Conflicting Income yes/no at Exit",
            "Conflicting Non-cash Benefits yes/no at Entry",
            "Conflicting Non-cash Benefits yes/no at Exit",
            "Health Insurance Missing at Entry",
            "Health Insurance Missing at Exit",
            "Income Missing at Entry",
            "Income Missing at Exit",
            "Non-cash Benefits Missing at Entry",
            "Non-cash Benefits Missing at Exit"
          )
      )  %>%
      dplyr::mutate(
        Type = dplyr::if_else(Issue == "Missing County Served", "High Priority", Type),
        Type = factor(Type, levels = c("High Priority",
                                       "Error",
                                       "Warning"))
      )
  }
}

#' @title Find missing client locations
#' @family ServicePoint Checks
#' @inherit data_quality_tables params return
#' @export
dq_sp_missing_client_location <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())

  served_in_date_range %>%
    dplyr::filter(is.na(ClientLocation),
                  RelationshipToHoH == 1) %>%
    dplyr::mutate(Type = "High Priority",
                  Issue = "Missing Client Location",
                  Guidance = guidance$missing_client_loc) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Households with Too Many Head of Household
#' @inherit data_quality_tables params return
#' @family ServicePoint Checks
#' @family DQ: Household Checks
#' @export
dq_sp_hh_too_many_hohs <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1) %>%
    dplyr::group_by(HouseholdID) %>%
    dplyr::summarise(HoHsinHousehold = dplyr::n(),
                     PersonalID = min(PersonalID)) %>%
    dplyr::filter(HoHsinHousehold > 1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
    dplyr::mutate(Issue = "Too Many Heads of Household",
                  Type = "High Priority",
                  Guidance = guidance$hh_too_many_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))


}

#' @title Find Households with Missing Relationship to Head of Household
#' @inherit data_quality_tables params return
#' @family ServicePoint Checks
#' @family DQ: Household Checks
#' @export
dq_sp_hh_missing_rel_to_hoh <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())
) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  hh_no_hoh <- dq_hh_no_hoh(served_in_date_range, vars, guidance, app_env = NULL)
  served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 99) %>%
    dplyr::anti_join(hh_no_hoh["HouseholdID"], by = "HouseholdID") %>%
    dplyr::mutate(Issue = "Missing Relationship to Head of Household",
                  Type = "High Priority",
                  Guidance = guidance$missing_rel_to_hoh) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}

#' @title Find Missing Length of Stay
#' @inherit data_quality_tables params return
#' @family ServicePoint Checks
#' @family DQ: Missing Data at Entry
#' @export
dq_sp_missing_LoS <- function(served_in_date_range, vars, guidance = NULL, app_env = get_app_env(e = rlang::caller_env())) {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  missing_LoS <- served_in_date_range %>%
    dplyr::select(dplyr::all_of(vars$prep),
                  AgeAtEntry,
                  RelationshipToHoH,
                  LengthOfStay) %>%
    dplyr::filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
                    (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
    dplyr::mutate(Issue = "Missing Length of Stay",
                  Type = "Error",
                  Guidance = guidance$missing_los) %>%
    dplyr::select(dplyr::all_of(vars$we_want))
}
