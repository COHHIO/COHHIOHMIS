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
#' @include app_dependencies.R
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



data_quality <- function(
  clarity_api = get_clarity_api(e = rlang::caller_env()),
  app_env = get_app_env(e = rlang::caller_env())
  ) {
  force(clarity_api)
  force(app_env)
  app_env$merge_deps_to_env()




  # Providers to Check ------------------------------------------------------
  projects_current_hmis <- projects_current_hmis()

  app_env$gather_deps(projects_current_hmis)
  # Clients to Check --------------------------------------------------------

  served_in_date_range <- served_in_date_range()
  app_env$gather_deps(served_in_date_range)
  # The Variables That We Want ----------------------------------------------

  vars <- list()
  vars$prep <- c(
    "HouseholdID",
    "PersonalID",
    "UniqueID",
    "ProjectName",
    "ProjectType",
    "EntryDate",
    "MoveInDateAdjust",
    "ExitDate",
    "UserCreating",
    "ProjectRegion"
  )

  vars$we_want <- c(vars$prep,
                            "Issue",
                            "Type",
                            "Guidance")
  app_env$gather_deps(vars)

  dqs <- purrr::imap(stringr::str_subset(ls(envir = .getNamespace("Rm_data"), pattern = "^dq"), "^((?!\\_sp\\_)(?!dose)(?!vaccine)(?!referrals).)*$"), ~{
    args <- names(rlang::fn_fmls(getFromNamespace(.x, "Rm_data"))) |>
      {\(x) {x[!x %in% c("app_env", "served_in_date_range")]}}()
    Rm_env$merge_deps_to_env("served_in_date_range")
                  message(.x)
                  .call <- rlang::call2(.x, !!!Rm_env$merge_deps_to_env(args, as_list = TRUE), app_env = NULL, served_in_date_range = served_in_date_range)

                  rlang::eval_bare(.call)
                })

dq_main <- dqs |>
  make_profile_link_df()
detail_eligibility <- dq_check_eligibility(detail = TRUE)



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




# TODO SP: Add to SP Checks
  # unsh_overlaps <- dq_overlaps %>%
  #   dplyr::filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
  #   dplyr::left_join(Users, by = "UserCreating") %>%
  #   dplyr::select(PersonalID,
  #                 DefaultProvider,
  #                 EntryDate,
  #                 ExitDate,
  #                 PreviousProject)

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


  # AP No Recent Referrals --------------------------------------------------
  co_APs <- Project %>%
    dplyr::filter(ProjectType == 14) %>% # not incl Mah CE
    dplyr::select(
      ProjectID,
      OperatingStartDate,
      OperatingEndDate,
      ProjectName,
      HMISParticipatingProject,
      ProjectCounty
    )

  aps_no_referrals <- Referrals %>%
    dplyr::right_join(co_APs, by = c("ReferringProjectID" = "ProjectID")) %>%
    dplyr::filter(is.na(PersonalID)) %>%
    dplyr::select(ReferringProjectID) %>%
    unique()

  aps_with_referrals <- Referrals %>%
    dplyr::right_join(co_APs, by = c("ReferringProjectID" = "ProjectID")) %>%
    dplyr::filter(!is.na(PersonalID)) %>%
    dplyr::select(ReferringProjectID) %>%
    unique()

  data_APs <- dplyr::data.frame(
    category = c("No Referrals", "Has Created Referrals"),
    count = c(nrow(aps_no_referrals), nrow(aps_with_referrals)),
    providertype = rep("Access Points"),
    total = rep(c(
      nrow(aps_no_referrals) + nrow(aps_with_referrals)
    )),
    stringsAsFactors = FALSE
  )

  data_APs <- data_APs %>%
    dplyr::mutate(percent = count / total,
                  prettypercent = scales::percent(count / total))

  dq_plot_aps_referrals <-
    ggplot2::ggplot(data_APs, ggplot2::aes(fill = category, x = providertype, y = percent)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity",
                      width = .1) +
    ggplot2::geom_label(
      ggplot2::aes(label = paste(
        data_APs$category,
        "\n",
        data_APs$prettypercent
      )),
      position = ggplot2::position_stack(),
      vjust = 2,
      fill = "white",
      colour = "black",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = c("#00952e", "#a11207"), guide = FALSE) +
    ggplot2::theme_void()



  rm(aps_with_referrals, co_APs)



  # Side Door ---------------------------------------------------------------
  # use Referrals, get logic from ART report- it's pretty lax I think


  # Old Outstanding Referrals -----------------------------------------------
  # CW says ReferringProjectID should work instead of Referred-From Provider
  # Using ReferringProjectID instead. Either way, I feel this should go in the
  # Provider Dashboard, not the Data Quality report.
# TODO Refresh this once ReferralOutcome is figured out in Clarity  2021-09-03
#internal_old_outstanding_referrals <- dq_internal_old_outstanding_referrals(served_in_date_range, Referrals, vars)

  # ^^this is pulling in neither the Unsheltered NOR referrals from APs

  staging_outstanding_referrals <-
    internal_old_outstanding_referrals %>%
    dplyr::left_join(Project[c("ProjectName", "ProjectID")], by = "ProjectName") %>%
    dplyr::select(ProjectName, ProjectID, PersonalID) %>%
    dplyr::group_by(ProjectName, ProjectID) %>%
    dplyr::summarise(Open_Referrals = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(Open_Referrals)) %>%
    dplyr::mutate(Project = paste0(ProjectName, ":", ProjectID))




  # Unsheltered Incorrect Residence Prior -----------------------------------
  unsheltered_enrollments <- served_in_date_range %>%
    dplyr::filter(ProjectID == 1695) %>%
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

  # TODO SP: Add to SP Checks
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


  # SSVF --------------------------------------------------------------------

  ssvf_served_in_date_range <- Enrollment %>%
    dplyr::select(
      EnrollmentID,
      HouseholdID,
      PersonalID,
      ProjectName,
      ProjectType,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      UserCreating,
      RelationshipToHoH,
      PercentAMI,
      LastPermanentStreet,
      LastPermanentCity,
      LastPermanentState,
      LastPermanentZIP,
      AddressDataQuality,
      VAMCStation,
      HPScreeningScore,
      ThresholdScore,
      IraqAfghanistan,
      FemVet
    ) %>%
    dplyr::right_join(
      served_in_date_range %>%
        dplyr::filter(GrantType == "SSVF") %>%
        dplyr::select(PersonalID, EnrollmentID, HouseholdID, ProjectRegion),
      by = c("PersonalID", "EnrollmentID", "HouseholdID")
    ) %>%
    dplyr::left_join(
      Client %>%
        dplyr::select(
          PersonalID,
          VeteranStatus,
          YearEnteredService,
          YearSeparated,
          WorldWarII,
          KoreanWar,
          VietnamWar,
          DesertStorm,
          AfghanistanOEF,
          IraqOIF,
          IraqOND,
          OtherTheater,
          MilitaryBranch,
          DischargeStatus
        ),
      by = "PersonalID"
    )

  veteran_missing_year_entered <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(YearEnteredService) ~ "Missing Year Entered Service",
        YearEnteredService > lubridate::year(lubridate::today()) ~ "Incorrect Year Entered Service"),
      Type = "Error",
      Guidance = guidance$missing_at_entry
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_year_separated <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        is.na(YearSeparated) ~ "Missing Year Separated",
        YearSeparated > lubridate::year(lubridate::today()) ~ "Incorrect Year Separated"),
      Type = "Error",
      Guidance = guidance$missing_at_entry
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_wars <- ssvf_served_in_date_range %>%
    dplyr::filter(
      VeteranStatus == 1 &
        (
          is.na(WorldWarII) | WorldWarII == 99 |
            is.na(KoreanWar) | KoreanWar == 99 |
            is.na(VietnamWar) | VietnamWar == 99 |
            is.na(DesertStorm) | DesertStorm == 99 |
            is.na(AfghanistanOEF) | AfghanistanOEF == 99 |
            is.na(IraqOIF) | IraqOIF == 99 |
            is.na(IraqOND) | IraqOND == 99 |
            is.na(OtherTheater) |
            OtherTheater == 99
        )
    ) %>%
    dplyr::mutate(Issue = "Missing War(s)",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_branch <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1 &
                    is.na(MilitaryBranch)) %>%
    dplyr::mutate(Issue = "Missing Military Branch",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  veteran_missing_discharge_status <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1 & is.na(DischargeStatus)) %>%
    dplyr::mutate(Issue = "Missing Discharge Status",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  dkr_client_veteran_info <- ssvf_served_in_date_range %>%
    dplyr::filter(VeteranStatus == 1) %>%
    dplyr::mutate(
      Issue = dplyr::case_when(
        WorldWarII %in% c(8, 9) |
          KoreanWar %in% c(8, 9) |
          VietnamWar %in% c(8, 9) |
          DesertStorm  %in% c(8, 9) |
          AfghanistanOEF %in% c(8, 9) |
          IraqOIF %in% c(8, 9) |
          IraqOND %in% c(8, 9) |
          OtherTheater  %in% c(8, 9)  ~ "Don't Know/Refused War(s)",
        MilitaryBranch %in% c(8, 9) ~ "Missing Military Branch",
        DischargeStatus %in% c(8, 9) ~ "Missing Discharge Status"
      ),
      Type = "Warning",
      Guidance = guidance$dkr_data
    ) %>%
    dplyr::filter(!is.na(Issue)) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  ssvf_missing_percent_ami <- ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    is.na(PercentAMI)) %>%
    dplyr::mutate(Issue = "Missing Percent AMI",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  ssvf_missing_vamc <- ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    is.na(VAMCStation)) %>%
    dplyr::mutate(Issue = "Missing VAMC Station Number",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

  ssvf_missing_address <- ssvf_served_in_date_range %>%
    dplyr::filter(RelationshipToHoH == 1 &
                    (
                      is.na(LastPermanentStreet) |
                        is.na(LastPermanentCity) |
                        # is.na(LastPermanentState) | # still not fixed in export
                        is.na(LastPermanentZIP)
                    )) %>%
    dplyr::mutate(Issue = "Missing Some or All of Last Permanent Address",
                  Type = "Error",
                  Guidance = guidance$missing_at_entry) %>%
    dplyr::select(dplyr::all_of(vars$we_want))

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


  # All together now --------------------------------------------------------


  dq_main <- rbind(
    aps_with_ees,
    check_disability_ssi,
    check_eligibility,
    conflicting_disabilities,
    conflicting_health_insurance_entry,
    conflicting_health_insurance_exit,
    conflicting_income_entry,
    conflicting_income_exit,
    conflicting_ncbs_entry,
    conflicting_ncbs_exit,
    differing_manufacturers,
    dkr_client_veteran_info,
    dkr_destination,
    dkr_living_situation,
    dkr_LoS,
    dkr_months_times_homeless,
    dkr_residence_prior,
    dose_date_error,
    dose_date_warning,
    dq_dob,
    dq_ethnicity,
    dq_gender,
    dq_name,
    dq_overlaps %>% dplyr::select(-PreviousProject),
    dq_race,
    dq_ssn,
    dq_veteran,
    duplicate_ees,
    entered_ph_without_spdat,
    extremely_long_stayers,
    future_ees,
    future_exits,
    hh_issues,
    incorrect_ee_type,
    incorrect_path_contact_date,
    internal_old_outstanding_referrals,
    invalid_months_times_homeless,
    lh_without_spdat,
    mahoning_ce_60_days,
    maybe_psh_destination,
    # maybe_rrh_destination,
    missing_approx_date_homeless,
    missing_client_location,
    missing_county_served,
    missing_county_prior,
    missing_destination,
    missing_disabilities,
    missing_health_insurance_entry,
    missing_health_insurance_exit,
    missing_income_entry,
    missing_income_exit,
    # missing_interims,
    missing_living_situation,
    missing_LoS,
    missing_months_times_homeless,
    missing_path_contact,
    missing_previous_street_ESSH,
    missing_ncbs_entry,
    missing_ncbs_exit,
    missing_residence_prior,
    missing_vaccine_current,
    missing_vaccine_exited,
    no_bos_rrh,
    no_bos_psh,
    no_bos_th,
    no_bos_sh,
    path_enrolled_missing,
    path_missing_los_res_prior,
    path_no_status_at_exit,
    path_reason_missing,
    path_SOAR_missing_at_exit,
    path_status_determination,
    referrals_on_hh_members,
    referrals_on_hh_members_ssvf,
    rent_paid_no_move_in,
    services_on_hh_members,
    services_on_hh_members_ssvf,
    should_be_psh_destination,
    should_be_rrh_destination,
    should_be_th_destination,
    should_be_sh_destination,
    spdat_on_non_hoh,
    ssvf_missing_address,
    ssvf_missing_vamc,
    ssvf_missing_percent_ami,
    # ssvf_hp_screen,
    unknown_manufacturer_error,
    unknown_manufacturer_warning,
    unlikely_ncbs_entry,
    veteran_missing_year_entered,
    veteran_missing_year_separated,
    veteran_missing_wars,
    veteran_missing_branch,
    veteran_missing_discharge_status
  ) %>%
    dplyr::filter(!ProjectName %in% c(
      "Diversion from Homeless System",
      "Unsheltered Clients - OUTREACH"
    ))

  dq_main <- dq_main %>%
    unique()   %>%
    dplyr::mutate(Type = factor(Type, levels = c("High Priority",
                                                 "Error",
                                                 "Warning")))

  # filtering out AP errors that are irrlevant to APs

  dq_main <- dq_main %>%
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

  # # Waiting on Something ----------------------------------------------------
  #
  #     dq_main <- dq_main %>%
  #       filter(
  #         !Issue %in% c(
  #           "Missing PATH Contact", # waiting on AW comments
  #           "No Contact End Date (PATH)", # waiting on AW comments
  #           "No PATH Contact Entered at Entry" # waiting on AW comments
  #         )
  #       )

  # Unsheltered DQ ----------------------------------------------------------

  dq_unsheltered <- rbind(
    check_disability_ssi,
    dkr_destination,
    dkr_months_times_homeless,
    dkr_residence_prior,
    dkr_LoS,
    dq_dob,
    dq_ethnicity,
    dq_race,
    dq_gender,
    dq_name,
    dq_overlaps %>% dplyr::select(-PreviousProject),
    duplicate_ees,
    future_ees,
    future_exits,
    hh_issues,
    incorrect_ee_type,
    internal_old_outstanding_referrals,
    lh_without_spdat,
    maybe_psh_destination,
    # maybe_rrh_destination,
    missing_approx_date_homeless,
    missing_destination,
    missing_county_served,
    missing_LoS,
    missing_months_times_homeless,
    missing_residence_prior,
    no_bos_rrh,
    no_bos_psh,
    no_bos_th,
    no_bos_sh,
    referrals_on_hh_members,
    should_be_psh_destination,
    should_be_rrh_destination,
    should_be_th_destination,
    should_be_sh_destination,
    spdat_on_non_hoh,
    unsheltered_not_unsheltered,
    unsheltered_long_not_referred
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
    )

  dq_unsheltered <- dq_unsheltered %>%
    dplyr::mutate(
      Type = dplyr::if_else(Issue == "Missing County Served", "High Priority", Type),
      Type = factor(Type, levels = c("High Priority",
                                             "Error",
                                             "Warning"))
    )

  # Controls what is shown in the CoC-wide DQ tab ---------------------------

  # for CoC-wide DQ tab

  dq_past_year <- dq_main %>%
    HMIS::served_between(rm_dates$hc$check_dq_back_to,
                         lubridate::today()) |>
    dplyr::left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")

  # for project evaluation reporting

  dq_for_pe <- dq_main  |>
    HMIS::served_between(rm_dates$hc$project_eval_start, rm_dates$hc$project_eval_end)
    dplyr::left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")


  dq_providers <- sort(projects_current_hmis$ProjectName)


  # Clean up the house ------------------------------------------------------

  rm(
    aps_with_ees,
    check_disability_ssi,
    check_eligibility,
    conflicting_disabilities,
    conflicting_health_insurance_entry,
    conflicting_health_insurance_exit,
    conflicting_income_entry,
    conflicting_income_exit,
    conflicting_ncbs_entry,
    conflicting_ncbs_exit,
    # detail_eligibility, # the app needs this; keep this commented out
    detail_missing_disabilities,
    dkr_living_situation,
    dkr_months_times_homeless,
    dkr_residence_prior,
    dkr_destination,
    dkr_LoS,
    dkr_client_veteran_info,
    dq_data_unsheltered_high,
    dq_dob,
    dq_ethnicity,
    dq_gender,
    dq_name,
    dq_race,
    dq_ssn,
    dq_veteran,
    duplicate_ees,
    entered_ph_without_spdat,
    extremely_long_stayers,
    future_ees,
    future_exits,
    hh_issues,
    incorrect_ee_type,
    incorrect_path_contact_date,
    missing_path_contact,
    internal_old_outstanding_referrals,
    lh_without_spdat,
    missing_approx_date_homeless,
    missing_client_location,
    missing_county_prior,
    missing_county_served,
    missing_destination,
    missing_disabilities,
    missing_health_insurance_entry,
    missing_health_insurance_exit,
    missing_income_entry,
    missing_income_exit,
    invalid_months_times_homeless,
    missing_living_situation,
    missing_LoS,
    missing_months_times_homeless,
    missing_ncbs_entry,
    missing_ncbs_exit,
    missing_previous_street_ESSH,
    missing_residence_prior,
    path_enrolled_missing,
    path_missing_los_res_prior,
    path_no_status_at_exit,
    path_reason_missing,
    path_SOAR_missing_at_exit,
    path_status_determination,
    projects_current_hmis,
    referrals_on_hh_members,
    referrals_on_hh_members_ssvf,
    rent_paid_no_move_in,
    served_in_date_range,
    services_on_hh_members,
    services_on_hh_members_ssvf,
    project_small,
    spdat_on_non_hoh,
    ssvf_missing_address,
    ssvf_missing_vamc,
    ssvf_missing_percent_ami,
    ssvf_served_in_date_range,
    staging_outstanding_referrals,
    stray_services_warning,
    unlikely_ncbs_entry,
    unsheltered_enrollments,
    unsheltered_not_unsheltered,
    unsheltered_long_not_referred,
    va_funded,
    vars$prep,
    vars$we_want,
    veteran_missing_year_entered,
    veteran_missing_year_separated,
    veteran_missing_wars,
    veteran_missing_branch,
    veteran_missing_discharge_status
  )
  rm(list = ls(pattern = "dq_data_"))
  rm(list = ls(pattern = "guidance$"))

  # WARNING save.image does not save the environment properly, save must be used.
app_env$gather_deps()
app_env
}
