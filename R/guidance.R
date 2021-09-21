list(
    dq_aps_with_ees = list(
        list(
            Issue = "Access Point with Entry Exits",
            Type = "High Priority",
            Guidance = "Access Points should only be entering Referrals and Diversion Services into the AP provider- not Entry Exits. If a user has done this, the Entry Exit should be deleted. Please see the <a href='http://hmis.cohhio.org/index.php?pg=kb.page&id=151' target='_blank'>Coordinated Entry workflow</a>."
        )
    ),
    dq_check_disability_ssi = list(
        list(
            Issue = "Client with No Disability Receiving SSI/SSDI (could be ok)",
            Type = "Warning",
            Guidance = "If a client is receiving SSI or SSDI for THEIR OWN disability, that disability should be indicated in the Disabilities data elements. If an adult is receiving SSI or SSDI benefits on behalf a minor child, then there is no action needed."
        )
    ),
    dq_check_eligibility = list(
        list(
            Issue = "Check Eligibility",
            Type = "Warning",
            Guidance = "Your Residence Prior data suggests that this project is either serving ineligible households, the household was entered into the wrong project, or the Residence Prior data at Entry is incorrect. Please check the terms of your grant or speak with your CoC Team Coordinator if you are unsure of eligibility criteria for your project type."
        )
    ),
    dq_conflicting_hi_ee = list(
        list(
            Issue = "Conflicting Health Insurance yes/no at Entry",
            Type = "Error",
            Guidance = "If the user answered 'Yes' to 'Covered by Health Insurance?', then there should be a Health Insurance subassessment where it indicates which type of health insurance the client is receiving. Similarly if the user answered 'No', there should not be any Health Insurance records that say the client is receiving that type of Health Insurance."
        ),
        list(
            Issue = "Conflicting Health Insurance yes/no at Exit",
            Type = "Error",
            Guidance = "If the user answered 'Yes' to 'Covered by Health Insurance?', then there should be a Health Insurance subassessment where it indicates which type of health insurance the client is receiving. Similarly if the user answered 'No', there should not be any Health Insurance records that say the client is receiving that type of Health Insurance."
        )
    ),
    dq_conflicting_income = list(
        list(
            Issue = "Conflicting Income yes/no at Entry",
            Type = "Error",
            Guidance = "If the user answered 'Yes' to 'Income from any source', then there should be an income subassessment where it indicates which type of income the client is receiving. Similarly if the user answered 'No', there should not be any income records that say the client is receiving that type of income."
        ),
        list(
            Issue = "Conflicting Income yes/no at Exit",
            Type = "Error",
            Guidance = "If the user answered 'Yes' to 'Income from any source', then there should be an income subassessment where it indicates which type of income the client is receiving. Similarly if the user answered 'No', there should not be any income records that say the client is receiving that type of income."
        )
    ),
    dq_conflicting_unlikely_ncbs = list(
        list(
            Issue = "Client has ALL SIX Non-cash Benefits at Entry",
            Type = "Warning",
            Guidance = "This client has every single Non-Cash Benefit, according to HMIS, which is highly unlikely. Please correct (unless it's actually true)."
        ),
        list(
            Issue = "Conflicting Non-cash Benefits yes/no at Entry",
            Type = "Error",
            Guidance = "If the user answered 'Yes' to 'Non-cash benefits from any source', then there should be a Non-cash benefits subassessment where it indicates which type of income the client is receiving. Similarly if the user answered 'No', then there should not be any non-cash records that say the client is receiving that type of benefit"
        ),
        list(
            Issue = "Conflicting Non-cash Benefits yes/no at Exit",
            Type = "Error",
            Guidance = "If the user answered 'Yes' to 'Non-cash benefits from any source', then there should be a Non-cash benefits subassessment where it indicates which type of income the client is receiving. Similarly if the user answered 'No', then there should not be any non-cash records that say the client is receiving that type of benefit"
        )
    ),
    dq_detail_missing_disabilities = list(
        list(
            Issue = "Missing Disabling Condition",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        ),
        list(
            Issue = "Conflicting Disability of Long Duration yes/no",
            Type = "Error",
            Guidance = "If the user answered 'Yes' to the 'Does the client have a disabling condition?', then there should be a disability subassessment that indicates the disability determination is Yes *and* the 'If yes,... long duration' question is Yes. Similarly if the user answered 'No', the client should not have any disability subassessments that indicate that they do have a Disabling Condition."
        )
    ),
    dq_dkr_destination = list(
        list(
            Issue = "Don't Know/Refused Destination",
            Type = "Warning",
            Guidance = "It is widely understood that not every client will be able to or consent to answer every question in every assessment. If you do have any of this data, but it is just not entered into HMIS yet, please enter it. If you can reasonably attempt again to collect this data from the client (like if they are still in your project), then please do so. Otherwise, there is no action needed."
        )
    ),
    dq_dkr_living_situation = list(
        list(
            Issue = "Don't Know/Refused Living Situation",
            Type = "Warning",
            Guidance = "It is widely understood that not every client will be able to or consent to answer every question in every assessment. If you do have any of this data, but it is just not entered into HMIS yet, please enter it. If you can reasonably attempt again to collect this data from the client (like if they are still in your project), then please do so. Otherwise, there is no action needed."
        )
    ),
    dq_dkr_LoS = list(
        list(
            Issue = "Don't Know/Refused Residence Prior",
            Type = "Warning",
            Guidance = "It is widely understood that not every client will be able to or consent to answer every question in every assessment. If you do have any of this data, but it is just not entered into HMIS yet, please enter it. If you can reasonably attempt again to collect this data from the client (like if they are still in your project), then please do so. Otherwise, there is no action needed."
        )
    ),
    dq_dkr_months_times_homeless = list(
        list(
            Issue = "Don't Know/Refused Months or Times Homeless",
            Type = "Warning",
            Guidance = "It is widely understood that not every client will be able to or consent to answer every question in every assessment. If you do have any of this data, but it is just not entered into HMIS yet, please enter it. If you can reasonably attempt again to collect this data from the client (like if they are still in your project), then please do so. Otherwise, there is no action needed."
        )
    ),
    dq_dkr_residence_prior = list(
        list(
            Issue = "Don't Know/Refused Residence Prior",
            Type = "Warning",
            Guidance = "It is widely understood that not every client will be able to or consent to answer every question in every assessment. If you do have any of this data, but it is just not entered into HMIS yet, please enter it. If you can reasonably attempt again to collect this data from the client (like if they are still in your project), then please do so. Otherwise, there is no action needed."
        )
    ),
    dq_dob = list(
        list(
            Issue = "dplyr::case_when(is.na(DOB) & DOBDataQuality %in% c(1, 2) ~\n  \"Missing DOB\", DOBDataQuality == 99 ~\n  \"Missing Date of Birth Data Quality\", DOBDataQuality %in% c(2, 8, 9) ~\n  \"Don't Know/Refused or Approx. Date of Birth\", AgeAtEntry < 0 |\n  AgeAtEntry > 95 ~ \"Incorrect Date of Birth or Entry Date\")",
            Type = "dplyr::case_when(Issue %in% c(\"Missing DOB\",\n  \"Incorrect Date of Birth or Entry Date\",\n  \"Missing Date of Birth Data Quality\") ~ \"Error\", Issue ==\n  \"Don't Know/Refused or Approx. Date of Birth\" ~ \"Warning\")",
            Guidance = "dplyr::case_when(Issue == \"Incorrect Date of Birth or Entry Date\" ~\n  \"The HMIS data is indicating the client entered the project PRIOR to\\n      being born. Correct either the Date of Birth or the Entry Date, whichever\\n      is incorrect.\",\nIssue %in% c(\"Missing DOB\", \"Missing Date of Birth Data Quality\") ~\n  guidance$missing_at_entry, Issue ==\n  \"Don't Know/Refused or Approx. Date of Birth\" ~ guidance$dkr_data)"
        )
    ),
    dq_dose_date_error = list(
        list(
            Type = "Error",
            Issue = "Vaccine Date Incorrect",
            Guidance = "Vaccination date precedes the vaccine being available in the US."
        )
    ),
    dq_duplicate_ees = list(
        list(
            Issue = "Duplicate Entry Exits",
            Type = "High Priority",
            Guidance = "Users sometimes create this error when they forget to click into a program stay by using the Entry pencil, and instead they click 'Add Entry/Exit' each time. To correct, EDA to the project the Entry/Exit belongs to, navigate to the Entry/Exit tab and delete the program stay that was accidentally added for each household member."
        )
    ),
    dq_ethnicity = list(
        list(
            Issue = "dplyr::case_when(Ethnicity == 99 ~ \"Missing Ethnicity\", Ethnicity %in%\n  c(8, 9) ~ \"Don't Know/Refused Ethnicity\")",
            Type = "dplyr::case_when(Issue == \"Missing Ethnicity\" ~ \"Error\", Issue ==\n  \"Don't Know/Refused Ethnicity\" ~ \"Warning\")",
            Guidance = "dplyr::if_else(Type == \"Warning\", guidance$dkr_data, guidance$\n  missing_at_entry)"
        )
    ),
    dq_future_ees = list(
        list(
            Issue = "Future Entry Date",
            Type = "Warning",
            Guidance = "Users should not be entering a client into a project on a date in the future. If the Entry Date is correct, there is no action needed, but going forward, please be sure that your data entry workflow is correct according to your project type."
        )
    ),
    dq_future_exits = list(
        list(
            Issue = "Future Exit Date",
            Type = "Error",
            Guidance = "This client's Exit Date is a date in the future. Please enter the exact date the client left your program. If this client has not yet exited, delete the Exit and then enter the Exit Date once the client is no longer in your program."
        )
    ),
    dq_gender = list(
        list(
            Issue = "dplyr::case_when(Gender == 99 ~ \"Missing Gender\", Gender %in% c(8, 9) ~\n  \"Don't Know/Refused Gender\")",
            Type = "dplyr::case_when(Issue == \"Missing Gender\" ~ \"Error\", Issue ==\n  \"Don't Know/Refused Gender\" ~ \"Warning\")",
            Guidance = "dplyr::if_else(Type == \"Warning\", guidance$dkr_data, guidance$\n  missing_at_entry)"
        )
    ),
    dq_hh_children_only = list(
        list(
            Issue = "Children Only Household",
            Type = "High Priority",
            Guidance = "Unless your project serves youth younger than 18\n         exclusively, every household should have at least one adult in it. If\n         you are not sure how to correct this, please contact the HMIS team for\n         help."
        )
    ),
    dq_hh_missing_rel_to_hoh = list(
        list(
            Issue = "Missing Relationship to Head of Household",
            Type = "High Priority",
            Guidance = "Check inside the Entry pencil to be sure each household member has\n          \"Relationship to Head of Household\" answered and that only one of\n          them says \"Self (head of household)\"."
        )
    ),
    dq_hh_no_oh = list(
        list(
            Issue = "No Head of Household",
            Type = "High Priority",
            Guidance = "Please be sure all members of the household are included in the program\n        stay, and that each household member's birthdate is correct. If those\n        things are both true, or the client is a single, check inside the Entry\n        pencil to be sure each household member has \"Relationship to Head of\n        Household\" answered and that one of them says Self (head of household).\n        Singles are always Self (head of household)."
        )
    ),
    dq_hh_too_many_hohs = list(
        list(
            Issue = "Too Many Heads of Household",
            Type = "High Priority",
            Guidance = "Check inside the Entry pencil to be sure each household member has\n        \"Relationship to Head of Household\" answered and that only one of\n        them says \"Self (head of household)\"."
        )
    ),
    dq_incorrect_path_contact_date = list(
        list(
            Issue = "No PATH Contact Entered at Entry",
            Type = "Error",
            Guidance = "Every adult or head of household should have a Living Situation contact record where the Contact Date matches the Entry Date. This would represent the initial contact made with the client."
        )
    ),
    dq_internal_old_outstanding_referrals = list(
        list(
            Issue = "Old Outstanding Referral",
            Type = "Warning",
            Guidance = "Referrals should be closed in about 2 weeks. Please be sure you are\n      following up with any referrals and helping the client to find permanent\n      housing. Once a Referral is made, the receiving agency should be saving\n      the \"Referral Outcome\" once it is known. If you have Referrals that are\n      legitimately still open after 2 weeks because there is a lot of follow\n      up going on, no action is needed since the HMIS data is accurate."
        )
    ),
    dq_invalid_months_times_homeless = list(
        list(
            Issue = "dplyr::case_when(MonthDiff <= 0 ~\n  \"Homelessness Start Date Later Than Entry\",\nMonthsHomelessPastThreeYears < 100 ~\n  \"Number of Months Homeless Can Be Determined\", DateMonthsMismatch ==\n1 ~ \"Invalid Homelessness Start Date/Number of Months Homeless\")",
            Type = "Warning",
            Guidance = "dplyr::case_when(MonthDiff <= 0 ~\n  \"This client has an Approximate Date Homeless in their Entry that is after\\n          their Entry Date. The information in the Entry should reflect the\\n          client's situation at the point of Entry, so this date may have been\\n          incorrectly entered.\",\nMonthsHomelessPastThreeYears < 100 ~\n  \"According to this client's entry, they experienced a single episode of\\n          homelessness in the three years prior to their entry and the approximate\\n          start date of their homelessness is known, but there was no response\\n          entered for the number of months they experienced homelessness prior to\\n          this entry. It should be possible to determine and enter the number of\\n          months homeless based on the Approximate Date Homeless and the Entry Date.\",\nDateMonthsMismatch == 1 ~\n  \"According to this client's entry, they experienced a single episode of\\n          homelessness in the three years prior to their entry and the approximate\\n          start date of their homelessness is known, but the recorded number of\\n          months they experienced homelessness prior to this entry is inconsistent\\n          with the given dates. Please double-check this information for\\n          consistency and accuracy.\")"
        )
    ),
    dq_mahoning_ce_60_days = list(
        list(
            Issue = "60 Days in Mahoning Coordinated Entry",
            Type = "Warning",
            Guidance = "If this household is 'unreachable' as defined in the Mahoning County Coordinated Entry Policies and Procedures, they should be exited."
        )
    ),
    dq_missing_approx_date_homeless = list(
        list(
            Issue = "Missing Approximate Date Homeless",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        )
    ),
    dq_missing_client_location = list(
        list(
            Type = "High Priority",
            Issue = "Missing Client Location",
            Guidance = "If Client Location is missing, this household will be\n         excluded from all HUD reporting."
        )
    ),
    dq_missing_county_prior = list(
        list(
            Issue = "Missing County of Prior Residence",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        )
    ),
    dq_missing_county_served = list(
        list(
            Issue = "Missing County Served",
            Type = "Error",
            Guidance = "County Served must be collected at Entry for all clients. County is very important so that the client is prioritized into the correct service areas for various housing solutions. This can be corrected through the Entry pencil."
        )
    ),
    dq_missing_destination = list(
        list(
            Issue = "Missing Destination",
            Type = "Warning",
            Guidance = "It is widely understood that not every client will complete an exit interview, especially for high-volume emergency shelters. A few warnings for Missing Destination is no cause for concern, but if there is a large number, please contact your CoC Team Coordinator"
        )
    ),
    dq_missing_hi_entry = list(
        list(
            Issue = "Health Insurance Missing at Entry",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        )
    ),
    dq_missing_hi_exit = list(
        list(
            Issue = "Health Insurance Missing at Exit",
            Type = "Error",
            Guidance = "Please enter the data for this item by clicking into the Exit pencil on the given Client ID on the appropriate program stay."
        )
    ),
    dq_missing_income = list(
        list(
            Issue = "Income Missing at Entry",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        ),
        list(
            Issue = "Income Missing at Exit",
            Type = "Error",
            Guidance = "Please enter the data for this item by clicking into the Exit pencil on the given Client ID on the appropriate program stay."
        )
    ),
    dq_missing_living_situation = list(
        list(
            Issue = "Incomplete Living Situation Data",
            Type = "Error",
            Guidance = "When responding to the Living Situation questions in your Entry Assessment, users must answer questions about some clients' situation prior to the 'Residence Prior' that are important to help determine that client's Chronicity. Please answer these questions to the best of your knowledge."
        )
    ),
    dq_missing_LoS = list(
        list(
            Issue = "Missing Length of Stay",
            Type = "Error",
            Guidance = "This data element may be answered with an old value or it\n         may simply be missing. If the value selected is \"One week or less (HUD)\",\n         you will need to change that value to either \"One night or less (HUD)\"\n         or \"Two to six nights (HUD)\"."
        )
    ),
    dq_missing_months_times_homeless = list(
        list(
            Issue = "Missing Months or Times Homeless",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        )
    ),
    dq_missing_ncbs = list(
        list(
            Issue = "Non-cash Benefits Missing at Entry",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        ),
        list(
            Issue = "Non-cash Benefits Missing at Exit",
            Type = "Error",
            Guidance = "Please enter the data for this item by clicking into the Exit pencil on the given Client ID on the appropriate program stay."
        )
    ),
    dq_missing_path_contact = list(
        list(
            Issue = "Missing PATH Contact",
            Type = "High Priority",
            Guidance = "Every adult or Head of Household must have a Living Situation contact record. If you see a record there but there is no Date of Contact, saving the Date of Contact will correct this issue."
        )
    ),
    dq_missing_previous_street_ESSH = list(
        list(
            Issue = "Missing Previously From Street, ES, or SH (Length of Time Homeless questions)",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        )
    ),
    dq_missing_residence_prior = list(
        list(
            Issue = "Missing Residence Prior",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        )
    ),
    dq_missing_vaccine_current = list(
        list(
            Type = "Error",
            Issue = "Vaccine data not collected on current client",
            Guidance = "Client was literally homeless on Feb 5th, 2021 or later and is\n    missing their vaccine data. Because the client has not exited the project,\n    this data can still be collected. Please see\n    <a href = \"https://cohhio.org/boscoc/covid19/\" target = \"blank\">\n    for more information</a>."
        )
    ),
    dq_missing_vaccine_exited = list(
        list(
            Type = "Warning",
            Issue = "Vaccine data not collected and client has exited",
            Guidance = "Client was literally homeless on Feb 5th, 2021 or later and\n         is missing their vaccine data, and the client has exited the project.\n         If you are unable to follow up with the client, leave the client as is.\n         Please see the guidance\n         <a href = \"https://cohhio.org/boscoc/covid19/\" target = \"blank\">\n         for more information</a>."
        )
    ),
    dq_name = list(
        list(
            Issue = "dplyr::case_when(FirstName == \"Missing\" ~ \"Missing Name Data Quality\",\nFirstName %in% c(\"DKR\", \"Partial\") ~\n  \"Incomplete or Don't Know/Refused Name\")",
            Type = "dplyr::case_when(Issue == \"Missing Name Data Quality\" ~ \"Error\",\nIssue == \"Incomplete or Don't Know/Refused Name\" ~ \"Warning\")",
            Guidance = "dplyr::if_else(Type == \"Warning\", guidance$dkr_data, guidance$\n  missing_pii)"
        )
    ),
    dq_overlaps = list(
        list(
            Issue = "Overlapping Project Stays",
            Type = "High Priority",
            Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not. Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the 'Previous Provider's' mistake, but if you are seeing clients here, it means your project stay was entered last. If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected or send an email to hmis@cohhio.org if you cannot get it resolved. These clients will NOT show on their Data Quality app. If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed."
        )
    ),
    dq_overlaps_psh = list(
        list(
            Issue = "Overlapping Project Stays",
            Type = "High Priority",
            Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not. Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the 'Previous Provider's' mistake, but if you are seeing clients here, it means your project stay was entered last. If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected or send an email to hmis@cohhio.org if you cannot get it resolved. These clients will NOT show on their Data Quality app. If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed."
        )
    ),
    dq_overlaps_rrh = list(
        list(
            Issue = "Overlapping Project Stays",
            Type = "High Priority",
            Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not. Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the 'Previous Provider's' mistake, but if you are seeing clients here, it means your project stay was entered last. If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected or send an email to hmis@cohhio.org if you cannot get it resolved. These clients will NOT show on their Data Quality app. If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed."
        )
    ),
    dq_overlaps_same_day = list(
        list(
            Issue = "Overlapping Project Stays",
            Type = "High Priority",
            Guidance = "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not. Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the 'Previous Provider's' mistake, but if you are seeing clients here, it means your project stay was entered last. If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected or send an email to hmis@cohhio.org if you cannot get it resolved. These clients will NOT show on their Data Quality app. If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed."
        )
    ),
    dq_path_enrolled_missing = list(
        list(
            Issue = "Missing PATH Enrollment at Exit",
            Type = "Error",
            Guidance = "Please enter the data for this item by clicking into the Entry or Exit pencil and creating an Interim. In the assessment, enter the correct PATH Enrollment Date and Save."
        )
    ),
    dq_path_missing_los_res_prior = list(
        list(
            Issue = "Missing Residence Prior Length of Stay (PATH)",
            Type = "Error",
            Guidance = "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."
        )
    ),
    dq_path_no_status_at_exit = list(
        list(
            Issue = "PATH Status at Exit Missing or Incomplete",
            Type = "Error",
            Guidance = "Please enter the data for this item by clicking into the Exit pencil on the given Client ID on the appropriate program stay."
        )
    ),
    dq_path_reason_missing = list(
        list(
            Issue = "Missing Reason Not PATH Enrolled",
            Type = "Error",
            Guidance = "The user has indicated the household was not enrolled into PATH, but no reason was selected."
        )
    ),
    dq_path_SOAR_missing_at_exit = list(
        list(
            Issue = "Missing Connection with SOAR at Exit",
            Type = "Error",
            Guidance = "Please enter the data for this item by clicking into the Exit pencil on the given Client ID on the appropriate program stay."
        )
    ),
    dq_path_status_determination = list(
        list(
            Issue = "Missing Date of PATH Status",
            Type = "Error",
            Guidance = "Users must indicate the PATH Status Date for any adult enrolled in PATH."
        )
    ),
    dq_ph_without_spdats = list(
        list(
            Issue = "Non-DV HoHs Entering PH or TH without SPDAT",
            Type = "Warning",
            Guidance = "Every household (besides those fleeing domestic violence) must have a VI-SPDAT score to aid with prioritization into a Transitional Housing or Permanent Housing (RRH or PSH) project."
        ),
        list(
            Issue = "HoHs in shelter for 8+ days without SPDAT",
            Type = "Warning",
            Guidance = "Any household who has been in shelter or a Safe Haven for over 8 days should be assessed with the VI-SPDAT so that they can be prioritized for Permanent Housing (RRH or PSH)."
        ),
        list(
            Issue = "SPDAT Created on a Non-Head-of-Household",
            Type = "Warning",
            Guidance = "It is very important to be sure that the VI-SPDAT score goes on the Head of Household of a given program stay because otherwise that score may not pull into any reporting. It is possible a Non Head of Household was a Head of Household in a past program stay, and in that situation, this should not be corrected unless the Head of Household of your program stay is missing their score. To correct this, you would need to completely re-enter the score on the correct client's record."
        )
    ),
    dq_project_small = list(),
    dq_psh_check_exit_destination = list(
        list(
            Issue = "Check Exit Destination (may be \"Permanent housing (other\n      than RRH)...\")",
            Type = "Warning",
            Guidance = "This household appears to have an Entry into a PSH project that overlaps their Exit from your project. Typically this means the client moved into a Permanent Supportive Housing unit after their stay with you. If that is true, the Destination should be 'Permanent housing (other than RRH) for formerly homeless persons'. If you are sure the current Destination is accurate, then please leave it the way it is."
        )
    ),
    dq_psh_incorrect_destination = list(
        list(
            Issue = "Incorrect Exit Destination (should be \"Permanent housing (other\n    than RRH)...\")",
            Type = "Error",
            Guidance = "This household appears to have a Move-In Date into a PSH project that matches their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to PSH. The correct Destination for households entering PSH from your project is 'Permanent housing (other than RRH) for formerly homeless persons'."
        )
    ),
    dq_psh_missing_project_stay = list(
        list(
            Issue = "Missing PSH Project Stay or Incorrect Destination",
            Type = "Warning",
            Guidance = "The Exit Destination for this household indicates that they exited to Permanent Supportive Housing, but there is no PSH project stay on the client. If the PSH project the household exited to is outside of the Balance of State CoC or Mahoning County CoC, then no correction is necessary. If they entered PSH in the Balance of State CoC or Mahoning County CoC, then this household is missing their PSH project stay. If they did not actually enter PSH at all, the Destination should be corrected."
        )
    ),
    dq_race = list(
        list(
            Issue = "dplyr::case_when(RaceNone == 99 ~ \"Missing Race\", RaceNone %in% c(8, 9) ~\n  \"Don't Know/Refused Race\")",
            Type = "dplyr::case_when(Issue == \"Missing Race\" ~ \"Error\", Issue ==\n  \"Don't Know/Refused Race\" ~ \"Warning\")",
            Guidance = "dplyr::if_else(Type == \"Warning\", guidance$dkr_data, guidance$\n  missing_at_entry)"
        )
    ),
    dq_referrals_on_hh_members_ssvf = list(
        list(
            Issue = "Referral on a Non Head of Household (SSVF)",
            Type = "Error",
            Guidance = "Users should not checkbox all the household members when creating a Referral. Only the Head of Household needs the Referral. It is recommended that users delete any referrals on Non Heads of Household related to this project stay so that the receiving agency does not have to deal with them and they stop showing in reporting."
        )
    ),
    dq_rent_paid_no_move_in = list(
        list(
            Issue = "Rent Payment Made, No Move-In Date",
            Type = "Error",
            Guidance = "This client does not have a valid Move-In Date, but there is at least one rent/deposit payment Service Transaction recorded for this program. Until a Move-In Date is entered, this client will continue to be counted as literally homeless while in your program. Move-in dates must be on or after the Entry Date. If a client is housed then returns to homelessness while in your program, they need to be exited from their original Entry and re-entered in a new one that has no Move-In Date until they are re-housed."
        )
    ),
    dq_rrh_check_exit_destination = list(
        list(
            Issue = "Maybe Incorrect Exit Destination (did you mean 'Rental by client, with RRH...'?)",
            Type = "Warning",
            Guidance = "This household has a Move-In Date into an RRH project that matches their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to Rapid Rehousing. If the household exited to a Destination that was not 'Rental by client', but it is a permanent destination attained through a Rapid Rehousing project, then there is no change needed. If this is not the case, then the Destination should be 'Rental by client, with RRH or equivalent subsidy'."
        )
    ),
    dq_rrh_missing_project_stay = list(
        list(
            Issue = "Missing RRH Project Stay or Incorrect Destination",
            Type = "Warning",
            Guidance = "The Exit Destination for this household indicates that they exited to Rapid Rehousing, but there is no RRH project stay on the client. If the RRH project the household exited to is outside of the Balance of State or Mahoning County CoCs, then no correction is necessary. If they received RRH services in the Balance of State CoC or Mahoning County CoC, then this household is missing their RRH project stay. If they did not actually receive RRH services at all, the Destination should be corrected."
        )
    ),
    dq_services_on_hh_members_ssvf = list(
        list(
            Issue = "Service Transaction on a Non Head of Household (SSVF)",
            Type = "Error",
            Guidance = "Users should not checkbox all the household members when creating a Service Transaction. Only the Head of Household needs a Service Transaction. Delete any extraneous Service Transactions related to this project stay."
        )
    ),
    dq_services_on_non_hoh = list(
        list(
            Issue = "Service Transaction on a Non Head of Household",
            Type = "Warning",
            Guidance = "Users should not checkbox all the household members when creating a Service Transaction. Only the Head of Household needs a Service Transaction. Delete any extraneous Service Transactions related to this project stay."
        )
    ),
    dq_sh_check_exit_destination = list(
        list(
            Issue = "Incorrect Exit Destination (should be \"Safe Haven\")",
            Type = "Error",
            Guidance = "This household appears to have an Entry into a Safe Haven that overlaps their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to a Safe Haven. The correct Destination for households entering SH from your project is 'Safe Haven'."
        )
    ),
    dq_sh_missing_project_stay = list(
        list(
            Issue = "Missing Safe Haven Project Stay or Incorrect Destination",
            Type = "Warning",
            Guidance = "The Exit Destination for this household indicates that they exited to a Safe Haven, but there is no Entry in HMIS into a Safe Haven. Keep in mind that there is only one Safe Haven in the Balance of State and they are no longer operating as of 1/1/2021. If you meant to indicate that the household exited to a Domestic Violence shelter, please select 'Emergency shelter'."
        )
    ),
    dq_sp_incorrect_ee_type = list(
        list(
            Issue = "Incorrect Entry Exit Type",
            Type = "High Priority",
            Guidance = "The user selected the wrong Entry Exit Type. To correct, click the Entry pencil and Save & Continue. The Entry Exit Type at the top can then be changed. Click \"Update\" to make this change take effect."
        )
    ),
    dq_sp_referrals_on_hh_members = list(
        list(
            Issue = "Referral on a Non Head of Household",
            Type = "Warning",
            Guidance = "Users should not checkbox all the household members when creating a Referral. Only the Head of Household needs the Referral. It is recommended that users delete any referrals on Non Heads of Household related to this project stay so that the receiving agency does not have to deal with them and they stop showing in reporting."
        )
    ),
    dq_sp_referrals_on_hh_members_ssvf = list(
        list(
            Issue = "Referral on a Non Head of Household (SSVF)",
            Type = "Error",
            Guidance = "Users should not checkbox all the household members when creating a Referral. Only the Head of Household needs the Referral. It is recommended that users delete any referrals on Non Heads of Household related to this project stay so that the receiving agency does not have to deal with them and they stop showing in reporting."
        )
    ),
    dq_sp_stray_services = list(
        list(
            Issue = "Service Not Attached to an Entry Exit",
            Type = "Warning",
            Guidance = "This Service does not fall between any project stay, so it will not show in any reporting."
        )
    ),
    dq_ssn = list(
        list(
            Issue = "dplyr::case_when(SSN == \"Missing\" ~ \"Missing SSN\", SSN == \"Invalid\" ~\n  \"Invalid SSN\", SSN == \"DKR\" ~ \"Don't Know/Refused SSN\", SSN ==\n  \"Incomplete\" ~ \"Invalid SSN\")",
            Type = "dplyr::case_when(Issue %in% c(\"Missing SSN\", \"Invalid SSN\") ~ \"Error\",\nIssue == \"Don't Know/Refused SSN\" ~ \"Warning\")",
            Guidance = "dplyr::case_when(Issue == \"Don't Know/Refused SSN\" ~ guidance$dkr_data,\nIssue == \"Missing SSN\" ~ guidance$missing_pii, Issue == \"Invalid SSN\" ~\n  \"The Social Security Number does not conform with standards set by the Social Security Administration. This includes rules like every SSN is exactly 9 digits and cannot have certain number patterns. Correct by navigating to the client's record, then clicking the Client Profile tab, then click into the Client Record pencil to correct the data.\")"
        )
    ),
    dq_th_check_exit_destination = list(
        list(
            Issue = "Incorrect Exit Destination (should be \"Transitional housing...\")",
            Type = "Error",
            Guidance = "This household appears to have an Entry into a Transitional Housing project that overlaps their Exit from your project, but the Exit Destination from your project does not indicate that the household exited to Transitional Housing. The correct Destination for households entering TH from your project is 'Transitional housing for homeless persons (including homeless youth)."
        )
    ),
    dq_th_missing_project_stay = list(
        list(
            Issue = "Missing TH Project Stay or Incorrect Destination",
            Type = "Warning",
            Guidance = "The Exit Destination for this household indicates that they exited to Transitional Housing, but there is no TH project stay on the client. If the TH project that the household exited to is outside of the Balance of State CoC or Mahoning County CoC, then no correction is necessary. If they went into a TH project in the Balance of State CoC or Mahoning County CoC, then this household is missing their TH project stay. If they did not actually enter Transitional Housing at all, the Destination should be corrected."
        )
    ),
    dq_th_stayers_bos = list(
        list(
            Issue = "Extremely Long Stayer",
            Type = "Warning",
            Guidance = "This client is showing as an outlier for Length of Stay for this project type in your CoC. Please verify that this client is still in your project. If they are, be sure there are no alternative permanent housing solutions for this client. If the client is no longer in your project, please enter their Exit Date as the closest estimation of the day they left your project."
        )
    ),
    dq_veteran = list(
        list(
            Issue = "dplyr::case_when((AgeAtEntry >= 18 | is.na(AgeAtEntry)) &\n  VeteranStatus == 99 ~ \"Missing Veteran Status\", (AgeAtEntry >= 18 |\n  is.na(AgeAtEntry)) & VeteranStatus %in% c(8, 9) ~\n  \"Don't Know/Refused Veteran Status\", (AgeAtEntry >= 18 | is.na(\n  AgeAtEntry)) & RelationshipToHoH == 1 & VeteranStatus == 0 &\n  Destination %in% c(19, 28) ~ \"Check Veteran Status for Accuracy\")",
            Type = "dplyr::case_when(Issue == \"Missing Veteran Status\" ~ \"Error\", Issue %in%\n  c(\"Don't Know/Refused Veteran Status\",\n    \"Check Veteran Status for Accuracy\") ~ \"Warning\")",
            Guidance = "dplyr::case_when(Issue == \"Check Veteran Status for Accuracy\" ~\n  \"You have indicated the\\n      household exited to a destination that only veterans are eligible for, but\\n      the head of household appears to be not a veteran. Either the Veteran\\n      Status is incorrect or the Destination is incorrect.\",\nIssue == \"Missing Veteran Status\" ~ guidance$missing_pii, Issue ==\n  \"Don't Know/Refused Veteran Status\" ~ guidance$dkr_data)"
        )
    )
)
