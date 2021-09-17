# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
guidance <- function(app_env = get_app_env(e = rlang::caller_env())) {
  force(app_env)

  guidance = list()
  guidance$conflicting_hi <-
    "If the user answered 'Yes' to 'Covered by Health Insurance?', then there should be a Health Insurance subassessment where it indicates which type of health insurance the client is receiving. Similarly if the user answered 'No', there should not be any Health Insurance records that say the client is receiving that type of Health Insurance."

  guidance$conflicting_income <-
    "If the user answered 'Yes' to 'Income from any source', then there should be an income subassessment where it indicates which type of income the client is receiving. Similarly if the user answered 'No', there should not be any income records that say the client is receiving that type of income."

  guidance$conflicting_ncbs <-
    "If the user answered 'Yes' to 'Non-cash benefits from any source', then there should be a Non-cash benefits subassessment where it indicates which type of income the client is receiving. Similarly if the user answered 'No', then there should not be any non-cash records that say the client is receiving that type of benefit"

  guidance$unlikely_ncbs <- "This client has every single Non-Cash Benefit, according to HMIS, which is highly unlikely. Please correct (unless it's actually true)."

  guidance$missing_at_exit <-
    "Please enter the data for this item by clicking into the Exit pencil on the given Client ID on the appropriate program stay."

  guidance$missing_at_entry <-
    "This data element is required to be collected at project Entry. Please click into the client's Entry pencil to save this data to HMIS."

  guidance$missing_pii <-
    "Please correct by navigating to the client's record, then clicking the Client Profile tab, then click into the Client Record pencil to save the missing data."

  guidance$referral_on_non_hoh <-
    "Users should not checkbox all the household members when creating a Referral. Only the Head of Household needs the Referral. It is recommended that users delete any referrals on Non Heads of Household related to this project stay so that the receiving agency does not have to deal with them and they stop showing in reporting."

  guidance$path_enrolled_missing <- "Please enter the data for this item by clicking into the Entry or Exit pencil and creating an Interim. In the assessment, enter the correct PATH Enrollment Date and Save."

  guidance$path_reason_missing <- "The user has indicated the household was not enrolled into PATH, but no reason was selected."

  guidance$missing_path_contact <- "Every adult or Head of Household must have a Living Situation contact record. If you see a record there but there is no Date of Contact, saving the Date of Contact will correct this issue."

  guidance$incorrect_path_contact_date <- "Every adult or head of household should have a Living Situation contact record where the Contact Date matches the Entry Date. This would represent the initial contact made with the client."

  guidance$duplicate_ees <- "Users sometimes create this error when they forget to click into a program stay by using the Entry pencil, and instead they click 'Add Entry/Exit' each time. To correct, EDA to the project the Entry/Exit belongs to, navigate to the Entry/Exit tab and delete the program stay that was accidentally added for each household member."

  guidance$future_ees <- "Users should not be entering a client into a project on a date in the future. If the Entry Date is correct, there is no action needed, but going forward, please be sure that your data entry workflow is correct according to your project type."

  guidance$future_exits <- "This client's Exit Date is a date in the future. Please enter the exact date the client left your program. If this client has not yet exited, delete the Exit and then enter the Exit Date once the client is no longer in your program."

  guidance$lh_without_spdats <- "Any household who has been in shelter or a Safe Haven for over 8 days should be assessed with the VI-SPDAT so that they can be prioritized for Permanent Housing (RRH or PSH)."


  guidance$ph_without_spdats <- "Every household (besides those fleeing domestic violence) must have a VI-SPDAT score to aid with prioritization into a Transitional Housing or Permanent Housing (RRH or PSH) project."

  guidance$spdat_on_non_hoh <- "It is very important to be sure that the VI-SPDAT score goes on the Head of Household of a given program stay because otherwise that score may not pull into any reporting. It is possible a Non Head of Household was a Head of Household in a past program stay, and in that situation, this should not be corrected unless the Head of Household of your program stay is missing their score. To correct this, you would need to completely re-enter the score on the correct client's record."


  guidance$services_on_non_hoh <-
    "Users should not checkbox all the household members when creating a Service Transaction. Only the Head of Household needs a Service Transaction. Delete any extraneous Service Transactions related to this project stay."

  guidance$dkr_data <-
    "It is widely understood that not every client will be able to or consent to answer every question in every assessment. If you do have any of this data, but it is just not entered into HMIS yet, please enter it. If you can reasonably attempt again to collect this data from the client (like if they are still in your project), then please do so. Otherwise, there is no action needed."

  guidance$project_stays <- "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not. Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the 'Previous Provider's' mistake, but if you are seeing clients here, it means your project stay was entered last. If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected or send an email to hmis@cohhio.org if you cannot get it resolved. These clients will NOT show on their Data Quality app. If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed."

  guidance$aps_with_ees <- "Access Points should only be entering Referrals and Diversion Services into the AP provider- not Entry Exits. If a user has done this, the Entry Exit should be deleted. Please see the <a href='http://hmis.cohhio.org/index.php?pg=kb.page&id=151' target='_blank'>Coordinated Entry workflow</a>."


  guidance$incorrect_ee_type <- "The user selected the wrong Entry Exit Type. To correct, click the Entry pencil and Save & Continue. The Entry Exit Type at the top can then be changed. Click \"Update\" to make this change take effect."

  guidance$stray_service <- "This Service does not fall between any project stay, so it will not show in any reporting."
  guidance$check_disability_ssi <- "If a client is receiving SSI or SSDI for THEIR OWN disability, that disability should be indicated in the Disabilities data elements. If an adult is receiving SSI or SSDI benefits on behalf a minor child, then there is no action needed."

  app_env$gather_deps(guidance)
}


