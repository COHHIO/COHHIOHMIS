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
#' @include 07_SPM_utils.R

spms <- function(clarity_api,
                 app_env,
                 e = rlang::caller_env()) {
  if (missing(clarity_api))
    clarity_api <- get_clarity_api(e = e)
  if (missing(app_env))
    app_env <- get_app_env(e = e)
  app_env$set_parent(missing_fmls())
  #  Load SPM ----
  # Tue Dec 28 11:47:35 2021
  spm <- load_csv_spm()

  spm <- spm |>
    dplyr::mutate(FiscalYear = lubridate::year(ReportEndDate)) |>
    dplyr::select(
      CoCCode,
      ESSHTHAvgTime_1A,
      SOExitPH_2,
      SOReturn0to180_2,
      SOReturn181to365_2,
      SOReturn366to730_2,
      ESExitPH_2,
      ESReturn0to180_2,
      ESReturn181to365_2,
      ESReturn366to730_2,
      THExitPH_2,
      THReturn0to180_2,
      THReturn181to365_2,
      THReturn366to730_2,
      SHExitPH_2,
      SHReturn0to180_2,
      SHReturn181to365_2,
      SHReturn366to730_2,
      PHExitPH_2,
      PHReturn0to180_2,
      PHReturn181to365_2,
      PHReturn366to730_2,
      TotalAnnual_3,
      ESAnnual_3,
      SHAnnual_3,
      THAnnual_3,
      IncreaseTotal4_3,
      AdultStayers_4,
      IncreaseEarned4_4,
      IncreaseOther4_5,
      IncreaseTotal4_6,
      EnterESSHTH5_1,
      ESSHTHWithPriorSvc5_1,
      EnterESSHTH5_2,
      ESSHTHPHWithPriorSvc5_2,
      SOExit_7,
      SOExitTempInst_7,
      SOExitPH_7,
      ESSHTHRRHExit_7,
      ESSHTHRRHToPH_7,
      PHClients_7,
      PHClientsStayOrExitPH_7
    )

  app_env$gather_deps(spm)
}
