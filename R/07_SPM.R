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

spms <- function() {
  if (is_app_env(app_env))
    app_env$set_parent(missing_fmls())
  #  Load SPM ----
  # Tue Dec 28 11:47:35 2021
  spm <- load_csv_spm()

  spm <- spm |>
    dplyr::mutate(FiscalYear = lubridate::year(ReportEndDate))
}
