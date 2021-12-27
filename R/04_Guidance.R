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
#' @include guidance.R

if (interactive() && curl::has_internet() && clarity.looker::is_dev() && (difftime(Sys.time(), attr(guidance, "last_update") %||% Sys.time()) > lubridate::days(7))) {
  googlesheets4::gs4_auth(path = "inst/auth/rminor@rminor-333915.iam.gserviceaccount.com.json")
  dq_id <- "15HsbSGmsscGtUIZnBDSVPaU4Zsotp7Dj79mXpPAu_lw"
  dq_nms <- googlesheets4::sheet_names(dq_id)
  dq_guidance <- purrr::map(rlang::set_names(dq_nms), ~googlesheets4::read_sheet(dq_id, sheet = .x, col_types = "c"))
  guidance <- purrr::map(rlang::set_names(dq_guidance$`Guidance list`$name), ~{
    dq_guidance$`Guidance list`$guidance[dq_guidance$`Guidance list`$name == .x]
  })
  f <- ifelse(clarity.looker::is_dev(), "R/guidance.R", file.path(system.file(package = "RmData"), "R", "guidance.R"))
  attr(guidance, "last_update") <- Sys.time()
  dump("guidance", f)
}


