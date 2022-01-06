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
#' @include guidance.R relevant_dq.R

if (interactive() && curl::has_internet() && clarity.looker::is_dev() && (difftime(Sys.time(), attr(guidance, "last_update") %||% Sys.time()) > lubridate::days(7))) {
  googlesheets4::gs4_auth(path = "inst/auth/rminor@rminor-333915.iam.gserviceaccount.com.json")
  dq_id <- "15HsbSGmsscGtUIZnBDSVPaU4Zsotp7Dj79mXpPAu_lw"
  dq_nms <- googlesheets4::sheet_names(dq_id)
  dq_guidance <- purrr::map(rlang::set_names(dq_nms), ~googlesheets4::read_sheet(dq_id, sheet = .x, col_types = "c"))
  # Handle irrelevant
  if (is_clarity()) {
    irrelevant <- dq_guidance$`Guidance list`$name[(nchar(dq_guidance$`Guidance list`$irrelevant) > 0) %|% FALSE]
    relevant_dq <- unique(dq_guidance$Checks$DQ_Check[!stringr::str_extract(dq_guidance$Checks$Guidance, "(?<=guidance\\$)[\\w\\_]+") %in% irrelevant])
  } else {
    relevant_dq <- stringr::str_subset(ls(envir = .getNamespace("RmData"), pattern = "^dq\\_"), "^((?!\\_sp\\_)(?!\\_overlaps)(?!\\_check_eligibility).)*$")
  }
  dump("relevant_dq", file.path("R","relevant_dq.R"))
  guidance <- purrr::map(rlang::set_names(dq_guidance$`Guidance list`$name), ~{
    dq_guidance$`Guidance list`$guidance[dq_guidance$`Guidance list`$name == .x]
  })
  f <- ifelse(clarity.looker::is_dev(), "R/guidance.R", file.path(system.file(package = "RmData"), "R", "guidance.R"))
  attr(guidance, "last_update") <- Sys.time()
  dump("guidance", f)
}


#' @title guidance
#' @name guidance
#' @description A list of instructions tailored to the specific HMIS software for each Data Quality Issue.
#' @export
guidance

#' @title Relevant dq
#' @name relevant_dq
#' @description A list of the Data Quality Checks that are relevant and should be run in `data_quality`.
#' @export
relevant_dq
