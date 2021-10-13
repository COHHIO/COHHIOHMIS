# Services_Custom ----
# Thu Sep 30 15:03:07 2021
FundingSources <- readxl::read_xlsx("data/cohhio_connection_model agencies 2021-09-27T1409.xlsx")
# Deleted funding sources
FundingSources[1:97,]
Services_Custom <- list()
Services_Custom$box = clarity.looker::hud_load("Services_Custom", "../Rm_data/data")
anyDuplicated(Services_Custom$box)
possible_dupes <- Services_Custom$box |>
  dplyr::group_by(PersonalID) |>
  dplyr::summarise(possible_dupes =
                     any(duplicated(FAAmount)) &
                     any(duplicated(DateCreated))) |>
  dplyr::pull(possible_dupes)
possible_dupes <- unique(Services_Custom$box$PersonalID)[possible_dupes]

possible_dupes <- dplyr::filter(Services_Custom$box, PersonalID %in% possible_dupes) |> dplyr::arrange(PersonalID)

possible_dupes <- possible_dupes |>
  dplyr::filter(!is.na(FAAmount)) |>
  dplyr::arrange(PersonalID, DateCreated)
# date_dupes <- c(which(duplicated(possible_dupes$DateCreated)), which(duplicated(possible_dupes$DateCreated, fromLast = T)))
# amount_dupes <- c(which(duplicated(possible_dupes$FAAmount)), which(duplicated(possible_dupes$FAAmount, fromLast = T)))
duplicates <- possible_dupes |> dplyr::select(- ServicesID) |> dplyr::distinct()
duplicates <- dplyr::left_join(duplicates, possible_dupes, by = UU::common_names(duplicates, possible_dupes))
duplicates <- duplicates |>
  dplyr::arrange(DateCreated, FAAmount)
