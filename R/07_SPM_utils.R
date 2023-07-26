load_csv_spm <- function() {
  spm <- list.files(path = dirs$spm,
                    pattern = "*.csv",
                    full.names = T) |>
    purrr::map_df(~readr::read_csv(., col_types = readr::cols(.default = "c")))
}
