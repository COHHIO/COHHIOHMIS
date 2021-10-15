
# function for adding bed nights per ee
bed_nights_per_ee <- function(table, interval) {
  is <- lubridate::int_start(interval)
  ie <- lubridate::int_end(interval)
  # if the ee date range and a given interval (in my reporting, a month) overlap,
  dplyr::if_else(lubridate::int_overlaps(table$StayWindow, interval),
                 # then return the difference between
                 as.numeric(difftime(
                   # if the exit date precedes the end of the interval, then the exit
                   # date, otherwise the end of the interval
                   dplyr::if_else(
                     table$ExitAdjust <=  ie,
                     table$ExitAdjust,
                     ie + lubridate::days(1)
                   ),
                   # if the entry date is after the start of the interval, then the
                   # entry date, otherwise the beginning of the interval
                   dplyr::if_else(
                     table$EntryAdjust >= is,
                     table$EntryAdjust,
                     is
                   ),
                   # give it to me in days
                   units = "days"
                 )), 0
  )
}

# function for bed capacity at the bed record level

bed_capacity <- function(BedCapacity, interval) {
  is <- lubridate::int_start(interval)
  ie <- lubridate::int_end(interval)
  dplyr::if_else(lubridate::int_overlaps(BedCapacity$AvailableWindow, interval),
                 (as.numeric(difftime(
                   dplyr::if_else(
                     BedCapacity$InventoryEndAdjust <=  ie,
                     BedCapacity$InventoryEndAdjust,
                     ie
                   ),
                   dplyr::if_else(
                     BedCapacity$InventoryStartAdjust >= is,
                     BedCapacity$InventoryStartAdjust,
                     is
                   ),
                   units = "days"
                 ))+1) * BedCapacity$BedInventory, 0
  )
}


#' @title Create an interval of the previous n-th month
#'
#' @param n \code{(integer/numeric)}
#'
#' @return \code{(interval)}
#' @export

nth_Month <- function(n) {
  d <- lubridate::`%m-%`(lubridate::floor_date(Sys.Date(), "months"), lubridate:::months.numeric(n))
  lubridate::interval(d, lubridate::ceiling_date(d, "months") - 1)
}

