#' Aggregate a numeric column over a specified time grouping.
#'
#' @description
#' \code{group_by_period} takes a date column and a numeric column and aggregates
#' the numeric column over specified date period. This is a common pattern in
#' data analysis in business where we want to convert transactional data to
#' daily, weekly or monthly data.
#'
#' @param .data A \code{data.frame} to apply \code{group_by_period} to
#' @param .date_col A date column that will be used for grouping the date.
#'   This should be of type \code{date} or \code{datetime}
#' @param .summarise_col The column which will be aggregated
#' @param unit A character string specifying a time unit or a multiple of
#'   a unit to be rounded to. See \code{?lubridate::floor_date} for more details
#' @param agg_fun A function used to aggregate the numeric column
#' @param na.rm \code{NA} handling - this is passed to the \code{na.rm} argument of the
#'   aggregating function
#'
#' @examples
#'
#' library(nycflights13)
#' library(tidyr)
#' library(dplyr)
#'
#' data("flights")
#'
#' flights_w_date <- flights %>%
#'   unite(flight_date, year, month, day, sep = "-") %>%
#'   mutate(flight_date = ymd(flight_date))
#'
#' flights_w_date %>%
#'   group_by_period(flight_date, distance, "month")
#'

#' @export

group_by_period <- function(.data, .date_col, .summarise_col,
                            unit, agg_fun = sum, na.rm = FALSE){
  .date_col <- enquo(.date_col)
  .date_col_name <- as.symbol(get_expr(.date_col))
  .summarise_col <- enquo(.summarise_col)
  .summarise_col_name <- as.symbol(get_expr(.summarise_col))

  .data %>%
    mutate(!!.date_col_name := lubridate::floor_date((!!.date_col), unit)) %>%
    group_by(!!.date_col) %>%
    summarise(!!.summarise_col_name := agg_fun(!!.summarise_col, na.rm = na.rm))
}
