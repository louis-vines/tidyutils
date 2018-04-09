#' Filter a dataframe to keep the top n of a given column.
#'
#' @description
#' \code{keep_top_n} takes a data frame and filters it to only return the rows
#'   coinciding with the top n of a given column. This enables the user to quickly
#'   extract data based on the \emph{first} or \emph{largest} of something.
#'
#' @param .data A \code{data.frame} to apply the filter method too
#' @param .ranking_col The column used to filter the data
#' @param n The number of rows to keep
#'
#' @details
#' The ranking mechanism uses the \code{dplyr::row_number}. This means if there
#'   are any ties when ranking the data, the data corresponding to the first
#'   row positionally is kept.
#'
#' @examples
#'
#' library(dplyr)
#'
#' data("starwars")
#'
#' starwars %>%
#'   group_by(gender) %>%
#'   keep_top_n(height)
#'
#' starwars %>%
#'   group_by(gender) %>%
#'   keep_top_n(desc(height), n = 3)
#'
#'
#' @export
#'

keep_top_n <- function(.data, .ranking_col, n = 1){
  .ranking_col <- enquo(.ranking_col)

  .data %>%
    mutate(.col_rankings = row_number(!!.ranking_col)) %>%
    filter(.col_rankings <= n) %>%
    select(-.col_rankings)
}
