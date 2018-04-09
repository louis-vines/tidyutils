#' Spread multiple columns simultaneously.
#'
#' @description
#' \code{spread_multi} works simililarly to \code{tidyr::spread} but allows
#' for multiple value columns to be passed to the ellipsis (\code{...}) argument
#' which are spread simultaneously
#'
#' @param .data a data.frame to be spread
#' @param .key the key column which will be spread across the columns of the
#'   resulting data frame
#' @param ... the value columns to be spread. All column naming conventions that
#'  can be used with \code{dplyr::select} should behave well with
#'  \code{spread_multi}
#' @param sep the seperator to create column names of the form
#'  `<key_name><sep><key_value>`
#'
#' @examples
#'
#' df <- data_frame(
#'   month = rep(1:3,2),
#'   student = rep(c("Amy", "Bob"), each=3),
#'   x = c(9, 7, 6, 8, 6, 9),
#'   y = c(6, 7, 8, 5, 6, 7),
#'   z = c(10, 5, 3, 2, 1, -1)
#' )
#'
#' df %>%
#'   spread_multi(student, x:z)
#'
#'
#' @export

spread_multi <- function(.data, .key, ..., sep = "_"){
  .key <- enquo(.key)
  .cols_to_spread <- enquos(...)

  df %>%
    tidyr::gather(.var, .value, !!!.cols_to_spread) %>%
    tidyr::unite(.temp_keys_to_spread, !!.key, .var, sep = sep) %>%
    tidyr::spread(.temp_keys_to_spread, .value)
}
