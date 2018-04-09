
spread_multi <- function(.data, .key, ..., sep = "_"){
  .key <- enquo(.key)
  .cols_to_spread <- enquos(...)

  df %>%
    gather(.var, .value, !!!.cols_to_spread) %>%
    unite(.temp_keys_to_spread, !!.key, .var, sep = sep) %>%
    spread(.temp_keys_to_spread, .value)
}
