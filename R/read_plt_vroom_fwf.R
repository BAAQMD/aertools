read_plt_vroom_fwf <- function (
  path,
  cols,
  show_col_types = FALSE,
  ...,
  .id = "path",
  verbose = getOption("verbose", default = FALSE)
) {

  col_positions <-
    fwf_plt_cols(
      path[[1]],
      cols = cols)

  plt_data <-
    vroom::vroom_fwf(
      path,
      skip = 8,
      id = .id,
      show_col_types = show_col_types,
      progress = FALSE,
      ...,
      col_positions = col_positions)

  return(plt_data)

}
