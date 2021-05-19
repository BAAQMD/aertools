#' read_plt_dt_fread
#'
#' @importFrom data.table data.table fread rbindlist
#'
#' @return [data.table::data.table] object
#'
#' @noRd
read_plt_dt_fread <- function (
  path,
  cols,
  col_positions,
  showProgress = FALSE,
  nThread = 1,
  strip.white = FALSE,
  data.table = TRUE,
  .id = "path",
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  col_positions <-
    fwf_plt_cols(
      path[[1]],
      cols = cols)

  fread_plt <- function (path) {
    data.table::fread(
      path,
      showProgress = showProgress,
      nThread = nThread,
      select = col_positions[["j"]],
      col.names = col_positions[["col_names"]],
      strip.white = strip.white,
      data.table = data.table,
      skip = 8)
  }

  plt_dt <- data.table::data.table(path = path)
  plt_dt[, id := as.character(.I)]
  plt_dt[, contents := .(lapply(path, fread_plt))]

  plt_dt <- plt_dt[, data.table::rbindlist(setNames(contents, path), idcol = .id)]
  return(plt_dt)

}
