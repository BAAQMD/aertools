#' fwf_plt_cols
#'
#' Return a column specification suitable for use with [readr::read_fwf()] or [vroom::vroom_fwf()].
#'
#' @param path (character) single path (just one!)
#' @param cols (character) select and optionally rename columns of the PLT file
#'
#' @importFrom stringr str_extract_all str_sub
#' @importFrom tidyselect vars_select
#' @importFrom readr fwf_empty read_fwf fwf_positions
#'
#' @return a tibble, identical in structure to the return value of [readr::fwf_positions()]
#'
fwf_plt_cols <- function (
  path,
  cols = c(X = "X", Y = "Y", CONC = "AVERAGE CONC"),
  skip = 6
) {

  if (length(path) > 1) {
    warning("[fwf_plt_cols] length(path) > 1; taking only the first element!")
    path <- path[[1]]
  }

  # Read in the header row of the first of the PLT paths.
  header_fwf <- readr::fwf_empty(path, skip = skip)
  header_row <- suppressMessages(readr::read_fwf(path, col_positions = header_fwf, skip = skip, n_max = 1))

  # We support on-the-fly renaming of columns via the `cols` argument.
  header_vars <- unname(unlist(header_row))
  j <- match(cols, header_vars)

  col_positions <-
    readr::fwf_positions(
      header_fwf$begin[j],      # only those indexed by j
      header_fwf$end[j],        # only those indexed by j
      col_names = names(cols))  # only those indexed by j

  col_positions$j <- (j - 1)

  return(col_positions)

}
