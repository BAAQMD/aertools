#' Import an AERMOD .PLT file as tabular data
#'
#' @param path (character) path to .PLT file(s)
#' @param as (character) format of result
#' @param complete (logical) complete with `NA` for (X, Y) combinations that don't appear in the file
#' @param verbose (logical)
#'
#' @note `complete = FALSE` only makes sense when `as` is "tbl" or "data.table".
#'
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows first
#' @importFrom purrr map
#' @importFrom stringr str_sub str_extract_all
#' @importFrom vroom vroom_fwf
#' @importFrom readr read_lines
#'
#' @return an object of type `as` (default: `tbl`)
#'
#' @seealso predecessor `read_AERMOD_plt.R` in West Oakland AB617 work
#'
#' @export
read_plt <- function (
  path,
  as = c("tbl", "tbl_cube", "array", "data.table", "raster"),
  cols = c(X = "X", Y = "Y", CONC = "AVERAGE CONC"),
  complete = TRUE,
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_plt] ", ...)
  as <- match.arg(as)

  stopifnot(is.character(cols))

  if (as == "tbl") {
    plt_tbl <- read_plt_vroom_fwf(path, cols = cols, ..., verbose = verbose)
    result <- plt_tbl
  } else if (as == "data.table") {
    plt_dt <- read_plt_dt_fread(path, cols = cols, ..., verbose = verbose)
    result <- plt_dt
  } else if (as == "tbl_cube") {
    err_msg <- "not yet supported"
    stop(err_msg)
  } else if (as == "array") {
    plt_dt <- read_plt(path, as = "data.table", cols = cols, ..., verbose = verbose)
    result <- xy2array.data.table(plt_dt)
  } else if (as == "raster") {
    result <- read_plt_as_raster(path, ..., verbose = verbose)
  } else {
    err_msg <- "not yet supported"
    stop(err_msg)
  }

  return(result)

}
