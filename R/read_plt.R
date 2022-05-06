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
#' @importFrom terra rast
#' @importFrom sf st_as_sf st_crs
#'
#' @return an object of type `as` (default: `tbl`)
#'
#' @seealso predecessor `read_AERMOD_plt.R` in West Oakland AB617 work
#'
#' @export
read_plt <- function (
  path,
  as = c("tbl", "tbl_cube", "array", "data.table", "raster", "terra", "sf"),
  cols = c(X = "X", Y = "Y", CONC = "AVERAGE CONC"),
  complete = TRUE,
  crs = NULL,
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
    rst_obj <- read_plt_as_raster(path, ..., crs = crs, verbose = verbose)
    result <- rst_obj
  } else if (as == "terra") {
    rst_obj <- read_plt_as_raster(path, ..., crs = crs, verbose = verbose)
    terra_obj <- terra::rast(rst_obj)
    result <- terra_obj
  } else if (as == "sf") {
    plt_tbl <- read_plt_vroom_fwf(path, cols = cols, ..., verbose = verbose)
    sf_obj <- sf::st_as_sf(plt_tbl, coords = cols[c("X", "Y")])
    if (isFALSE(is.null(crs))) {
      sf::st_crs(sf_obj) <- crs
    } else {
      warning("[read_plt] crs is NULL")
    }
    result <- sf_obj
  } else {
    err_msg <- "not yet supported"
    stop(err_msg)
  }

  return(result)

}
