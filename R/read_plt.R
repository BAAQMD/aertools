#' Import an AERMOD .PLT file as tabular data
#'
#' @param path (character) path to .PLT file(s)
#' @param as (character) format of result
#' @param verbose (logical)
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
    # plt_dt_list <- map(path, read_plt, as = "data.table", cols = cols, ..., verbose = verbose)
    # result_list <- map(plt_dt_list, dt2cube)
    # result <- result_list
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

#' read_plt_as_raster
#'
#' @param path (character) filesystem path(s) (see Note)
#' @param extent a [raster::Extent]
#' @param crs a coordinate reference system (as yielded by [sp::CRS()])
#' @param cols (character) as in [read_plt()]
#' @param verbose (logical)
#'
#' @importFrom raster raster values extent crs
#'
#' @note If `path` has more than one element, then the return value will be a [raster::RasterStack], with layer names
#'     set equal to the [basename()] of each path. Otherwise, the return value will be a [raster::RasterLayer].
#'
#' @return A [raster::RasterLayer] or [raster::RasterStack] (see Note)
#'
read_plt_as_raster <- function (
  path,
  extent,
  crs = NULL,
  cols = c(X = "X", Y = "Y", CONC = "AVERAGE CONC"),
  verbose = getOption("verbose", default = FALSE)
) {

  dt2layer <- function (dt, extent, crs) {
    xy_data <- dt[, names(cols), with = FALSE]
    arr <- xy2array.data.table(xy_data)
    rst <- raster::raster(nrow = dim(arr)[["Y"]], ncol = dim(arr)[["X"]])
    raster::values(rst) <- t(as.matrix(arr))
    if (missing(extent)) {
      XY <- attr(arr, "XY") # list(X = <unique x values>, Y = <unique y values>)
      extent <- auto_extent(XY[["X"]], XY[["Y"]])
    }
    raster::extent(rst) <- extent
    raster::crs(rst) <- crs
    return(rst)
  }

  dt <- read_plt(path, as = "data.table", cols = cols, .id = "path")
  dt_list <- split(dt, dt$path)

  layer_list <- lapply(dt_list, dt2layer, crs = crs)
  names(layer_list) <- sapply(names(layer_list), basename)

  if (length(layer_list) == 1) {
    return(layer_list[[1]])
  } else {
    return(raster::stack(layer_list))
  }

}
