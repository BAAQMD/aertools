#' xy2cube.data.table
#'
#' @param xy_data a `data.table` object with columns `X`, `Y`, and whatever `value_var` is
#' @param value_var (character) name of column containing values
#' @param careful (logical) perform additional QA on-the-fly
#'
xy2cube.data.table <- function (
  xy_data,
  value_var = "CONC",
  careful = FALSE
) {

  # Only supporting data.table for now
  stopifnot(data.table::is.data.table(xy_data))

  xy_data <- xy_data[,c("X","Y","CONC")]

  dim_names <- setdiff(names(xy_data), value_var)
  names(dim_names) <- dim_names
  dims <- lapply(dim_names, function (dn) sort(unique(xy_data[[dn]])))

  # # expanded grid --- all possible combinations of X and Y
  # grid <- do.call(data.table::CJ, dims)
  # expanded <- xy_data[grid, on = .(Y, X)]
  #
  # # construct and return a `tbl_cube` with (X, Y) -> CONC
  # nrow <- length(dims$X)
  # ncol <- length(dims$Y)
  # mets <- list(CONC = matrix(expanded$CONC, nrow = nrow, ncol = ncol))
  # cube <- cubelyr::tbl_cube(dims, mets)

  X <- sort(unique(xy_data$X))
  Y <- sort(unique(xy_data$Y))

  # expanded grid --- all possible combinations of X and Y
  grid <- do.call(data.table::CJ, rev(dims))
  data.table::setkeyv(xy_data, rev(dim_names))
  expanded <- xy_data[grid] # WAS: xy_data[grid, on = .(Y, X)]

  # construct and return a `tbl_cube` with (X, Y) -> CONC
  nrow <- length(dims$X)
  ncol <- length(dims$Y)
  mets <- list(CONC = matrix(expanded$CONC, nrow = length(X), ncol = length(Y)))
  #dims <- list(X = X, Y = Y)
  cube <- cubelyr::tbl_cube(dims, mets)

  if (isTRUE(careful)) {
    if (is.unsorted(cube$dims$X) || is.unsorted(cube$dims$Y)) {
      stop("X or Y is not sorted")
    }
  }

  return(cube)

}
