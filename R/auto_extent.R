auto_extent <- function (x, y) {

  # Account for the 1/2 cell of "padding" on each side. This is due
  # to the fact that the (x, y) coords are assumed to represent the
  # centers of the cells.
  pad_range <- function (e) {
    rng <- range(e, na.rm = TRUE)
    n_cell <- length(e)
    span <- max(rng) - min(rng)
    delta <- span / (n_cell - 1)
    padding <- c(-1/2 * delta, +1/2 * delta)
    return(rng + padding)
  }

  extent <- raster::extent(c(pad_range(x), pad_range(y)))
  return(extent)

}
