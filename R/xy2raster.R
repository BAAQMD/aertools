# xy2raster.data.table <- function (
#   xy_data,
#   extent,
#   crs = NULL,
#   value_var = "CONC",
#   careful = TRUE
# ) {
#
#   # Only supporting data.table for now
#   stopifnot(data.table::is.data.table(xy_data))
#
#   if (missing(extent)) {
#     bb <- with(xy_data, c(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y)))
#     extent <- geotools::st_extent(bb)
#   }
#
#   arr <- xy2array.data.table(xy_data)
#
#   rst <- raster::raster(
#     arr
#   )
#
#   # x <- xy_data$X
#   # y <- xy_data$Y
#   # nx <- dplyr::n_distinct(x) # expecting nx = 129
#   # ny <- dplyr::n_distinct(y) # expecting ny = 149
#   # rst <- raster::raster(nrow = ny, ncol = nx, ext = extent)
#   # i <- raster::cellFromRowCol(rst_empty, y, x)
#   # rst[i] <- xy_data[[value_var]]
#
#   if (isFALSE(is.null(crs))) {
#     raster::crs(rst) <- crs
#   }
#
#   return(rst)
#
# }
