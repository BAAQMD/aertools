xy2array.data.table <- function (
  xy_data,
  value_var = "CONC"
) {

  # Only supporting data.table for now
  stopifnot(data.table::is.data.table(xy_data))

  dim_names <- setdiff(names(xy_data), value_var)
  names(dim_names) <- dim_names
  dims <- lapply(dim_names, function (dn) sort(unique(xy_data[[dn]])))

  # expanded grid --- all possible combinations of X and Y
  grid <- do.call(data.table::CJ, rev(dims))
  data.table::setkeyv(xy_data, rev(dim_names))
  expanded <- xy_data[grid] # WAS: xy_data[grid, on = .(Y, X)]

  # populate array and then flip it on the Y
  # FIXME: seems brittle; understand this better!
  arr <- array(expanded[[value_var]], dim = lengths(dims), dimnames = dims)
  indices <- sapply(dims, seq_along)
  indices[["Y"]] <- rev(indices[["Y"]])
  flipped <- do.call(`[`, append(list(arr), indices))
  return(flipped)

}
