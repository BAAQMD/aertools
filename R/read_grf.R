#' read_grf
#'
#' Treats a .GRF file as a concatenation of .PLT files.
#' Returns a list where each element is the result of a call to [read_plt()].
#' Names are extracted from the header chunks in the .GRF file.
#'
#' @param path (character) ending in .GRF
#' @param ... passed to [read_plt()]
#' @param verbose (logical)
#'
#' @importFrom readr read_lines
#' @importFrom stringr str_detect str_match str_remove_all str_c
#' @importFrom fs path path_temp dir_delete dir_create
#'
#' @export
read_grf <- function (
  path,
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_grf] ", ...)

  grf_lines <- readr::read_lines(path)

  (chunk_start <- which(stringr::str_detect(grf_lines, "^\\* AERMOD")))
  (chunk_end <- c(chunk_start[-1] - 1, length(grf_lines)))

  chunk_names <- local({
    #pattern <- "PLOT FILE OF\\s+([A-Za-z0-9-\\s]+?) VALUES FOR SOURCE GROUP:\\s+([A-Za-z0-9]+)"
    pattern <- "(PERIOD|1-HR) VALUES FOR SOURCE GROUP:\\s+([A-Za-z0-9]+)"
    i <- which(stringr::str_detect(grf_lines, "SOURCE GROUP"))
    matches <- stringr::str_match(grf_lines[i], pattern)
    sanitize <- function (x) stringr::str_remove_all(x, "[-_]")
    stringr::str_c(sanitize(matches[, 2]), sanitize(matches[, 3]), sep = "_")
  })

  (tmpdn <- fs::path(fs::path_temp(), basename(path)))
  try(fs::dir_delete(tmpdn), silent = TRUE)
  fs::dir_create(tmpdn)

  plt_path <- character()

  for (i in 1:length(chunk_start)) {
    plt_fn <- stringr::str_c(chunk_names[i], ".PLT")
    plt_path[i] <- fs::path(tmpdn, plt_fn)
    plt_lines <- grf_lines[seq(chunk_start[i], chunk_end[i])]
    msg("writing ", length(plt_lines), " lines to ", basename(plt_path[i]))
    cat(plt_lines, file = plt_path[i], sep = "\n")
  }

  plt_obj_lst <- lapply(plt_path, aertools::read_plt, ..., verbose = verbose)
  names(plt_obj_lst) <- chunk_names

  return(plt_obj_lst)

}
