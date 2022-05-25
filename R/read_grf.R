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
#' @importFrom funtools progressively
#' @importFrom purrr map2 map iwalk
#'
#' @export
read_grf <- function (
  path,
  ids = NULL,
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_grf] ", ...)

  grf_lines <- readr::read_lines(path)

  chunk_start <- which(stringr::str_detect(grf_lines, "^\\* AERMOD"))
  chunk_end <- c(chunk_start[-1] - 1, length(grf_lines))

  str_extract_first_match <- function (x, ...) {
    matches <- stringr::str_match(x, ...)
    return(matches[, 2])
  }

  chunk_names <- local({
    i <- which(stringr::str_detect(grf_lines, "SOURCE GROUP"))
    interval <- str_extract_first_match(grf_lines[i], "(PERIOD|1-HR) VALUES")
    group <- str_extract_first_match(grf_lines[i], "SOURCE GROUP:\\s+([A-Za-z0-9]+)")
    sanitize <- function (x) str_remove_all(x, "[-_]")
    stringr::str_c(sanitize(interval), sanitize(group), sep = "_")
  })

  msg("chunk_names: ", str_csv(chunk_names))

  if (is.null(ids)) {
    i <- 1:length(chunk_names)
  } else {
    i <- match(ids, chunk_names)
    if (any(is.na(i))) {
      stop(str_glue("[read_grf] nothing named {str_or(ids[is.na(i)])}"))
    }
  }

  tmpdn <- fs::path(fs::path_temp(), basename(path))
  try(fs::dir_delete(tmpdn), silent = TRUE)
  fs::dir_create(tmpdn)

  chunk_path <- fs::path(tmpdn, stringr::str_c(chunk_names[i], ".PLT"))
  chunk_lines <- purrr::map2(chunk_start[i], chunk_end[i], seq)

  chunk_content <- purrr::map(chunk_lines, ~ grf_lines[.])
  names(chunk_content) <- chunk_path

  write_tmp <- function (content, path) {
    cat(content, file = path, sep = "\n")
    return(path)
  }

  purrr::iwalk(
    chunk_content,
    progressively(
      write_tmp,
      title = "write_tmp",
      total = length(chunk_content)))

  plt_obj_lst <-
    names(chunk_content) %>%
    purrr::map(
      funtools::progressively(
        aertools::read_plt,
        title = "read_plt",
        total = length(.)))

  names(plt_obj_lst) <- chunk_names[i]

  return(plt_obj_lst)

}
