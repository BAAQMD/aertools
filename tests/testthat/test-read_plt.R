options(digits = 9, width = 80)

paths <-
  system.file(
    "extdata",
    c("PE_RAI1044_0-0.PLT", "PE_RAI1043_0-0.PLT"),
    package = "aertools",
    mustWork = TRUE)

cols <- c(
  X = "X",
  Y = "Y",
  CONC = "AVERAGE CONC")

test_that("read_plt_dt_fread(path, ...) yields expected first and last values", {

  path <- paths[[1]]

  result <- read_plt_dt_fread(
    path,
    cols = cols,
    .id = "path")

  expect_equal(
    head(result, 1)[["CONC"]],
    0.114864E+01)

  expect_equal(
    tail(result, 1)[["CONC"]],
    0.975456E+00)

})


test_that("read_plt_dt_fread(path, ...) yields expected snapshot", {

  path <- paths[[1]]

  result <- read_plt_dt_fread(
    path,
    cols = cols,
    .id = "path")

  expect_snapshot(digest::digest(result, algo = "md5"))
  expect_s3_class(result, "data.table")

  expect_equal(
    result,
    read_plt(path, as = "data.table"),
    ignore_attr = TRUE)

})

test_that("read_plt_vroom_fwf(path, ...) yields expected snapshot", {

  path <- paths[[1]]

  result <- read_plt_vroom_fwf(
    path,
    cols = cols,
    .id = "path")

  expect_snapshot(digest::digest(result, algo = "md5"))
  expect_s3_class(result, "tbl")

  expect_equal(
    result,
    read_plt(path, as = "tbl"),
    ignore_attr = TRUE)

})

test_that("read_plt(paths, as = 'array', ...) yields expected snapshot", {

  results <- read_plt(
    paths,
    as = "array",
    cols = cols,
    .id = "path")

  expect_equal(
    dim(results),
    c(path = 2, X = 129, Y = 149))

  expect_equal(
    results[paths[[1]],,],
    read_plt(paths[[1]], as = "array", cols = cols, .id = "path"),
    ignore_attr = TRUE)

})

test_that("read_plt(paths, as = 'array', ...) completes missing (X, Y) combination with NA", {

  result <- read_plt(
    paths[[1]],
    as = "array",
    cols = cols,
    .id = "path")

  expect_identical(
    result[63, 81],
    NA_real_)

  expect_false(
    any(is.na(result[-63, -81])))

})


test_that("read_plt(paths, as = 'raster', ...) yields a RasterLayer or RasterStack with correct dims and crs", {

  given_crs <- sp::CRS(projargs = "+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80")
  expected_extent <- raster::extent(549616.33, 562516.33, 4193209.66, 4208109.66)

  single_layer <- read_plt(
    paths[[1]],
    as = "raster",
    crs = given_crs)

  expect_s4_class(single_layer, "RasterLayer")
  expect_equal(raster::ncell(single_layer), 19221)
  expect_equal(raster::res(single_layer), c(100, 100))
  expect_equal(raster::crs(single_layer), given_crs)
  expect_equal(raster::extent(single_layer), expected_extent)
  expect_equal(dim(single_layer), c(149, 129, 1))

  multi_layer <- read_plt(
    paths,
    as = "raster",
    crs = given_crs)

  expect_equal(raster::ncell(multi_layer), 19221)
  expect_equal(raster::res(multi_layer), c(100, 100))
  expect_equal(raster::crs(multi_layer), given_crs)
  expect_equal(raster::extent(multi_layer), expected_extent)
  expect_equal(dim(multi_layer), c(149, 129, 2))

})
