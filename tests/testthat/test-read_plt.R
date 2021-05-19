path <- system.file("extdata", "PE_RAI1044_0-0.PLT", package = "aertools", mustWork = TRUE)
cols <- c(X = "X", Y = "Y", CONC = "AVERAGE CONC")

test_that("read_plt_dt_fread(path, ...) yields expected snapshot", {

  result <- read_plt_dt_fread(path, cols = cols, .id = "path")
  expect_snapshot(digest::digest(result, algo = "md5"))

  expect_equal(result, read_plt(path, as = "data.table"), ignore_attr = TRUE)
  expect_s3_class(result, "data.table")

})

test_that("read_plt_vroom_fwf(path, ...) yields expected snapshot", {

  result <- read_plt_vroom_fwf(path, cols = cols, .id = "path")
  expect_snapshot(digest::digest(result, algo = "md5"))

  expect_equal(result, read_plt(path, as = "tbl"), ignore_attr = TRUE)
  expect_s3_class(result, "tbl")

})

test_that("read_plt(paths, as = 'array', ...) yields expected snapshot", {

  paths <- system.file(
    "extdata",
    c("PE_RAI1044_0-0.PLT", "PE_RAI1043_0-0.PLT"),
    package = "aertools",
    mustWork = TRUE)

  results <- read_plt(paths, as = "array", cols = cols, .id = "path")

  expect_equal(dim(results), c(path = 2, X = 129, Y = 149))

  expect_equal(
    results[paths[[1]],,],
    read_plt(paths[[1]], as = "array", cols = cols, .id = "path"),
    ignore_attr = TRUE)

})

test_that("read_plt(paths, as = 'raster', ...) yields a RasterLayer or RasterStack with correct dims and crs", {

  paths <- system.file(
    "extdata",
    c("PE_RAI1044_0-0.PLT", "PE_RAI1043_0-0.PLT"),
    package = "aertools",
    mustWork = TRUE)

  given_crs <- sp::CRS(projargs = "+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  expected_extent <- raster::extent(549666.33, 562466.33, 4193259.66, 4208059.66)

  single_layer <- read_plt(paths[[1]], as = "raster", crs = given_crs)

  expect_s4_class(single_layer, "RasterLayer")
  expect_equal(raster::ncell(single_layer), 19221)
  expect_equal(raster::crs(single_layer), given_crs)
  expect_equal(raster::extent(single_layer), expected_extent)
  expect_equal(dim(single_layer), c(149, 129, 1))

  multi_layer <- read_plt(paths, as = "raster", crs = given_crs)
  expect_equal(raster::ncell(multi_layer), 19221)
  expect_equal(raster::crs(multi_layer), given_crs)
  expect_equal(raster::extent(multi_layer), expected_extent)
  expect_equal(dim(multi_layer), c(149, 129, 2))

})
