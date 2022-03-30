# README

This R package provides one key function, `read_plt()`, that can be used to import AERMOD-generated .PLT files in various ways.

By default it just looks for the `X`, `Y`, and `AVERAGE CONC` columns, and abbreviates that last one as `CONC`. 
You can use `read_plt(path, cols = c(X = "X", Y = "Y", ...))` for finer-grained control, but that's highly experimental. 

## Example Usage

```
# You can import the contents of a PLT file as tabular data:
read_plt("foo.plt", as = "data.table")
read_plt("foo.plt", as = "tbl")

# A single PLT file can also be imported as a RasterLayer:
read_plt("foo.plt", as = "raster", crs = sp::CRS(...))

# To read multiple files at once in tabular format, use `.id` to index the paths:
read_plt(c("foo.plt", "bar.plt"), as = "data.table", .id = "path")
read_plt(c("foo.plt", "bar.plt"), as = "tbl", .id = "path")


# You can also read in multiple files as a 3-D array or a `tbl_cube`
# (see the `cubelyr` package for more on `tbl_cube`)
read_plt(c("foo.plt", "bar.plt"), as = "array", .id = "path")
read_plt(c("foo.plt", "bar.plt"), as = "tbl_cube", .id = "path")

# Last but not least, you can import multiple PLT files in the form
# of a RasterStack. Layers will be named according to the basenames
# (i.e., file names, with no directories) of the paths supplied.
read_plt(c("foo.plt", "bar.plt"), as = "raster")
```

See `?read_plt()` for more.
