# Koniaków ----------------------------------------------------------------------------------------------

bb <- osmdata::getbb("Koniaków, gmina Istebna", format_out = "sf_polygon")

### LAZ ----
a <- rgugik::DEM_request(bb)
a |>
  subset(product == "PointCloud" & year == "2021") |>
  rgugik::tile_download(outdir = "data/laz", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")

### ORTHO ----

a <- rgugik::ortho_request(bb) 
a |>
  subset(composition == "RGB" & year == "2021") |>
  rgugik::tile_download(outdir = "data/ortho", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")

f <- list.files("data/ortho/", pattern = "tif$", full.names = TRUE)
sf::gdal_utils("buildvrt", source = f, destination = "data/ortho/ortho.vrt")
rm(a)
rm(f)

### highways ----

bb <- bb |>
  sf::st_bbox() |>
  matrix(nrow = 2)
rownames(bb) <- c("x", "y")
colnames(bb) <- c("min", "max")

h <- osmdata::opq(bb, timeout = 90) |>
  osmdata::add_osm_feature(key = "highway") |>
  osmdata::osmdata_sf() |>
  osmdata::unique_osmdata()

h <- h$osm_lines

highway_filteres <- c(
  "primary",
  "secondary",
  "tertiary",
  "unclassified",
  "service",
  "residential",
  "trunk")

h <- h |>
  subset(highway %in% highway_filteres)

