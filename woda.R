s <- osmdata::getbb("Suchedniów", format_out = "sf_polygon") |>
  sf::st_centroid() |>
#  sf::st_transform(crs = "EPSG:2180") |>
  sf::st_buffer(dist = 1000) |>
  rgugik::DEM_request()

s |>
  subset(product == "PointCloud" & year == "2023") |>
  rgugik::tile_download(outdir = "data/suchedniow", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")

l <- lidR::readLAScatalog("data/suchedniow/")

s <- osmdata::getbb("Suchedniów", format_out = "sf_polygon") |>
  sf::st_centroid() |>
  #  sf::st_transform(crs = "EPSG:2180") |>
  sf::st_buffer(dist = 1000) |>
  rgugik::ortho_request()

s |>
  subset(composition == "RGB" & year == "2022") |>
  rgugik::tile_download(outdir = "data/suchedniow", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")

f <- list.files("data/suchedniow", pattern = "tif$", full.names = TRUE)
sf::gdal_utils("buildvrt", source = f, destination = "data/suchedniow/ortho.vrt")
ortho <- terra::vrt("data/suchedniow/ortho.vrt")
  
library(tmap)
tmap::tmap_mode("view")
q <- tmap::qtm(l@data, fill_alpha = 0.4)

bb <- sf::st_bbox(l@data) |>
  sf::st_transform(crs = "EPSG:4326") |>
  matrix(nrow = 2)
rownames(bb) <- c("x", "y")
colnames(bb) <- c("min", "max")

w <- osmdata::opq(bb, timeout = 90) |>
  osmdata::add_osm_feature(key = "waterway") |>
  osmdata::osmdata_sf() |>
  osmdata::unique_osmdata()

qq <- w$osm_multilines |>
  sf::st_transform(crs = "EPSG:2180") |>
  sf::st_intersection(dplyr::summarize(l@data))

vertices <- qq |>
  sf::st_coordinates() |>
  as.data.frame() |>
  sf::st_as_sf(coords = c("X", "Y"), crs = "EPSG:2180")

tmap::tmap_mode("view")

tmap::qtm(l@data, fill_alpha = 0.4) +
  tmap::qtm(qq, col = "blue") +
  tmap::qtm(vertices, size = 0.4)

v <- 101 #7

p1 <- vertices[v, "geometry"] |>
  sf::st_coordinates()

p2 <- vertices[v+1, "geometry"] |>
  sf::st_coordinates()

dx <- p2[1] - p1[1]
dy <- p2[2] - p1[2]

ll <- sqrt(dx^2 + dy^2)

xmin <- min(p1[1], p2[1])
xmax <- max(p1[1], p2[1])
ymin <- min(p1[2], p2[2])
ymax <- max(p1[2], p2[2])

#' alpha in radians
alpha <- atan(dy/dx)
#' macierz obrotu??
#' 
if(dx >= 0L & dy < 0L) {
  xm <- p1[1] + ll * cos(alpha) / 2
  ym <- p1[2] + ll * sin(alpha) / 2
} else if(dx < 0L & dy >= 0L){
  xm <- p1[1] - ll * cos(alpha) / 2
  ym <- p1[2] - ll * sin(alpha) / 2
}

pm <- sf::st_sfc(sf::st_point(c(xm, ym)), crs = "EPSG:2180")

d <- 10
xm1 <- xm - d * cos(pi/2 + alpha) 
ym1 <- ym - d * sin(pi/2 + alpha)
pm1 <- sf::st_sfc(sf::st_point(c(xm1, ym1)), crs = "EPSG:2180")

xm2 <- xm + d * cos(pi/2 + alpha) 
ym2 <- ym + d * sin(pi/2 + alpha)
pm2 <- sf::st_sfc(sf::st_point(c(xm2, ym2)), crs = "EPSG:2180")

# line
coords <- rbind(c(xm1, ym1), c(xm2, ym2))
line <- sf::st_linestring(coords)
line <- sf::st_sfc(line)

poly <- sf::st_buffer(line, dist = 3, endCapStyle = "FLAT") |>
  sf::st_sfc(crs = "EPSG:2180")

bb <- sf::st_bbox(poly)

#' for short linestring, the bbox of transect polygon can be bigger than 
#' bbox of linestring itself, therefore choosing bigger bbox of those two
#' for plotting

xmin <- min(xmin, bb["xmin"])
xmax <- max(xmax, bb["xmax"])
ymin <- min(ymin, bb["ymin"])
ymax <- max(ymax, bb["ymax"])

terra::plotRGB(ortho,
               xlim = c(xmin - 0.05*ll, xmax + 0.05*ll),
               ylim = c(ymin - 0.05*ll, ymax + 0.05*ll),
               axes = TRUE,
               mar = c(1.5, 0, 1.0, 0))

plot(sf::st_geometry(qq), add = TRUE)
plot(sf::st_geometry(vertices), pch = 20, col = "blue", add = TRUE)



plot(poly, lwd = 0.6, col = "#80808088", lty = 3, add = TRUE)

plot(pm, pch = 20, col = "red", add = TRUE)

plot(pm1, pch = 16, size = 1.2, col = "green", add = TRUE)
plot(pm2, pch = 16, size = 1.2, col = "green", add = TRUE)
plot(line, lty = 3, col = "green", add = TRUE)

x <- lidR::clip_transect(l, c(xm1, ym1), c(xm2, ym2), width = 6, xz = TRUE)

#' removing outliers
#' 

stats <- boxplot(x@data$Z, plot = FALSE)$stats

x <- lidR::filter_poi(x, Classification != 12L, Z >= min(stats), Z <= max(stats))
bb <- lidR::st_bbox(x)

class_cols <- c(
  "0" = "black",       # never classified
  "1" = "gray90",      # unassigned
  "2" = "gray50",      # ground
  "3" = "lightgreen",  # low vegetation
  "4" = "green",       # medium vegetation
  "5" = "darkgreen",   # high vegetation
  "6" = "brown",       # building
  "7" = "gray90",      # noise
  "8" = "gray90",      # reserved
  "9" = "blue",        # water
  "10" = "gray33",     # rail
  "11" = "gray33",     # road surface
  "12" = "black")      # reserved

library(ggplot2)

ggplot(x@data, aes(X, Z, color = Z)) +
  geom_point(size = 0.5) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradientn(colours = lidR::height.colors(50))

ggplot(x@data, aes(X, Z, color = factor(Classification))) +
  geom_point(size = 0.8) +
  coord_equal() +
  theme_minimal() +
  scale_color_manual(values = class_cols, name = "", labels = c("Ground", "Low Veg.", "Medium Veg.", "High Veg.", "Noise", "Water"))

ggplot(x@data, aes(X, Z, color = Intensity)) +
  geom_point(size = 0.5) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradientn(colours = lidR::height.colors(50))

ggplot(x@data, aes(X, Y, color = factor(Classification))) +
  geom_point(size = 0.8) +
  coord_equal() +
  theme_minimal() +
  scale_color_manual(values = class_cols, name = "", labels = c("Ground", "Low Veg.", "Medium Veg.", "High Veg."))

