# data prep ---------------------------------------------------------------
library(osmdata)
library(rgugik)

dtmcsv = "data/jaworzynka.csv"
miejsce = "Jaworzynka, cieszyński"
geometrieosm = "data/jaworzynka.gpkg"

if (!dir.exists("data")) {
  dir.create("data")
}

options(timeout = 60*20)
if(!file.exists(dtmcsv)) {
  a <- osmdata::getbb(miejsce, format_out = "sf_polygon") |>
    sf::st_centroid() |>
    sf::st_buffer(dist = 500) |>
    rgugik::DEM_request()
  
  write.csv(a, file = dtmcsv)
} else {
  a <- read.csv(file = dtmcsv)
}

a |>
  subset(product == "PointCloud" & year == "2021" & resolution == "4 p/m2") |>
  rgugik::tile_download(outdir = "data", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")
rm(a)

f <- list.files(path = "data", pattern = "laz", full.names = TRUE)

convertLAZ <- function(lazfile, outdir = ".", filter = "-keep_class 2 9", crs = "EPSG:2180") {
  if(!dir.exists(outdir)) { dir.create(outdir, recursive = TRUE)}
  message(lazfile)
  .file <- basename(lazfile)
  .outfile <- paste0(outdir, "/", stringi::stri_replace_all_fixed(.file, "laz", "las"))
  if(!file.exists(.outfile)) {
    las <- lidR::readLAS(files = lazfile, filter = {{filter}})
    if(is.na(lidR::crs(las))) {
      lidR::crs(las) <- {{crs}}
    }
    lidR::writeLAS(las, file = .outfile, index = TRUE)
  }
  else {
    message("Output file ", .outfile, " already exists, skipping conversion.")
  }
}

lapply(f, convertLAZ, filter = "", outdir = "data")
rm(f)
rm("convertLAZ", envir = .GlobalEnv)

# ALS frame selection ---------------------------------------------------------
pliklaz = "data/75089_1118671_M-34-86-B-d-3-1-3.laz"
pliklas = "data/75089_1118671_M-34-86-B-d-3-1-3.las"

if(!file.exists(geometrieosm)) {
  b <- osmdata::getbb(miejsce) |>
    osmdata::opq() |>
    osmdata::add_osm_features(features = c("\"boundary\"" = "\"administrative\"", 
                                           "\"highway\"")) |>
    osmdata::osmdata_sf()
  
  l <- lidR::readLAS(pliklas)
  
  l_ext <- lidR::ext(l)  |>
    terra::as.polygons() |>
    sf::st_as_sf()
  
  sf::st_crs(l_ext) <- lidR::crs(l)
  
  h <- 
    b$osm_lines |>
    subset(!is.na(highway) & 
             !highway %in% c("track", "path", "footway", "cycleway")) |>
    sf::st_transform(crs = lidR::crs(l))
  
  h <- sf::st_intersection(h, l_ext)
  
  sf::write_sf(h, geometrieosm, append = FALSE)
  
}

lidR::plot(l)

if (!file.exists("data/r.tif")) {
  l <- lidR::readLAS(pliklas)
  
  r <- lidR::rasterize_terrain(l, res = 0.2)
  terra::writeRaster(r, "data/r.tif", overwrite = TRUE)
} 

x <- lidR::plot(l, bg = "white", size = 3)
lidR::add_dtm3d(x, r)

## orthofoto
# plikorto = "data/79044_1300905_M-34-86-B-d-3-1.tif"
plikorto = "data/79045_1297829_M-34-86-B-d-3-1.tif"

if(!file.exists(plikorto)) {
  l <- lidR::readLAS(pliklas)
  
  l_ext <- lidR::ext(l)  |>
    terra::as.polygons() |>
    sf::st_as_sf()
  
  sf::st_crs(l_ext) <- lidR::crs(l)
  
  a <- rgugik::ortho_request(l_ext)
  a |>
    subset(year == "2023" & grepl("B-d-3-1", filename) & composition == "RGB") |>
    rgugik::tile_download(, outdir = "data", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")
}

# read data -----------------------------------------------------------------------------------

l <- lidR::readLAS(pliklas)
# r <- terra::rast("data/r.tif")
ortho <- terra::rast(plikorto)
h <- sf::read_sf(geometrieosm)

h <- h |>
  subset(highway == "residential")  
  #|>
  # sf::st_cast(to = "MULTIPOINT") |>
  # sf::st_union() |>
  # sf::st_cast(to = "LINESTRING")

h <- h[1, ] |>
  sf::st_cast(to = "LINESTRING")


m <- sf::st_bbox(h)
dx <- as.integer(m["xmax"] - m["xmin"])
dy <- as.integer(m["ymax"] - m["ymin"])

# par(pty = "s")
x_min <- as.integer(m["xmin"] - 0.1 * dx)
x_max <- as.integer(m["xmax"] + 0.1 * dx)
y_min <- as.integer(m["ymin"] - 0.1 * dy)
y_max <- as.integer(m["ymax"] + 0.1 * dy)

terra::plotRGB(ortho, 
               xlim = c(x_min, x_max),
               ylim = c(y_min, y_max),
               axes = TRUE,
               mar = c(1.5, 0, 1.5, 0)
               )

terra::plot(h, col = "red", add = TRUE)

# plot(sf::st_geometry(h), axes = TRUE)
 
# sample_points <- sf::st_line_sample(h, density = 0.01, type = "regular") |>
#    sf::st_cast(to = "POINT")
# # 
# sample_points |>
#   plot(add = TRUE)
# 
# terrain <- terra::terrain(r, v = c("slope", "aspect", "TPI", "TRI", "roughness", "flowdir"), unit = "radians")
# terra::plot(terrain$roughness, xlim = c(355600, 355700), ylim = c(375150, 375250))
# terra::plot(r, xlim = c(355600, 355700), ylim = c(375150, 375250))
# terra::plot(h, add = TRUE)
# dtm_hillshade <- terra::shade(slope = terrain$slope, aspect = terrain$aspect)
# terra::plot(dtm_hillshade, 
#              col = gray(0:30 / 30), 
#             # xlim = c(355600, 355800), 
#             # ylim = c(375150, 375350)
#             )
# plot(sf::st_geometry(h), add = TRUE)
# sample_points |>
#   plot(add = TRUE)
# 
# vertices ----------------------------------------------------------------------------------------------

vertices <- h |>
  sf::st_coordinates() |>
  as.data.frame() |>
  sf::st_as_sf(coords = c("X", "Y"), crs = "EPSG:2180")

plot(vertices$geometry, col = "red", pch = 20, add = TRUE)

## example ----

v <- 2 #7

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

plot(sf::st_geometry(h), add = TRUE)
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
  scale_color_manual(values = class_cols, name = "", labels = c("Ground", "Low Veg.", "Medium Veg.", "High Veg."))

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

y <- x |>
  lidR::filter_poi(Classification %in% c(2L, 3L))

y$X <- y$X - (bb["xmin"] + (bb["xmax"]-bb["xmin"])/2)
y$Y <- y$Y - (bb["ymin"] + (bb["ymax"]-bb["ymin"])/2)

ggplot(y@data, aes(X, Z, color = Intensity)) +
  geom_point(size = 0.5) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradientn(colours = lidR::height.colors(50))

ggplot(y@data, aes(X, Y, color = Intensity)) +
  geom_point(size = 0.5) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradientn(colours = lidR::height.colors(50))

aa <-  y@data |>
  subset(X <= 0.2 & X > -0.2, select = c(Z, Intensity))
mean(aa$Intensity)
sd(aa$Intensity)
Imin <- mean(aa$Intensity) - 2 * sd(aa$Intensity)
Imax <- mean(aa$Intensity) + 2 * sd(aa$Intensity)

Zmin <- mean(aa$Z) - 3 * sd(aa$Z)
Zmax <- mean(aa$Z) + 3 * sd(aa$Z)

s <- seq(-5, 5, 0.1)

df_list <- vector('list', length(s)-1)

#' Seems for some 10 cm wide strips there are no points in (LIDAR 4 pt/m^2) 
#' which caused an error:
#' 
#' Error in if ((Imax >= meanI & meanI >= Imin) & (Zmax >=meanZ & meanZ >= Zmin))
#'   missing value where TRUE/FALSE needed
#'   
#' Solution with if(nrow(aa))...
#' 

for (i in 1:(length(s)-1)) {
#  print(i)
  aa <-  y@data |>
    subset(X >= s[i] & X < s[i+1], select = c(Z, Intensity))
  if(nrow(aa) > 0L) {
    meanI <- mean(aa$Intensity)
    meanZ <- mean(aa$Z)
    if((Imax >=  meanI & meanI >= Imin) &
       (Zmax >=meanZ & meanZ >= Zmin)) {
      df <- data.frame(
        s = s[i],
        Im = meanI,
        Zm = meanZ,
        road_surface = "yes"
      )
    } else {
      df <- data.frame(
        s = s[i],
        Im = meanI,
        Zm = meanZ,
        road_surface = "no"
      )
    }
  } else {
    df <- data.frame(
      s = s[i],
      Im = NA,
      Zm = NA,
      road_surface = NA
    )
  }
  df_list[[i]] <- df
}
df <- do.call('rbind', df_list)
# df

#' cleaning:
#'  if there is yes, yes, NA|no, yes, yes => road_surface == yes
#'  if there is no, no, yes|NA, no, no =>    road_surface == no
#'  if there is no, yes, yes, yes, no =>     road_surface == no
#'  etc?

df <- df |>
  dplyr::mutate(alag1 = dplyr::lag(road_surface, n=1, default = "no"), 
                alag2 = dplyr::lag(road_surface, n=2, default = "no"), 
                alead1 = dplyr::lead(road_surface, n=1, default = "no"),
                alead2 = dplyr::lead(road_surface, n=2, default = "no"),
                road_surface = ifelse((is.na(road_surface) | road_surface == "no") & alag1 == "yes" & alag2 == "yes" & alead1 == "yes" & alead2 == "yes", "yes", road_surface),
                road_surface = ifelse((is.na(road_surface) | road_surface == "yes") & alag1 == "no" & alag2 == "no" & alead1 == "no" & alead2 == "no", "no", road_surface),
                road_surface = ifelse(road_surface == "yes" & alag2 == "no" & alead2 == "no", "no", road_surface),
                road_surface = ifelse(is.na(road_surface), "no", road_surface)) |>
  subset(select = c(s, Im, Zm, road_surface))

# df

h_min <- min(df$s[df$road_surface == "yes"])
h_max <- max(df$s[df$road_surface == "yes"])
h_width <- abs(h_min) + abs(h_max)

abs(h_max) - (abs(h_min) + abs(h_max))/2

message("Szerokość drogi = ", h_width, " m")

ggplot(y@data, aes(X, Y, color = Intensity)) +
  geom_point(size = 0.5) +
  #  ylim(114, 116) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradientn(colours = lidR::height.colors(50)) +
  geom_vline(xintercept = h_min) +
  geom_vline(xintercept = h_max)



# to poniżej to chłam z przeszłości, może się wykorzysta -----

# sample points -----------------------------------------------------------

points_from_highway <- h |>
  sf::st_coordinates() |>
  as.data.frame() |>
  sf::st_as_sf(coords = c("X", "Y"), crs = lidR::crs(l))

plot(points_from_highway$geometry, pch = 20, add = TRUE)


p1 <- sample_points[1]
plot(p1, pch = 20, col = "red", add = TRUE)

terra::plot(dtm_hillshade,
  col = gray(0:30 / 30),
  xlim = c(sf::st_coordinates(p1)[[1]] - 10, sf::st_coordinates(p1)[[1]] + 10),
  ylim = c(sf::st_coordinates(p1)[[2]] - 10, sf::st_coordinates(p1)[[2]] + 10)
)
terra::plot(h, add = TRUE)

terra::plot(p1, pch = 20, col = "red", add = TRUE)

#' and the closest point from highway, to calculate the angle
#'
p2 <- points_from_highway$geometry[sf::st_nearest_feature(p1, points_from_highway)]
plot(p2, pch = 20, col = "blue", add = TRUE)
alpha <- 180 * atan(
  (sf::st_coordinates(p1)[1] - sf::st_coordinates(p2)[1]) / (sf::st_coordinates(p1)[2] - sf::st_coordinates(p2)[2])
) / pi


y1 <- 10 * cos(alpha - 90)
x1 <- 10 * sin(alpha - 90)
y2 <- 10 * cos(alpha + 90)
x2 <- 10 * sin(alpha + 90)


p3 <- p1 + c(x1, y1)
sf::st_crs(p3) <- sf::st_crs(points_from_highway)
plot(p3, pch = 20, col = "green", add = TRUE)

p4 <- p1 + c(-x1, -y1)
sf::st_crs(p4) <- sf::st_crs(points_from_highway)
plot(p4, pch = 20, col = "green", add = TRUE)

l34 <- sf::st_cast(sf::st_union(p3, p4), "LINESTRING") |>
  sf::st_as_sf(crs = sf::st_crs(points_from_highway))

plot(l34, add = TRUE, col = "green")

terra::plot(terrain$roughness,
  xlim = c(sf::st_coordinates(p1)[[1]] - 10, sf::st_coordinates(p1)[[1]] + 10),
  ylim = c(sf::st_coordinates(p1)[[2]] - 10, sf::st_coordinates(p1)[[2]] + 10)
)

# transect ----------------------------------------------------------------

l <- l |>
  lidR::filter_poi(Classification %in% c(2, 12))

library(lidR)
library(ggplot2)
las_tr <- lidR::clip_transect(l,
  c(
    sf::st_coordinates(p3)[1],
    sf::st_coordinates(p3)[2]
  ),
  c(
    sf::st_coordinates(p4)[1],
    sf::st_coordinates(p4)[2]
  ),
  width = 4, xz = TRUE
)

ggplot(las_tr@data, aes(X, Z, color = Z)) +
  geom_point(size = 0.5) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))

las_tr_filtered <- las_tr |>
  lidR::filter_poi(Classification %in% c(2, 12) & Z >= boxplot(las_tr@data$Z, plot = FALSE)$stats[1,1] & Z <= boxplot(las_tr@data$Z, plot = FALSE)$stats[4,1])

ggplot(las_tr_filtered@data, aes(X, Intensity, color = Intensity)) +
  geom_point(size = 0.5) +
#  coord_equal() +
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))


las_tr_filtered@data[1:20,]

las_tr@data |>
  subset(X < -5 & X > -6)

l34

#' roughness
#'
sf::st_length(l34)
roughness_tr <- terra::extract(terrain$roughness, terra::vect(l34), cells = TRUE, xy = TRUE)
plot(x = seq(1:nrow(roughness_tr)), y = roughness_tr$roughness)
terrain$roughness
las_tr@data$X
roughness_tr

terra::plot(terrain$slope, xlim = c(355600, 355700), ylim = c(375150, 375250))
slope_tr <- terra::extract(terrain$slope, terra::vect(l34), cells = TRUE, xy = TRUE)
plot(x = seq(1:nrow(slope_tr)), y = slope_tr$slope)
nrow(roughness_tr)

plot(x = seq(1:nrow(roughness_tr)), y = roughness_tr$roughness, col = "blue")
points(x = seq(1:nrow(slope_tr)), y = slope_tr$slope, pch = 20, add = TRUE)


library(ALSroads)
library(lidR)
library(sf)
library(raster)

ctg <- readLAScatalog("data")
dtm <- raster("data/r.tif")
road <- h |>
  sf::st_as_sf()
road
plot(dtm, col = gray(1:50/50))
plot(ctg, add = TRUE)
plot(st_geometry(road), add = TRUE, col = "red")

a <- system.file("extdata", "j5gr_centerline_971487.gpkg", package="ALSroads")
a <- st_read(a, "original", quiet = TRUE)

index(ctg)

dres <- measure_road(ctg, road, dtm = dtm)
res
poly <- sf::st_buffer(res, res$ROADWIDTH/2)
plot(dtm, col = gray(1:50/50))
plot(st_geometry(road), col = "red", add = TRUE) # Inaccurate road track
plot(st_geometry(dres), col = "darkgreen", add = TRUE) # Corrected road track

conductivity <- ALSroads::rasterize_conductivity(las = lidR::readLAS(pliklaz))
conductivity <- raster(conductivity)

plot(conductivity, col = viridis::viridis(50))

res <- measure_road(ctg, road, conductivity = conductivity)

plot(st_geometry(road), col = "red") # Inaccurate road track
plot(st_geometry(res), col = "blue", add = TRUE) # Corrected road track

tmap::tmap_mode("view")
tmap::tm_shape(h) +
  tmap::tm_lines() +
  tmap::tm_shape(dres) +
  tmap::tm_lines(col = "darkgreen") +
  tmap::tm_shape(sf::st_cast(h, to = "POINT")) +
  tmap::tm_dots()

  

x <- c(dd[3, 1] - dd[2, 1], dd[3, 2] - dd[2, 2])
y <- c(0.01, 0.01)

x%*%y
norm(y, "2")
ALSroads:::angle(x, y)

