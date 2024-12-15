# Koniaków ----------------------------------------------------------------------------------------------

place <- "Koniaków, gmina Istebna"

bb <- osmdata::getbb(place, format_out = "sf_polygon")

### LAZ ----
f <- list.files("data/laz", pattern = "laz$", full.names = FALSE)

a <- rgugik::DEM_request(bb) |>
  subset(product == "PointCloud" & year == "2021") |>
  subset(!basename(URL) %in% f)

if(nrow(a) > 0L) {
  rgugik::tile_download(a, outdir = "data/laz", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")
}


### ORTHO ----

f <- list.files("data/ortho", pattern = "tif$", full.names = TRUE)

a <- rgugik::ortho_request(bb) |>
  subset(composition == "RGB" & year == "2021") |>
  subset(!basename(URL) %in% basename(f))

if(nrow(a) > 0L) {
  rgugik::tile_download(a, outdir = "data/ortho", method = "wget", extra = "--no-check-certificate -c --progress=bar:force")
}

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

h |>
  subset(highway %in% highway_filteres) |>
  sf::st_transform(crs = "EPSG:2180") |>
  sf::write_sf(dsn = format_file_name(place, dir = "data", "gpkg"), append = FALSE)

rm(bb)
rm(h)
rm(highway_filteres)


# read data ---------------------------------------------------------------------------------------------

las <- lidR::readLAScatalog("data/laz")
ortho <- terra::vrt("data/ortho/ortho.vrt")
h <- sf::read_sf("data/koniakow_gmina_istebna.gpkg")


# analiza -----------------------------------------------------------------------------------------------

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

vertices <- h |>
  sf::st_coordinates() |>
  as.data.frame() |>
  sf::st_as_sf(coords = c("X", "Y"), crs = "EPSG:2180")

plot(vertices$geometry, col = "red", pch = 20, add = TRUE)

#'
#'

v <- 1 #7

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

xm <- p1[1] + ll * cos(alpha) / 2
ym <- p1[2] + ll * sin(alpha) / 2

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


#'
#'

x <- lidR::clip_transect(las, c(xm1, ym1), c(xm2, ym2), width = 6, xz = TRUE)

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

df

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

ggplot(y@data, aes(X, Z, color = Intensity)) +
  geom_point(size = 0.5) +
  coord_equal() +
#  ylim(674.5, 675.5) +
  theme_minimal() +
  scale_color_gradientn(colours = lidR::height.colors(50)) +
  geom_vline(xintercept = h_min) +
  geom_vline(xintercept = h_max)

ggplot(y@data, aes(X, Z, color = factor(Classification))) +
  geom_point(size = 0.8) +
  coord_equal() +
  theme_minimal() +
  scale_color_manual(values = class_cols, name = "", labels = c("Ground", "Low Veg.", "Medium Veg.", "High Veg.")) +
  geom_vline(xintercept = h_min) +
  geom_vline(xintercept = h_max)

h

# funkcje -----------------------------------------------------------------------------------------------

format_file_name <- function(name, dir = ".", ext = ".csv") {
  file_name <- stringi::stri_replace_all_regex(name, pattern = "[[:punct:]]", "") |>
    iconv(to="ASCII//TRANSLIT") |>
    tolower() |>
    stringi::stri_replace_all_fixed(pattern = " ", replacement = "_") |>
    stringi::stri_replace_all_regex(pattern = "\\_+", replacement = "_")

  if(!grepl("^\\.", {{ext}})) {
    .ext <- paste0(".", {{ext}})
  } else {
    .ext <- {{ext}}
  }
  
  file_name <- paste0(dir, "/", file_name, .ext)
  
  return(file_name)
}
