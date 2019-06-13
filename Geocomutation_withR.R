#https://github.com/geocompr/egu_19
install.packages(c("sf", "raster", "spData", "dplyr", "RQGIS"))
library(sf)
library(spData)
library(dplyr)
library(RQGIS)
library(raster)

# Filename: 02_vector_solution.R (2018-05-09)
#
# TO DO: Solution to the tasks of 02_vector.Rmd
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. ATTRIBUTE OPERATIONS
# 3. SPATIAL ATTRIBUTE OPERATIONS
# 4. GEOMETRIC OPERATIONS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("dplyr")
library("spData")

#**********************************************************
# 2 ATTRIBUTE OPERATIONS-----------------------------------
#**********************************************************

# Select all observations of `random_points` (`data("random_points, package =
# "RQGIS"`) which have more than 10 species (column `spri`). Plot the geometry
# of all points and add your selection to the plot in another color
data("random_points", package = "RQGIS")
sel = filter(random_points, spri > 10) %>%
  st_geometry
plot(st_geometry(random_points))
plot(sel, pch = 16, col = "salmon", add = TRUE)

# Based on `spri` add a categorical column to `random_points`  with 0-5
# corresponding to `low`, 6-10 to `medium` and >=11 to `high`.
random_points = 
  mutate(random_points,
         cat = cut(spri, breaks = c(0, 5, 10, max(spri) + 1),
                   labels = c("low", "medium", "high"), 
                   include.lowest = TRUE)) %>% as.data.frame
# and the base R way
random_points$cat = cut(random_points$spri, 
                        breaks = c(0, 5, 10, max(random_points$spri) + 1), 
                        labels = c("low", "medium", "high"),
                        include.lowest = TRUE)

# Optional: create two points of class `sfg` and convert them into an object of
# class `sf` which has an `id` and a `geometry` column.
p = st_point(c(1, 1))
p_2 = st_point(c(1, 2))
sfc = st_sfc(list(p, p_2))
st_sf(data.frame(id = 1:2), geometry = sfc)

# also possible:
d = data.frame(id = 1:2, x = 1:2, y = 1:2)
st_as_sf(d, coords = c("x", "y"), crs = 4326)

#**********************************************************
# 3 SPATIAL ATTRIBUTE OPERATIONS---------------------------
#**********************************************************

# Filter the Canterbury region from `nz`, and plot all summits of `nz_height`
# that do not intersect with the Canterbury region (both datasets come with the
# `spData` package).
sel = nz %>% filter(Name == "Canterbury")
nz_height[sel, op = st_disjoint] %>%
  st_geometry %>%
  plot
plot(st_geometry(sel), col = "lightgray", add = TRUE)


# What happens if we spatially join the elevation column of `nz_height` to `nz`?
join = st_join(nz, select(nz_height, elevation))
# rows with more than one match are duplicated, rows without a match are also
# given back with NA
# inner join -> only give back rows with a match
st_join(nz, select(nz_height, elevation), left = FALSE)

#**********************************************************
# 4 GEOMETRIC OPERATIONS-----------------------------------
#**********************************************************


# Create two overlapping circles (see below) and compute and plot their
# geometric intersection. Secondly union the circles.
pts = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
# use the buffer function to create circles from points
circles = st_buffer(pts, dist = 1) 
x = circles[1]
y = circles[2]
# intersection
int = st_intersection(x, y)
plot(circles)
plot(int, col = "salmon", add = TRUE)
# union
plot(st_union(x, y))

# Compute the average population (`total_pop_15`) for each `REGION` of
# `us_states`. Plot your result.
agg = us_states %>%
  group_by(REGION) %>%
  summarize(mean = mean(total_pop_15, na.rm = TRUE))
plot(agg[, "mean"])

# Find out about the CRS of `nz`, reproject it into a geographic CRS (EPSG:
# 4326) and plot the original and unproject `nz` object next to each other.
st_crs(nz)  # EPSG: 2193
nz_repr = st_transform(nz, crs = 4326)
par(mfrow = c(1, 2))
plot(st_geometry(nz), axes = TRUE)
plot(st_geometry(nz_repr), axes = TRUE)


# Filename: 02_raster_solution.R (2018-05-09)
#
# TO DO: Solution to the tasks of 02_vector.Rmd
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. ATTRIBUTE AND SPATIAL RASTER OPERATIONS
# 3. GEOMETRIC RASTER OPERATIONS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("raster")
library("dplyr")
library("spData")

#**********************************************************
# 2 ATTRIBUTE AND SPATIAL RASTER OPERATIONS----------------
#**********************************************************


# Attach `data("dem", package = "RQGIS")`. Retrieve the altitudinal values of
# the 10th row.
data("dem", package = "RQGIS")
dem[10, ]

# Sample randomly 10 coordinates of `dem` with the help of the
# `coordinates()`-command, and extract the corresponding altitudinal values.
coords = sp::coordinates(dem)
int = sample(1:nrow(coords), 10)
coords = coords[int, ]
extract(dem, coords)


# Attach `data("random_points", package = "RQGIS")` and find the corresponding
# altitudinal values. Plot altitude against `spri`.
data("random_points", package = "RQGIS")
random_points$dem = extract(dem, random_points)
plot(spri ~ dem, data = random_points)

# Compute the hillshade of `dem` (hint: `?hillShade`). Overlay the hillshade
# with `dem` while using an appropriate level of transparency.
slope = terrain(dem, opt = "slope")
aspect = terrain(dem, opt = "aspect")
hshade = hillShade(slope, aspect)
dev.off()
plot(hshade, col = gray.colors(n = 100),legend = FALSE)
plot(dem, add = TRUE, alpha = 0.4)

#**********************************************************
# 3 GEOMETRIC RASTER OPERATIONS----------------------------
#**********************************************************

# Decrease the resolution of `dem` by a factor of 10. Plot the result.
dem_agg = aggregate(dem, fact = 10, fun = mean)
par(mfrow = c(1, 2))
plot(dem)
plot(dem_agg)

# Reproject `dem` into WGS84. Plot the output next to the original object.
dem_wgs84 = projectRaster(dem, crs = st_crs(4326)$proj4string)
par(mfrow = c(1, 2))
plot(dem)
plot(dem_wgs84)

# Randomly select three points of `random_points`. Convert these into a polygon
# (hint: `st_cast`). Extract all altitudinal values falling inside the created
# polygon Use the polygon to clip `dem`. What is the difference between
# `intersect` and `mask`. Hint: `sf` objects might not work as well with
# **raster** commands as `SpatialObjects`. Assuming your polygon of class `sf`
# is named `poly`, convert it into a `SpatialObject` with `as(sf_object,
# "Spatial`)`.

# randomly sample 3 observations
poly = sample(1:nrow(random_points), 3) %>%
  slice(random_points, .) %>%
  st_geometry %>%
  st_cast("LINESTRING", ids = rep(1, 3)) %>%
  st_cast("POLYGON") %>%
  as(., "Spatial")

# extract values falling inside the polygon
extract(dem, poly)

# clip and mask
clip = raster::intersect(dem, poly)
clip_mask = raster::mask(dem, poly)
par(mfrow = c(1, 2))
plot(clip)
plot(clip_mask)




