
# This script is used to get elevation data. Run before "rb_designation"


################################################################################

### DEM ###  

## Load and clean elevation data
# Load digital elevation model data and reproject
dem_9s <- rast("Data/Data_9secDEM_D8/dem-9s/dem-9s.tif") |> 
  project("EPSG:3577")

# Clean and resample to coarser resolution used in study
(dem_qld_1km <- dem_9s |> 
  mask(qld) |>
  crop(qld) |> 
  resample(hpop21_qld, method = "bilinear") |> 
  rename("dem-1km" = "dem-9s"))

plot(dem_qld_1km)

## Filter for below 200m elevation
less200_elev <- dem_qld_1km <= 200
less100_elev <- dem_qld_1km <= 100
less20_elev <- dem_qld_1km <= 20
plot(low_elev)
plot(less100_elev)

################################################################################

### Drainage divisions ###

## Load and clean basin data
rbasins_97 <- vect("Data/River Basins/rbasin_polygon.shp") |>  #Geosciences Australia
  project("EPSG:3577") |> 
  crop(qld)

## Using the coastline to get coastal basins
# Get Australia wide coastline vector
state <- vect("Data/State Lines/STE_2021_AUST_GDA2020.shp")
state <- project(state, "EPSG:3577")
aus <- state[state$STE_CODE21 %in% 1:8, ]
aus <- aggregate(aus)
aus_coast <- as.lines(aus)
# Get QLD coast line only
qld_coast <- crop(aus_coast, qld)
# Keep only intersecting basins (coastal line is touching or in)
coastal_basins <- rbasins_97[relate(rbasins_97, qld_coast, relation = "intersects"), ]
plot(coastal_basins)

################################################################################

### Combine Elevation and Drainage Divisions ###

low_elev_coast_less200 <- mask(less200_elev, coastal_basins)
low_elev_coast_less200[low_elev_coast_less200 == 0] <- NA

low_elev_coast_less100 <- mask(less100_elev, coastal_basins) #added for 100m
low_elev_coast_less100[low_elev_coast_less100 == 0] <- NA

low_elev_coast_less20 <- mask(less20_elev, coastal_basins) #added for 20m
low_elev_coast_less20[low_elev_coast_less20 == 0] <- NA

## Get new coastal basins within the 200m elevation
# Convert TRUE cells to polygons
elev_poly_200m <- as.polygons(low_elev_coast_less200)
elev_poly_100m <- as.polygons(low_elev_coast_less100)
elev_poly_20m <- as.polygons(low_elev_coast_less20)

# Intersect to get coastal with <=200m elevation
coastal_basins_200m <- terra::intersect(coastal_basins, elev_poly_200m)
coastal_basins_100m <- terra::intersect(coastal_basins, elev_poly_100m)
coastal_basins_20m <- terra::intersect(coastal_basins, elev_poly_20m)

ggplot() +
  geom_spatvector(data = coastal_basins_200m, aes(fill = BNAME)) +
  theme(
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 6),
  ) +
  guides(fill = guide_legend(ncol = 2))

basins_by_name_200m <- aggregate(coastal_basins_200m, by = "BNAME")
basins_by_name_100m <- aggregate(coastal_basins_100m, by = "BNAME")
basins_by_name_20m <- aggregate(coastal_basins_20m, by = "BNAME")





