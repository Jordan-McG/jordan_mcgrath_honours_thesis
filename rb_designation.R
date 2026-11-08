
# This script was used to identify which bioregions each river basin sat within,
# decided based on produced maps. It produces bioregions for the <20m, <100m, 
# and <200m ASL. Run "elevation_for_bioregions" first.


### Highlighted map for river basin - store designation in csv to load later ###

focus_rb <- "WHITSUNDAY ISLAND"
basins_by_name_200m <- mutate(basins_by_name_200m, highlight = ifelse(BNAME == focus_rb, focus_rb, "Other"))

ggplot() +
  geom_spatvector(data = qld) +
  geom_spatvector(data = basins_by_name_200m, aes(fill = "Elevation")) +
  geom_spatvector(data = basins_by_name_200m[basins_by_name_200m$BNAME == focus_rb, ], 
                  aes(fill = highlight), alpha = 0.5, , linewidth = 1, colour = "red") +
  scale_fill_viridis_d() +
  coord_sf(datum = sf::st_crs(basins_by_name_200m))


### Create bioregions ###

(rb_bioregions <- read.csv("Data/basin_names.csv") |> 
  select(x, bioregion, discrepancy) |> 
  rename(BNAME = x))
  
basins_by_name_bioregion_200m <- terra::merge(basins_by_name_200m, rb_bioregions, by = "BNAME") 
basins_by_name_bioregion_100m <- terra::merge(basins_by_name_100m, rb_bioregions, by = "BNAME") 
basins_by_name_bioregion_20m <- terra::merge(basins_by_name_20m, rb_bioregions, by = "BNAME") 

bioregions_200m <- aggregate(basins_by_name_bioregion_200m, by = "bioregion") 
bioregions_200m <- bioregions_200m[!is.na(bioregions_200m$bioregion),]

bioregions_100m <- aggregate(basins_by_name_bioregion_100m, by = "bioregion") 
bioregions_100m <- bioregions_100m[!is.na(bioregions_100m$bioregion),]

bioregions_20m <- aggregate(basins_by_name_bioregion_20m, by = "bioregion") 
bioregions_20m <- bioregions_20m[!is.na(bioregions_20m$bioregion),]


ggplot() +
  geom_spatvector(data = qld) +
  geom_spatvector(data = bioregions_200m, aes(fill = bioregion)) +
  coord_sf(datum = sf::st_crs(bioregions_200m))


### Produce plot like monitoring report ###

bioregion_colours <- c(
  "1a" = "greenyellow",
  "1b" = "mediumpurple",
  "1c" = "wheat",
  "1d" = "lightblue",
  "2" = "plum",
  "3" = "red",
  "4" = "pink",
  "5a" = "salmon",
  "5b" = "lightsteelblue",
  "5c" = "orchid",
  "6a" = "dodgerblue",
  "6b" = "lightpink",
  "7" = "yellow"
)

p1 <- ggplot() +
  geom_spatvector(data = qld) +
  geom_spatvector(data = bioregions_200m, aes(fill = bioregion)) +
  scale_fill_manual(values = bioregion_colours) +
  theme_minimal() +
  coord_sf(datum = sf::st_crs(bioregions_200m))

p2 <- ggplot() +
  geom_spatvector(data = qld) +
  geom_spatvector(data = bioregions_100m, aes(fill = bioregion)) +
  scale_fill_manual(values = bioregion_colours) +
  theme_minimal() +
  coord_sf(datum = sf::st_crs(bioregions_100m)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

p3 <- ggplot() +
  geom_spatvector(data = qld) +
  geom_spatvector(data = bioregions_20m, aes(fill = bioregion)) +
  scale_fill_manual(values = bioregion_colours) +
  theme_minimal() +
  coord_sf(datum = sf::st_crs(bioregions_20m)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

# Plot all together
p1 + p2 + p3 + 
  plot_layout(guides = "collect", axes = "collect_y") & 
  theme(legend.position = "bottom") &
  guides(fill = guide_legend(nrow = 1))

