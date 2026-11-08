
# This script was used to help in assigning bioregions designations to LGAs.


### LOOKING AT LGA's Bioregion Designation

# Choose lga of interest
focus_lga <- "Cook"


# Qld plot
lga_qld <- mutate(lga_qld, highlight = ifelse(LGA_NAME25 == focus_lga, focus_lga, "Other"))
ggplot() +
  geom_spatvector(data = lga_qld, aes(fill = highlight), color = "white", linewidth = 0.1) +
  scale_fill_manual(values = setNames(c("red", "grey90"), c(focus_lga, "Other")))

# Focused plot - iteration 1
focus_lga_ext <- ext(subset(lga_qld, lga_qld$LGA_NAME25 == focus_lga))
lga_crop <- crop(lga_qld, focus_lga_ext)
ggplot() +
  geom_spatvector(data = lga_crop, aes(fill = LGA_NAME25)) +
  geom_spatvector(data = coastal_basins_200m, aes(fill = "Elevation")) +
  coord_sf(xlim = c(focus_lga_ext[1], focus_lga_ext[2]),
           ylim = c(focus_lga_ext[3], focus_lga_ext[4]),
           expand = TRUE) +
  labs(fill = "LGA")


lines(lga_qld$LGA_NAME25 == focus_lga)

# Focused plot - iteration 2
# NEEDS lga_qld, bioregions_200m and elev_poly loaded
focus_lga <- "Cook"
lga_qld <- mutate(lga_qld, highlight = ifelse(LGA_NAME25 == focus_lga, focus_lga, "Other"))

ggplot() +
  geom_spatvector(data = qld) +
  geom_spatvector(data = bioregions_200m) +
  geom_spatvector(data = lga_qld[lga_qld$LGA_NAME25 == focus_lga, ], 
                  aes(fill = highlight), alpha = 0.5) +
  scale_fill_viridis_d() +
  coord_sf(datum = sf::st_crs(lga_qld))

is.related(lga_qld[lga_qld$LGA_NAME25 == focus_lga, ], elev_poly, relation = "intersects")

# Focused plot - iteration 3
focus_poly <- filter(lga_qld, LGA_NAME25 == focus_lga)
focus_lga_ext <- ext(focus_poly)
bio_crop <- crop(bioregions_200m, focus_lga_ext)

ggplot() +
  geom_spatvector(data = bio_crop, aes(fill = bioregion), colour = "grey20",
                  linewidth = 0.2) +
  geom_spatvector(data = focus_poly, aes(colour = LGA_NAME25), fill = NA,
                  alpha = 0.5, linewidth = 0.9) +
  scale_colour_manual(values = "black", name = "LGA")
  coord_sf(xlim = c(focus_lga_ext[1], focus_lga_ext[2]),
           ylim = c(focus_lga_ext[3], focus_lga_ext[4]),
           expand = NULL) +
  labs()

# Does it touch?
lga_qld$touches <- is.related(lga_qld, bioregions, relation = "intersects")
plot(lga_qld[lga_qld$touches == TRUE, ])
lines(bioregions)

# Using bioregion backdrop
focus_lga <- "Yarrabah"
lga_qld <- mutate(lga_qld, highlight = ifelse(LGA_NAME25 == focus_lga, focus_lga, "Other"))

labels <- centroids(bioregions, inside = TRUE)
ggplot() +
  geom_spatvector(data = qld) +
  geom_spatvector(data = bioregions, aes(fill = bioregion)) +
  geom_spatvector_text(data = labels, aes(label = bioregion), size = 3, colour = "black", fontface = "bold") +
  geom_spatvector(data = lga_qld[lga_qld$LGA_NAME25 == focus_lga, ], 
                  aes(fill = highlight), alpha = 0.5, colour = "red") +
#  scale_fill_viridis_d() +
  coord_sf(datum = sf::st_crs(lga_qld))

#is.related(lga_qld[lga_qld$LGA_NAME25 == focus_lga, ], bioregions, relation = "intersects")


# Check if area meets sufficient requirements
focus_lga_poly <- lga_qld[lga_qld$LGA_NAME25 == focus_lga, ]
focus_lga_area <- expanse(focus_lga_poly)
covered <- terra::intersect(focus_lga_poly, bioregions)
covered$area_piece <- expanse(covered)
sum(covered$area_piece) > 0.1 * focus_lga_area
sum(covered$area_piece) / focus_lga_area




