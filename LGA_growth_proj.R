
# This script is being used to create a table of LGA projection data and their
# respective growth rates, as well as produce a map. 


# Bring in bioregion designation file to get relevant lgas
relevant_lga <- read.csv("Data/lga_qld_bioregions.csv", row.names = 1) |> 
  drop_na(bioregion) |> 
  rename(lga = x)

# Import projection data and filter for what we care about, and JOIN bioregion
lga_proj <- read_excel("Data/LGA_projection_data.xlsx") |> 
  rename(lga = `Local Government Area`) |> 
  filter(lga %in% relevant_lga$lga) |> 
  left_join(relevant_lga, by = "lga")

##### Plot of each relevant lga through time #####
lga_proj_long <- lga_proj |> 
  pivot_longer(
    cols = `2021`:`2046`,
    names_to = "year",
    values_to = "population"
  ) |> 
  mutate(year = as.numeric(year))

lga_proj_plot1 <- c("1a", "1b", "1c", "1d", "2", "3", "4", "5a")
lga_proj_plot2 <- c("5b", "5c", "6a", "6b", "7")

lga_proj_long |> 
  filter(bioregion %in% lga_proj_plot1) |> 
  ggplot(aes(x = year, y = population, colour = lga)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ bioregion, ncol = 2) +
    theme(legend.position = "none")
lga_proj_long |> 
  filter(bioregion %in% lga_proj_plot2) |> 
  ggplot(aes(x = year, y = population, colour = lga)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bioregion, ncol = 2) +
  theme(legend.position = "none")

##### 

# Calculate 
lga_final <- lga_proj |> 
  select(-`2026`, -`2031`, -`2036`, -`2041`) |> 
  mutate(
    growth_factor = `2046` / `2021`,
    absolute_change = `2046` - `2021`,
    percent_change = (growth_factor - 1) * 100
  ) |> 
  relocate(bioregion, .after = lga)

write.csv(lga_final, "Data/lga_with_start_and_end.csv", row.names = FALSE)

# Plot on map
lga <- vect("Data/LGAs/LGA_2025_AUST_GDA2020.shp")
(lga_qld <- lga |> 
    project("EPSG:3577") |> 
    crop(qld) |> 
    filter(STE_NAME21 == "Queensland"))

lga_spat_growth <- terra::merge(lga_qld, lga_final, by.x = "LGA_NAME25", by.y = "lga")
ggplot() +
  geom_spatvector(data = qld, linewidth = 0.3) +
  geom_spatvector(data = lga_spat_growth, aes(fill = growth_factor), colour = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "Growth Factor\n2021-2046") +
  theme_minimal() +
  coord_sf(datum = "EPSG:3577")



