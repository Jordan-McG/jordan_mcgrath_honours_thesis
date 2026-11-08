
# This script provides tables and plot for the risk product.


# One growth table  -----------------------------------------------------

# Count usable neighbours (those with their own fitted model) per bioregion
count_neighbours <- function(neighbours, fitted_draws) {
  neighbours |> 
    filter(neighbour %in% fitted_draws$bioregion) |> 
    count(bioregion, name = "n")
}

density_n <- count_neighbours(density_neighbours, density_fitted) |> 
  rename(density_n = n)
prop_n <- count_neighbours(prop_neighbours, prop_fitted) |> 
  rename(prop_n = n)

(growth_table <- bioregion_summary |> 
    mutate(density_estimated = density_source == "neighbour",
           prop_estimated    = prop_source == "neighbour") |> 
    left_join(density_n, by = "bioregion") |> 
    left_join(prop_n, by = "bioregion") |> 
    mutate(density_n = if_else(density_estimated, density_n, NA_integer_),
           prop_n    = if_else(prop_estimated, prop_n, NA_integer_)) |> 
    select(bioregion,
           density_gf = density_median, density_lower, density_upper, density_estimated, density_n,
           prop_gf = prop_median, prop_lower, prop_upper, prop_estimated, prop_n) |> 
    bind_rows(tibble(bioregion = "7", density_gf = 0, density_estimated = FALSE,
                     prop_estimated = FALSE)) |> 
    arrange(bioregion) |> 
    mutate(across(c(density_gf, density_lower, density_upper,
                    prop_gf, prop_lower, prop_upper), ~ round(.x, 4))))

# Two tables --------------------------------------------------------

# List usable neighbours (those with their own fitted model) per bioregion
list_neighbours <- function(neighbours, fitted_draws) {
  neighbours |> 
    filter(neighbour %in% fitted_draws$bioregion) |> 
    group_by(bioregion) |> 
    summarise(n_neighbours = n(),
              borrowed_from = paste(neighbour, collapse = ", "))
}

density_nb <- list_neighbours(density_neighbours, density_fitted)
prop_nb    <- list_neighbours(prop_neighbours, prop_fitted)


# Density growth factors
(density_gf <- density_draws |> 
   group_by(bioregion, density_source) |> 
   reframe(summarise_draws(density_ratio)) |> 
   left_join(density_nb, by = "bioregion") |> 
   mutate(estimated     = density_source == "neighbour",
          n_neighbours  = if_else(estimated, n_neighbours, NA_integer_),
          borrowed_from = if_else(estimated, borrowed_from, NA_character_)) |> 
   select(bioregion, growth_factor = median, lower, upper, p_increase,
          estimated, n_neighbours, borrowed_from) |> 
   bind_rows(tibble(bioregion = "7", growth_factor = 0, estimated = FALSE)) |> 
   arrange(bioregion) |> 
   mutate(across(c(growth_factor, lower, upper, p_increase), ~ round(.x, 4))))

write.csv(density_gf, "Data/density_growth_factor_table.csv", row.names = FALSE)

# Proportion of large growth factors
(prop_gf <- prop_draws |> 
   group_by(bioregion, prop_source) |> 
   reframe(summarise_draws(prop_ratio)) |> 
   left_join(prop_nb, by = "bioregion") |> 
   mutate(estimated     = prop_source == "neighbour",
          n_neighbours  = if_else(estimated, n_neighbours, NA_integer_),
          borrowed_from = if_else(estimated, borrowed_from, NA_character_)) |> 
   select(bioregion, growth_factor = median, lower, upper, p_increase,
          estimated, n_neighbours, borrowed_from) |> 
   bind_rows(tibble(bioregion = "7", growth_factor = NA_real_, estimated = FALSE)) |> 
   arrange(bioregion) |> 
   mutate(across(c(growth_factor, lower, upper, p_increase), ~ round(.x, 4))))

write.csv(prop_gf, "Data/prop_growth_factor_table.csv", row.names = FALSE)


# Risk product table with uncertainty -------------------------------------

# lga_risk has the median, 95% interval and p_increase of the risk product.
# density_gf and prop_gf are the bioregion growth factor tables.

(risk_table <- lga_risk |> 
   left_join(density_gf |> select(bioregion, density_gf = growth_factor),
             by = "bioregion") |> 
   left_join(prop_gf |> select(bioregion, prop_gf = growth_factor),
             by = "bioregion") |> 
   select(LGA = LGA_NAME25, Bioregion = bioregion,
          `Human growth` = growth,
          `Croc density growth` = density_gf,
          `Large croc growth` = prop_gf,
          `Risk product` = risk_median,
          Lower = risk_lower,
          Upper = risk_upper,
          `P(increase)` = p_increase) |> 
   mutate(croc = `Croc density growth` * `Large croc growth`) |> 
   arrange(LGA))


(results_poly <- merge(lga_qld |> rename(LGA = LGA_NAME25), risk_table, by = "LGA"))

ggplot() +
  geom_spatvector(data = qld) + 
  geom_spatvector(data = results_poly, aes(fill = `Risk product`)) +
  scale_fill_viridis_c(option = "turbo") +
  theme_minimal() +
  coord_sf(datum = "EPSG:3577")

write.csv(risk_table, "Data/lga_risk_table.csv", row.names = FALSE)
