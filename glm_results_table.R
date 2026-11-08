
# This script creates tables for the generalised linear model results.


# Density results table ---------------------------------------------------

(density_table <- negbin_results |> 
   mutate(n_surveys = map_int(data, nrow),
          n_rivers  = map_int(data, ~ n_distinct(.x$waterway_level_1)),
          d2021     = map_dbl(negbin, ~ coef(.x)[1]),
          slope     = map_dbl(negbin, ~ coef(.x)[2]),
          slope_se  = map_dbl(negbin, ~ coef(summary(.x))[2, 2]),
          p_value   = map_dbl(negbin, ~ coef(summary(.x))[2, 4]),
          lower     = slope - 1.96 * slope_se,
          upper     = slope + 1.96 * slope_se) |> 
   select(bioregion, n_surveys, n_rivers, d2021, slope, slope_se, lower, upper,
          p_value, nb_disp))

write.csv(density_table, "Data/glm_density_results.csv", row.names = FALSE)


# Proportion results table ------------------------------------------------

(prop_table <- prop_results |> 
   mutate(n_surveys = map_int(data, nrow),
          n_rivers  = map_int(data, ~ n_distinct(.x$waterway_level_1)),
          slope_se  = map_dbl(prop_model, ~ coef(summary(.x))[2, 2]),
          p_value   = map_dbl(prop_model, ~ coef(summary(.x))[2, 4]),
          lower     = slope - 1.96 * slope_se,
          upper     = slope + 1.96 * slope_se) |> 
   select(bioregion, n_surveys, n_rivers, p2021, slope, slope_se,
          lower, upper, p_value, dispersion))

write.csv(prop_table, "Data/glm_prop_results.csv", row.names = FALSE)

# Both together -----------------------------------------------------------

(glm_results <- bind_rows(
  density_table |> 
    mutate(response = "density") |> 
    rename(start_2021 = d2021, dispersion = nb_disp),
  prop_table |> 
    mutate(response = "prop_large") |> 
    rename(start_2021 = p2021)) |> 
   select(response, bioregion, n_surveys, n_rivers, start_2021, slope, slope_se,
          lower, upper, p_value, dispersion) |> 
   arrange(response, bioregion) |> 
   mutate(across(c(start_2021, slope, slope_se, lower, upper, dispersion), 
                 ~ round(.x, 3)),
          p_value = if_else(p_value < 0.001, "<0.001", as.character(round(p_value, 3)))))
