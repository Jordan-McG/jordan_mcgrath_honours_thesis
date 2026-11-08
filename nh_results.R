
# This script runs the non-hatchling density model, first for Poisson and checking 
# dispersion, then for negative binomial.

crocs_raw <- read_excel("Data/Taplin Raw Data Cleaned.xlsx")

(crocs_cleaned <- crocs_raw |> 
    clean_names(case = "snake") |>
    distinct() |> 
    mutate(start_date = as.Date(start_date, format = "%d/%m/%Y")) |> 
    #mutate(prop_large = 1 - as.numeric(proportion_less_than_6ft)) |> 
    filter_out(data_quality %in% c("B", "BQ")) |> 
 #   filter_out(aggregation_system == "Proserpine River System") |> # Taplin app. 4
    mutate(year = year(start_date)) |> 
    relocate(year, .after = start_date) |> 
    mutate(proportion_less_than_6ft = as.numeric(proportion_less_than_6ft),
           prop_large = 1 - proportion_less_than_6ft,
           x2_3ft = as.numeric(x2_3ft),
           nh_excl_2_3ft = nh - x2_3ft,
           count = nh) |> 
    relocate(prop_large, .after = proportion_less_than_6ft) |> 
    relocate(nh_excl_2_3ft, .after = nh)
)

# Checking dispersion in Poisson to justify -------------------------------

# Define poisson glm
fit_poisson <- function(df) {
  glm(count ~ 0 + transect_km + I(transect_km * (year - 2021)),
    family = poisson(link = "identity"),
    start = c(sum(df$count) / sum(df$transect_km), 0),
    data = df)
}

# Pull out model fit and calculate dispersion
dispersion_poisson_results <- crocs_cleaned |> 
  filter(!is.na(count)) |> 
  group_by(bioregion) |> 
  filter(n_distinct(year) > 1, sum(count) > 0) |> 
  nest() |> 
  ungroup() |> 
  mutate(pois = map(data, fit_poisson),
         pois_disp = map_dbl(pois, ~ sum(residuals(.x, type = "pearson")^2) / 
                               df.residual(.x))) # calculate via pearson chi^2 residuals

dispersion_poisson_results

# Plot dispersion as a col graph
ggplot(dispersion_poisson_results, aes(bioregion, pois_disp)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  labs(x = "Bioregion", y = "Dispersion")

# Plot linear fit on data
dispersion_poisson_results |> 
  mutate(fit = map(pois, fitted)) |> 
  unnest(c(data, fit)) |>
  ggplot(aes(year)) +
    geom_point(aes(y = count / transect_km)) +
    geom_line(aes(y = fit / transect_km), colour = "blue") +
    facet_wrap(~ bioregion, scales = "free_y") +
  labs(x = "Year", y = "Density (NH/KM)")




# Running negative binomial -----------------------------------------------

# Define negative binomial glm
fit_negbin <- function(df) {
  MASS::glm.nb(count ~ 0 + transect_km + I(transect_km * (year - 2021)),
      link = identity,
      start = c(sum(df$count) / sum(df$transect_km), 0),
      data = df)
}

# Pull out negative binomial results
(negbin_results <- crocs_cleaned |> 
  filter(!is.na(count)) |> 
  group_by(bioregion) |> 
  filter(n_distinct(year) > 1, sum(count) > 0) |> 
  nest() |> 
  ungroup() |> 
  mutate(negbin = map(data, fit_negbin),
         theta = map_dbl(negbin, ~ .x$theta),
         nb_disp = map_dbl(negbin, ~ sum(residuals(.x, type = "pearson")^2) /
                             df.residual(.x))))

negbin_results |> select(bioregion, theta, nb_disp)

# Can check model summary using list notation - currently checking bioregion 2
summary(negbin_results$negbin[[4]])

# Plot linear fit on data
negbin_results |> 
  mutate(fit = map(negbin, fitted)) |> 
  unnest(c(data, fit)) |>
  ggplot(aes(x = year)) +
    geom_point(aes(y = count / transect_km)) +
    geom_line(aes(y = fit / transect_km), colour = "blue") +
    facet_wrap(~ bioregion, scales = "free_y") +
    labs(x = "Year", y = "Density (NH/KM)")

# Predict forward
(negbin_pred <- negbin_results |> 
  #mutate(pred = map(negbin, ~ {
    #nd <- data.frame(year = 1979:2046, transect_km = 1)  
  mutate(pred = map2(negbin, data, ~ {
    nd <- data.frame(year = min(.y$year):2046, transect_km = 1)
    p <- predict(.x, newdata = nd, type = "link", se.fit = TRUE)
    tibble(year = nd$year,
           density = p$fit,
           lower = p$fit - 1.96 * p$se.fit,
           upper = p$fit + 1.96 * p$se.fit)
})) |> 
  select(bioregion, pred) |> 
  unnest(pred) |> 
  mutate(across(c(density, lower, upper), ~pmax(.x, 0)))) # text out to see negative values

# Pull out 2021 and 2046 densities, and take the ratio
negbin_pred |> 
  filter(year %in% c(2021, 2046)) |> 
  select(bioregion, year, density) |> 
  pivot_wider(names_from = year, values_from = density, names_prefix = "d") |> 
  mutate(ratio = d2046 / d2021)

# Plot results
ggplot() +
  geom_point(data = unnest(negbin_results, data), 
             aes(x = year, y = count / transect_km, size = transect_km),
             colour = "grey40", alpha = 0.5) +
  geom_ribbon(data = negbin_pred, aes(x = year, ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  geom_line(data = negbin_pred, aes(x = year, y = density), 
            colour = "blue", linewidth =0.8) +
  geom_vline(xintercept = c(2021, 2046), linetype = "dashed") +
  facet_wrap(~ bioregion, scales = "free_y") +
  labs(x = "Year", y = "Density excl. 2-3ft size class (NH/km)") +
  theme_bw()
  

 