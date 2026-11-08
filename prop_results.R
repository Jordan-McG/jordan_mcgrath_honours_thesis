
# This script runs the proportion-large model.


# Proportion of large crocodiles ------------------------------------------

# Define binomial glm
fit_prop <- function(df) {
  glm(prop_large ~ I(year - 2021),
      family = quasibinomial(link = make.link("identity")),
      weights = nh,
      start = c(weighted.mean(df$prop_large, df$nh), 0),
      data = df)
}

# Pull out binomial results
prop_results <- crocs_cleaned |>
  filter(!is.na(prop_large), nh > 0) |> 
  group_by(bioregion) |> 
  filter(n() >= 3, n_distinct(year) > 1) |> 
  nest() |> 
  ungroup() |> 
  mutate(prop_model = map(data, fit_prop),
         converged  = map_lgl(prop_model, ~ .x$converged),
         dispersion = map_dbl(prop_model, ~ summary(.x)$dispersion),
         p2021      = map_dbl(prop_model, ~ coef(.x)[1]),
         slope      = map_dbl(prop_model, ~ coef(.x)[2]))

prop_results |> select(bioregion, converged, dispersion, p2021, slope)

# Can check model summary using list notation - currently checking bioregion 2
summary(prop_results$prop_model[[4]])

# Plot linear fit on data
prop_results |> 
  mutate(fit = map(prop_model, fitted)) |> 
  unnest(c(data, fit)) |>
  ggplot(aes(x = year)) +
    geom_point(aes(y = prop_large, size = nh), colour = "grey40", alpha = 0.5) +
    geom_line(aes(y = fit), colour = "blue") +
    facet_wrap(~ bioregion) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_size_area(max_size = 4, name = "Non-hatclings\n(weight)") +
    labs(x = "Year", y = "Proportion of large crocodiles (>= 6ft)") +
    theme_bw()

# Predict forward
(prop_pred <- prop_results |> 
    #mutate(pred = map(negbin, ~ {
    #nd <- data.frame(year = 1979:2046, transect_km = 1)  
    mutate(pred = map2(prop_model, data, ~ {
      nd <- data.frame(year = min(.y$year):2046)
      p <- predict(.x, newdata = nd, type = "link", se.fit = TRUE)
      tibble(year = nd$year,
             prop_large = p$fit,
             lower = p$fit - 1.96 * p$se.fit,
             upper = p$fit + 1.96 * p$se.fit)
    })) |> 
    select(bioregion, pred) |> 
    unnest(pred) |> 
    mutate(across(c(prop_large, lower, upper), ~pmin(pmax(.x, 0), 1)))) # text out to see negative values

# Pull out 2021 and 2046 densities, and take the ratio
prop_pred |> 
  filter(year %in% c(2021, 2046)) |> 
  select(bioregion, year, prop_large) |> 
  pivot_wider(names_from = year, values_from = prop_large, names_prefix = "l") |> 
  mutate(ratio = l2046 / l2021)

# Plot results
ggplot() +
  geom_point(data = unnest(prop_results, data), 
             aes(x = year, y = prop_large, size = nh),
             colour = "grey40", alpha = 0.5) +
  geom_ribbon(data = prop_pred, aes(x = year, ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  geom_line(data = prop_pred, aes(x = year, y = prop_large), 
            colour = "blue", linewidth =0.8) +
  geom_vline(xintercept = c(2021, 2046), linetype = "dashed") +
  facet_wrap(~ bioregion, ncol = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Year", y = "Proportion of large crocodiles (>= 6ft)") +
  theme_bw()
