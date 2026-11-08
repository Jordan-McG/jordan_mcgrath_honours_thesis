
# This script runs the monte carlo simulation, and returns some results and a
# ridgeline plot.


# 0. Set values -----------------------------------------------------------

set.seed(2021)
n_sims <- 20000
horizon <- 2046 - 2021


# 1. Draw growth rates ---------------------------------------------------

# Draw intercept (a; 2021) and slope (b) from linear model
draw_ratio <- function(model, lower = 0, upper = Inf, n = n_sims) {
  draws <- MASS::mvrnorm(n, mu = coef(model), Sigma = vcov(model))
  a <- draws[,1]
  b <- draws[,2]
  # Clamp both populations to be nonnegative, and between 0 to 1 for populations
  d2021 <- pmin(pmax(a, 1e-3), upper) # clamp to above zero to avoid dividing by zero 
  d2046 <- pmin(pmax(a + horizon * b, lower), upper)
  tibble(sim = 1:n, d2021 = d2021, d2046 = d2046, ratio = d2046 / d2021)
}

# Need to get rid of unreliable models for bioregions
density_not_used <- c("5c") # due to majorly uncertain results
prop_not_used <- c("1b", "5c") # results are nonsensical

# Use sampling function to get density ratio
density_fitted <- negbin_results |> 
  filter(!bioregion %in% density_not_used) |> 
  mutate(draws = map(negbin, draw_ratio)) |> 
  select(bioregion, draws) |> 
  unnest(draws) |> 
  select(-d2021,-d2046) # remove for 2021 and 2046 values

# Use sampling function to get density ratio
prop_fitted <- prop_results |> 
  filter(!bioregion %in% prop_not_used) |> 
  mutate(draws = map(prop_model, draw_ratio, upper = 1)) |> # modified upper limit
  select(bioregion, draws) |> 
  unnest(draws) |> 
  select(-d2021,-d2046) # remove for 2021 and 2046 values
  

# 2. Use neighbour lookups -----------------------------------------------

# Neighbours = adjacent bioregions on the Taplin bioregion map that have their
# own fitted model. Where no adjacent bioregion has one, the next one along.

# Density lookup table
density_neighbours <- tribble(
  ~bioregion, ~neighbour,
  "1a", "1b",                   # only neighbour
  "5a", "4", "5a", "5b",        # both adjacent
  "5c", "5b", "5c", "6b",       # adjacent, and 6a and 7 have no estimate
  "6a", "5b"                    # adjacent, and 5c has no estimate
)

# Proportion large lookup table 
prop_neighbours <- tribble(
  ~bioregion, ~neighbour,
  "1a", "1c",                   # via 1b, which itself uses 1c
  "1b", "1c",                   # adjacent
  "5a", "4", "5a", "5b",        # both adjacent
  "5c", "5b",                   # only adjacent bioregion with size data
  "6a", "5b",                   # adjacent, and 5c has no size data
  "6b", "5b"                    # neighbours 5c and 7 lack size data (uses next)
)

borrow_method = "mean" # or pick for random pick

borrow_draws <- function(fitted_draws, neighbours, method = borrow_method) {
  usable <- neighbours |> filter(neighbour %in% fitted_draws$bioregion)
  
  if (method == "mean") {
    usable |> 
      left_join(fitted_draws, by = c("neighbour" = "bioregion"),
                relationship = "many-to-many") |> 
      group_by(bioregion, sim) |> 
      summarise(ratio = mean(ratio), .groups = "drop")
  } else {
    usable |> 
      group_by(bioregion) |> 
      reframe(sim = 1:n_sims, 
              neighbour = sample(neighbour, n_sims, replace = TRUE)) |> 
      left_join(fitted_draws, by = c("neighbour" = "bioregion", "sim"),
                relationship = "many-to-many") |> 
      select(bioregion, sim, ratio)
  }
}

density_draws <- bind_rows(
  density_fitted |> mutate(density_source = "fitted"),
  borrow_draws(density_fitted, density_neighbours) |> mutate(density_source = "neighbour")
) |> rename(density_ratio = ratio)

prop_draws <- bind_rows(
  prop_fitted |> mutate(prop_source = "fitted"),
  borrow_draws(prop_fitted, prop_neighbours) |> mutate(prop_source = "neighbour")
) |> rename(prop_ratio = ratio)



# 3. Combine the croc factors per draw -----------------------------------

croc_draws <- inner_join(density_draws, prop_draws, by = c("bioregion", "sim")) |> 
  mutate(croc_ratio = density_ratio * prop_ratio) # change in large crocs per km

summarise_draws <- function(x) {
  if (all(is.na(x))) {  # included for NA bioregions e.g. bioregion 7
    return(tibble(median = NA_real_, lower = NA_real_,
                  upper = NA_real_, p_increase = NA_real_))
  }
  tibble(median = median(x, na.rm = TRUE),
         lower = quantile(x, 0.025, na.rm = TRUE),
         upper = quantile(x, 0.975, na.rm = TRUE),
         p_increase = mean(x > 1, na.rm = TRUE))
  
}

(bioregion_summary <- croc_draws |> 
  group_by(bioregion, density_source, prop_source) |> 
  reframe(density = summarise_draws(density_ratio),
          prop    = summarise_draws(prop_ratio),
          croc    = summarise_draws(croc_ratio)) |> 
  unnest(c(density, prop, croc), names_sep = "_"))


# 4. Combine human draws --------------------------------------------------

(lga_growth <- read_excel("Data/LGA Multiplicative Growth Rate.xlsx") |> 
   clean_names(case = "snake") |> 
   rename(LGA_NAME25 = local_government_area_a) |> 
   select(-"percent_change"))

(lga_bioregions <- read.csv("Data/lga_qld_bioregions.csv") |> 
    select(x, bioregion) |> 
    rename(LGA_NAME25 = x) |> 
    as_tibble())

(lga_draws <- lga_growth |> 
  left_join(lga_bioregions, by = "LGA_NAME25") |> 
  drop_na(bioregion) |> 
  left_join(croc_draws, by = "bioregion", relationship = "many-to-many") |>
  mutate(risk_ratio = growth * croc_ratio))

(lga_risk <- lga_draws |> 
  group_by(LGA_NAME25, bioregion, growth) |> 
  reframe(summarise_draws(risk_ratio)) |> 
  rename(risk_median = median, risk_lower = lower, risk_upper = upper))

lga_risk |> print(n = Inf)
lga_risk |> filter(is.na(risk_median))

lga_risk |> filter(LGA_NAME25 %in% c("Isaac", "Livingstone", "Whitsunday", "Hope Vale",
                                     "Cook", "Torres", "Torres Strait Island", "Mornington", "Palm Island"))

lga_risk |> filter(is.na(risk_median))   # should be exactly 7 rows (bioregion 7)


# Ridgeline plot

library(ggridges)   # install.packages("ggridges") if needed

lga_draws |> 
  filter(!is.na(risk_ratio)) |> 
  mutate(LGA = fct_reorder(LGA_NAME25, risk_ratio, median)) |> 
  ggplot(aes(x = risk_ratio, y = LGA)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975),
                      scale = 1.5, rel_min_height = 0.01, from = 0, fill = "#2a78d6", alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Change in risk, 2046 / 2021", y = NULL) +
  theme_bw()

# Crocodile growth factors
croc_summary <- croc_draws |> 
  group_by(bioregion) |> 
  reframe(summarise_draws(croc_ratio))


