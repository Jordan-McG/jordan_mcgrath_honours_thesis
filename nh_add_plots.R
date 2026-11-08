
# This script gives additional plots for both density and proportion results.


# Take glm results and filter to the range without projection
survey_data <- unnest(negbin_results, data)
negbin_fit_range <- negbin_pred |> 
  left_join(survey_data |> group_by(bioregion) |> summarise(last_yr = max(year)),
            by = "bioregion") |> 
  filter(year <= last_yr)
  

# Faceted plot ------------------------------------------------------------

plot_rivers <- function(bioregions) {
  ggplot(filter(survey_data, bioregion %in% bioregions),
         aes(x = year, y = count / transect_km)) +
    # Waterway level lines
    geom_smooth(aes(colour = waterway_level_1), method = "lm", formula = y ~ x,
                linetype = "dashed", se = FALSE, linewidth = 0.5, show.legend = FALSE) +
    geom_point(aes(colour = waterway_level_1), alpha = 0.6, show.legend = FALSE) +
    # Negative binomial results
    geom_ribbon(data = filter(negbin_fit_range, bioregion %in% bioregions),
                aes(x = year, ymin = lower, ymax = upper),
                inherit.aes = FALSE, fill = "grey20", alpha = 0.15) +
    geom_line(data = filter(negbin_fit_range, bioregion %in% bioregions),
              aes(x = year, y = density),
              inherit.aes = FALSE, colour = "black", linewidth = 0.9) +
    facet_wrap(~bioregion, ncol = 3, scales = "free_y") +
      labs(title = "Non-hatchling density between 1979 and 2019 \n by bioregion",
           y = "Density excl. 2-3ft (NH/km)", x = "Year") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
} 

bioregion_glm_plot1 <- c("1b", "1c", "1d", "2", "3", "4") #first six panels
bioregion_glm_plot2 <- c("5b", "5c", "6b", "7") #second four panels
bioregion_glm_plot3 <- c("1b", "1c", "1d", "2", "3", "4", "5b", "5c", "6b", "7")
plot_rivers(bioregion_glm_plot1)
plot_rivers(bioregion_glm_plot2)
plot_rivers(bioregion_glm_plot3) # change ncol for larger plot


# Faceted plots -----------------------------------------------------------

survey_data      <- unnest(negbin_results, data)
prop_survey_data <- unnest(prop_results, data)
km_limits <- range(survey_data$transect_km, na.rm = TRUE)
nh_limits <- range(prop_survey_data$nh, na.rm = TRUE)

# Density
plot_density <- function(bioregions, ncol = 2) {
  ggplot() +
    geom_point(data = filter(survey_data, bioregion %in% bioregions),
               aes(x = year, y = count / transect_km, size = transect_km),
               colour = "grey40", alpha = 0.5) +
    geom_ribbon(data = filter(negbin_pred, bioregion %in% bioregions),
                aes(x = year, ymin = lower, ymax = upper),
                fill = "blue", alpha = 0.2) +
    geom_line(data = filter(negbin_pred, bioregion %in% bioregions),
              aes(x = year, y = density),
              colour = "blue", linewidth = 0.8) +
    geom_vline(xintercept = c(2021, 2046), linetype = "dashed") +
    facet_wrap(~ bioregion, ncol = ncol, scales = "free_y") +
    labs(x = "Year", y = "Crocodile density (NH/km)",
         size = "Transect km") +
    scale_size_continuous(limits = km_limits, range = c(1, 5)) +
    theme_bw() +
    theme(legend.position = "bottom")
}

# Proportion of large crocodiles
plot_prop <- function(bioregions, ncol = 2) {
  ggplot() +
    geom_point(data = filter(prop_survey_data, bioregion %in% bioregions),
               aes(x = year, y = prop_large, size = nh),
               colour = "grey40", alpha = 0.5) +
    geom_ribbon(data = filter(prop_pred, bioregion %in% bioregions),
                aes(x = year, ymin = lower, ymax = upper),
                fill = "blue", alpha = 0.2) +
    geom_line(data = filter(prop_pred, bioregion %in% bioregions),
              aes(x = year, y = prop_large),
              colour = "blue", linewidth = 0.8) +
    geom_vline(xintercept = c(2021, 2046), linetype = "dashed") +
    facet_wrap(~ bioregion, ncol = ncol) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Year", y = "Proportion of large crocodiles (>= 6ft)",
         size = "Non-hatchlings") +
    scale_size_continuous(limits = nh_limits, range = c(1, 5)) +
    theme_bw() +
    theme(legend.position = "bottom")
}

bioregion_glm_plot1 <- c("1b", "1c", "1d", "2", "3", "4") # first six panels
bioregion_glm_plot2 <- c("5b", "5c", "6b", "7")           # second four panels
bioregion_glm_plot3 <- c("1b", "1c", "1d", "2", "3", "4", "5b", "5c", "6b", "7")

plot_density(bioregion_glm_plot1)
plot_density(bioregion_glm_plot2, ncol = 2)
plot_density(bioregion_glm_plot3, ncol = 3)

plot_prop(bioregion_glm_plot1)
plot_prop(bioregion_glm_plot2, ncol = 2)
plot_prop(bioregion_glm_plot3, ncol = 3)

# Single plots, one per bioregion -----------------------------------------

plot_fn_2 <- function(x, b){
  
  fit_b <- filter(negbin_pred |> filter(year <= 2019), bioregion == b)
  
  p = ggplot(x, aes(x = year, y = count / transect_km)) +
    geom_smooth(method = "lm", aes(colour = waterway_level_1), formula = y ~ x,
                linetype = "dashed", se = FALSE) +
    geom_point(aes(colour = waterway_level_1)) +
    geom_ribbon(data = fit_b, aes(x = year, ymin = lower, ymax = upper),
                inherit.aes = FALSE, fill = "grey20", alpha = 0.15) +
    geom_line(data = fit_b, aes(x = year, y = density), inherit.aes = FALSE, 
              colour = "black", linewidth = 0.9) +
    labs(colour = "River Name",
        title = paste0("Non-hatchling density between 1979 and 2019 in Bioregion ", b),
        y = "Density excl. 2-3ft (NH/km)", x = "Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
    
    p
  }
  
nested_plots_2 <- survey_data |> 
  nest(.by = bioregion) |> 
  mutate(plot = map2(data, bioregion, plot_fn_2)) # data because that is new column name

walk(nested_plots_2$plot, print)







  