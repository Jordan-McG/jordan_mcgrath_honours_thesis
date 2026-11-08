
# This script imports and cleans the state and lga boundaries.



################################################################################

### QLD POPULATION ###

# Import QLD boundary polygon
state <- vect("Data/State Lines/STE_2021_AUST_GDA2020.shp") #2021
# qld <- state[state$STE_CODE21 %in% 3,]
# qld <- project(qld, "EPSG:3577")
qld <- state |> 
  subset(state$STE_CODE21 %in% 3) |> 
  project("EPSG:3577")

################################################################################

### Local Government Areas (LGAs) ###

# Import and clean for QLD LGAs
lga <- vect("Data/LGAs/LGA_2025_AUST_GDA2020.shp")
lga <- project(lga, "EPSG:3577")
lga_qld <- crop(lga, qld)
lga_qld <- lga_qld[lga_qld$STE_NAME21 %in% "Queensland"]

# Alternative piped workflow
lga <- vect("Data/LGAs/LGA_2025_AUST_GDA2020.shp")
(lga_qld <- lga |> 
  project("EPSG:3577") |> 
  crop(qld) |> 
  filter(STE_NAME21 == "Queensland"))
  
print(values(lga_qld[, c("LGA_NAME25", "STE_NAME21")])) #check only qld
plot(lga_qld)



