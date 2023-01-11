### Decay-Weighted Crows Fly Distance Accessibility Script
### Henry McKay (Henry.McKay@dot.ca.gov)
### Last Updated: 01/11/2023

# Load needed packages
library(dplyr)
library(sf)
library(mapview)

# Read in Shapefile of grid (or origin) points. Points must have two required columns:
  #1. grid_code: The value of the opportunities being measured. In this case, the number of opportunities within the grid cell.
  #2. Point_ID: A unique id for each grid in character format.
grid_points <- st_read("Path to shapefile")

# Transform the grid points to your preferred CRS
grid_points <- st_as_sf(grid_points) %>%
  st_transform(crs = 3857)

# Create a SF dataset for destination points by filtering grid points to only those with opportunities > 0 (this reduces computing time)
Dest_Points <- grid_points %>%
  filter(grid_code > 0)

# Define origin points for analysis. If dataset is large, it may be useful to limit these or break them into chunks
origins <- grid_points

# For loop to perform crows fly access calcualtions for each origin in the defined dataset
out = NULL
for(i in 1:nrow(origins)) {
  
  # Create a buffer around the origin point and select points within that buffer
  # Define buffer distance (in miles)
  buffer_dis <- 5
  buffer <- st_buffer(origins[i, ], (buffer_dis * 1609.34))
  intersected_points <- st_intersection(Dest_Points, buffer)
  
  # Select origin point ID and remove spatial data
  origin <- origins[i, ] %>%
    select(Point_ID) %>%
    st_drop_geometry()
  
  # If the sum of opportunities within the buffer is > 1, perform access calculations
    if(nrow(intersected_points) >= 1) {
  
      # Calculate distance matrix between origin and point and all destination points within buffer
      dist <- st_distance(origins[i, ], intersected_points)
      dist <- matrix(dist, ncol = 1)
      # Add distance to intersected points DF
      intersected_points$dist <- dist
      
      # Decay-weight opportunities by travel time
      # Define travel speed (in MPH)
      speed <- 10
      # Define time cutoff (in minutes)
      cutoff <- 60
      intersected_points <- as.data.frame(intersected_points) %>%
      mutate(decay_weighted_opps = grid_code * exp(log(0.5) * (60*(dist * 0.000621371) / speed) / cutoff))
      
      # Sum decay-weighted opportunities by origin
      access <- sum(intersected_points$decay_weighted_opps)
      access_df <- data.frame(origin, access)
  
      out <- rbind.data.frame(access_df, out)
      
      # Print % progress (optional)
      print(i / nrow(origins))
    
    # If the sum of opportunities within the buffer is zero, access is zero
    } else {
      access <- 0
      access_df <- data.frame(origin, access)
      out <- rbind.data.frame(access_df, out)
      print(i / nrow(origins))
    }
}

# Write output to CSV
write.csv(out, "/Users/Username/Downloads/CrowsFlyWeighted.csv", na = "")

