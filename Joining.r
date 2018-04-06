  #==================================================================================
  #
  # Spatial shape joining
  #
  #
  #==================================================================================
  
  rm(list = ls())
  library(sf)
  library(tidyverse)
  library(writexl)
  
  setwd("C:/Users/a.kelly/Documents/Projects/Geospatial analysis")
  
  # Shapefile with regions to join
  areas <- st_read("./DZN/DZN_2016_AUST.shp")
  name <- "lilydale"
  
  # Selection of regions to join
  selection <- c("212782543", "212782531", "212782544", "212782548", "212782539", "212782547", "212782546", "212782538", "212782545")
  
  # Filter areas for just selected regions
  output <- areas %>% filter(DZN_CODE16 %in% selection)
  
  # Join region
  output <- st_union(output)
  
  # Plot it to check it looks right
  plot(st_geometry(output))
  
  # Save it
  st_write(output, paste0(name, ".shp"))
