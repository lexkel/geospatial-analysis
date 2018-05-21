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
  name <- "bendigo_eic"
  
  # Selection of regions to join
  selection <- c("210193460",  "210203502",  "210253485",  "210183483",  "210183441",  "210183445",  "210183446",  "210183447",
                 "210180001",  "210183442",  "210183448",  "210183457",  "210203481",  "210183458",  "210203486",  "210203475",
                 "210203473",  "210203477",  "210213470",  "210213469",  "210203474",  "210203476",  "210203480",  "210203482",
                 "210183468",  "210183456",  "210183454",  "210223493",  "210220002",  "210183449",  "210183452",  "210183453",
                 "210183455",  "210213479",  "210213478",  "210223467",  "210183450",  "210223438",  "210223437",  "210223494",
                 "210223496",  "210223498")
  
  # Filter areas for just selected regions
  output <- areas %>% filter(DZN_CODE16 %in% selection)
  
  # Join region
  output <- st_union(output)
  
  # Plot it to check it looks right
  plot(st_geometry(output))
  
  # Save it
  st_write(output, paste0(name, ".shp"))
