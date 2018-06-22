
  #==================================================================================
  #
  # Shapefile: distance between centroids
  #
  #  - Determine what lower grade shapes fall within a higher grade shape.
  #  - Example here is Lake Macquarie LGA and which SA2s comprise it.
  #
  #==================================================================================
  
      # Setup
      rm(list=ls())
      library(sf)
      library(rgeos)
      library(tidyverse)
      library(geosphere)
  
      # Directory
      setwd("C:/Users/a.kelly/Documents/Projects/Geospatial analysis")
      
      # Set map theme
      theme_map <- function(...) {
        theme_classic() +
          theme(
            text = element_text(color = "#22211d"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_line(color = "grey90", size = 0.2),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = NA, color = NA), 
            panel.background = element_rect(fill = NA, color = NA), 
            legend.background = element_rect(fill = NA, color = NA),
            panel.border = element_blank(),
            ...
          )
      }
      theme_set(theme_map())
    
    #----------------------------------------------------------------------------------
    # Import shapefiles
    #----------------------------------------------------------------------------------
    
      # LGA layer shapefile
      STE <- st_read("./STE/STE_2016_AUST.shp") %>% st_transform(crs = 4283)
      LGA <- st_read("./LGA/LGA_2016_AUST.shp") %>% st_transform(crs = 4283)
      VIC <- LGA %>% filter(STE_CODE16 == 2)
      
      # Capital city
      MLB <- VIC %>% filter(LGA_NAME16 == "Melbourne (C)")
      MLB.point <- st_centroid(MLB)
      
      # LGA centroid - drop empty polygons for No usual address and Migratory ...
      VIC <- VIC[!st_is_empty(VIC),]
      VIC.points <- st_centroid(VIC)
        
      # Plot map with joining great circles lines
      # https://www.jessesadler.com/post/great-circles-sp-sf/
      ggplot() +
         geom_sf(data = STE %>% filter(STE_CODE16 == 2), colour = "grey75", fill = "grey95") +
         geom_sf(data = MLB.point, colour = "red", size = 1.5) +
         geom_sf(data = VIC.points, colour = "grey25", size = 1.5)
      
      distances <- MLB.point %>% st_distance(VIC.points)
      distances
      