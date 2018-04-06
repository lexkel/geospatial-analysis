  
  #==================================================================================
  #
  # Spatial shape splitting
  #
  #
  #==================================================================================
  
    # Setup
    rm(list=ls())
    library(sf)
    library(tidyverse)
    library(lazyeval)
    library(readxl)
    library(writexl)
    library(ggrepel)
    library(ggpubr)
    library(ggmap)
    
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
  
    # Target regions to be split - e.g. destination zone
    split.target <- st_read("./Custom/Townships.shp")
    split.target <- st_buffer(split.target, 0)
    
    # Regions to be used to split the target layer - e.g. mornington peninsula townships
    split.by <- st_read("./DZN/DZN_2016_AUST.shp")
    split.by <- st_buffer(split.by, 0)
  
  #----------------------------------------------------------------------------------
  # Check coordinate reference systems of both layers is the same - project if not
  #----------------------------------------------------------------------------------
  
    st_crs(split.target) == st_crs(split.by)  
  
  #----------------------------------------------------------------------------------
  # Split
  #----------------------------------------------------------------------------------  
  
    # Find intersections and calculate overlapping area (to use as a threshold)
    int <- st_intersection(split.target, split.by)
    int$overlap.pct <- round(as.numeric(st_area(int)) / as.numeric(st_area(split.by)) * 100, 2) 
    int <- int %>% filter(overlap.pct >= 1.0) # exclude overlaps of less than 1 pct
    
    # Centroids for adding labels
    centroids <- st_centroid(int)
    coords <- st_coordinates(centroids)
    
    # Filter list 
    SA2.list <- c("Point Nepean", "Flinders", "Rosebud - McCrae",
                  "Dromana", "Mount Martha", "Mornington", "Mount Eliza",
                  "Somerville", "Hastings - Somers")
    
    # Base r plot (faster)
    plot(st_geometry(split.by %>% filter(SA2_NAME16 %in% SA2.list)), border = "red", 
        col = adjustcolor("grey", alpha.f = 0.1), lwd = 1)
    plot(st_geometry(split.target), add = TRUE, border = "black", lwd = 2)
    #text(coords[,1], coords[,2], labels = centroids$Zone.Name, pch = 20)
    title(main = "Mornington Peninsula townships",
          sub = "Red lines indicate where destinations zones cut through the townships")
    
    # Ggplot (slower)
    ggplot() +
      geom_sf(data = split.by %>% filter(SA2_NAME16 %in% SA2.list), colour = "red", fill = "grey95")  +
      geom_sf(data = split.target, colour = "black", size = 1, fill = "NA") +
      #geom_text_repel(data = coords %>% as.data.frame(),
      #                aes(x = X, y = Y, label = centroids$Zone.Name), box.padding = 0.25, size = 2.5) +
      labs(title = "Mornington Peninsula townships",
           subtitle = "Red lines indicate where destinations zones cut through the townships") +
      coord_sf()

