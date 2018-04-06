
  #==================================================================================
  #
  # Map check
  #
  #==================================================================================
  
    #----------------------------------------------------------------------------------
    # Setup
    #----------------------------------------------------------------------------------
      
      # Set environment
      rm(list=ls())
      library(sf)
      library(readxl)
      library(tidyverse)
    
      # Set directory
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
    # Check map is complete
    #----------------------------------------------------------------------------------
    
      # Import shapefiles
      subregions <- st_read("./SA1/SA1_2016_AUST.shp")
      var <- quo(!! rlang::sym("SA1_7DIG16"))
      
      region <- st_read("./Custom/lilydale.shp")
      #region <- region %>% filter(LGA_NAME16 == "Yarra Ranges (S)")
      
      # Import list of subregions
      #selections <- read.csv("C:/Users/a.kelly/Documents/Projects/Lilydale/southyarra_dzns.csv")
      selections <- read_xlsx("C:/Users/a.kelly/Documents/Projects/Lilydale/lilydale_SA1s.xlsx")
      selections <- selections %>% select(Region) %>% unlist()
      
      # Map
      ggplot() +
        geom_sf(data = subregions %>% filter((!!var) %in% selections), colour = "grey", size = 0.5, fill = "grey95") +
        geom_sf(data = region, colour = "red", size = 1, fill = NA)
        
      
      
      
