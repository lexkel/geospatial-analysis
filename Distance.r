
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
      
    #----------------------------------------------------------------------------------
    # Calculate distances between LGA centroids and capital city centroid
    #----------------------------------------------------------------------------------  
    
      # Capital cities
      capital.cities <- LGA %>% 
                          filter(LGA_NAME16 %in% c("Sydney (C)", "Melbourne (C)", "Brisbane (C)", "Adelaide (C)", "Perth (C)", 
                                                   "Hobart (C)", "Darwin (C)", "Unincorporated ACT"))
      capital.cities <- st_centroid(capital.cities)

      # LGA centroid - drop empty polygons for "No usual address" and "Migratory - Offshore - Shipping" zones
      LGA <- LGA[!st_is_empty(LGA),]
      LGA.points <- st_centroid(LGA)

      dist.from.capital.cities <- map_df(c(1:8), function(x) {
        x <- capital.cities[x,]
        y <- LGA.points %>% filter(STE_CODE16 == x$STE_CODE16)
        z <- st_distance(x, y)
        z <- data.frame(LGA_CODE16 = LGA.points %>% filter(STE_CODE16 == x$STE_CODE16) %>% select(LGA_CODE16) %>% st_set_geometry(NULL) %>% unname() %>% unlist(),
                        State = x$STE_CODE16,
                        Distance = as.vector(z))
      })
      
    #----------------------------------------------------------------------------------
    # Plot distances between LGA centroids and capital city centroid by state
    #----------------------------------------------------------------------------------    
      
      state <- 2
      
      geo <- LGA.points %>% 
              filter(STE_CODE16 == state)
      
      councils <- LGA.points %>% 
                    filter(STE_CODE16 == state) %>%
                    mutate(id = row_number()) %>%
                    st_set_geometry(NULL)
      
      capital <- capital.cities %>% 
                  filter(STE_CODE16 == state) %>% 
                  slice(rep(1, each = nrow(councils))) %>% 
                  mutate(id = 1:nrow(councils)) %>% 
                  st_set_geometry(NULL)
      
      points <- capital %>% 
                  bind_rows(councils) %>%
                  left_join(geo)
      
      lines <- st_as_sf(points) %>% 
                  group_by(id) %>% 
                  summarise(do_union = FALSE) %>% 
                  st_cast("LINESTRING") 
      
      councils <- councils %>% left_join(geo)
      
      capital <- capital %>% left_join(geo)
      
      furthest <- councils %>% 
                    filter(LGA_CODE16 == dist.from.capital.cities %>% 
                             filter(State == state) %>% 
                             filter(Distance == max(Distance)) %>% 
                             select(LGA_CODE16) %>% 
                             unlist())
      
      # Plot map with centroids
      ggplot() +
        geom_sf(data = STE %>% filter(STE_CODE16 == state), colour = "grey75", fill = "grey95") +
        geom_sf(data = LGA %>% filter(STE_CODE16 == state), colour = "grey75", fill = "grey95") +
        geom_sf(data = lines, colour = "grey70", alpha = 0.5) +
        geom_sf(data = councils, colour = "grey25", size = 1.5) +
        geom_sf(data = capital, colour = "red", size = 3) +
        geom_sf(data = furthest, colour = "blue", size = 3)
        #labs(title = STE %>% filter(STE_CODE16 == state) %>% select(STE_NAME16) %>% st_set_geometry(NULL) %>% unlist())

                                             