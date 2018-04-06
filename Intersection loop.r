  
  #==================================================================================
  #
  # Spatial shape comparison - loop through top layer regions
  #
  #  - Determine what lower grade shapes fall within a higher grade shape.
  #  - Example here is Lake Macquarie LGA and which SA2s comprise it.
  #
  #==================================================================================
  
  # Setup
    rm(list = ls())
    library(sf)
    library(tidyverse)
    library(lazyeval)
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
  
    # Top layer shapefile (larger area) - e.g. LGA
    top <- st_read("./Custom/Townships.shp")
    
    # Bottom layer shapefile (smaller areas) - e.g. SA2s
    bottom <- st_read("./SA1/SA1_2016_AUST.shp")
  
  #----------------------------------------------------------------------------------
  # Check coordinate reference systems of both layers is the same - project if not
  #----------------------------------------------------------------------------------
    
    st_crs(top) == st_crs(bottom)  
  
  #----------------------------------------------------------------------------------
  # Selection variable names
  #----------------------------------------------------------------------------------    
    
    top.var <- "Zone.Name"
    top.varQ <- quo(!! rlang::sym(top.var))
    
    bottom.var <- "SA1_7DIG16"
    bottom.varQ <- quo(!! rlang::sym(bottom.var))

  #----------------------------------------------------------------------------------
  # Determine overlapping regions with percentage area overlap
  #----------------------------------------------------------------------------------
    
    # Create a vector of all items to loop through
    list.vector <- top  %>% st_set_geometry(NULL) %>% select(!! top.varQ) %>% arrange(!! top.varQ) %>% unlist()
    list.vector <- list.vector %>% unname()
    
    # Create a data frame to save results
    output <- data.frame(matrix(ncol = 3, nrow = 0))
    names(output) <- c("Top_region", "Bottom_region", "Overlap_pct")
    
    # Filter the top region as per list.vector - e.g. Filter LGAs for Lake Macquarie (C)
    for (i in 1:length(list.vector)){
      
      # Select ith item in list.vector
      selection <- list.vector[i] %>% as.character()
      top.select <- top %>% filter((!! top.varQ) == selection)
      top.select <- st_buffer(top.select, 0)
      
      # polygon of portions of bottom layer regions that fall within top layer region
      int <- st_intersection(bottom, top.select)  
      
      # create vector of regions in int
      int.vector <- int %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
      int.vector <- int.vector %>% unname() %>% as.character()
      
      # filter bottom to just include the regions in int.vector
      bottom.select <- bottom %>% filter((!! bottom.varQ) %in% int.vector)
      
      # calculate overlap area percentage 100.00 = completely, <100.00 = somewhat
      int$overlap.pct <- round(as.numeric(st_area(int)) / 
                                 as.numeric(st_area(bottom.select)) * 100, 2)  
      
      # filter out if overlap.pct is tiny
      int <- int %>% filter(overlap.pct >= 1.0) 
      int.vector <- int %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
      int.vector <- int.vector %>% unname() %>% as.character()
      bottom.select <- bottom %>% filter((!! bottom.varQ) %in% int.vector)
      
      # reorder variables in int for viewing
      int <- int %>% select(!! bottom.varQ, overlap.pct, everything())
      
      # Save to data.frame
      temp <- data.frame(a = c(rep(selection, nrow(int))),
                         b = int %>% select(!! bottom.varQ) %>% st_set_geometry(NULL),
                         c = int %>% select(overlap.pct) %>% st_set_geometry(NULL))
      names(temp) <- c("Top_region", "Bottom_region", "Overlap_pct")
      output <- bind_rows(output, temp)
      assign(paste0("_", selection), temp)

      # Calculate centroids for bottom (int region) labels
      bottom.select$centroid <- st_centroid(st_geometry(bottom.select))

      # Create nice plots
      g1 <- ggplot() +
              geom_sf(data = top, colour = "grey75", fill = "grey95") +
              geom_sf(data = top.select, colour = "red", size = 0.5, fill = "red", alpha = 0.15) #+
              #labs(title = paste("Mornington peninsula townships:", selection))
              #ggsave(paste0(selection, "_i.png"), units = "cm", width = 12, height = 8, dpi = 300)

      g2 <- ggplot() +
              geom_sf(data = bottom.select, colour = "grey75", fill = "grey95") +
              geom_sf(data = top.select, colour = "red", size = 0.5, fill = "red", alpha = 0.15) +
              geom_point(data = st_coordinates(bottom.select$centroid) %>% as.data.frame, aes(x = X, y = Y)) +
              geom_text_repel(data = st_coordinates(bottom.select$centroid) %>% as.data.frame,
                              aes(x = X, y = Y, label = bottom.select$SA1_7DIG16), box.padding = 0.75, size = 2.5, force = 5) +
              labs(title = paste("Mornington peninsula townships:", selection),
                   subtitle = "These SA1s at least partially fall within the selected township zone")
              #ggsave(paste0(selection, "_ii.png"), units = "cm", width = 24, height = 16, dpi = 300)
      
      print(ggarrange(g2, g1, nrow = 2))
      ggsave(paste0(selection, "_iii.png"), units = "cm", width = 24, height = 24, dpi = 300)
      Sys.sleep(1)
      
    }

    # Save to excel file
    write_xlsx(output, "output.xlsx")
    
