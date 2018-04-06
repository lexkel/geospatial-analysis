  
  #==================================================================================
  #
  # Spatial shape comparison - single top layer region
  #
  #  - Determine what lower grade shapes fall within a higher grade shape.
  #  - Example here is Lake Macquarie LGA and which SA2s comprise it.
  #
  #==================================================================================
  
  # Setup
    rm(list=ls())
    library(sf)
    library(tidyverse)
    library(lazyeval)
    library(writexl)
    library(ggrepel)
    library(ggpubr)
  
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
    top <- st_read("./Custom/lilydale.shp")
    #top <- st_buffer(top, 0)
    #top <- top %>% filter(LGA_NAME16 == "Wyndham (C)")
  
  # middle layer shapefile (smaller areas) - e.g. SA2s
    middle <- st_read("./SA1/SA1_2016_AUST.shp")
    #middle <- st_buffer(middle, 0)
    
  # bottom layer
    bottom <- st_read("./MB/MB_2016_VIC.shp")
    #bottom <- st_buffer(middle, 0)
    
  #----------------------------------------------------------------------------------
  # Check coordinate reference systems of both layers is the same - project if not
  #----------------------------------------------------------------------------------
  
  st_crs(top) == st_crs(middle) 
  st_crs(top) == st_crs(bottom)
  
  #----------------------------------------------------------------------------------
  # Selection variable names
  #----------------------------------------------------------------------------------    
  
  top.region.name = "Lilydale"
  
  middle.region.area = "SA1"
  middle.var <- "SA1_MAIN16"
  middle.varQ <- quo(!! rlang::sym(middle.var))
  
  bottom.region.area = "MB"
  bottom.var <- "MB_CODE16"
  bottom.varQ <- quo(!! rlang::sym(bottom.var))
  
  #--------------------
  # First tier (middle)
  #--------------------

  # Polygon of middle layer regions that fall within top layer region
  int <- st_intersection(middle, top)  
  
  # Create vector of regions in int
  int.vector <- int %>% st_set_geometry(NULL) %>% select(!! middle.varQ) %>% unlist()
  int.vector <- int.vector %>% unname() %>% as.character()
  
  # filter middle to just include the regions in int.vector
  middle.select <- middle %>% filter((!! middle.varQ) %in% int.vector)
  
  # calculate overlap area percentage 100.00 = completely, <100.00 = somewhat
  int$overlap.pct <- round(as.numeric(st_area(int)) / 
                             as.numeric(st_area(middle.select)) * 100, 2)  
  
  # filter out if overlap.pct is less than X
  int <- int %>% filter(overlap.pct > 1) 
  int.vector <- int %>% st_set_geometry(NULL) %>% select(!! middle.varQ) %>% unlist()
  int.vector <- int.vector %>% unname() %>% as.character()
  middle.select <- middle %>% filter((!! middle.varQ) %in% int.vector)
  
  # reorder variables in int for viewing
  int <- int %>% select(!! middle.varQ, overlap.pct, everything())

  # Create a data frame to save results
  output <- data.frame(a = rep(middle.region.area, nrow(int)),
                       b = int %>% select(!! middle.varQ) %>% st_set_geometry(NULL),
                       c = int %>% select(overlap.pct) %>% st_set_geometry(NULL))
  names(output) <- c("Class", "Region", "Overlap_pct")
  
  # Calculate centroids for middle (int region) labels
  middle.select$centroid <- st_centroid(st_geometry(middle.select))
  
  # Create plots to visualise the areas
  ggplot() +
    geom_sf(data = middle.select, colour = "grey75", fill = "grey95") +
    geom_sf(data = top, colour = "red", size = 0.75, fill = "red", alpha = 0.15) +
    geom_point(data = st_coordinates(middle.select$centroid) %>% as.data.frame, aes(x = X, y = Y)) +
    geom_text_repel(data = st_coordinates(middle.select$centroid) %>% as.data.frame,
    aes(x = X, y = Y, label = middle.select$SA1_MAIN16), box.padding = 0.75, size = 2.5, force = 5) +
    labs(title = top.region.name,
         subtitle = paste0("These ", middle.region.area, " regions fall wholely or partially within ", top.region.name, " (red zone)"))
  
  #--------------------
  # Next tier (bottom)
  #--------------------
  
  # Vector of middle area to be split into bottom regions
  boundary.areas <- c("21305146707", "21305136806", "21305136804", "21305136821", "21305136809", "21305146804")

  # Filter by boundary.areas
  middle.omit <- middle.select %>% filter((!! middle.varQ) %in% boundary.areas)
  
  # ggplot() + 
  #   geom_sf(data = middle.omit) +
  #   geom_sf(data = top, col = "red", alpha = 0.1)
  
  # Polygon of bottom layer regions that fall within middle layer regions
  int2 <- st_intersection(bottom, middle.omit) 
  int2.vector <- int2 %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
  int2.vector <- int2.vector %>% unname() %>% as.character()
  bottom.select <- bottom %>% filter((!! bottom.varQ) %in% int2.vector)
  
  int2 <- st_intersection(bottom.select, top) 
  int2.vector <- int2 %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
  int2.vector <- int2.vector %>% unname() %>% as.character()
  bottom.select <- bottom %>% filter((!! bottom.varQ) %in% int2.vector)
  
  # calculate overlap area percentage
  int2$overlap.pct <- NULL
  int2$overlap.pct <- round(as.numeric(st_area(int2)) / 
                              as.numeric(st_area(bottom.select)) * 100, 2)

  # filter out if overlap.pct is less than X
  int2 <- int2 %>% filter(overlap.pct > 1) 
  int2.vector <- int2 %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
  int2.vector <- int2.vector %>% unname() %>% as.character()
  bottom.select <- bottom.select %>% filter((!! bottom.varQ) %in% int2.vector)
  
  # ggplot() + 
  #   geom_sf(data = bottom.select) +
  #   geom_sf(data = top, col = "red", alpha = 0.1)
  
  int2 <- st_intersection(bottom.select, middle.omit) 
  int2.vector <- int2 %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
  int2.vector <- int2.vector %>% unname() %>% as.character()
  bottom.select <- bottom.select %>% filter((!! bottom.varQ) %in% int2.vector)
  
  # calculate overlap area percentage
  int2$overlap.pct <- NULL
  int2$overlap.pct <- round(as.numeric(st_area(int2)) / 
                              as.numeric(st_area(bottom.select)) * 100, 2)
  
  # filter out if overlap.pct is less than X
  int2 <- int2 %>% filter(overlap.pct > 1) 
  int2.vector <- int2 %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
  int2.vector <- int2.vector %>% unname() %>% as.character()
  bottom.select <- bottom.select %>% filter((!! bottom.varQ) %in% int2.vector)
  
  # ggplot() + 
  #   geom_sf(data = bottom.select) +
  #   geom_sf(data = top, col = "red", alpha = 0.1)

  # Calculate centroids for labels on plot
  bottom.select$centroid <- st_centroid(st_geometry(bottom.select))
  bottom.coord <- st_coordinates(bottom.select$centroid) %>% as.data.frame()
  bottom.coord$Z <- bottom.select$MB_CODE16
  
  middle.coord <- st_coordinates(middle.select$centroid) %>% as.data.frame()
  middle.coord$Z <- middle.select$SA1_7DIG16
  middle.coord$ZZ <- middle.select$SA1_MAIN16
  drop <- middle.coord$ZZ %in% boundary.areas
  middle.coord <- middle.coord[drop == "FALSE",]
  middle.coord <- middle.coord %>% select(-ZZ)
  
  coord <- bind_rows(bottom.coord, middle.coord)
  
  # Create plots to visualise the areas
  ggplot() +
    geom_sf(data = middle.select, colour = "grey75", fill = "grey90", alpha = 0.15) +
    geom_sf(data = bottom.select, colour = "grey50", fill = "grey75") +
    geom_sf(data = top, colour = "red", size = 0.75, fill = "red", alpha = 0.1) +
    geom_point(data = coord, aes(x = X, y = Y)) +
    geom_text_repel(data = coord, aes(x = X, y = Y, label = Z), box.padding = 0.75, size = 2.5, force = 1.5) +
    labs(title = top.region.name,
         subtitle = paste0("These ", middle.region.area, " and ", bottom.region.area, " fall within the ", top.region.name, " (red zone)"),
         caption = paste0("The darker shade areas are ", bottom.region.area, " the lighter shade are ", middle.region.area))
  
  output2 <- data.frame(a = rep(bottom.region.area, nrow(int2)),
                        b = int2 %>% select(!! bottom.varQ) %>% st_set_geometry(NULL),
                        c = int2 %>% select(overlap.pct) %>% st_set_geometry(NULL))
  names(output2) <- c("Class", "Region", "Overlap_pct")
  
  '%not in%' <- Negate('%in%')
  output <- output %>% filter(Region %not in% boundary.areas)
  output <- bind_rows(output, output2)
  
  # Recalculate overlap pct for output
  selection <- output$Region %>% unlist()
  middle.select <- middle %>% filter((!! middle.varQ) %in% selection) %>% filter((!! middle.varQ) %not in% boundary.areas)
  bottom.select <- bottom %>% filter((!! bottom.varQ) %in% selection)
  
  int <- st_intersection(middle.select, top)
  int$overlap.pct <- round(as.numeric(st_area(int)) / 
                             as.numeric(st_area(middle.select)) * 100, 2)
  st_geometry(int) <- NULL
  
  int2 <- st_intersection(bottom.select, top)
  int2$overlap.pct <- round(as.numeric(st_area(int2)) / 
                              as.numeric(st_area(bottom.select)) * 100, 2)
  st_geometry(int2) <- NULL
  
  output <- data.frame(a = rep(middle.region.area, nrow(int)),
                        b = int %>% select(!! middle.varQ),
                        c = int %>% select(overlap.pct))
  names(output) <- c("Class", "Region", "Overlap_pct")
  
  output2 <- data.frame(a = rep(bottom.region.area, nrow(int2)),
                       b = int2 %>% select(!! bottom.varQ),
                       c = int2 %>% select(overlap.pct))
  names(output2) <- c("Class", "Region", "Overlap_pct")
  
  output <- bind_rows(output, output2)
  
  # Save to excel file
  write_xlsx(output, "output.xlsx")
  
  
  
