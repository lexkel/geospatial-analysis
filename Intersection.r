  
  #==================================================================================
  #
  # Spatial shape comparison - single top layer region, exports table builder csv
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
    top <- st_read("./Custom/bendigo_eic.shp")
    #top <- top %>% filter(LGA_NAME16 == "Yarra Ranges (S)")
    
    # Bottom layer shapefile (smaller areas) - e.g. SA2s
    bottom <- st_read("./MB/MB_2016_VIC.shp")
  
  #----------------------------------------------------------------------------------
  # Check coordinate reference systems of both layers is the same - project if not
  #----------------------------------------------------------------------------------
  
    st_crs(top) == st_crs(bottom)  
  
  #----------------------------------------------------------------------------------
  # Selection variable names
  #----------------------------------------------------------------------------------    
  
    top.region.name = "Bendigo EIC"
    bottom.region.area = "MB"
    bottom.var <- "MB_CODE16"
    bottom.varQ <- quo(!! rlang::sym(bottom.var))
    type = "POUR"
  
  #----------------------------------------------------------------------------------
  # Determine overlapping regions with percentage area overlap
  #----------------------------------------------------------------------------------

    # Polygon of portions of bottom layer regions that fall within top layer region
    int <- st_intersection(bottom, top)  
    
    # Create vector of regions in int
    int.vector <- int %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
    int.vector <- int.vector %>% unname() %>% as.character()
    
    # filter bottom to just include the regions in int.vector
    bottom.select <- bottom %>% filter((!! bottom.varQ) %in% int.vector)
    
    # calculate overlap area percentage 100.00 = completely, <100.00 = somewhat
    int$overlap.pct <- round(as.numeric(st_area(int)) / 
                               as.numeric(st_area(bottom.select)) * 100, 2)  
    
    # filter out if overlap.pct is less than X
    int <- int %>% filter(overlap.pct > 1) 
    int.vector <- int %>% st_set_geometry(NULL) %>% select(!! bottom.varQ) %>% unlist()
    int.vector <- int.vector %>% unname() %>% as.character()
    bottom.select <- bottom %>% filter((!! bottom.varQ) %in% int.vector)
    
    # reorder variables in int for viewing
    int <- int %>% select(!! bottom.varQ, overlap.pct, everything())
  
    # Create a data frame to save results
    output <- data.frame(a = int %>% select(!! bottom.varQ) %>% st_set_geometry(NULL),
                         b = int %>% select(overlap.pct) %>% st_set_geometry(NULL))
    names(output) <- c("Region", "Overlap_pct")
    output <- arrange(output, Overlap_pct)
    
    # Calculate centroids for bottom (int region) labels
    bottom.select$centroid <- st_centroid(st_geometry(bottom.select))
    
    # Create plots to visualise the areas
      
      # Top
      ggplot() +
        geom_sf(data = top, colour = "grey75", fill = "grey95") +
        geom_sf(data = top, colour = "red", size = 0.5, fill = "red", alpha = 0.15) +
        labs(title = top.region.name,
             subtitle = "")
    
      # # Bottom
      # ggplot() +
      #   geom_sf(data = bottom.select, colour = "grey75", fill = "grey95") +
      #   geom_sf(data = top, colour = "red", size = 0.5, fill = "red", alpha = 0.15) +
      #   geom_point(data = st_coordinates(bottom.select$centroid) %>% as.data.frame, aes(x = X, y = Y)) +
      #   geom_text_repel(data = st_coordinates(bottom.select$centroid) %>% as.data.frame,
      #   aes(x = X, y = Y, label = bottom.select$SA1_7DIG16), box.padding = 0.75, size = 2.5, force = 5) +
      #   labs(title = top.region.name,
      #        subtitle = paste0("These ", bottom.region.area, " at least partially fall within the ", top.region.name))
      # 
      # # Overlapping areas
      # overlapping <- int %>% filter(overlap.pct < 100) %>% select(!! bottom.varQ)
      # st_geometry(overlapping) <- NULL
      # overlapping <- overlapping %>% unlist() %>% as.vector()
      # overlapping.centroid <- bottom.select %>% filter(!! bottom.varQ %in% overlapping)
      # st_geometry(overlapping.centroid) <- NULL
      # 
      # ggplot() +
      #   geom_sf(data = bottom.select %>% filter(!! bottom.varQ %in% overlapping), colour = "grey50", fill = "grey95") +
      #   geom_sf(data = top, colour = "red", size = 0.5, fill = "red", alpha = 0.15) +
      #   geom_point(data = st_coordinates(overlapping.centroid$centroid) %>% as.data.frame, aes(x = X, y = Y)) +
      #   geom_text_repel(data = st_coordinates(overlapping.centroid$centroid) %>% as.data.frame,
      #   aes(x = X, y = Y, label = overlapping.centroid$MB_CODE16), box.padding = 0.75, size = 2.5, force = 5) +
      #   labs(title = top.region.name,
      #        subtitle = paste0("The overlapping ", bottom.region.area, " within ", top.region.name))

    # Save to excel file
    write_xlsx(output, "output.xlsx")
  
  #----------------------------------------------------------------------------------
  # Create table builder custom geography tables
  #----------------------------------------------------------------------------------
  
    # Table builder custom geography
    table_builder <- data.frame("FactTableCode" = rep("Person Records", length(output$Region)),
                                "FactTableName" = rep(if(type == "POW") {"Persons Aged 15 Years and Over, Place of Work"} 
                                                      else if(type == "POUR") {"Persons Place of Usual Residence"}, 
                                                      length(output$Region)),
                                "FieldCode"     = rep(if(type == "POW") {"2189354"} 
                                                      else if(type == "POUR") {"2152296"}, 
                                                      length(output$Region)),
                                "FieldName"     = rep(if(type == "POW") {"Main Statistical Area Structure (Main ASGS) (POW)"} 
                                                      else if(type == "POUR") {"MB by Main Statistical Area Structure (Main ASGS) (UR)"}, 
                                                      length(output$Region)),
                                "ValueSetCode"  = rep(if(type == "POW") {paste0("2189354_", bottom.region.area)}  
                                                      else if(type == "POUR") {paste0("2152296_", bottom.region.area)}, 
                                                      length(output$Region)),
                                "ValueSetName"  = rep(if(type == "POW") {paste0("2189354_", bottom.region.area)} 
                                                      else if(type == "POUR") {paste0("2152296_", bottom.region.area)},
                                                      length(output$Region)),
                                "ValueCode"     = output$Region,
                                "ValueName"     = output$Region,
                                "GroupName"     = rep(paste0(type, " ", top.region.name), length(output$Region)))
    
    write.csv(table_builder, "table_builder.csv", row.names = FALSE)
    
    # WORK and NOT WORK
      # all.dzns <- read.csv("./DZN/TB_POW_DZN_2016_AUS.csv")
      # all.dzns <- lapply(all.dzns, as.character) %>% as.data.frame()
      # all.dzns <- all.dzns %>% mutate(region = case_when(ValueCode %in% int.vector ~ "in",
      #                                                    !ValueCode %in% int.vector ~ "out"))
      # 
      # region <- all.dzns %>% filter(region == "in") %>% select(-c(GroupName, region))
      # region$GroupName <- rep("WORK Monash NEIC", nrow(region))
      # write.csv(region, "WORK Monash NEIC.csv", row.names = FALSE)
      # 
      # not.region <- all.dzns %>% filter(region == "out") %>% select(-c(GroupName, region))
      # not.region$GroupName <- rep("NOT WORK Monash NEIC", nrow(not.region))
      # write.csv(not.region, "NOT WORK Monash NEIC.csv", row.names = FALSE)
    
    # RESI and NOT RESI
      # all.sa1s <- read.csv("./SA1/TB_POUR_SA1_2016_AUS.csv")
      # all.sa1s <- lapply(all.sa1s, as.character) %>% as.data.frame()
      # int.vector <- int %>% st_set_geometry(NULL) %>% select(SA1_7DIG16) %>% unlist()
      # all.sa1s <- all.sa1s %>% mutate(region = case_when(ValueCode %in% int.vector ~ "in",
      #                                                    !ValueCode %in% int.vector ~ "out"))
      # 
      # region <- all.sa1s %>% filter(region == "in") %>% select(-c(GroupName, region))
      # region$GroupName <- rep("RESI Monash NEIC", nrow(region))
      # write.csv(region, "RESI Monash NEIC.csv", row.names = FALSE) #Table builder custom geography format
      # 
      # not.region <- all.sa1s %>% filter(region == "out") %>% select(-c(GroupName, region))
      # not.region$GroupName <- rep("NOT RESI Monash NEIC", nrow(not.region))
      # write.csv(not.region, "NOT RESI Monash NEIC.csv", row.names = FALSE) #Table builder custom geography format
  

