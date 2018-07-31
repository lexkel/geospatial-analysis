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
  lga <- st_read("./LGA/LGA_2016_AUST.shp")
  
  areas <- st_read("./RA/RA_2016_AUST.shp") %>% 
            filter(RA_NAME16 %in% c("Inner Regional Australia",
                                    "Outer Regional Australia",
                                    "Major Cities of Australia",
                                    "Remote Australia",
                                    "Very Remote Australia"))
  
  areas <- areas %>% 
            st_simplify(dTolerance = 0.1) %>% 
            group_by(RA_NAME16)

  ggplot(data = areas) +
    geom_sf(aes(fill = RA_NAME16))

  x <- st_intersection(lga, areas)
  x <- x %>% 
        #st_set_geometry(NULL) %>% 
        mutate(Remoteness = case_when(RA_NAME16 == "Very Remote Australia" ~ 5,
                                      RA_NAME16 == "Remote Australia" ~ 4,
                                      RA_NAME16 == "Outer Regional Australia" ~ 3,
                                      RA_NAME16 == "Inner Regional Australia" ~ 2,
                                      RA_NAME16 == "Major Cities of Australia" ~ 1))
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  x <- x %>% 
        group_by(LGA_CODE16) %>% 
        summarise(Remoteness_code = getmode(Remoteness)) %>%
        mutate(Remoteness_area = case_when(Remoteness_code == 5 ~ "Very Remote Australia",
                                           Remoteness_code == 4 ~ "Remote Australia",
                                           Remoteness_code == 3 ~ "Outer Regional Australia",
                                           Remoteness_code == 2 ~ "Inner Regional Australia",
                                           Remoteness_code == 1 ~ "Major Cities of Australia"))
  
  ggplot(data = x) +
    geom_sf(aes(fill = Remoteness_code))
  
  write.csv(x %>% st_set_geometry(NULL), "lga_remoteness.csv")
  
