
  #===================================================================================================
  #
  # Custom region dataset 
  #
  #   1. Run the geospatial script to get the custom geography csv for table builder
  #   2. Upload custom geography to table builder
  #   3. Download table builder data
  #   4. Run this script
  #
  #================================================================================================== 
  
    #----------------------------------------------------------------------------------
    # Setup
    #----------------------------------------------------------------------------------
    
      # Set environment
      rm(list=ls())
      library(sf)
      library(readxl)
      library(tidyverse)
      
      # Set directory
      setwd("C:/Users/a.kelly/Documents/Projects/Custom geography datasets") 
      
    #----------------------------------------------------------------------------------
    # Import data
    #----------------------------------------------------------------------------------

      # Organise lga
      lga.scaled <- read_xlsx("./POW_scaled.xlsx", sheet = "LGA")
      lga.scaled <- lga.scaled %>% slice(c(1,3:nrow(lga.scaled)))
      lga.scaled <- lga.scaled %>% mutate(IND.ID = paste0(`LGA (POW)`, "---", X__1)) %>% select(IND.ID, everything())
      colnames(lga.scaled) <- paste0(colnames(lga.scaled), "---", lga.scaled[1,])
      lga.scaled <- lga.scaled %>% slice(c(2:nrow(lga.scaled))) %>% select(-c(2:3))
      lga.scaled.tidy <- lga.scaled %>% gather("lga", "Value", -1) %>% select("Industry" = 1, everything())
      lga.scaled.tidy <- lga.scaled.tidy %>% separate(Industry, into = c("IND.code", "IND.name"), sep = "---") %>% 
                                              separate(lga, into = c("lga.code", "lga.name"), sep = "---")
      
      # Organise sa2
      sa2.scaled <- read_xlsx("./POW_scaled.xlsx", sheet = "SA2")
      sa2.scaled <- sa2.scaled %>% slice(c(1,3:nrow(sa2.scaled)))
      sa2.scaled <- sa2.scaled %>% mutate(IND.ID = paste0(`SA2 (POW)`, "---", X__1)) %>% select(IND.ID, everything())
      colnames(sa2.scaled) <- paste0(colnames(sa2.scaled), "---", sa2.scaled[1,])
      sa2.scaled <- sa2.scaled %>% slice(c(2:nrow(sa2.scaled))) %>% select(-c(2:3))
      sa2.tidy <- sa2.scaled %>% gather("sa2", "Value", -1) %>% select("Industry" = 1, everything())
      sa2.tidy <- sa2.tidy %>% separate(Industry, into = c("IND.code", "IND.name"), sep = "---") %>% 
                                separate(sa2, into = c("sa2.code", "sa2.name"), sep = "---")
      
      # Organise sa4
      sa4.scaled <- read_xlsx("./POW_scaled.xlsx", sheet = "SA4")
      sa4.scaled <- sa4.scaled %>% slice(c(1,3:nrow(sa4.scaled)))
      sa4.scaled <- sa4.scaled %>% mutate(IND.ID = paste0(`SA4 (POW)`, "---", X__1)) %>% select(IND.ID, everything())
      colnames(sa4.scaled) <- paste0(colnames(sa4.scaled), "---", sa4.scaled[1,])
      sa4.scaled <- sa4.scaled %>% slice(c(2:nrow(sa4.scaled))) %>% select(-c(2:3))
      sa4.tidy <- sa4.scaled %>% gather("sa4", "Value", -1) %>% select("Industry" = 1, everything())
      sa4.tidy <- sa4.tidy %>% separate(Industry, into = c("IND.code", "IND.name"), sep = "---") %>% 
                                separate(sa4, into = c("sa4.code", "sa4.name"), sep = "---")
      
    #----------------------------------------------------------------------------------
    # Save data image
    #----------------------------------------------------------------------------------
      
      save.image(file = "POW.scaled.rData")
      