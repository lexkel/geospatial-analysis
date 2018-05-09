
  #===================================================================================================
  #
  # Custom region dataset 
  #
  #   1. Run the geospatial script to get the custom geography csv for table builder
  #   2. Upload custom geography csv to table builder
  #   3. Download custom geography POW data
  #   4. Run this script
  #
  #================================================================================================== 
  
    #----------------------------------------------------------------------------------
    # Setup
    #----------------------------------------------------------------------------------
    
      # Set environment
      rm(list=ls())
      library(sf)
      library(xlsx)
      library(readxl)
      library(tidyverse)
      
      # Set directory
      setwd("C:/Users/a.kelly/Documents/Projects/Custom geography datasets") 
      
      # Names
      lga.name = "Yarra Ranges (S)"
      custom.name = "Lilydale"
      
    #----------------------------------------------------------------------------------
    # Import data
    #----------------------------------------------------------------------------------
      
      # Read in tidy scaled POW data (LGA, SA2 & SA4)
      load("C:/Users/a.kelly/Documents/Projects/Custom geography datasets/POW.scaled.rData")
      lga.scaled <- lga.scaled.tidy %>% filter(lga.name == "Yarra Ranges (S)") %>% filter(IND.name != c("Population (Scaled)", "Population (Census)"))
      rm(list = c("sa2.scaled", "sa2.tidy", "sa4.scaled", "sa4.tidy", "lga.scaled.tidy"))
      lga.scaled$Value <- as.numeric(lga.scaled$Value)
      lga.scaled$Value[is.na(lga.scaled$Value)] <- 0
      
      # Read in table builder destination zone POW data
      dzns.in.lga <- read_xlsx("C:/Users/a.kelly/Documents/Projects/Custom geography datasets/Lilydale/POW - Yarra Ranges.xlsx")
      #dzns.in.lga <- read.csv("C:/Users/a.kelly/Documents/Projects/Custom geography datasets/Lilydale/POW - Yarra Ranges.csv")
      dzns.in.lga <- dzns.in.lga %>% slice(2:(nrow(dzns.in.lga)-1)) %>% select(-Total)
      
    #----------------------------------------------------------------------------------
    # Prepare destination zones for scaling
    #----------------------------------------------------------------------------------     

      # Adjustment factor for destination zones that don't fall exactly within LGA region (visually inspect them first)
      factor <- read_xlsx("C:/Users/a.kelly/Documents/Projects/Custom geography datasets/Lilydale/yarraranges_dzns.xlsx")
      
      # Round adjustment factor to nearest 5 
      factor$Overlap_pct <- (5 * round(factor$Overlap_pct/5)) / 100
      
      # Order factor vector to line up with the right destination zone 
      # ***check this step***
      factor$index <- match(t(factor$Region), colnames(dzns.in.lga[,2:ncol(dzns.in.lga)]))
      factor <- arrange(factor, index) %>% select(Overlap_pct) %>% unlist() 
      attributes(factor) <- NULL
      
      # Apply adjustment factor to data (to account for overflowing destination zones)
      IND.code <- dzns.in.lga[,1]
      dzns.in.lga <- data.frame(mapply(`*`,dzns.in.lga[,2:ncol(dzns.in.lga)], factor))
      dzns.in.lga <- bind_cols(IND.code, dzns.in.lga)
      dzns.in.lga$total <- rowSums(dzns.in.lga[,2:ncol(dzns.in.lga)])
      colnames(dzns.in.lga) <- gsub("X", "", colnames(dzns.in.lga))
      rm(list = "IND.code")
      
      # Make any other manual additions to pow data for partial overlapping areas (such as zeroing the mining jobs in one zone etc...)
      # i = match("8210", dzns.in.lga[,1] %>% unlist())   # row number
      # j = match("X212742508", colnames(dzns.in.lga))    # column number
      # dzns.in.lga[i,j] <- 0

    #----------------------------------------------------------------------------------
    # Scale to LGA totals
    #----------------------------------------------------------------------------------          
      
      # Calculate difference between destination zone row sums and scaled LGA total
      joined <- left_join(lga.scaled, dzns.in.lga, by = c("IND.code" = "DZN (POW)"))
      joined <- joined %>% slice(1:(nrow(joined)-1))
      joined <- joined %>% mutate(difference = Value - total) %>% filter(IND.code != "Total")
    
      # Scale destination zones
      lstA <- joined[, 6:(ncol(joined)-2)]                              # row data
      lstB <- replicate(ncol(lstA), joined$difference) %>% data.frame() # row differences
      lstC <- replicate(ncol(lstA), joined$total) %>% data.frame()      # row sums
      lstD <- lstA/lstC
      
      dzns.scaled <- pmap(list(lstA, lstB, lstC, lstD), function(data, diff, total, pct){
                          ifelse(diff == 0, data,
                          ifelse(diff != 0 & total == 0, (1/ncol(lstA)) * diff,
                                 data + pct * diff))})
      
      dzns.scaled <- dzns.scaled %>% data.frame()
      colnames(dzns.scaled) <- gsub("X", "", colnames(dzns.scaled))

      # Check totals to ensure scaling is correct
      round(rowSums(dzns.scaled, na.rm = TRUE), 2)
      sum(rowSums(dzns.scaled, na.rm = TRUE))
      
      round(colSums(dzns.scaled, na.rm = TRUE), 2)
      sum(colSums(dzns.scaled, na.rm = TRUE))

    #----------------------------------------------------------------------------------
    # Create custom region from scaled destination zones
    #---------------------------------------------------------------------------------- 
              
      # Assign destination zones in the custom area
      dzns.in.custom <- c("212782531", "212782538", "212782539", "212782543", "212782544", "212782545", "212782546", "212782547", "212782548")
      dzns.in.custom <- quo(!! rlang::sym(dzns.in.custom))
      
      # Filter for custom region destination zones
      custom.region <- dzns.scaled %>% select(dzns.in.custom)
      
      # Add total row and column
      custom.region$Total <- rowSums(custom.region)
      custom.region <- custom.region %>% bind_rows(colSums(custom.region))
      
      # Add ind.code column
      Ind.code <- append(joined$IND.code[1:nrow(joined)], "Total")
      custom.region$Ind.code <- as.factor(Ind.code)
      custom.region <- custom.region %>% select(Ind.code, everything())
      
      # Reorder for IOIG conversion
      ind.code.order <- read.csv("industry code order.csv")
      custom.region <- ind.code.order %>% left_join(custom.region)
    
      # Tidy up
      rm(list = c("lstA", "lstB", "lstC", "lstD", "joined", "ind.code.order", "factor", "Ind.code", "dzns.in.lga", "dzns.in.custom"))
    
    #----------------------------------------------------------------------------------
    # Format dzns.scaled for spreadsheet
    #----------------------------------------------------------------------------------   
        
      # Add total row and column
      dzns.scaled$Total <- rowSums(dzns.scaled)
      dzns.scaled <- dzns.scaled %>% bind_rows(colSums(dzns.scaled))
      
      # Add ind.code column
      Ind.code <- custom.region$Ind.code
      dzns.scaled$Ind.code <- as.factor(Ind.code)
      dzns.scaled <- dzns.scaled %>% select(Ind.code, everything())
      
      # Reorder for IOIG conversion
      ind.code.order <- read.csv("industry code order.csv")
      dzns.scaled <- ind.code.order %>% left_join(dzns.scaled)
      
      # Add scaled LGA **************************
      dzns.scaled <- dzns.scaled %>% left_join(lga.scaled %>% select(Scaled.LGA = Value, IND.code), by = c("Ind.code" = "IND.code"))
      
    #----------------------------------------------------------------------------------
    # Create custom region from scaled destination zones
    #---------------------------------------------------------------------------------- 
      
      # Save 
      wb = createWorkbook()
      sheet = createSheet(wb, custom.name)
      addDataFrame(custom.region, sheet=sheet, startColumn=1, row.names=FALSE)
      sheet = createSheet(wb, lga.name)
      addDataFrame(dzns.scaled, sheet=sheet, startColumn=1, row.names=FALSE)
      saveWorkbook(wb, paste0(custom.name, ".xlsx"))
      
      save.image("scaling.rData")
      
      
      